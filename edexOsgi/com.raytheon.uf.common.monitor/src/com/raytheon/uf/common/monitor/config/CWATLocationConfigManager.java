package com.raytheon.uf.common.monitor.config;

/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.ThreatLocation;
import com.raytheon.uf.common.monitor.scan.xml.CWATLocationsXML;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import org.locationtech.jts.geom.Coordinate;

/**
 * Manages the configurations for CWAT locations
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * Oct 02, 2013 2361       njensen     Use JAXBManager for XML
 * Oct 08, 2015 4912       rferrel     Update configXml when configuration file changes
 *                                      and removed deprecated code.
 * Jan 12, 2016 5244       njensen     Replaced calls to deprecated LocalizationFile methods
 * May 18, 2018 6661       tgurney     Move generated files to configured level
 *
 * </pre>
 *
 * @author lvenable
 */
public class CWATLocationConfigManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWATLocationConfigManager.class);

    private static final SingleTypeJAXBManager<CWATLocationsXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(CWATLocationsXML.class);

    /**
     * Location of configuration files.
     */
    private static final String CWAT_DIR = "cwat";

    /**
     * Append to the site name to get the configuration file name.
     */
    private static final String CONFIG_FILE_SUFFIX = "Locations.xml";

    /**
     * FFMP Source Configuration XML object.
     */
    protected CWATLocationsXML configXml;

    /**
     * Site coordinate
     */
    public Coordinate siteCoor = null;

    /**
     * Site Name
     */
    public String siteName = null;

    /**
     * Site CRS
     */
    public ProjectedCRS crs = null;

    /** Singleton instance of this class */
    private static CWATLocationConfigManager instance = null;

    /**
     * Observer to force updating the configXml.
     */
    private ILocalizationPathObserver configXmlObserver = new ILocalizationPathObserver() {
        @Override
        public void fileChanged(ILocalizationFile file) {
            readConfigXml();
        }
    };

    /**
     * Private Constructor.
     */
    private CWATLocationConfigManager() {
        configXml = new CWATLocationsXML();
        IPathManager pm = PathManagerFactory.getPathManager();
        /*
         * No need to remove the observer since the instance of this class
         * remains until the JRE is shutdown.
         */
        pm.addLocalizationPathObserver(CWAT_DIR, configXmlObserver);
    }

    /**
     * Get an instance of this singleton.
     *
     * @return Instance of this class
     */
    public static synchronized CWATLocationConfigManager getInstance() {
        if (instance == null) {
            instance = new CWATLocationConfigManager();
        }

        return instance;
    }

    private ILocalizationFile getConfigFile(LocalizationLevel level) {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                level);
        return pm.getLocalizationFile(lc, CWAT_DIR + IPathManager.SEPARATOR
                + getSiteName() + CONFIG_FILE_SUFFIX);
    }

    /**
     * Read the XML configuration data for the current XML file name with
     * coordinate
     */
    public void readConfigXml(Coordinate siteCoor, String siteName,
            ProjectedCRS crs) {
        this.siteCoor = siteCoor;
        this.siteName = siteName;
        this.crs = crs;
        readConfigXml();
    }

    /**
     * Read the XML configuration data for the current XML file name. Tries SITE
     * level first, then CONFIGURED. If neither exists, creates new default
     * config at CONFIGURED.
     */
    public void readConfigXml() {
        CWATLocationsXML tmpConfigXml = null;
        for (LocalizationLevel level : new LocalizationLevel[] {
                LocalizationLevel.SITE, LocalizationLevel.CONFIGURED }) {
            ILocalizationFile lFile = getConfigFile(level);
            try {
                if (lFile.exists()) {
                    statusHandler.info("Reading CWAT configuration file: "
                            + lFile.getPath());
                    try (InputStream stream = lFile.openInputStream()) {
                        tmpConfigXml = jaxb.unmarshalFromInputStream(stream);
                        break;
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "Unable to load location file: " + lFile.getPath(), e);
            }
        }
        if (tmpConfigXml == null) {
            statusHandler.handle(Priority.WARN,
                    "No CWAT locations file found. Generating the file: "
                            + getConfigFile(LocalizationLevel.CONFIGURED)
                                    .getPath());
            createNewConfigXml();
        } else {
            configXml = tmpConfigXml;
        }
    }

    /**
     * Create new config XML at CONFIGURED localization level.
     */
    public void createNewConfigXml() {
        ArrayList<ThreatLocation> locations = ScanUtils
                .getCWASites(getSiteCoor(), getCRS());
        configXml = new CWATLocationsXML();
        configXml.setThreats(locations);
        ILocalizationFile lFile = getConfigFile(LocalizationLevel.CONFIGURED);
        statusHandler
                .info("Saving CWAT configuration file: " + lFile.getPath());
        try (SaveableOutputStream stream = lFile.openOutputStream()) {
            jaxb.marshalToStream(configXml, stream);
            stream.save();
        } catch (LocalizationException | SerializationException
                | IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to save localized file: " + lFile.getPath(), e);
        }
    }

    public Coordinate getSiteCoor() {
        return siteCoor;
    }

    public void setSiteCoor(Coordinate siteCoor) {
        this.siteCoor = siteCoor;
    }

    public String getSiteName() {
        return siteName;
    }

    public void setSiteCoor(String siteName) {
        this.siteName = siteName;
    }

    public ProjectedCRS getCRS() {
        return crs;
    }

    public void setCRS(ProjectedCRS crs) {
        this.crs = crs;
    }

    /**
     * Get the list of threat locations
     *
     * @return
     */
    public ArrayList<ThreatLocation> getLocations() {
        return configXml.getThreats();
    }
}
