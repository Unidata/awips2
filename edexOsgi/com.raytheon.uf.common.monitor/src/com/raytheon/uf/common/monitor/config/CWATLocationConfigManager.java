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

import java.io.File;
import java.util.ArrayList;

import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.ThreatLocation;
import com.raytheon.uf.common.monitor.scan.xml.CWATLocationsXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.vividsolutions.jts.geom.Coordinate;

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
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class CWATLocationConfigManager {

    private static final SingleTypeJAXBManager<CWATLocationsXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(CWATLocationsXML.class);

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

    /* Private Constructor */
    private CWATLocationConfigManager() {
        configXml = new CWATLocationsXML();
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
     * Read the XML configuration data for the current XML file name.
     */
    public void readConfigXml() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File file = pm.getFile(lc, "cwat" + File.separatorChar
                    + getSiteName() + "Locations.xml");
            System.out.println("Reading -- " + file.getAbsolutePath());

            CWATLocationsXML configXmltmp = jaxb.unmarshalFromXmlFile(file
                    .getAbsolutePath());

            configXml = configXmltmp;

        } catch (Exception e) {
            System.err.println("No CWAT locations file found");
            // create a new one
            ArrayList<ThreatLocation> locations = ScanUtils.getCWASites(
                    getSiteCoor(), getCRS());
            configXml = new CWATLocationsXML();
            configXml.setThreats(locations);
            // writes one to site
            saveConfigXml();
        }
    }

    /**
     * Save the XML configuration data to the current XML file name.
     */
    public void saveConfigXml() {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc, "cwat"
                + File.separatorChar + getSiteName() + "Locations.xml");

        if (newXmlFile.getFile().getParentFile().exists() == false) {
            // System.out.println("Creating new directory");

            if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
                // System.out.println("Could not create new directory...");
            }
        }

        try {
            System.out.println("Saving -- "
                    + newXmlFile.getFile().getAbsolutePath());
            jaxb.marshalToXmlFile(configXml, newXmlFile.getFile()
                    .getAbsolutePath());
            newXmlFile.save();
        } catch (Exception e) {
            e.printStackTrace();
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
