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
package com.raytheon.uf.common.monitor.cpg;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Monitor State Configuration XML File Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 05, 2009            dhladky     Initial creation
 * Oct 01, 2013 2361       njensen     Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MonitorStateConfigurationManager {

    /** Path to Monitoring Area Configuration XML. */
    private static final String CONFIG_FILE_NAME = "monitoring"
            + File.separatorChar + "MonitorPluginState.xml";

    private static final SingleTypeJAXBManager<MonitorStateXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(MonitorStateXML.class);

    /** Singleton instance of this class */
    private static MonitorStateConfigurationManager instance = null;

    /**
     * Monitoring State Configuration XML object.
     */
    private MonitorStateXML configXml;

    /* Private Constructor */
    private MonitorStateConfigurationManager() {
        configXml = new MonitorStateXML();
        readConfigXml();
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static synchronized MonitorStateConfigurationManager getInstance() {
        if (instance == null) {
            instance = new MonitorStateConfigurationManager();
        }

        return instance;
    }

    /**
     * Save the XML configuration data to the current XML file name.
     */
    public void saveConfigXml() {
        // Save the xml object to disk
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                CONFIG_FILE_NAME);

        if (newXmlFile.getFile().getParentFile().exists() == false) {
            System.out.println("Creating new directory");

            if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("Could not create new directory...");
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

        readConfigXml();
    }

    /**
     * Read the XML configuration data for the current XML file name.
     */
    public void readConfigXml() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            File file = pm.getFile(lc, CONFIG_FILE_NAME);
            System.out.println("Reading -- " + file.getAbsolutePath());
            MonitorStateXML configXmltmp = jaxb.unmarshalFromXmlFile(file
                    .getAbsolutePath());
            configXml = configXmltmp;

        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("No configuration file found");
        }
    }

    /**
     * FFMP state
     * 
     * @return
     */
    public boolean getFFMPState() {
        return configXml.isFfmp();
    }

    /**
     * CWAT state
     * 
     * @return
     */
    public boolean getCWATState() {
        return configXml.isCwat();
    }

    /**
     * VIL state
     * 
     * @return
     */
    public boolean getVILState() {
        return configXml.isVil();
    }

    /**
     * QPF state
     * 
     * @return
     */
    public boolean getQPFState() {
        return configXml.isQpf();
    }

    /**
     * Fog state
     * 
     * @return
     */
    public boolean getFogState() {
        return configXml.isFog();
    }

    /**
     * Fog state
     * 
     * @return
     */
    public boolean getPrecipRateState() {
        return configXml.isPrecipRate();
    }

    /**
     * HydroDualPol state
     * @return
     */
    public boolean getHydroDualPolState() {
        return configXml.isHydroDualPol();
    }
    
    /**
     * Fog state
     * 
     * @return
     */
    public boolean getScanState() {
        return configXml.isScan();
    }

    /**
     * Fog state
     * 
     * @return
     */
    public boolean getFSSState() {
        return configXml.isFssobs();
    }
}
