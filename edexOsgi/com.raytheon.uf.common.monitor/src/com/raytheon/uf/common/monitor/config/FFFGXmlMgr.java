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
package com.raytheon.uf.common.monitor.config;

import java.util.ArrayList;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.xml.FFFGDataXML;
import com.raytheon.uf.common.monitor.xml.FFFGSourceItemXML;
import com.raytheon.uf.common.monitor.xml.FFFGSourceXML;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * 
 * FFFG XML manager for the master data and the user data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010 #4517      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FFFGXmlMgr {
    /**
     * Data XML.
     */
    private FFFGDataXML dataXML;

    /**
     * Data file name.
     */
    private String dataFileName = null;

    /**
     * Path to the data file.
     */
    private String dataFilePath;

    /**
     * XML type enumeration.
     */
    public static enum FFFGXmlType {
        MASTER, USER
    };

    /**
     * XML type.
     */
    private FFFGXmlType xmlType;

    /**
     * Forcing configured flag.
     */
    private boolean forcingConfigured = false;

    /**
     * Constructor.
     * 
     * @param dataFileName
     *            Data file name.
     * @param dataFilePath
     *            Path to the data file.
     * @param xmlType
     */
    public FFFGXmlMgr(String dataFileName, String dataFilePath,
            FFFGXmlType xmlType) {
        this.dataFileName = dataFileName;
        this.xmlType = xmlType;
        this.dataFilePath = dataFilePath;
    }

    /**
     * Get the data file name.
     * 
     * @return The data file name.
     */
    public String getDataFileName() {
        return this.dataFileName;
    }

    /**
     * Set the data XML.
     * 
     * @param newDataXML
     *            New data XML.
     */
    public void setDataXML(FFFGDataXML newDataXML) {
        this.dataXML = newDataXML;
    }

    /**
     * Get the data XML.
     * 
     * @return The data XML.
     */
    public FFFGDataXML getXMLData() {
        return this.dataXML;
    }

    /**
     * Load the data file.
     * 
     * @param dataFileName
     *            Data file name.
     */
    public void loadDataFile(String dataFileName) {
        if (xmlType == FFFGXmlType.USER) {
            this.dataFileName = dataFileName;
            readThresholdXml();
        }
    }

    /**
     * Read in the XML.
     */
    public void readThresholdXml() {
        try {
            String fullPathAndFileName = dataFilePath + dataFileName;
            dataXML = null;
            IPathManager pm = PathManagerFactory.getPathManager();
            String path = pm.getStaticFile(fullPathAndFileName)
                    .getAbsolutePath();
            // System.out.println("*** reading XML path = " + path);

            dataXML = (FFFGDataXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(path);
            setForcingConfigured();
        } catch (Exception e) {
            // e.printStackTrace();
            System.out.println("*** FFFGMasterData.xml not available.");
            dataXML = null;
        }
    }

    /**
     * Save the XML to a different file name.
     * 
     * @param newFileName
     *            New file name.
     */
    public void saveFFFGDataXmlAs(String newFileName) {
        if (xmlType == FFFGXmlType.USER) {
            this.dataFileName = newFileName;
            saveFFFGDataXml();
        }
    }

    /**
     * Save the XML to the current file name.
     */
    public void saveFFFGDataXml() {
        String fullPathAndFileName = dataFilePath + dataFileName;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm.getLocalizationFile(context,
                fullPathAndFileName);

        if (locFile.getFile().getParentFile().exists() == false) {
            System.out.println("Creating new directory");

            if (locFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("Could not create new directory...");
            }
        }

        try {
            System.out.println("--- Saving XML path = "
                    + locFile.getFile().getAbsolutePath());
            SerializationUtil.jaxbMarshalToXmlFile(dataXML, locFile.getFile()
                    .getAbsolutePath());
            locFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }

        setForcingConfigured();
    }

    /**
     * Convenience method to print the data.
     */
    public void printData() {
        if (xmlType == FFFGXmlType.MASTER) {
            System.out.println("******* Master XML Data *********");
        } else {
            System.out.println("******* User XML Data *********");
        }

        System.out.println("Experation Time = " + dataXML.getExpTimeInMillis());

        ArrayList<FFFGSourceXML> sources = dataXML.getSources();

        for (FFFGSourceXML src : sources) {
            if (src.getSourceName() == null) {
                System.out.println("+++ Source Name is null ");
            } else {
                System.out.println("+++ Source Name = " + src.getSourceName());
            }

            if (src.getAreaFFGValue() == null) {
                System.out.println("+++ Area FFG is null");
            } else {
                System.out
                        .println("+++ Area FFG    = " + src.getAreaFFGValue());
            }

            ArrayList<FFFGSourceItemXML> srcItems = src.getSourceItems();

            if (srcItems == null) {
                System.out.println("Source Items are null");
                return;
            }

            for (FFFGSourceItemXML si : srcItems) {
                System.out.println("------ type = " + si.getType());
                System.out.println("------ name = " + si.getName());
                System.out.println("------ id   = " + si.getId());
                System.out.println("------ val  = " + si.getValue());
            }
        }
    }

    private void setForcingConfigured() {
        if ((dataXML.getSources() != null) || (dataXML.getSources().size() > 0)) {
            forcingConfigured = true;
        }
    }

    /**
     * Are any forcings configured?
     * 
     * @return true if forcings are configured
     */
    public boolean isForcingConfigured() {
        return this.forcingConfigured;
    }
}
