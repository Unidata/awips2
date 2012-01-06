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

import java.io.File;
import java.util.ArrayList;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.xml.FFTIDataXML;
import com.raytheon.uf.common.monitor.xml.FFTISettingXML;
import com.raytheon.uf.common.serialization.SerializationUtil;

public class FFTIDataManager implements ILocalizationFileObserver {
    /** Path to FFTI config. */
    private static final String CONFIG_FILE_NAME = "ffmp" + File.separatorChar
            + "FFTIData.xml";

    /**
     * FFTI Source Configuration XML object.
     */
    protected FFTIDataXML configXml;

    /** Singleton instance of this class */
    private static FFTIDataManager instance = new FFTIDataManager();

    private LocalizationFile lf = null;

    /* Private Constructor */
    private FFTIDataManager() {
        configXml = new FFTIDataXML();
        readConfigXml();
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static FFTIDataManager getInstance() {
        return instance;
    }

    /**
     * Read the XML configuration data for the current XML file name.
     */
    public synchronized void readConfigXml() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);
            lf.addFileUpdatedObserver(this);

            File file = lf.getFile();

            SerializationUtil.jaxbUnmarshalFromXmlFile(file.getAbsolutePath());
            FFTIDataXML configXmltmp = (FFTIDataXML) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(file.getAbsolutePath());

            configXml = configXmltmp;

        } catch (Exception e) {
            System.err
                    .println("No SITE FFTI Source configuration file found.");
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
        LocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                CONFIG_FILE_NAME);

        if (newXmlFile.getFile().getParentFile().exists() == false) {
            // System.out.println("Creating new directory");

            if (newXmlFile.getFile().getParentFile().mkdirs() == false) {
                // System.out.println("Could not create new directory...");
            }
        }

        try {
            // System.out.println("Saving -- "
            // + newXmlFile.getFile().getAbsolutePath());
            SerializationUtil.jaxbMarshalToXmlFile(configXml, newXmlFile
                    .getFile().getAbsolutePath());
            newXmlFile.save();

            lf = newXmlFile;
        } catch (Exception e) {
            System.err
                    .println("Failed to save SITE FFTI Source configuration file.");
        }
    }

    public ArrayList<String> getCwaList() {
        return this.configXml.getCwa().getCwaList();
    }

    public void setCwaList(ArrayList<String> cwaList) {
        this.configXml.getCwa().setCwaList(cwaList);
    }
    
    public void addCwa(String cwa) {
        this.configXml.getCwa().addCwa(cwa);
    }

    public void setSettings(ArrayList<FFTISettingXML> settings) {
        this.configXml.setSettingList(settings);
    }

    public ArrayList<FFTISettingXML> getSettingList() {
        return this.configXml.getSettingList();
    }
    
    public void addSetting(FFTISettingXML setting) {
        this.configXml.addSetting(setting);
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {

        try {
            if (message.getFileName().equals(CONFIG_FILE_NAME)) {
                readConfigXml();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    public void clear() {
        configXml = new FFTIDataXML();
    }
}
