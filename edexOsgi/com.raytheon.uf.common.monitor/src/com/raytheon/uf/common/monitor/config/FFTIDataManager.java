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
import java.io.InputStream;
import java.util.ArrayList;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.monitor.xml.FFTIDataXML;
import com.raytheon.uf.common.monitor.xml.FFTISettingXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Singleton data manager for FFTI.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * Oct 02, 2013 2361       njensen     Use JAXBManager for XML
 * Feb 15, 2016 5244       nabowle     Replace deprecated LocalizationFile methods.
 *                                     Replace system.out with UFStatus.
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class FFTIDataManager implements ILocalizationPathObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTIDataManager.class);

    /** Path to FFTI config. */
    private static final String CONFIG_FILE_NAME = "ffmp" + File.separatorChar
            + "FFTIData.xml";

    // This needs to initialize before the instance since the constructor will
    // makes use of JAXB. JVM spec 12.4.2 step 9 indicates this will
    // initialize ahead of the instance since it is earlier in
    // in the text source.
    private static final SingleTypeJAXBManager<FFTIDataXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(FFTIDataXML.class);

    /** Singleton instance of this class */
    private static FFTIDataManager instance = new FFTIDataManager();

    /**
     * FFTI Source Configuration XML object.
     */
    protected FFTIDataXML configXml;

    /* Private Constructor */
    private FFTIDataManager() {
        configXml = new FFTIDataXML();
        IPathManager pm = PathManagerFactory.getPathManager();
        pm.addLocalizationPathObserver(CONFIG_FILE_NAME, this);
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
            ILocalizationFile lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);

            try (InputStream is = lf.openInputStream()) {
                FFTIDataXML configXmltmp = jaxb.unmarshalFromInputStream(is);
                configXml = configXmltmp;
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "No SITE FFTI Source configuration file found.");
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
        ILocalizationFile newXmlFile = pm.getLocalizationFile(lc,
                CONFIG_FILE_NAME);

        try (SaveableOutputStream sos = newXmlFile.openOutputStream()) {
            jaxb.marshalToStream(configXml, sos);
            sos.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Failed to save SITE FFTI Source configuration file.");
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
    public void fileChanged(ILocalizationFile file) {

        try {
            if (file.getPath().equals(CONFIG_FILE_NAME)) {
                readConfigXml();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "Error handling file changed.",
                    e);
        }
    }

    public void clear() {
        configXml = new FFTIDataXML();
    }
}
