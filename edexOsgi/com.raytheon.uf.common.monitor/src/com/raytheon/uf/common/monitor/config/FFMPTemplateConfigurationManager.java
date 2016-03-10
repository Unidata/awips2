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
import java.util.concurrent.CopyOnWriteArrayList;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.monitor.xml.FFMPTemplateXML;
import com.raytheon.uf.common.monitor.xml.VGBXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Template area configuration xml.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009            lvenable     Initial creation
 * Oct 25, 2012 DR 15514   gzhang       Adding getHucLevelsInArray()
 * Aug 18, 2013  1742      dhladky      Concurrent mod exception on update fixed
 * Oct 02, 2013  2361      njensen      Use JAXBManager for XML
 * Feb 15, 2016 5244       nabowle      Replace deprecated LocalizationFile methods.
 * </pre>
 *
 * @author dhladky
 * @version 1.0
 */

public class FFMPTemplateConfigurationManager implements
        ILocalizationPathObserver {

    /** Path to FFMP Template config. */
    private static final String CONFIG_FILE_NAME = "ffmp" + File.separatorChar
            + "FFMPTemplateConfig.xml";

    private static final SingleTypeJAXBManager<FFMPTemplateXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(FFMPTemplateXML.class);

    /**
     * FFMP Source Configuration XML object.
     */
    public FFMPTemplateXML configXml;

    /** Singleton instance of this class */
    private static FFMPTemplateConfigurationManager instance = null;

    private ArrayList<String> hucLevels = null;

    private CopyOnWriteArrayList<MonitorConfigListener> listeners = new CopyOnWriteArrayList<MonitorConfigListener>();

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPTemplateConfigurationManager.class);

    /* Private Constructor */
    private FFMPTemplateConfigurationManager() {
        configXml = new FFMPTemplateXML();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        pm.addLocalizationPathObserver(CONFIG_FILE_NAME, this);
    }

    /**
     * Get an instance of this singleton.
     *
     * @return Instance of this class
     */
    public static synchronized FFMPTemplateConfigurationManager getInstance() {
        if (instance == null) {
            instance = new FFMPTemplateConfigurationManager();
        }

        return instance;
    }

    public void addListener(MonitorConfigListener fl) {
        listeners.add(fl);
    }

    public void removeListener(MonitorConfigListener fl) {
        listeners.remove(fl);
    }

    /**
     * Read the XML configuration data for the current XML file name.
     */
    public void readConfigXml() throws Exception {

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        ILocalizationFile lf = pm.getLocalizationFile(lc, CONFIG_FILE_NAME);

        try (InputStream is = lf.openInputStream()) {
            FFMPTemplateXML configXmltmp = jaxb.unmarshalFromInputStream(is);
            configXml = configXmltmp;
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
            statusHandler.handle(Priority.ERROR, "Couldn't save config file.",
                    e);
        }
    }

    /**
     * Gets the Huc Depth
     *
     * @return
     */
    public int getHucDepth() {
        return configXml.getHucDepth();
    }

    public void setHucDepth(int hucDepth) {
        configXml.setHucDepth(hucDepth);
    }

    /**
     * Do we have virtual basins?
     *
     * @return
     */
    public boolean getVirtual() {
        return configXml.getVirtual();
    }

    public void setVirtual(boolean virtual) {
        configXml.setVirtual(virtual);
    }

    /**
     * Gets you the number of HUC levels
     *
     * @return
     */
    public int getNumberOfHuc() {
        return configXml.getNumberOfHuc();
    }

    public void setNumberOfHuc(int numberOfHuc) {
        configXml.setNumberOfHuc(numberOfHuc);
    }

    /**
     * Get HUC Levels. Ordered for template and geometry unification order.
     *
     * @return
     */
    public ArrayList<String> getHucLevels() {

        hucLevels = new ArrayList<String>();
        hucLevels.add("ALL");
        hucLevels.add("COUNTY");
        if (getVirtual()) {
            hucLevels.add("VIRTUAL");
        }
        for (int i = getNumberOfHuc() - 1; i >= 0; i--) {
            hucLevels.add("HUC" + i);
        }

        return hucLevels;
    }

    public void setExtents(Double extents) {
        configXml.setExtents(extents);
    }

    public Double getExtents() {
        return configXml.getExtents();
    }

    public VGBXML getExcludedVGBs() {
        return configXml.getExcludedVGBs();
    }

    public void setExcludedVGBs(ArrayList<String> vgbs) {
        VGBXML vgbx = new VGBXML();
        for (String vgb : vgbs) {
            vgbx.add(vgb);
        }
        configXml.setExcludedVGBs(vgbx);
    }

    public boolean isRegenerate() {
        return configXml.isRegenerate();
    }

    public void setRegenerate(boolean regenerate) {
        configXml.setRegenerate(regenerate);
    }

    @Override
    public void fileChanged(ILocalizationFile localizationFile) {

        if (localizationFile.getPath().equals(CONFIG_FILE_NAME)) {
            try {
                readConfigXml();
                // inform listeners
                synchronized (listeners) {
                    for (MonitorConfigListener fl : listeners) {
                        fl.configChanged(new MonitorConfigEvent(this));
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(
                        Priority.WARN,
                        "FFMPTemplateConfigurationManager: "
                                + localizationFile.getPath()
                                + " couldn't be updated.", e);
            }
        }
    }

    /**
     * DR 15514: based on getHucLevels()
     */
    public String[] getHucLevelsInArray() {

        Integer hucNum = 4;
        Boolean isVirtual = true;
        String[] result = null;
        java.util.concurrent.locks.ReentrantLock lock = new java.util.concurrent.locks.ReentrantLock();

        synchronized (configXml) {
            hucNum = getNumberOfHuc();
            isVirtual = getVirtual();
        }

        lock.lock();
        try {
            java.util.List<String> list = new ArrayList<String>();
            list.add("ALL");
            list.add("COUNTY");

            if (isVirtual) {
                list.add("VIRTUAL");
            }

            for (int i = hucNum - 1; i >= 0; i--) {
                list.add("HUC" + i);
            }

            result = list.toArray(new String[] {});

        } finally {
            if (result == null)
                result = new String[] {};// guaranteed not null
            lock.unlock();
        }

        return result;
    }

}
