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
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.events.MonitorConfigEvent;
import com.raytheon.uf.common.monitor.events.MonitorConfigListener;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.purge.PurgeRule;
import com.raytheon.uf.edex.database.purge.PurgeRuleSet;

/**
 * FFMPRetentionTimeManager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2013 1742       dhladky     fixed concurrent mod exception on update
 * 
 * </pre>
 * 
 */


public class FFMPRetentionTimeManager implements ILocalizationFileObserver {

    /** Path to FFMP Source config. */
    private static final String CONFIG_FILE_NAME = "purge" + File.separatorChar
            + "ffmpPurgeRules.xml";

    private LocalizationFile lf = null;

    /**
     * FFMP Source Configuration XML object.
     */
    protected PurgeRuleSet configXml;

    private CopyOnWriteArrayList<MonitorConfigListener> listeners = new CopyOnWriteArrayList<MonitorConfigListener>();

    /** Singleton instance of this class */
    private static FFMPRetentionTimeManager instance = new FFMPRetentionTimeManager();

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPRetentionTimeManager.class);
    
    /* Private Constructor */
    private FFMPRetentionTimeManager() {
        configXml = new PurgeRuleSet();
        readConfigXml();
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static FFMPRetentionTimeManager getInstance() {
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
    public void readConfigXml() {

        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            Map<LocalizationLevel, LocalizationFile> map = pm
                    .getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                            CONFIG_FILE_NAME);

            if (map.containsKey(LocalizationLevel.SITE)) {
                lf = map.get(LocalizationLevel.SITE);
            } else if (map.containsKey(LocalizationLevel.BASE)) {
                lf = map.get(LocalizationLevel.BASE);
            }

            lf.addFileUpdatedObserver(this);
            File file = lf.getFile();
            System.out.println("Reading -- " + file.getAbsolutePath());

            PurgeRuleSet configXmltmp = null;

            configXmltmp = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    PurgeRuleSet.class, file.getAbsolutePath());

            configXml = configXmltmp;

        } catch (Exception e) {
            statusHandler
                    .handle(Priority.WARN,
                            "No SITE level purge file found.",
                            e);
        }

    }

    /**
     * Save the XML configuration data to the current XML file name.
     */
    private void saveConfigXml() {
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
            statusHandler.handle(Priority.WARN,
                    "Couldn't save config.", e);
        }

    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        if (message.getFileName().equals(CONFIG_FILE_NAME)) {
            try {
                readConfigXml();

                // inform listeners
                for (MonitorConfigListener fl : listeners) {
                    fl.configChanged(new MonitorConfigEvent(this));
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "FFMPRetentionTimeManager: " + message.getFileName()
                                + " couldn't be updated.", e);
            }
        }
    }

    /**
     * Gets the time
     * 
     * @return
     */
    public long getRetentionTime() {
        List<PurgeRule> rules = configXml.getDefaultRules();

        if ((rules != null) && !rules.isEmpty()) {
            return rules.get(0).getPeriodInMillis();
        }

        return 0l;
    }

    /**
     * Sets the retention time
     * 
     * @param time
     */
    public void setRetentionTime(String time) {
        List<PurgeRule> rules = configXml.getDefaultRules();
        PurgeRule rule = null;

        if ((rules == null) || rules.isEmpty()) {
            rule = new PurgeRule();
            configXml.setDefaultRule(rule);
        } else {
            rule = rules.get(0);
        }

        rule.setPeriod(time);

        saveConfigXml();
    }
}
