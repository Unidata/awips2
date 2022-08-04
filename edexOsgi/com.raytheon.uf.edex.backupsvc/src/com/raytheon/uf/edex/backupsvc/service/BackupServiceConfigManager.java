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
package com.raytheon.uf.edex.backupsvc.service;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Singleton that manages configuration for backup service
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2016 5937       tgurney     Initial creation
 * Dec  9, 2016 5937       tgurney     Make thread safe. Change reload strategy
 * Jul 24, 2017 6352       tgurney     Host getters return ArrayList
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupServiceConfigManager {

    private static final String SETTINGS_FILE = "backupsvc"
            + IPathManager.SEPARATOR + "backupSvc.xml";

    private static final Logger logger = LoggerFactory
            .getLogger(BackupServiceConfigManager.class);

    private static final BackupServiceConfigManager INSTANCE = new BackupServiceConfigManager();

    private SingleTypeJAXBManager<BackupServiceConfig> jaxbManager;

    private volatile BackupServiceConfig config;

    private BackupServiceConfigManager() {
        reload();
        PathManagerFactory.getPathManager()
                .addLocalizationPathObserver(SETTINGS_FILE, (file) -> {
                    reload();
                });
    }

    public static BackupServiceConfigManager getInstance() {
        return INSTANCE;
    }

    /**
     * Load backup service configuration from the localization XML file. Will
     * always load the file, regardless of last modified time. If there is an
     * error when loading the file then we will fall back to the default
     * configuration which contains no backup hosts.
     */
    public synchronized void reload() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(siteCtx, SETTINGS_FILE);
        BackupServiceConfig newConfig = null;
        try {
            if (file.exists()) {
                if (jaxbManager == null) {
                    jaxbManager = new SingleTypeJAXBManager<>(true,
                            BackupServiceConfig.class);
                }
                try (InputStream is = file.openInputStream()) {
                    newConfig = jaxbManager.unmarshalFromInputStream(
                            BackupServiceConfig.class, is);
                } catch (IOException e) {
                    logger.debug("Error on stream close", e);
                }
            } else {
                // Load the default config
                newConfig = new BackupServiceConfig();
            }
        } catch (JAXBException | SerializationException
                | LocalizationException e) {
            logger.error("Failed to load settings from " + file.getPath(), e);
            logger.warn("Falling back to default config with no hosts");
            newConfig = new BackupServiceConfig();
        }
        config = newConfig;
    }

    public List<BackupHost> getBackupHosts() {
        return config.getHosts().stream()
                .map((aBackupHost) -> new BackupHost(aBackupHost))
                .collect(Collectors.toCollection(ArrayList::new));
    }

    public List<String> getHostnamesOnly() {
        return config.getHosts().stream().map(BackupHost::getName)
                .collect(Collectors.toCollection(ArrayList::new));
    }

    public int getPollIntervalSeconds() {
        return config.getPollIntervalSeconds();
    }

    public int getBigJobSize() {
        return config.getBigJobSize();
    }

    public int getRateLimitKBps() {
        return config.getRateLimitKBps();
    }
}
