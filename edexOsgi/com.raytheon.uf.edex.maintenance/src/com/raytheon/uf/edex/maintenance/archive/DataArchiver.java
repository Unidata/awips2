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
package com.raytheon.uf.edex.maintenance.archive;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.maintenance.archive.config.DataArchiveConfig;

/**
 * Handles archiving of data. Has two interfaces for registering data archive.
 * Data archived based on archiving for each plugin and general data archive
 * programs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DataArchiver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataArchiver.class);

    private List<IPluginArchiver> pluginArchivers = new ArrayList<IPluginArchiver>();

    private List<IDataArchiver> dataArchivers = new ArrayList<IDataArchiver>();

    private String archivePath = null;

    private String defaultPlugin = "default";

    private String configDir = "archiver";

    public DataArchiver(String archivePath) {
        this.archivePath = archivePath;
    }

    public void archivePlugins() {
        statusHandler.info("Archival of plugin data starting");

        // get list of plugins, ordered by plugin
        Set<String> availablePlugins = new TreeSet<String>(PluginRegistry
                .getInstance().getRegisteredObjects());

        Map<String, DataArchiveConfig> configs = getDataArchiveConfigs();
        DataArchiveConfig defaultConf = configs.get(defaultPlugin);
        File baseArchive = new File(archivePath);

        for (String pluginName : availablePlugins) {
            DataArchiveConfig conf = configs.get(pluginName);
            if (conf == null) {
                conf = defaultConf;
            }

            if (conf.getHoursToKeep() > 0) {
                File pluginArchive = new File(baseArchive, pluginName);
                if (pluginArchive.isDirectory()) {
                    long purgeThreshold = System.currentTimeMillis()
                            - (conf.getHoursToKeep() * 60 * 60 * 1000);
                    purgeDirectory(pluginArchive, purgeThreshold);
                }
            }

            if (Boolean.TRUE.equals(conf.getArchivingEnabled())) {
                for (IPluginArchiver pluginArchiver : pluginArchivers) {
                    pluginArchiver.archivePlugin(pluginName, archivePath, conf);
                }
            }
        }

        statusHandler.info("Archival of plugin data complete");
    }

    public Object registerPluginArchiver(IPluginArchiver archiver) {
        if (!pluginArchivers.contains(archiver)) {
            pluginArchivers.add(archiver);
        } else {
            statusHandler.warn("Plugin archiver already registered: "
                    + archiver);
        }

        return this;
    }

    public Object registerDataArchiver(IDataArchiver archiver) {
        if (!dataArchivers.contains(archiver)) {
            dataArchivers.add(archiver);
        } else {
            statusHandler.warn("Data archiver already registered: " + archiver);
        }

        return this;
    }

    private boolean purgeDirectory(File directory, long purgeThreshold) {
        File[] listing = directory.listFiles();
        int numDeleted = 0;
        for (File file : listing) {
            if (file.isDirectory()) {
                if (purgeDirectory(file, purgeThreshold)) {
                    numDeleted++;
                }
            } else if (file.lastModified() < purgeThreshold) {
                if (file.delete()) {
                    numDeleted++;
                }
            }
        }

        // we deleted all files/directories, or there were no files in this
        // directory
        if (numDeleted == listing.length
                && !directory.getAbsolutePath().equals(archivePath)) {
            if (directory.delete()) {
                return true;
            }
        }

        return false;
    }

    private Map<String, DataArchiveConfig> getDataArchiveConfigs() {
        Map<String, DataArchiveConfig> configs = new HashMap<String, DataArchiveConfig>();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        // process in reverse order so BASE is processed before CONFIGURED
        // before SITE
        List<LocalizationContext> contexts = Arrays.asList(pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC));
        Collections.reverse(contexts);
        String[] extensions = new String[] { "xml" };
        for (LocalizationContext ctx : contexts) {
            statusHandler.info("Loading context: " + ctx);
            LocalizationFile[] lfs = pathMgr.listFiles(ctx, configDir,
                    extensions, false, true);
            if (lfs != null && lfs.length > 0) {
                for (LocalizationFile lf : lfs) {
                    String fileName = lf.getName();
                    try {
                        File f = lf.getFile(true);
                        fileName = f.getAbsolutePath();
                        Object obj = SerializationUtil
                                .jaxbUnmarshalFromXmlFile(f);
                        if (obj instanceof DataArchiveConfig) {
                            DataArchiveConfig conf = (DataArchiveConfig) obj;
                            String plugin = conf.getPluginName();
                            if (plugin != null) {
                                plugin = plugin.trim();
                                if (!plugin.isEmpty()) {
                                    configs.put(plugin, conf);
                                } else {
                                    throw new Exception(
                                            "Configuration file does not specify pluginName");
                                }
                            } else {
                                throw new Exception(
                                        "Configuration file does not specify pluginName");
                            }
                        } else {
                            throw new Exception(
                                    "File in wrong format, expected "
                                            + DataArchiveConfig.class
                                            + ", found " + obj.getClass());
                        }
                    } catch (Throwable e) {
                        statusHandler.error(
                                "Failed to load archive configuration file: "
                                        + fileName, e);
                    }
                }
            }
        }

        DataArchiveConfig defaultConf = configs.get(defaultPlugin);
        if (defaultConf == null) {
            // default plugin didn't load from disk, force a default config
            statusHandler
                    .warn("Failed to find default configuration, using internal defaults");
            defaultConf = new DataArchiveConfig();
            defaultConf.setPluginName(defaultPlugin);
            configs.put(defaultPlugin, defaultConf);
        }

        if (!defaultConf.isArchivingEnabledSet()) {
            defaultConf.setArchivingEnabled(Boolean.TRUE);
        }

        if (!defaultConf.isCompressionEnabledSet()) {
            defaultConf.setCompressionEnabled(Boolean.TRUE);
        }

        if (!defaultConf.isHoursToKeepSet()) {
            defaultConf.setHoursToKeep(6);
        } else if (defaultConf.getHoursToKeep() < 0) {

        }

        // override unset fields with default
        for (DataArchiveConfig pluginConf : configs.values()) {
            if (pluginConf.getPluginName().equals(defaultPlugin)) {
                // skip default conf
                continue;
            }

            if (!pluginConf.isArchivingEnabledSet()) {
                pluginConf.setArchivingEnabled(defaultConf
                        .getArchivingEnabled());
            }

            if (!pluginConf.isCompressionEnabledSet()) {
                pluginConf.setCompressionEnabled(defaultConf
                        .getArchivingEnabled());
            }

            if (!pluginConf.isHoursToKeepSet()) {
                pluginConf.setHoursToKeep(defaultConf.getHoursToKeep());
            }
        }

        try {
            statusHandler.info("DefaultConfiguration:\n"
                    + SerializationUtil.marshalToXml(defaultConf));
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "Failed to deserialize config",
                    e);
        }
        return configs;
    }
}
