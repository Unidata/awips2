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
package com.raytheon.uf.edex.archive;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;

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
 * Nov 05, 2013 2499       rjpeter     Repackaged, updated to use System properties.
 * Dec 11, 2013 2555       rjpeter     archivePath overridable via System properties.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DataArchiver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataArchiver.class);

    // enables/disables archiving as a whole
    private final static String ENABLE_PROPERTY = "archive.enable";

    // allows for disabling of specific plugins if desired
    private final static String DISABLE_PROPERTY = "archive.disable";

    private final static String PATH_PROPERTY = "archive.path";

    private final boolean ARCHIVING_ENABLED;

    private final Set<String> DISABLED_PLUGINS;

    private final List<IPluginArchiver> pluginArchivers = new LinkedList<IPluginArchiver>();

    private final List<IDataArchiver> dataArchivers = new LinkedList<IDataArchiver>();

    private final String archivePath;

    public DataArchiver() {
        ARCHIVING_ENABLED = Boolean.getBoolean(ENABLE_PROPERTY);
        String disabledPluginList = System.getProperty(DISABLE_PROPERTY);
        if (disabledPluginList != null) {
            String[] plugins = disabledPluginList.split(",");
            DISABLED_PLUGINS = new HashSet<String>(plugins.length);
            for (String plugin : plugins) {
                DISABLED_PLUGINS.add(plugin.trim());
            }
        } else {
            DISABLED_PLUGINS = Collections.emptySet();
        }

        // default to /archive
        archivePath = System.getProperty(PATH_PROPERTY, "/archive");
    }

    public void archivePlugins() {
        Thread.currentThread().setName("Archiver");
        if (ARCHIVING_ENABLED) {
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            statusHandler.info("Archival of plugin data started");

            // get list of plugins, ordered by plugin
            Set<String> availablePlugins = new TreeSet<String>(PluginRegistry
                    .getInstance().getRegisteredObjects());

            for (String pluginName : availablePlugins) {
                if (DISABLED_PLUGINS.contains(pluginName)) {
                    statusHandler.info(pluginName + ": Archiving disabled");
                } else {
                    for (IPluginArchiver pluginArchiver : pluginArchivers) {
                        pluginArchiver.archivePlugin(pluginName, archivePath);
                    }
                }
            }

            timer.stop();
            statusHandler
                    .info("Archival of plugin data completed.  Time to run: "
                            + TimeUtil.prettyDuration(timer.getElapsedTime()));
        } else {
            statusHandler.info("Archival of plugin data disabled, exiting");
        }
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
}
