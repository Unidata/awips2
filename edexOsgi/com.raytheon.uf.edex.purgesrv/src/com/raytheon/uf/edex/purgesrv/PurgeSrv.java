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

package com.raytheon.uf.edex.purgesrv;

import java.util.Date;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang.time.StopWatch;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.status.StatusConstants;

/**
 * The dao implementation associated with the PluginVersion class used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/15/07      141         garmenda    Initial checkin  
 * 10/6/08                  bphillip    Added custom purge behavior support
 * 10/8/2008    1532        bphillip    Refactor to support custom purging
 * 02/06/09     1990        bphillip    Refactored to use plugin daos. Moved initialization code out
 * Apr 19, 2012 #470        bphillip    Refactored to use PurgeManager
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class PurgeSrv {

    /** Message to delete all existing data */
    public static final String DELETE_ALL_DATA = "PURGE_ALL_DATA";

    /** Message to delete all expired data according to the rules */
    public static final String DELETE_EXPIRED_DATA = "PURGE_EXPIRED_DATA";

    /**
     * Message to delete all expired data according to the rules for a specific
     * plugin
     */
    public static final String DELETE_PLUGIN_DATA = "PURGE_PLUGIN=";

    /**
     * Message to delete all data for a specific plugin
     */
    public static final String DELETE_ALL_PLUGIN_DATA = "PURGE_ALL_PLUGIN=";

    /** The purge cron message */
    public static final String PURGE_CRON = "PURGE_CRON";

    /**
     * Constructs a new PurgeSrv. This method verifies the metadata database has
     * been constructed and exports the schema if necessary
     */
    public PurgeSrv() {
    }

    public void purgeCron() throws Exception {
        purge(PURGE_CRON);
    }

    /**
     * Executes the appropriate purge routing based on the message received.
     * <p>
     * The following are the valid messages:
     * <p>
     * PURGE_ALL_DATA - Purges all data from the database and HDF5 data
     * repository<br>
     * PURGE_EXPIRED_DATA - Executes a purge based on the purge rules specified<br>
     * PURGE_PLUGIN=pluginName - The data for the specified plugin will be
     * deleted from the database and HDF5 repositories according to the purge
     * rules specified<br>
     * PURGE_ALL_PLUGIN=pluginName - All data for the specified plugin will be
     * purged<br>
     * 
     * @param message
     *            The message in the format described above
     * @throws Exception
     *             If errors occur while purging the data
     */
    public void purge(String message) throws Exception {
        StopWatch timer = new StopWatch();
        PurgeLogger.logInfo("---------START PURGE---------",
                StatusConstants.CATEGORY_PURGE);

        if (message == null) {
            PurgeLogger.logError("NULL message received by Purge Service",
                    StatusConstants.CATEGORY_PURGE);
            return;
        }
        if (message.isEmpty()) {
            message = PURGE_CRON;
        }

        timer.start();
        PurgeLogger.logInfo("Purge Message Received: " + message,
                StatusConstants.CATEGORY_PURGE);

        if (message.equals(DELETE_ALL_DATA)) {
            purgeAllData();
        } else if (message.startsWith(DELETE_PLUGIN_DATA)) {
            String pluginToPurge = message.replace(DELETE_PLUGIN_DATA, "");
            PurgeManager.getInstance().purgeExpiredData(pluginToPurge);
        } else if (message.startsWith(DELETE_ALL_PLUGIN_DATA)) {
            String pluginToPurge = message.replace(DELETE_ALL_PLUGIN_DATA, "");
            PurgeManager.getInstance().purgeAllData(pluginToPurge);
        } else if (message.equals(PURGE_CRON)
                || message.equals(DELETE_EXPIRED_DATA)) {
            purgeExpiredData();
        } else {
            PurgeLogger
                    .logError("Unsupported command received by Purge Service: "
                            + message, StatusConstants.CATEGORY_PURGE);
        }
        PurgeLogger.logInfo("Purge Operation: " + message + " completed in "
                + millisToString(timer.getTime()),
                StatusConstants.CATEGORY_PURGE);
        PurgeLogger.logInfo("---------END PURGE-----------",
                StatusConstants.CATEGORY_PURGE);
    }

    /**
     * Purges all data in the database and HDF5 repository
     * 
     * @throws PluginException
     *             If errors occur during the purge routine
     */
    private void purgeAllData() throws PluginException {
        PurgeLogger.logInfo("Purge All Data Started at: " + new Date(),
                StatusConstants.CATEGORY_PURGE);

        // order the purge
        Set<String> availablePlugins = new TreeSet<String>(PluginRegistry
                .getInstance().getRegisteredObjects());
        for (String pluginName : availablePlugins) {
            if (PluginRegistry.getInstance().getRegisteredObject(pluginName) != null) {
                PurgeManager.getInstance().purgeAllData(pluginName);
            }
        }
        PurgeLogger.logInfo("Purge All Data Completed at: " + new Date(),
                StatusConstants.CATEGORY_PURGE);
    }

    /**
     * Purges expired data using the selected purger strategy
     * 
     * @throws PluginException
     *             If errors occur during the purge routine
     */
    private void purgeExpiredData() throws PluginException {
        PurgeLogger.logInfo("Purge Expired Data Started at: " + new Date(),
                StatusConstants.CATEGORY_PURGE);

        // order the purge
        Set<String> availablePlugins = new TreeSet<String>(PluginRegistry
                .getInstance().getRegisteredObjects());

        for (String pluginName : availablePlugins) {
            if (PluginRegistry.getInstance().getRegisteredObject(pluginName) != null) {
                PurgeManager.getInstance().purgeExpiredData(pluginName);
            }
        }
    }

    /**
     * Converts millisecond value to minutes and seconds
     * 
     * @param millis
     *            The value to convert
     * @return The millisecond value translated to minutes and seconds
     */
    private String millisToString(long millis) {
        long time = millis / 1000;
        long seconds = time % 60;
        long minutes = (time % 3600) / 60;

        StringBuilder builder = new StringBuilder();
        if (minutes > 0) {
            builder.append(minutes).append(" minute");
            if (minutes != 1) {
                builder.append("s ");
            } else {
                builder.append(" ");
            }
        }
        if (seconds > 0) {
            builder.append(seconds).append(" second");
            if (seconds != 1) {
                builder.append("s ");
            }
        }

        if (minutes == 0 && seconds == 0) {
            return "0 seconds";
        }
        return builder.toString();
    }

    public void dispose() {
    }

}