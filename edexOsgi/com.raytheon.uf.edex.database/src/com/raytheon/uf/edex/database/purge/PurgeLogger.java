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

package com.raytheon.uf.edex.database.purge;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.status.StatusConstants;

/**
 * Convenience class used for logging purge related items
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/15/11      #2469       bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class PurgeLogger {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PurgeLogger.class);

    /**
     * Logs an info message for the given plugin
     * 
     * @param message
     *            The message to log
     * @param plugin
     *            The plugin this message applies to
     */
    public static void logInfo(String message, String plugin) {
        logMsg(Priority.INFO, message, plugin, null);
    }

    /**
     * Logs a warning message for the given plugin
     * 
     * @param message
     *            The message to log
     * @param plugin
     *            The plugin this message applies to
     */
    public static void logWarn(String message, String plugin) {
        logMsg(Priority.WARN, message, plugin, null);
    }

    /**
     * Logs an error message for the given plugin
     * 
     * @param message
     *            The error message to log
     * @param plugin
     *            The plugin this message applies to
     */
    public static void logError(String message, String plugin) {
        logMsg(Priority.ERROR, message, plugin, null);
    }

    /**
     * Logs an error message with an exception for the given plugin
     * 
     * @param message
     *            The error message to log
     * @param plugin
     *            The plugin this message applies to
     * @param e
     *            The exception to log
     */
    public static void logError(String message, String plugin, Throwable e) {
        logMsg(Priority.ERROR, message, plugin, e);
    }

    /**
     * Logs a fatal message for the given plugin
     * 
     * @param message
     *            The fatal message to log
     * @param plugin
     *            The plugin this message applies to
     */
    public static void logFatal(String message, String plugin) {
        logMsg(Priority.FATAL, message, plugin, null);
    }

    /**
     * Logs a fatal message with an exception for the given plugin
     * 
     * @param message
     *            The fatal message to log
     * @param plugin
     *            The plugin this message applies to
     * @param e
     *            The exception to log
     */
    public static void logFatal(String message, String plugin, Throwable e) {
        logMsg(Priority.FATAL, message, plugin, e);
    }

    /**
     * Logs a debug message for the given plugin
     * 
     * @param message
     *            The message to log
     * @param plugin
     *            The plugin this message applies to
     */
    public static void logDebug(String message, String plugin) {
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            logMsg(Priority.DEBUG, message, plugin, null);
        }
    }

    /**
     * Checks if debug logging is enabled
     * 
     * @return True if debug logging is enabled
     */
    public static boolean isDebugEnabled() {
        return statusHandler.isPriorityEnabled(Priority.DEBUG);
    }

    /**
     * Private handler for logging.
     * 
     * @param priority
     *            The priority of the message
     * @param message
     *            The message
     * @param plugin
     *            The plugin this message applies to
     * @param e
     *            The exception, if any, to log
     */
    private static void logMsg(Priority priority, String message,
            String plugin, Throwable e) {
        if (plugin == null) {
            plugin = StatusConstants.CATEGORY_PURGE;
        }
        if (e == null) {
            statusHandler.handle(priority, plugin.toUpperCase() + "::"
                    + message);
        } else {
            statusHandler.handle(priority, plugin.toUpperCase() + "::"
                    + message, e);
        }
    }
}
