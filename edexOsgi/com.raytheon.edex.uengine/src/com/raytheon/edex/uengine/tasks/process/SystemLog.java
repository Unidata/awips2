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
package com.raytheon.edex.uengine.tasks.process;

import com.raytheon.edex.uengine.tasks.ScriptTask;

/**
 * Provides a simple logging capability for uEngine scripts, utilizing the
 * Apache Log4J facility. Logging is performed at the levels supported by
 * Log4J (debug, info, warn, error, fatal). 
 * <BR>
 * <B>Note:</B> This class is intended for use within a JS uEngine script.
 * <BR> 
 * <pre>
 * Usage:
 *    function myFunc() {
 *       var logger = new System Log();
 *       logger.log("info","executing myFunc");
 *       .
 *       .
 *       .
 *    }
 * </pre>
 * @author mfegan
 * @version 1
 */
public class SystemLog extends ScriptTask {

    private String level = "info";
    private String message = "";
    /**
     * Writes the message to the system log file at the specified level.
     * 
     * @see ScriptTask#execute(). 
     */
    @Override
    public Object execute() {
        if (level.equalsIgnoreCase("fatal")) {
            logger.fatal(message);
        } else if (level.equalsIgnoreCase("warn")) {
            logger.warn(message);
        } else if (level.equalsIgnoreCase("error")) {
            logger.error(message);
        } else if (level.equalsIgnoreCase("info")) {
            logger.info(message);
        } else {
            logger.debug(message);
        }
        return null;
    }
    /**
     * Writes a message to the system log. The message is logged at the specified
     * logging level. The message format depends on the system Log4J settings.
     * Messages level that are actually logged also depends on system Log4J configuration.
     * 
     * @param level the logging level. One of "fatal", "error", "warn", "info", and "debug"
     * @param message the message to log.
     */
    public void log(String level,String message) {
        this.level = level;
        this.message = message;
        execute();
    }
}
