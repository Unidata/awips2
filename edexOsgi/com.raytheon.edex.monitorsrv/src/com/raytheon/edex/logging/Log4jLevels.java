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
package com.raytheon.edex.logging;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Class to modify log4j levels at runtime
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/13/07     629        garmendariz Initial check-in
 * 
 * </pre>
 *
 * @author garmendariz
 * @version 1.0
 */
public class Log4jLevels {

    public String setLevel(String loggerName, String level) {
        
        if(!checkLevel(level))
        {
            return "Can not set level.  Valid levels are: debug, info, error, fatal, warn";
        }
        
        int nLevel = Level.toLevel(level.toLowerCase()).toInt();
        
        switch (nLevel) {
        case Level.DEBUG_INT:
            Logger.getLogger(loggerName).setLevel(Level.DEBUG);
            break;
        case Level.INFO_INT:
            Logger.getLogger(loggerName).setLevel(Level.INFO);
            break;
        case Level.ERROR_INT:
            Logger.getLogger(loggerName).setLevel(Level.ERROR);
            break;
        case Level.FATAL_INT:
            Logger.getLogger(loggerName).setLevel(Level.FATAL);
            break;
        case Level.WARN_INT:
            Logger.getLogger(loggerName).setLevel(Level.WARN);
            break;
        }
        
        return loggerName + " has been set to " + level + ".\nModify log4j.properties in the Mule conf for permanent changes";
    }

    /**
     * Check that the level is a valid one
     * @param level
     */
    private boolean checkLevel(String level) {
        
        if(level.equalsIgnoreCase("debug") || 
                level.equalsIgnoreCase("error") ||
                level.equalsIgnoreCase("debug") ||
                level.equalsIgnoreCase("fatal") ||
                level.equalsIgnoreCase("warn") ||
                level.equalsIgnoreCase("info"))
        {
            return true;
        }else
        {
            return false;
        }
    }

}