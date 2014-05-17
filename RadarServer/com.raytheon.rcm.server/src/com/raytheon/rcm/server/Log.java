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
package com.raytheon.rcm.server;

import org.apache.commons.logging.LogFactory;

public class Log {
    public static org.apache.commons.logging.Log logger = LogFactory.getLog("RadarServer");
    
    public static void debug(String msg) {
        logger.debug(msg);
    }
	public static void debugf(String format, Object... args) {
	    debug(String.format(format, args));
	}
	public static void error(String msg) {
		logger.error(msg);
	}
	public static void errorf(String format, Object... args) {
	    Throwable throwable = null;
		for (Object arg : args) {
			/* RuntimeExceptions should be all that is needed because checked
			 * exceptions should be easily traceable, but to be safe...
			 */
			if (throwable == null && arg instanceof /*Runtime*/Throwable)
				throwable = (Throwable) arg;
		}
		String msg = String.format(format, args);
		if (throwable != null)
		    logger.error(msg, throwable);
		else
		    logger.error(msg);
	}
	public static void warn(String msg) {
		logger.warn(msg);
	}
	public static void warnf(String format, Object... args) {
	    warn(String.format(format, args));
	}
	public static void event(String msg) {
		logger.info(msg);
	}
	public static void eventf(String format, Object... args) {
	    event(String.format(format, args));
	}
}
