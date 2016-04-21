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
package com.raytheon.edex.plugin.gfe.log;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Used by python code where old GFE called into static LogStream
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 14, 2008				njensen	    Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EdexLogStream {

    private static final Log logger = LogFactory.getLog(EdexLogStream.class);

    public static void logEvent(String message) {
        if (logger.isInfoEnabled()) {
            logger.info(message);
        }
    }

    public static void logProblem(String message) {
        logger.error(message);
    }

    public static void logVerbose(String message) {
        logger.debug(message);
    }

    public static void logDebug(String message) {
        logger.debug(message);
    }

}
