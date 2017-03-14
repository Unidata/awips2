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
package com.raytheon.viz.gfe.log;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;

/**
 * LogStream implementation for Viz. Deprecated. Use python's built in logging
 * with a UFStatusHandler instead.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 3, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@Deprecated
public class VizLogStream {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(VizLogStream.class);

    public static void logEvent(String message) {
        statusHandler.handle(Priority.EVENTA, message);
    }

    public static void logProblem(String message) {
        statusHandler.handle(Priority.PROBLEM, message);
    }

    public static void logVerbose(String message) {
        statusHandler.handle(Priority.VERBOSE, message);
    }

    public static void logDebug(String message) {
        statusHandler.handle(Priority.EVENTB, message);
    }

}
