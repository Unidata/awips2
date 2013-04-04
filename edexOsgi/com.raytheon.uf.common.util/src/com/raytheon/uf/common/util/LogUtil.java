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
package com.raytheon.uf.common.util;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Utilities for logging.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2012 1322       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class LogUtil {

    private LogUtil() {
    }

    /**
     * Logs the message, then each iterable to the output with a new line
     * separator between each iterable.
     * 
     * <pre>
     * 
     * Example:
     * 
     * This would be the message:
     *    iterable1
     *    iterable2
     * </pre>
     * 
     * @param statusHandler
     *            the status handler
     * @param priority
     *            the priority
     * @param message
     *            the message
     * @param iterable
     *            the iterables
     */
    public static <T> void logIterable(IUFStatusHandler statusHandler,
            Priority priority, String message, Iterable<T> iterables) {
        String msg = StringUtil.createMessage(message, iterables, 3);
        statusHandler.handle(priority, msg);
    }
}
