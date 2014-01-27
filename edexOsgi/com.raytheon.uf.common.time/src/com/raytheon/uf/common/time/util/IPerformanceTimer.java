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
package com.raytheon.uf.common.time.util;

import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.time.domain.api.IDuration;

/**
 * Extends {@link ITimer} to add some performance timing characteristics
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2013 2095       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IPerformanceTimer extends ITimer {

    /**
     * Marks a lap on the timer. Each invocation will return the time since
     * either the timer was started, or the last time {@link #lap()} was called.
     * By default will use a name of "lap-1, lap-2, ...". Lap names can be
     * customized by using the {@link #lap(String)} version.
     * 
     * @return the duration of the lap
     */
    IDuration lap();

    /**
     * Marks a lap on the timer. Each invocation will return the time since
     * either the timer was started, or the last time {@link #lap()} was called.
     * 
     * @param lapName
     *            the name to associate with the lap
     * @return the duration of the lap
     */
    IDuration lap(String lapName);

    /**
     * Convenience method to get the time of a lap in milliseconds.
     * 
     * @return the duration of the lap in milliseconds
     * @see {@link #lap()}
     */
    long lapMillis();

    /**
     * Convenience method to get the time of a lap in milliseconds.
     * 
     * @param lapName
     *            the name to associate with the lap
     * @return the duration of the lap in milliseconds
     * @see {@link #lap()}
     */
    long lapMillis(String lapName);

    /**
     * Logs laps to the {@link IPerformanceStatusHandler}.
     * 
     * @param prefix
     *            the base prefix which should denote the method or some other
     *            unique identifier
     * 
     * @param statusHandler
     *            the statusHandler
     */
    void logLaps(String prefix, IPerformanceStatusHandler statusHandler);
}
