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
package com.raytheon.uf.common.time.domain;

import com.raytheon.uf.common.time.domain.api.ITimeInterval;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

/**
 * Utility class to work with {@link ITimeInterval}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class TimeIntervals {

    /**
     * Construct a {@link ITimeInterval} from two {@link ITimePoint}s.
     * 
     * @param start
     *            the start of the time interval
     * @param end
     *            the end of the time interval
     * @return the interval
     */
    public static ITimeInterval fromTimePoints(ITimePoint start, ITimePoint end) {
        if (end.isBefore(start)) {
            throw new IllegalArgumentException(
                    "The end time point cannot be before the start time point:\n[end = "
                            + end + "] [start = " + start + "]");
        }
        return new TimeInterval(start, end);
    }

    /**
     * No construction.
     */
    private TimeIntervals() {

    }

}
