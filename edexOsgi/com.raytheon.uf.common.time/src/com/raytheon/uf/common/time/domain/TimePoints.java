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

import java.util.Date;

import com.raytheon.uf.common.time.domain.api.ITimePoint;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Utility class to work with {@link ITimePoint}s.
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

public final class TimePoints {

    /**
     * Return the {@link ITimePoint} instance for the specified milliseconds.
     * 
     * @return the milliseconds as an {@link ITimePoint}
     */
    public static ITimePoint fromMillis(long milliseconds) {
        return new TimePoint(milliseconds);
    }

    /**
     * Return the {@link ITimePoint} instance for the specified {@link Date}.
     * 
     * @return the date as an {@link ITimePoint}
     */
    public static ITimePoint fromDate(Date date) {
        return new TimePoint(date.getTime());
    }

    /**
     * Return the {@link ITimePoint} instance of the current time.
     * 
     * @return now, as an {@link ITimePoint}
     */
    public static ITimePoint now() {
        return fromDate(TimeUtil.newImmutableDate());
    }

    private TimePoints() {
    }
}
