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
package com.raytheon.uf.viz.npp;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Queue;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Class for time based utility functions for NPP data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NPPTimeUtility {

    /**
     * Group the {@link DataTime} collection based on
     * {@link #groupTimeRangeMinutes}
     * 
     * @param dataTimes
     * @return
     */
    public static Collection<DataTime> groupTimes(
            Collection<DataTime> dataTimes, long groupTimeInMillis) {
        List<DataTime> grouped = new ArrayList<DataTime>(dataTimes.size());
        Queue<DataTime> objects = new ArrayDeque<DataTime>(dataTimes);
        while (objects.size() > 0) {
            DataTime current = objects.remove();
            TimeRange prev, curr;
            prev = curr = current.getValidPeriod();
            while (curr != null) {
                prev = curr;
                curr = match(objects.iterator(), prev, groupTimeInMillis);
            }

            grouped.add(new DataTime(prev.getStart().getTime(), prev));
        }
        return grouped;
    }

    /**
     * Given a {@link DataTime} iterator and an existing time range, find a
     * {@link DataTime} that can included in the range based on
     * groupTimeInMillis. That DataTime is then removed from the iterator.
     * Returns the new TimeRange or null if no DataTime could be included
     * 
     * @param iter
     * @param time
     * @param groupTimeInMillis
     * @return
     */
    public static TimeRange match(Iterator<DataTime> iter, TimeRange time,
            long groupTimeInMillis) {
        long startT = time.getStart().getTime();
        long endT = time.getEnd().getTime();
        while (iter.hasNext()) {
            DataTime dt = iter.next();
            TimeRange dtRange = dt.getValidPeriod();
            long s = dtRange.getStart().getTime();
            long e = dtRange.getEnd().getTime();
            long startCheck = s - groupTimeInMillis;
            long endCheck = e + groupTimeInMillis;
            if ((startT <= startCheck && endT >= startCheck)
                    || (startCheck <= startT && endCheck >= startT)) {
                iter.remove();
                return new TimeRange(Math.min(s, startT), Math.max(e, endT));
            }
        }
        return null;
    }

}
