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
package com.raytheon.viz.lightning;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * <p>
 * When using a bin offset, display each bin in multiple frames. For example a
 * bin offset can be used to group data into 1 minute intervals and if the
 * repeat count is 5 then each bin will be displayed in 5 frames and each frame
 * would last 5 minutes. For example the first diagram below shows how 10 one
 * minute bins get grouped into 6 five minute frames. Also, the 12:04 and 12:05
 * are repeated in 5 different frames, if the time line was extended infinitely
 * then all other bins would be repeated.
 * </p>
 * 
 * <h1>Repeating Bin Offset(1 minute repeated 5 times)</h1>
 * 
 * <pre>
 * Bin times:  |-12:00-|-12:01-|-12:02-|-12:03-|-12:04-|-12:05-|-12:06-|-12:07-|-12:08-|-12:09-|
 * Frame 1:    |-------+-------+-------+-------+-------|
 * Frame 2:            |-------+-------+-------+-------+-------|
 * Frame 3:                    |-------+-------+-------+-------+-------|
 * Frame 4:                            |-------+-------+-------+-------+-------|
 * Frame 5:                                    |-------+-------+-------+-------+-------|
 * Frame 6:                                            |-------+-------+-------+-------+-------|
 * </pre>
 * 
 * <p>
 * For comparison here is a chart showing a normal BinOffset of 5 minutes and 1
 * minutes.
 * </p>
 * 
 * <h1>5 Minute Bin Offset(No Repeating)</h1>
 * 
 * <pre>
 * Bin times:  |-12:00---------------------------12:04-|-12:05---------------------------12:09-|
 * Frame 1:    |---------------------------------------|
 * Frame 2:                                            |---------------------------------------|
 * </pre>
 * 
 * <h1>1 Minute Bin Offset(No Repeating)</h1>
 * 
 * <pre>
 * Bin times:  |-12:00-|-12:01-|-12:02-|-12:03-|-12:04-|-12:05-|-12:06-|-12:07-|-12:08-|-12:09-|
 * Frame 1:    |-------|
 * Frame 2:            |-------|
 * Frame 3:                    |-------|
 * Frame 4:                            |-------|
 * Frame 5:                                    |-------|
 * Frame 6:                                            |-------|
 * Frame 7:                                                    |-------|
 * Frame 8:                                                            |-------|
 * Frame 9:                                                                    |-------|
 * Frame 10:                                                                           |-------|
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Sep 25, 2015  4605     bsteffen  Initial Creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RepeatingBinOffset {

    private final BinOffset binOffset;

    private final int repeatCount;

    public RepeatingBinOffset(BinOffset binOffset, int repeatCount) {
        this.binOffset = binOffset;
        this.repeatCount = repeatCount;
    }

    /**
     * For a given data time this will return the times of all the normalized
     * frames which should contain this data.
     */
    public List<DataTime> getNormalizedTimes(DataTime time) {
        return repeatNormalizedTime(binOffset.getNormalizedTime(time), false);
    }

    /**
     * Determine all the normalized frame times that contain data within the
     * specified range.
     */
    public List<DataTime> getNormalizedTimes(TimeRange range) {
        /*
         * Start off defining the result as all the times that should include
         * the end time.
         */
        DataTime normalEnd = binOffset.getNormalizedTime(new DataTime(range
                .getEnd()));
        List<DataTime> result = repeatNormalizedTime(normalEnd, false);
        /* Add all the bins that exist from the start time to the end time. */
        DataTime normalStart = binOffset.getNormalizedTime(new DataTime(range
                .getStart()));
        long intervalMs = binOffset.getInterval() * 1000L;
        while (!normalEnd.equals(normalStart)) {
            result.add(normalStart);
            long newTimeMs = normalStart.getRefTime().getTime() + intervalMs;
            normalStart = new DataTime(new Date(newTimeMs));
        }
        return result;
    }

    /**
     * When requesting data for a specific time, this can be used to get the
     * time of all the bins contained in a single frame of data. This is useful
     * for requesting data through code that understands a {@link BinOffset} but
     * does not understand this class.
     */
    public List<DataTime> getBinsToRequest(DataTime time) {
        return repeatNormalizedTime(time, true);
    }

    /**
     * Return a list of repeatCount times that are spaces according to the binOffset interval.
     */
    private List<DataTime> repeatNormalizedTime(DataTime time, boolean backwards) {
        List<DataTime> result = new ArrayList<>(repeatCount);
        result.add(time);
        long intervalMs = binOffset.getInterval() * 1000L;
        if (backwards) {
            intervalMs *= -1;
        }

        for (int i = 1; i < repeatCount; i += 1) {
            long newTimeMs = time.getRefTime().getTime() + intervalMs;
            time = new DataTime(new Date(newTimeMs));
            result.add(time);
        }
        return result;
    }

    public int getInterval() {
        return binOffset.getInterval() * repeatCount;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((binOffset == null) ? 0 : binOffset.hashCode());
        result = prime * result + repeatCount;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RepeatingBinOffset other = (RepeatingBinOffset) obj;
        if (binOffset == null) {
            if (other.binOffset != null)
                return false;
        } else if (!binOffset.equals(other.binOffset))
            return false;
        if (repeatCount != other.repeatCount)
            return false;
        return true;
    }

}
