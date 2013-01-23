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
package com.raytheon.viz.gfe.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * Time matcher that uses a set interval/offset and start/end time to calculate
 * DataTimes for a descriptor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GFEIntervalTimeMatcher extends GFETimeMatcher {

    private static final long HRS_TO_MILLIS = 3600 * 1000;

    private DataTime[] dataTimes = new DataTime[0];

    /**
     * Set the time matching interval data used to calculate the data times
     * 
     * @param intervalInHrs
     * @param intervalOffsetInHrs
     * @param timeRange
     */
    public void setTimeMatchingInterval(int intervalInHrs,
            int intervalOffsetInHrs, TimeRange timeRange) {
        long intervalInMillis = intervalInHrs * HRS_TO_MILLIS;
        long intervalOffsetInMillis = intervalOffsetInHrs * HRS_TO_MILLIS;

        List<DataTime> times = new ArrayList<DataTime>();
        long firstTime = timeRange.getStart().getTime();
        long lastTime = timeRange.getEnd().getTime();

        long time = ((firstTime / intervalInMillis) * intervalInMillis)
                + intervalOffsetInMillis;
        while (time <= lastTime) {
            if (time >= firstTime) {
                times.add(new DataTime(new Date(time)));
            }
            time += intervalInMillis;
        }
        dataTimes = times.toArray(new DataTime[0]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.GFETimeMatcher#calculateDescriptorTimes(com
     * .raytheon.uf.viz.core.drawables.IDescriptor,
     * com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo, java.util.Set)
     */
    @Override
    protected DataTime[] calculateDescriptorTimes(IDescriptor descriptor,
            FramesInfo currInfo, Set<GFEResource> tmbResources) {
        return Arrays.copyOf(dataTimes, dataTimes.length);
    }

}
