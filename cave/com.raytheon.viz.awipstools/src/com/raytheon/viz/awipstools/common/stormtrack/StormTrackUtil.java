/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.common.stormtrack;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;

/**
 * This class will contain various utility methods for the AWIPS Storm Track.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class StormTrackUtil {

    /* 16 minutes */
    private final int MAX_INTERVAL = 900;

    public StormTrackUtil() {
    }

    public int getFrameCount(FramesInfo info) {
        int len = makeUniqueTimes(info.getFrameTimes()).length;
        return len;
    }

    public int getCurrentFrame(FramesInfo info) {
        if (info.getFrameTimes() == null || info.getFrameCount() < 1) {
            return -1;
        }
        DataTime[] uniqueTimes = makeUniqueTimes(info.getFrameTimes());
        DataTime currTime = info.getFrameTimes()[info.getFrameIndex()];
        int i = -1;
        for (; i < uniqueTimes.length;) {
            ++i;
            if (currTime.equals(uniqueTimes[i], true)) {
                break;
            }
        }
        return i;
    }

    public DataTime[] getDataTimes(FramesInfo info) {
        return makeUniqueTimes(info.getFrameTimes());
    }

    public void setPivotIndexes(FramesInfo info, StormTrackState stormTrackState) {
        int lastFrame = getFrameCount(info) - 1;
        if (getCurrentFrame(info) == lastFrame) {
            // We Are Presently On The Last Frame.
            stormTrackState.otherPivotIndex = 0;
            stormTrackState.nextPivotIndex = lastFrame;
            stormTrackState.pivotIndex = lastFrame;
        } else {
            stormTrackState.otherPivotIndex = 0;
            stormTrackState.nextPivotIndex = -1;
            stormTrackState.pivotIndex = lastFrame;
        }
    }

    private DataTime[] makeUniqueTimes(DataTime[] times) {
        List<DataTime> uniqueTimes = new ArrayList<DataTime>();
        if (times != null && times.length > 0) {
            DataTime lastTime = times[0];
            uniqueTimes.add(times[0]);
            for (int i = 1; i < times.length; ++i) {
                if (!lastTime.equals(times[i], true)) {
                    uniqueTimes.add(times[i]);
                    lastTime = times[i];
                }
            }
        }
        return uniqueTimes.toArray(new DataTime[uniqueTimes.size()]);
    }

    /**
     * Returns the time between times in seconds using valid time
     * 
     * @param a
     * @param b
     * @return
     */
    public int timeBetweenDataTimes(DataTime a, DataTime b) {
        return (int) (Math.abs(a.getValidTime().getTimeInMillis()
                - b.getValidTime().getTimeInMillis()) / 1000);
    }

    /**
     * Returns the the smallest interval between data times in seconds
     * 
     * @param dataTimes
     * @return
     */
    public int minIntervalInSeconds(DataTime[] dataTimes) {
        int minTimeBtwnDataTimes = MAX_INTERVAL;
        for (int i = 0; i < dataTimes.length - 1; i++) {
            int timeBtwnDataTimes = timeBetweenDataTimes(dataTimes[i],
                    dataTimes[i + 1]);
            if (timeBtwnDataTimes < minTimeBtwnDataTimes) {
                minTimeBtwnDataTimes = timeBtwnDataTimes;
            }
        }

        return minTimeBtwnDataTimes;
    }

}
