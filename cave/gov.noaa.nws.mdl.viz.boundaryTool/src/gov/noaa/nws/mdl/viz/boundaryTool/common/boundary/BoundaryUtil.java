package gov.noaa.nws.mdl.viz.boundaryTool.common.boundary;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;

/**
 * 
 * @author bkowal
 * @version 1.0
 * 
 *          Slightly modified of "StormTrackUti" class
 */

public class BoundaryUtil {

    public BoundaryUtil() {
    }

    public int getFrameCount(FramesInfo info) {
        int len = makeUniqueTimes(info.getFrameTimes()).length;
        return len;
    }

    public int getCurrentFrame(FramesInfo info) {
        int returnIndex = 0;
        if (info.getFrameTimes() == null || info.getFrameCount() < 1) {
            return -1;
        }
        DataTime[] uniqueTimes = makeUniqueTimes(info.getFrameTimes());
        DataTime currTime = info.getFrameTimes()[info.getFrameIndex()];

        if (uniqueTimes == null) {
            return -1;
        }
        for (int i = 0; i < uniqueTimes.length; i++) {
            if (currTime.equals(uniqueTimes[i], true)) {
                returnIndex = i;
                break;
            }
        }
        return returnIndex;
    }

    public DataTime[] getDataTimes(FramesInfo info) {
        return makeUniqueTimes(info.getFrameTimes());
    }

    public void setPivotIndexes(FramesInfo info, BoundaryState boundaryState) {
        int lastFrame = getFrameCount(info) - 1;
        if (getCurrentFrame(info) == lastFrame) {
            // We Are Presently On The Last Frame.
            boundaryState.otherPivotIndex = 0;
            boundaryState.nextPivotIndex = lastFrame;
            boundaryState.pivotIndex = lastFrame;
        } else {
            boundaryState.otherPivotIndex = 0;
            boundaryState.nextPivotIndex = -1;
            boundaryState.pivotIndex = lastFrame;
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
     * Adjusts the angle from -360/360 to be between -180/180
     * 
     * @param angle
     * @return
     */
    protected double adjustAngle(double angle) {
        double newVal = angle % 360;
        if (newVal > 180) {
            newVal -= 360;
        } else if (newVal < -180) {
            newVal += 360;
        }
        return newVal;
    }

    /**
     * Adjusts the angle from -180/180 to be between 0/360
     * 
     * @param angle
     * @return
     */
    protected double unadjustAngle(double angle) {
        double newVal = angle;
        if (newVal < 0) {
            newVal = 360 - newVal;
        }
        return newVal;
    }

}
