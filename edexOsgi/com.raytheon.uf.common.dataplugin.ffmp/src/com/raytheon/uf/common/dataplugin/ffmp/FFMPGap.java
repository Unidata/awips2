package com.raytheon.uf.common.dataplugin.ffmp;

/**
 * gap for FFMP
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/03/11     7334        D. Hladky   Initial release
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

import java.util.ArrayList;
import java.util.Date;

public class FFMPGap {

    private Date startTime;

    private Date endTime;

    private double gap;
    
    public FFMPGap() {
        
    }

    public FFMPGap(Date startTime, Date endTime) {
        this.startTime = startTime;
        this.endTime = endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

    public Date getEndTime() {
        return endTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setGap(double gap) {
        this.gap = gap;
    }

    public double getGap() {
        return gap;
    }

    /**
     * Gets the GAP calculation for an FFMP source
     * 
     * @return Array of Gap data
     */
    public static ArrayList<FFMPGap> getGaps(ArrayList<Date> times,
            long expirationTime, Date barrierTime, Date mostRecentTime) {
        ArrayList<FFMPGap> gaps = new ArrayList<FFMPGap>();
        long gapStep = expirationTime * 60 * 1000;
        Date prevTime = null;
//        System.out.println("Calling getGaps()...Recent Time: " + mostRecentTime
//                + " BarrierTime: " + barrierTime);
        if (times.size() == 1) {
            FFMPGap gap = new FFMPGap();
            long totalMillis = mostRecentTime.getTime() - barrierTime.getTime() - gapStep;
            float gapMinutes = (totalMillis)/(60 * 1000);
            if (gapMinutes < 0.0) {
                gapMinutes = 0.0f;
            }
            gap.setGap(gapMinutes);
            gaps.add(gap);
            return gaps;
        }
        
        for (Date time : times) {
            // skip times that aren't in the current range
            if (time.before(barrierTime) || time.after(mostRecentTime)) {
                // System.out.println("Skipping time: " + time);
                continue;
            } else {
                if (prevTime == null) {
                    prevTime = barrierTime;
                }
            }
            long gapTime = time.getTime() - prevTime.getTime();
            if (gapTime > gapStep) {
                FFMPGap gap = new FFMPGap(prevTime, time);
                // convert to minutes and set gap
                // Need to subtract the expirationTime from the gapTime as well
                gap.setGap((gapTime - gapStep) / (60 * 1000));
                gaps.add(gap);
            }
            prevTime = time;
        }

        return gaps;
    }
}
