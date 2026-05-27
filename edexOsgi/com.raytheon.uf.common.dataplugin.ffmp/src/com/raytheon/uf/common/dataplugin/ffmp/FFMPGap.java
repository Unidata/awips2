package com.raytheon.uf.common.dataplugin.ffmp;
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
 * 01/27/13     1478        D. Hladky   Added use of constants for calculations
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.time.util.TimeUtil;

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
     * Get the gaps in the FFMP data
     * 
     * @param times
     * @param expirationTime
     * @param barrierTime
     * @param mostRecentTime
     * @return
     */
    public static List<FFMPGap> getGaps(List<Date> times,
            long expirationTime, Date barrierTime, Date mostRecentTime) {
        ArrayList<FFMPGap> gaps = new ArrayList<FFMPGap>();
        long gapStep = expirationTime * TimeUtil.MILLIS_PER_MINUTE;
        Date prevTime = null;
//        System.out.println("Calling getGaps()...Recent Time: " + mostRecentTime
//                + " BarrierTime: " + barrierTime);
        if (times.size() == 1) {
            FFMPGap gap = new FFMPGap();
            long totalMillis = mostRecentTime.getTime() - barrierTime.getTime() - gapStep;
            float gapMinutes = totalMillis/TimeUtil.MILLIS_PER_MINUTE;
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
                gap.setGap((gapTime - gapStep) / TimeUtil.MILLIS_PER_MINUTE);
                gaps.add(gap);
            }
            prevTime = time;
        }

        return gaps;
    }
}
