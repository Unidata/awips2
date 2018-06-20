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
package com.raytheon.viz.hydrobase;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 3, 2009            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class FloodReportUtils {

    public static FloodReportData floodreptInterp(FloodReportData pt1, FloodReportData pt2, double fs) {
        boolean foundPt = false;
        long newtime;
        /*
         Init newPt to have a value of flood stage.
         */
        FloodReportData newPt = FloodReportUtils.copyFloodReportData(pt1);
        newPt.setCrest(fs);
        
        /*
         If one of the points is exactly at flood stage,
         set the returned structure to it.
         */
        if (pt1.getCrest() == fs) {
            // newPt initialized to pt1
            foundPt = true;
        } else if (pt2.getCrest() == fs) {
            newPt = FloodReportUtils.copyFloodReportData(pt2);
            foundPt = true;
        }
        
        if (!foundPt) {
            /*
              Set the point-slope variables.
             */
            long timeDiff = pt2.getCrestDate().getTime() - pt1.getCrestDate().getTime();
            double valueDiff = pt2.getCrest() - pt1.getCrest();
            
            double slope = valueDiff / timeDiff;
            
            /*
             * Calc time of flood stage crossing.
             */
            newtime = (long) (pt1.getCrestDate().getTime() + ((newPt.getCrest() - pt1.getCrest()) / slope));
            Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
            d.setTime(newtime);
            newPt.setCrestDate(d);
        }
        
        return newPt;
    }
    
    public static FloodReportData copyFloodReportData(FloodReportData data) {
        FloodReportData copy = new FloodReportData();
        copy.setCrest(data.getCrest());
        copy.setCrestDate(data.getCrestDate());
        copy.setFloodEventId(data.getFloodEventId());
        copy.setFloodStage(data.getFloodStage());
        copy.setLastCrest(data.getLastCrest());
        copy.setLid(data.getLid());
        copy.setLongName(data.getLongName());
        
        return copy;
        
    }
}
