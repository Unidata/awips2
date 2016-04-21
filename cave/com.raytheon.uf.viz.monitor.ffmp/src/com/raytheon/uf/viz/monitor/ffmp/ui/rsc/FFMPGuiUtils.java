package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.SortedSet;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * FFMPDbUtility things
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/29/09      2152       D. Hladky   Initial release
 * 02/01/13     1569       D. Hladky   Added constants
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPGuiUtils {

    public static GeometryFactory factory = new GeometryFactory();
    
    public static HashMap<Double, Integer> getTimeOffsets(
            ArrayList<FFMPRecord> records) {
        HashMap<Double, Integer> ctimes = new HashMap<Double, Integer>();
        // size - 1 will be the most recent
        Date start = records.get(records.size() - 1).getDataTime().getRefTime();
        for (int i = 0; i < records.size(); i++) {
            Date end = records.get(i).getDataTime().getRefTime();
            ctimes.put(getTimeDiff(start, end), i);
        }
        return ctimes;
    }

    /**
     * Gets the offsets as a list.
     * 
     * @param records
     * @return
     */
    public static ArrayList<Double> getTimeOffsetList(SortedSet<Date> times) {
        ArrayList<Double> ctimes = new ArrayList<Double>();
        // size - 1 will be the most recent
        Date start = times.last();
        for (Date time : times) {
            ctimes.add(getTimeDiff(start, time));
        }
        return ctimes;
    }

    /**
     * Finds the time displacement from the Rate
     * 
     * @param start
     * @param end
     * @return
     */
    public static double getTimeDiff(Date start, Date end) {
        long diff = start.getTime() - end.getTime();
        double seconds = (diff / 1000);
        double minutes = seconds / 60;
        double hours = minutes / 60;
        return hours;
    }

    /**
     * Add an hour from the date, used for QPF
     * 
     * @param timeIn
     * @return
     */
    public static Date get1HourForward(Date timeIn) {
        return new Date(timeIn.getTime() + (TimeUtil.MILLIS_PER_HOUR));
    }

    /**
     * Alter date, used in graphing
     * 
     * @param timeIn
     * @return
     */
    public static Date getHourDisplacement(Date timeIn, double hour) {
        return new Date(timeIn.getTime() - (int) (TimeUtil.MILLIS_PER_HOUR * hour));
    }

}
