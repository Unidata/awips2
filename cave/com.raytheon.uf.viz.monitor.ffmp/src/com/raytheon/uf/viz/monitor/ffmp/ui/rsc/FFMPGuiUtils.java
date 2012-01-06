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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPGuiUtils {

    public static GeometryFactory factory = new GeometryFactory();

    /**
     * Gets the upstream basins for the visualization
     * 
     * @param cwa
     * @return
     */
    public static Set<Integer> getUpStreamBasins(Long pfaf) {

        String sql = "select upstream1, upstream2, upstream3, upstream4, upstream5, upstream6 from "
                + FFMPUtils.FFMP_TABLE + " where pfaf_id = '" + pfaf + "'";
        Set<Integer> basinIds = null;

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    FFMPUtils.MAPS_DB, QueryLanguage.SQL);

            if (results.size() > 0) {
                basinIds = new HashSet<Integer>(
                        (int) (results.size() * 1.3) + 1);
                Object[] results2 = results.get(0);
                for (int i = 0; i < results2.length; i++) {
                    if (((Long) results2[i]).intValue() != 0) {
                        basinIds.add(((Long) results2[i]).intValue());
                    }
                }
            }

        } catch (VizException e) {
            e.printStackTrace();
        }

        return basinIds;
    }

    /**
     * Gets the downstream basins for the visualization
     * 
     * @param cwa
     * @return
     */
    public static Set<Long> getDownStreamBasins(int basin_id) {

        String sql = "select pfaf_id from " + FFMPUtils.FFMP_TABLE
                + " where upstream1 = '" + basin_id + "' OR upstream2 = '"
                + basin_id + "' OR upstream3 = '" + basin_id
                + "' OR upstream4 = '" + basin_id + "' OR upstream5 = '"
                + basin_id + "' OR upstream6 = '" + basin_id + "'";
        Set<Long> pfafIds = null;

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    FFMPUtils.MAPS_DB, QueryLanguage.SQL);

            if (results.size() > 0) {
                pfafIds = new HashSet<Long>((int) (results.size() * 1.3) + 1);
                Object[] results2 = results.get(0);
                for (int i = 0; i < results2.length; i++) {
                    if (((String) results2[i]) != null) {
                        pfafIds.add(Long.parseLong((String) results2[i]));
                    }
                }
            }

        } catch (VizException e) {
            e.printStackTrace();
        }

        return pfafIds;
    }

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
        return new Date(timeIn.getTime() + (3600 * 1000));
    }

    /**
     * Alter date, used in graphing
     * 
     * @param timeIn
     * @return
     */
    public static Date getHourDisplacement(Date timeIn, double hour) {
        return new Date(timeIn.getTime() - (int) (3600 * 1000 * hour));
    }

}
