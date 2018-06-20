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
package com.raytheon.viz.hydro.util;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.timeseries.TimeSeriesDataManager;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Contains functions related to loading the max fcst info into the RiverStatus
 * table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LoadMaxFcst {

    /**
     * Loads the max fcst info into the RiverStatus table for the current
     * location and pe.
     * 
     * @param lid
     *            The location id
     * @param pe
     *            The physical element
     * @param ts
     *            The type source
     */
    public static void loadMaxFcstItem(String lid, String pe, String ts)
            throws VizException {
        TimeSeriesDataManager dman = TimeSeriesDataManager.getInstance();
        boolean useLatest = false;
        Date basisBeginTime = null;
        ArrayList<Forecast> fcstList = null;
        Forecast maxFcstRecord = null;

        /*
         * get the setting for the use_latest_fcst field for the current
         * location from the riverstat table.
         */
        String useLatestFcst = dman.getUseLatestForecast(lid);

        if (useLatestFcst == null) {
            useLatest = true;
        } else {
            if (useLatestFcst.equalsIgnoreCase("F")) {
                useLatest = false;
            } else {
                useLatest = true;
            }
        }

        /*
         * get the forecast time series for this location, pe, and ts using any
         * instructions on any type-source to screen and whether to use only the
         * latest basis time
         */
        SetTimeVals setTimeVals = new SetTimeVals();
        basisBeginTime = setTimeVals.getBasisTime();

        fcstList = HydroData.bldTsFcstRiv(lid, pe, ts, useLatest,
                basisBeginTime);

        /*
         * find the data for this location, pe, and ts given the forecast
         * time-series and the count of values in it. if data found, determine
         * its max and load the value
         */
        if ((fcstList != null) && (fcstList.size() > 0)) {
            maxFcstRecord = findMaxFcst(fcstList);

            /* load the maximum data into the RiverStatus table */
            HydroData.loadRiverStatus(maxFcstRecord);

        } else {
            /*
             * if no data were found, then delete any entries that may exist for
             * this key. this is needed if general applications are using this
             * function directly and delete all forecast data for a given key
             */
            HydroData.deleteRiverStatus(lid, pe, ts);
        }

        return;
    }

    /**
     * This gets the max forecast value from a forecast time-series that has
     * already been prepared. This function returns the ts, value, basistime,
     * and validtime for the maximum forecast value.
     * 
     * @param fcstList
     *            ArrayList of Forecast data
     * @return The max Forecast object
     */
    private static Forecast findMaxFcst(ArrayList<Forecast> fcstList) {
        Forecast max = null;
        double maxValue = HydroConstants.MISSING_VALUE;
        int maxIndex = HydroConstants.MISSING_VALUE;

        /* just in case */
        if ((fcstList == null) || (fcstList.size() == 0)) {
            return null;
        }

        /* loop and get the max */
        for (int i = 0; i < fcstList.size(); i++) {
            if (fcstList.get(i).getValue() > maxValue) {
                maxValue = fcstList.get(i).getValue();
                maxIndex = i;
            }
        }

        /* if for some bizarre reason, load the first record */
        if (maxIndex == HydroConstants.MISSING_VALUE) {
            // TODO Log message here
            // fprintf(stderr, "ERROR - find_maxfcst couldn't find max?!\n");
            maxIndex = 0;
        }

        max = fcstList.get(maxIndex);

        return max;
    }

    /**
     * Process forecast data for the given tablename.
     * 
     * @param table
     *            The table
     * @param lid
     *            The location id
     * @param pe
     *            The physical element
     */
    public static void loadMaxFcstDataLidPe(String table, String lid, String pe) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        /* get current local system time */
        Calendar now = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        /*
         * loop on all the ts entries for forecast data in the future and for
         * the appropriate table
         */
        String where = String.format(
                " WHERE lid='%s' AND pe='%s' AND validtime > '%s' AND "
                        + " probability < 0.0 ", lid, pe, sdf.format(now
                        .getTime()));
        ArrayList<String[]> uList = null;

        try {
            uList = HydroData.loadUnique("ts", table, where);

            if ((uList == null) || (uList.size() == 0)) {
                return;
            }

            /* extract the ts code from the unique string. */
            for (String[] sa : uList) {
                String ts = sa[0];

                /*
                 * perform the load_maxfcst operations for this lid, pe, ts
                 */
                loadMaxFcstItem(lid, pe, ts);
            }

        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
