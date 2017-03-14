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
package com.raytheon.viz.mpe.util;

import java.util.ArrayList;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class EstDailyTStations {

    DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    public void estimate_daily_tstations(int j,
            ArrayList<Station> temperature_stations, int numTstations) {

        int isom = dqc.isom;
        int maxmin_used = dqc.maxmin_used;
        int mpe_dqc_max_temp_neighbors = dqc.mpe_dqc_max_temp_neighbors;
        int mpe_dqc_min_good_stations = dqc.mpe_dqc_min_good_stations;
        int max_tstations = numTstations;
        int m, k, i, l, ii;
        double lat1, lon1, fdist, fdata, lat, lon;
        double distlon, dist, df;
        float conv = .0174f;
        float temp_climo = 0;
        float temp_climo1 = 0;
        int h;

        if (dqc.tdata[j].data_time == null) {
            return;
        }

        for (m = 0; m < max_tstations; m++) {

            /* Don't estimate missing 24 hour tstations */
            /*
             * Set the estimate factor which specifies where the estimate falls
             * in relation to the max/min temperatures to missing.
             */
            for (h = 0; h < 4; h++) {
                dqc.tdata[j].tstn[m].tlevel2[h].a = -99;
            }

            /*
             * Only compute estimate factors for cases where there are data for
             * all four six hour periods and the max/min.
             */
            for (h = 0; h < 6; h++) {
                if (dqc.tdata[j].tstn[m].tlevel2[h].data == -99
                        || (dqc.tdata[j].tstn[m].tlevel2[h].qual != 0
                                && dqc.tdata[j].tstn[m].tlevel2[h].qual != 8
                                && dqc.tdata[j].tstn[m].tlevel2[h].qual != 3 && dqc.tdata[j].tstn[m].tlevel2[h].qual != 2)) {
                    break;
                }
            }

            if (h != 6) {
                continue;
            }

            for (h = 0; h < 4; h++) {
                /*
                 * Added Nov. 28, 2007. Check to make sure the 6 hour value
                 * falls within the daily max/min value. If it doesn't then this
                 * station should not be used to estimate the temperature values
                 * of neighboring stations.
                 */
                if ((dqc.tdata[j].tstn[m].tlevel2[h].data >= dqc.tdata[j].tstn[m].tlevel2[5].data)
                        && (dqc.tdata[j].tstn[m].tlevel2[h].data <= dqc.tdata[j].tstn[m].tlevel2[4].data)) {
                    dqc.tdata[j].tstn[m].tlevel2[h].a = (dqc.tdata[j].tstn[m].tlevel2[h].data - dqc.tdata[j].tstn[m].tlevel2[5].data)
                            / (dqc.tdata[j].tstn[m].tlevel2[4].data - dqc.tdata[j].tstn[m].tlevel2[5].data);
                }

            }

        }

        for (m = 0; m < max_tstations; m++) {

            /*
             * If there are any six hour periods which have non-missing,
             * non-time-distributed data, then do not estimate for this station.
             */
            for (k = 0; k < 4; k++) {
                if (dqc.tdata[j].tstn[m].tlevel2[k].data != -99
                        && dqc.tdata[j].tstn[m].tlevel2[k].qual != 6) {
                    break;
                }
            }

            if (k != 4) {
                continue;
            }

            /*
             * Make sure all four six hour periods is set to missing. This
             * erases previously time distributed data.
             */
            for (k = 0; k < 4; k++) {
                dqc.tdata[j].tstn[m].tlevel2[k].data = -99;
            }

            /*
             * If either or both the max/min values are missing, then do not
             * estimate for this station.
             */
            if (dqc.tdata[j].tstn[m].tlevel2[4].data == -99
                    || dqc.tdata[j].tstn[m].tlevel2[5].data == -99) {
                continue;
            }

            /*
             * If the max/min value is bad, then do not estimate for this
             * station.
             */
            if (dqc.tdata[j].tstn[m].tlevel2[4].qual == 1
                    || dqc.tdata[j].tstn[m].tlevel2[4].qual == 5
                    || dqc.tdata[j].tstn[m].tlevel2[5].qual == 1
                    || dqc.tdata[j].tstn[m].tlevel2[5].qual == 5) {
                continue;
            }

            lat1 = temperature_stations.get(m).lat;
            lon1 = temperature_stations.get(m).lon;

            /* Retrieve the climate information for this station. */
            if (maxmin_used == 1) {
                temp_climo1 = (temperature_stations.get(m).max[isom] + temperature_stations
                        .get(m).min[isom]) / 2;
            }

            /* For each six hour period ... */
            for (k = 0; k < 4; k++) {
                fdist = 0.0;
                fdata = 0.0;

                l = 0;

                /*
                 * Look at the temperature values of the station's neighbors.
                 */
                for (ii = 0; ii < mpe_dqc_max_temp_neighbors; ii++) {

                    i = temperature_stations.get(m).index[ii];

                    /* dont estimate unless good or forced good */

                    if (dqc.tdata[j].tstn[i].tlevel2[k].qual != 0
                            && dqc.tdata[j].tstn[i].tlevel2[k].qual != 8
                            && dqc.tdata[j].tstn[i].tlevel2[k].qual != 3
                            && dqc.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                        continue;
                    }

                    /* dont use missing tstations */

                    if (dqc.tdata[j].tstn[i].tlevel2[k].data == -99
                            || dqc.tdata[j].tstn[i].tlevel2[k].a < -98) {
                        continue;
                    }

                    /* Retrieve the climate information for this station. */
                    if (maxmin_used == 1) {
                        temp_climo = (temperature_stations.get(i).max[isom] + temperature_stations
                                .get(i).min[isom]) / 2;
                    }

                    /* Compute distance between stations. */
                    lat = temperature_stations.get(i).lat;
                    lon = temperature_stations.get(i).lon;

                    distlon = (lon1 - lon)
                            * Math.cos(((lat1 + lat) / 2) * conv);

                    dist = Math.pow((lat1 - lat), 2) + Math.pow((distlon), 2);

                    dist = Math.pow(dist, .5) * 60;

                    df = 50 * (Math.abs(temperature_stations.get(m).elev
                            - temperature_stations.get(i).elev) / 5280);

                    dist = dist + df;

                    if (dist == 0.0) {
                        dist = .000001;
                    }
                    dist = 1 / dist;

                    if ((maxmin_used == 1) && (temp_climo1 > -99)
                            && (temp_climo > -99)) {
                        fdata = fdata
                                + dqc.tdata[j].tstn[i].tlevel2[k].a
                                * dist * (temp_climo1 / temp_climo);
                    } else {
                        fdata = fdata
                                + dqc.tdata[j].tstn[i].tlevel2[k].a
                                * dist;
                    }

                    fdist = fdist + dist;

                    l++;

                    if (l == mpe_dqc_min_good_stations) {
                        break;
                    }

                }

                if (l < mpe_dqc_min_good_stations) {

                    fdist = 0.0;
                    fdata = 0.0;

                    l = 0;

                    for (i = 0; i < max_tstations; i++) {

                        if (i == m) {
                            continue;
                        }

                        if (dqc.tdata[j].tstn[i].tlevel2[k].qual != 0
                                && dqc.tdata[j].tstn[i].tlevel2[k].qual != 8
                                && dqc.tdata[j].tstn[i].tlevel2[k].qual != 3
                                && dqc.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                            continue;
                        }

                        if (dqc.tdata[j].tstn[i].tlevel2[k].data == -99
                                || dqc.tdata[j].tstn[i].tlevel2[k].a < -98) {
                            continue;
                        }

                        /* Retrieve the climate information for this station. */
                        if (maxmin_used == 1) {
                            temp_climo = (temperature_stations.get(i).max[isom] + temperature_stations
                                    .get(i).min[isom]) / 2;
                        }

                        lat = temperature_stations.get(i).lat;
                        lon = temperature_stations.get(i).lon;

                        distlon = (lon1 - lon)
                                * Math.cos((lat1 + lat) / 2 * conv);

                        dist = Math.pow((lat1 - lat), 2)
                                + Math.pow((distlon), 2);

                        dist = Math.pow(dist, .5) * 60;

                        // GeodeticCalculator gc = new GeodeticCalculator();
                        // gc.setStartingGeographicPoint(lon1, lat1);
                        // gc.setDestinationGeographicPoint(lon, lat);
                        // dist = gc.getOrthodromicDistance();

                        df = 50 * (Math.abs(temperature_stations.get(m).elev
                                - temperature_stations.get(i).elev) / 5280);

                        dist = dist + df;

                        if (dist == 0.0) {
                            dist = .000001;
                        }

                        dist = 1 / dist;

                        if ((maxmin_used == 1) && (temp_climo1 > -99)
                                && (temp_climo > -99)) {
                            fdata = fdata
                                    + dqc.tdata[j].tstn[i].tlevel2[k].a
                                    * dist * (temp_climo1 / temp_climo);
                        } else {
                            fdata = fdata
                                    + dqc.tdata[j].tstn[i].tlevel2[k].a
                                    * dist;
                        }

                        fdist = fdist + dist;

                        l++;

                    }

                }

                if (l != 0) {

                    dqc.tdata[j].tstn[m].tlevel2[k].a = (float) (fdata / fdist);
                    dqc.tdata[j].tstn[m].tlevel2[k].data = dqc.tdata[j].tstn[m].tlevel2[k].a
                            * (dqc.tdata[j].tstn[m].tlevel2[4].data - dqc.tdata[j].tstn[m].tlevel2[5].data)
                            + dqc.tdata[j].tstn[m].tlevel2[5].data;
                    dqc.tdata[j].tstn[m].tlevel2[k].qual = 6;

                }
            }
        }
    }
}
