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

import java.util.List;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * Estimates the daily MPE temperature stations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            snaples     Initial creation
 * Mar 10, 2017  19625     snaples     Fixed erroneous estimates.
 * Oct 03, 2017  6407      bkowal      Eliminated warnings.
 * Oct 04, 2017  19908     snaples     Added check for neighbors to not exceed station count.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class EstDailyTStations {

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public void estimate_daily_tstations(int j,
            List<Station> temperature_stations, int numTstations) {
        int isom = DailyQcUtils.isom;
        boolean maxmin_used = dqc.maxmin_used;
        int mpe_dqc_max_temp_neighbors = DailyQcUtils.mpe_dqc_max_temp_neighbors;
        int mpe_dqc_min_good_stations = dqc.mpe_dqc_min_good_stations;
        int max_tstations = numTstations;
        int m, k, i, l, ii;
        double lat1, lon1, fdist, fdata, lat, lon;
        double distlon, dist, df;
        float conv = .0174f;
        float temp_climo = 0;
        float temp_climo1 = 0;
        int h;

        if (DailyQcUtils.tdata[j].data_time == null) {
            return;
        }

        if ( mpe_dqc_max_temp_neighbors > max_tstations){
            mpe_dqc_max_temp_neighbors = max_tstations;
        }
        
        for (m = 0; m < max_tstations; m++) {

            /* Don't estimate missing 24 hour tstations */
            /*
             * Set the estimate factor which specifies where the estimate falls
             * in relation to the max/min temperatures to missing.
             */
            for (h = 0; h < 4; h++) {
                DailyQcUtils.tdata[j].tstn[m].tlevel2[h].a = -99;
            }

            /*
             * Only compute estimate factors for cases where there are data for
             * all four six hour periods and the max/min.
             */
            for (h = 0; h < 6; h++) {
                if (DailyQcUtils.tdata[j].tstn[m].tlevel2[h].data == -99
                        || (DailyQcUtils.tdata[j].tstn[m].tlevel2[h].qual != 0
                                && DailyQcUtils.tdata[j].tstn[m].tlevel2[h].qual != 8
                                && DailyQcUtils.tdata[j].tstn[m].tlevel2[h].qual != 3
                                && DailyQcUtils.tdata[j].tstn[m].tlevel2[h].qual != 2)) {
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
                if ((DailyQcUtils.tdata[j].tstn[m].tlevel2[h].data >= DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data)
                        && (DailyQcUtils.tdata[j].tstn[m].tlevel2[h].data <= DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data)) {
                    DailyQcUtils.tdata[j].tstn[m].tlevel2[h].a = (DailyQcUtils.tdata[j].tstn[m].tlevel2[h].data
                            - DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data)
                            / (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data
                                    - DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data);
                }

            }

        }

        for (m = 0; m < max_tstations; m++) {

            /*
             * If there are any six hour periods which have non-missing,
             * non-time-distributed data, then do not estimate for this station.
             */
            for (k = 0; k < 4; k++) {
                if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data != -99
                        && DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual != 6) {
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
                DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data = -99;
            }

            /*
             * If either or both the max/min values are missing, then do not
             * estimate for this station.
             */
            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data == -99
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data == -99) {
                continue;
            }

            /*
             * If the max/min value is bad, then do not estimate for this
             * station.
             */
            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].qual == 1
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[4].qual == 5
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[5].qual == 1
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[5].qual == 5) {
                continue;
            }

            lat1 = temperature_stations.get(m).lat;
            lon1 = temperature_stations.get(m).lon;

            /* Retrieve the climate information for this station. */
            if (maxmin_used) {
                temp_climo1 = (temperature_stations.get(m).max[isom]
                        + temperature_stations.get(m).min[isom]) / 2;
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

                    if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 0
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 8
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 3
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                        continue;
                    }

                    /* dont use missing tstations */

                    if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data == -99
                            || DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a < -98) {
                        continue;
                    }

                    /* Retrieve the climate information for this station. */
                    if (maxmin_used) {
                        temp_climo = (temperature_stations.get(i).max[isom]
                                + temperature_stations.get(i).min[isom]) / 2;
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

                    dist += df;

                    if (dist == 0.0) {
                        dist = .000001;
                    }
                    dist = 1 / dist;

                    if ((maxmin_used) && (temp_climo1 > -99)
                            && (temp_climo > -99)) {
                        fdata += DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a
                                * dist * (temp_climo1 / temp_climo);
                    } else {
                        fdata += DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a
                                * dist;
                    }

                    fdist += dist;

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

                        if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 0
                                && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 8
                                && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 3
                                && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                            continue;
                        }

                        if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data == -99
                                || DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a < -98) {
                            continue;
                        }

                        /* Retrieve the climate information for this station. */
                        if (maxmin_used) {
                            temp_climo = (temperature_stations.get(i).max[isom]
                                    + temperature_stations.get(i).min[isom])
                                    / 2;
                        }

                        lat = temperature_stations.get(i).lat;
                        lon = temperature_stations.get(i).lon;

                        distlon = (lon1 - lon)
                                * Math.cos((lat1 + lat) / 2 * conv);

                        dist = Math.pow((lat1 - lat), 2)
                                + Math.pow((distlon), 2);

                        dist = Math.pow(dist, .5) * 60;

                        df = 50 * (Math
                                .abs(temperature_stations.get(m).elev
                                        - temperature_stations.get(i).elev)
                                / 5280);

                        dist += df;

                        if (dist == 0.0) {
                            dist = .000001;
                        }

                        dist = 1 / dist;

                        if ((maxmin_used) && (temp_climo1 > -99)
                                && (temp_climo > -99)) {
                            fdata += DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a
                                    * dist * (temp_climo1 / temp_climo);
                        } else {
                            fdata += DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a
                                    * dist;
                        }

                        fdist += dist;

                        l++;

                    }

                }

                if (l != 0) {
                    DailyQcUtils.tdata[j].tstn[m].tlevel2[k].a = (float) (fdata
                            / fdist);
                    DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data = DailyQcUtils.tdata[j].tstn[m].tlevel2[k].a
                            * (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data
                                    - DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data)
                            + DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data;
                    DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 6;
                }
            }
        }
    }
}
