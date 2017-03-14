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
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class EstMissingTStations {

    DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    public void estimate_missing_tstations(int j,
            ArrayList<Station> temperature_stations, int num_tstations,
            Tdata[] tdata) {

        int isom = dqc.isom;
        int maxmin_used = dqc.maxmin_used;
        int mpe_dqc_max_temp_neighbors = dqc.mpe_dqc_max_temp_neighbors;
        int mpe_dqc_min_good_stations = dqc.mpe_dqc_min_good_stations;

        int m, i, h, l, ii;
        double lat1 = 0., lon1 = 0., fdist, fdata, fval, lat, lon, distlon;
        double temp, dist;
        float conv = .0174f;
        float df;
        float a, b;
        float temp_climo = 0;
        float temp_climo1 = 0;
        int max_tstations = num_tstations;

        /* estimate max/mins first */

        for (m = 0; m < max_tstations; m++) {

            if (temperature_stations.get(m).elev <= 0) {
                continue;
            }

            for (h = 4; h < 6; h++) {

                if (tdata[j].used[h] == 0) {
                    continue;
                }

                /* only estimate missing data */

                if (tdata[j].tstn[m].tlevel2[h].data > -99
                        && tdata[j].tstn[m].tlevel2[h].qual != 5
                        && tdata[j].tstn[m].tlevel2[h].qual != 1) {
                    continue;
                }

                /*
                 * If the station was set bad by the user, use the estimated
                 * temperature which was displayed in the Edit Temperature
                 * Stations window.
                 */
                if (tdata[j].tstn[m].tlevel2[h].qual == 1) {
                    tdata[j].tstn[m].tlevel2[h].data = tdata[j].tstn[m].tlevel2[h].estimate;
                    tdata[j].tstn[m].tlevel2[h].qual = 5;
                    continue;
                }

                tdata[j].tstn[m].tlevel2[h].data = -99;

                if ((h == 4 && temperature_stations.get(m).max[isom] <= -99)
                        || (h == 5 && temperature_stations.get(m).min[isom] <= -99)) {
                    continue;
                }

                /* Retrieve the climate information for this station. */
                if (maxmin_used == 1) {
                    temp_climo1 = (temperature_stations.get(m).max[isom] + temperature_stations
                            .get(m).min[isom]) / 2;
                }

                lat1 = temperature_stations.get(m).lat;
                lon1 = temperature_stations.get(m).lon;

                fdist = 0.0;
                fdata = 0.0;
                l = 0;

                for (ii = 0; ii < mpe_dqc_max_temp_neighbors; ii++) {

                    i = temperature_stations.get(m).index[ii];

                    /* dont estimate unless good or forced good */

                    if (tdata[j].tstn[i].tlevel2[h].qual != 0
                            && tdata[j].tstn[i].tlevel2[h].qual != 8
                            && tdata[j].tstn[i].tlevel2[h].qual != 6
                            && tdata[j].tstn[i].tlevel2[h].qual != 3
                            && tdata[j].tstn[i].tlevel2[h].qual != 2) {
                        continue;
                    }

                    /* dont use missing tstations */

                    if (tdata[j].tstn[i].tlevel2[h].data == -99) {
                        continue;
                    }

                    if ((h == 4 && temperature_stations.get(i).max[isom] <= -99)
                            || (h == 5 && temperature_stations.get(i).min[isom] <= -99)) {
                        continue;
                    }

                    if (temperature_stations.get(i).elev <= 0) {
                        continue;
                    }

                    lat = temperature_stations.get(i).lat;
                    lon = temperature_stations.get(i).lon;

                    distlon = (lon1 - lon)
                            * (Math.cos((lat1 + lat) / 2) * conv);

                    dist = Math.pow((lat1 - lat), 2) + Math.pow(distlon, 2);

                    dist = Math.pow(dist, .5) * 60;

                    if (dist == 0.0) {
                        dist = .0001;
                    }

                    df = 50 * (Math.abs(temperature_stations.get(m).elev
                            - temperature_stations.get(i).elev) / 5280);

                    dist = dist + df;

                    dist = 1 / dist;

                    temp = tdata[j].tstn[i].tlevel2[h].data;

                    if (h == 4) {

                        temp = (temp + (temperature_stations.get(m).max[isom] - temperature_stations
                                .get(i).max[isom])) * dist;

                    }

                    if (h == 5) {

                        temp = (temp + (temperature_stations.get(m).min[isom] - temperature_stations
                                .get(i).min[isom])) * dist;
                    }

                    fdata = fdata + temp;

                    fdist = fdist + dist;

                    l++;

                    if (l == 8) {
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

                        if (tdata[j].tstn[i].tlevel2[h].qual != 0
                                && tdata[j].tstn[i].tlevel2[h].qual != 8
                                && tdata[j].tstn[i].tlevel2[h].qual != 6
                                && tdata[j].tstn[i].tlevel2[h].qual != 3
                                && tdata[j].tstn[i].tlevel2[h].qual != 2) {
                            continue;
                        }

                        /* dont use missing tstations */

                        if (tdata[j].tstn[i].tlevel2[h].data == -99) {
                            continue;
                        }

                        if ((h == 4 && temperature_stations.get(i).max[isom] <= -99)
                                || (h == 5 && temperature_stations.get(i).min[isom] <= -99)) {
                            continue;
                        }

                        if (temperature_stations.get(i).elev <= 0) {
                            continue;
                        }
                        lat = temperature_stations.get(i).lat;
                        lon = temperature_stations.get(i).lon;

                        distlon = (lon1 - lon)
                                * Math.cos((lat1 + lat) / 2 * conv);

                        dist = Math.pow((lat1 - lat), 2) + Math.pow(distlon, 2);

                        dist = Math.pow(dist, .5) * 60;

                        // GeodeticCalculator gc = new GeodeticCalculator();
                        // gc.setStartingGeographicPoint(lon1, lat1);
                        // gc.setDestinationGeographicPoint(lon, lat);
                        // dist = gc.getOrthodromicDistance();

                        if (dist == 0.0) {
                            dist = .0001;
                        }

                        df = 50 * Math.abs(temperature_stations.get(m).elev
                                - temperature_stations.get(i).elev) / 5280;

                        dist = dist + df;

                        dist = 1 / dist;

                        temp = tdata[j].tstn[i].tlevel2[h].data;

                        if (h == 4) {

                            temp = (temp + (temperature_stations.get(m).max[isom] - temperature_stations
                                    .get(i).max[isom])) * dist;

                        }

                        if (h == 5) {

                            temp = (temp + (temperature_stations.get(m).min[isom] - temperature_stations
                                    .get(i).min[isom])) * dist;
                        }

                        fdata = fdata + temp;

                        fdist = fdist + dist;

                        l++;

                    }

                }

                if (l == 0) {
                    fval = -99;
                } else {
                    fval = fdata / fdist;
                }

                tdata[j].tstn[m].tlevel2[h].data = (float) fval;
                tdata[j].tstn[m].tlevel2[h].qual = 5;

                /*
                 * logMessage("estimate %s %d %d\n",tstation[m].hb5,h,
                 * tdata[j].stn[m].tlevel2[h].data);
                 */

            }

            for (h = 0; h < 4; h++) {

                if (tdata[j].used[h] == 0) {
                    continue;
                }

                /*
                 * If the station was set bad by the user, use the estimated
                 * temperature which was displayed in the Edit Temperature
                 * Stations window. (added OB9.x)
                 */

                if (tdata[j].tstn[m].tlevel2[h].qual == 1) {
                    tdata[j].tstn[m].tlevel2[h].data = tdata[j].tstn[m].tlevel2[h].estimate;
                    tdata[j].tstn[m].tlevel2[h].qual = 5;
                    continue;
                }

                if (tdata[j].tstn[m].tlevel2[h].data != -99
                        && (tdata[j].tstn[m].tlevel2[h].qual == 0
                                || tdata[j].tstn[m].tlevel2[h].qual == 8
                                || tdata[j].tstn[m].tlevel2[h].qual == 6
                                || tdata[j].tstn[m].tlevel2[h].qual == 3 || tdata[j].tstn[m].tlevel2[h].qual == 2)) {
                    continue;
                }

                tdata[j].tstn[m].tlevel2[h].data = -99;

                if (tdata[j].tstn[m].tlevel2[4].data == -99
                        || tdata[j].tstn[m].tlevel2[5].data == -99) {
                    continue;
                }

                if (tdata[j].tstn[m].tlevel2[4].qual == 1
                        || tdata[j].tstn[m].tlevel2[5].qual == 1) {
                    continue;
                }

                fdist = 0.0;
                fdata = 0.0;
                l = 0;

                for (ii = 0; ii < mpe_dqc_max_temp_neighbors; ii++) {

                    i = temperature_stations.get(m).index[ii];

                    /* dont estimate unless good or forced good */

                    if (tdata[j].tstn[i].tlevel2[h].qual != 0
                            && tdata[j].tstn[i].tlevel2[h].qual != 8
                            && tdata[j].tstn[i].tlevel2[h].qual != 6
                            && tdata[j].tstn[i].tlevel2[h].qual != 3
                            && tdata[j].tstn[i].tlevel2[h].qual != 2) {
                        continue;
                    }

                    /* dont use missing tstations */

                    if (tdata[j].tstn[i].tlevel2[h].data == -99
                            || tdata[j].tstn[i].tlevel2[h].a < -98) {
                        continue;
                    }

                    if (temperature_stations.get(i).elev <= 0) {
                        continue;
                    }

                    /* Retrieve the climate information for this station. */
                    if (maxmin_used == 1) {
                        temp_climo = (temperature_stations.get(i).max[isom] + temperature_stations
                                .get(i).min[isom]) / 2;
                    }

                    lat = temperature_stations.get(i).lat;
                    lon = temperature_stations.get(i).lon;

                    distlon = (lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv);

                    dist = Math.pow((lat1 - lat), 2) + Math.pow(distlon, 2);

                    dist = Math.pow(dist, .5) * 60;

                    // GeodeticCalculator gc = new GeodeticCalculator();
                    // gc.setStartingGeographicPoint(lon1, lat1);
                    // gc.setDestinationGeographicPoint(lon, lat);
                    // dist = gc.getOrthodromicDistance();

                    if (dist == 0.0) {
                        dist = .0001;
                    }

                    df = 50 * Math.abs(temperature_stations.get(m).elev
                            - temperature_stations.get(i).elev) / 5280;

                    dist = dist + df;

                    dist = 1 / dist;

                    if ((maxmin_used == 1) && (temp_climo1 > -99)
                            && (temp_climo > -99)) {
                        fdata = fdata + tdata[j].tstn[i].tlevel2[h].a * dist
                                * (temp_climo1 / temp_climo);
                    } else {
                        fdata = fdata + tdata[j].tstn[i].tlevel2[h].a * dist;
                    }

                    fdist = fdist + dist;

                    l++;

                    if (l == 8) {
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

                        if (tdata[j].tstn[i].tlevel2[h].qual != 0
                                && tdata[j].tstn[i].tlevel2[h].qual != 8
                                && tdata[j].tstn[i].tlevel2[h].qual != 6
                                && tdata[j].tstn[i].tlevel2[h].qual != 3
                                && tdata[j].tstn[i].tlevel2[h].qual != 2) {
                            continue;
                        }

                        /* dont use missing tstations */

                        if (tdata[j].tstn[i].tlevel2[h].data == -99
                                || tdata[j].tstn[i].tlevel2[h].a < -98) {
                            continue;
                        }

                        if (temperature_stations.get(i).elev <= 0) {
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

                        dist = Math.pow((lat1 - lat), 2) + Math.pow(distlon, 2);

                        // GeodeticCalculator gc = new GeodeticCalculator();
                        // gc.setStartingGeographicPoint(lon1, lat1);
                        // gc.setDestinationGeographicPoint(lon, lat);
                        // dist = gc.getOrthodromicDistance();

                        if (dist == 0.0) {
                            dist = .0001;
                        }

                        df = 50 * Math.abs(temperature_stations.get(m).elev
                                - temperature_stations.get(i).elev) / 5280;

                        dist = dist + df;

                        dist = 1 / dist;

                        if ((maxmin_used == 1) && (temp_climo1 > -99)
                                && (temp_climo > -99)) {
                            fdata = fdata + tdata[j].tstn[i].tlevel2[h].a
                                    * dist * (temp_climo1 / temp_climo);
                        } else {
                            fdata = fdata + tdata[j].tstn[i].tlevel2[h].a
                                    * dist;
                        }

                        fdist = fdist + dist;

                        l++;

                    }

                }

                if (l == 0) {
                    tdata[j].tstn[m].tlevel2[h].data = -99;
                } else {

                    a = (float) (fdata / fdist);

                    b = a
                            * (tdata[j].tstn[m].tlevel2[4].data - tdata[j].tstn[m].tlevel2[5].data)
                            + tdata[j].tstn[m].tlevel2[5].data;

                    tdata[j].tstn[m].tlevel2[h].data = b;
                    tdata[j].tstn[m].tlevel2[h].qual = 5;

                    if (tdata[j].tstn[m].tlevel2[4].data == -99
                            || tdata[j].tstn[m].tlevel2[5].data == -99) {
                        tdata[j].tstn[m].tlevel2[h].data = -99;
                    }

                }

            }

        }
    }
}
