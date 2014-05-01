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

public class QCTStations {
    int good, ogood;

    private double fvalu;

    public void quality_control_tstations(int j,
            ArrayList<Station> temperature_stations, int numTstations) {

        ogood = 0;
        for (int k = 4; k < 6; k++) {

            if (DailyQcUtils.tdata[j].used[k] == 0) {
                ogood = 0;
                continue;
            }
            good = 0;
            qcTLoop(j, k, temperature_stations, numTstations);

            while (good > ogood) {

                ogood = good;
                good = 0;
                qcTLoop(j, k, temperature_stations, numTstations);

            }
        }

        for (int k = 0; k < 4; k++) {

            if (DailyQcUtils.tdata[j].used[k] == 0) {
                ogood = 0;
                continue;
            }
            good = 0;

            qcT6Loop(j, k, temperature_stations, numTstations);

            while (good > ogood) {

                ogood = good;
                good = 0;
                qcT6Loop(j, k, temperature_stations, numTstations);

            }
        }
    }

    private void qcTLoop(int j, int k, ArrayList<Station> temperature_stations,
            int numTstations) {
        int isom = DailyQcUtils.isom;
        int mpe_dqc_max_temp_neighbors = DailyQcUtils.mpe_dqc_max_temp_neighbors;
        int max_tstations = numTstations;
        int m, i, l, ii;
        double lat1, lon1, fdist, fdata, lat, lon;
        double distlon, dist, df, fdif;
        float conv = .0174f;
        float temp = 0;

        for (m = 0; m < max_tstations; m++) {

            if (temperature_stations.get(m).elev <= 0) {
                continue;
            }

            lat1 = temperature_stations.get(m).lat;
            lon1 = temperature_stations.get(m).lon;

            if ((k == 4 && temperature_stations.get(m).max[isom] <= -99)
                    || (k == 5 && temperature_stations.get(m).min[isom] <= -99)) {
                continue;
            }

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data == -99) {
                continue;
            }

            fdist = 0.0;
            fdata = 0.0;
            l = 0;

            for (ii = 0; ii < mpe_dqc_max_temp_neighbors; ii++) {

                i = temperature_stations.get(m).index[ii];

                /* only use good or forced good gages to estimate others */

                if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 0
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 8
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 6
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 3
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                    continue;
                }

                if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data == -99) {
                    continue;
                }

                if ((k == 4 && temperature_stations.get(i).max[isom] <= -99)
                        || (k == 5 && temperature_stations.get(i).min[isom] <= -99)) {
                    continue;
                }

                if (temperature_stations.get(i).elev <= 0) {
                    continue;
                }

                lat = temperature_stations.get(i).lat;
                lon = temperature_stations.get(i).lon;

                distlon = (Math.abs(lon1) - Math.abs(lon))
                        * Math.cos((lat1 + lat) / 2 * conv);

                dist = Math.pow((lat1 - lat), 2) + Math.pow((distlon), 2);

                dist = Math.pow(dist, .5) * 60;

                if (dist == 0.0) {
                    dist = .0001;
                }

                df = 50.0 * Math.abs(temperature_stations.get(m).elev
                        - temperature_stations.get(i).elev) / 5280;

                dist = dist + df;

                if (dist == 0.0) {
                    dist = .000001;
                }

                dist = 1 / dist;

                if (k == 4) {
                    temp = (float) ((DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data + (temperature_stations
                            .get(m).max[isom] - temperature_stations.get(i).max[isom])) * dist);
                }

                if (k == 5) {
                    temp = (float) ((DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data + (temperature_stations
                            .get(m).min[isom] - temperature_stations.get(i).min[isom])) * dist);
                }

                l++;

                fdata = fdata + temp;

                fdist = fdist + dist;

                if (l == 5) {
                    break;
                }

            }

            if (l < 5) {

                fdist = 0.0;
                fdata = 0.0;
                l = 0;

                for (i = 0; i < max_tstations; i++) {

                    if (i == m) {
                        continue;
                    }

                    /* only use good or forced good gages to estimate others */

                    if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 0
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 8
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 6
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 3
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                        continue;
                    }

                    if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data == -99) {
                        continue;
                    }

                    if ((k == 4 && temperature_stations.get(i).max[isom] <= -99)
                            || (k == 5 && temperature_stations.get(i).min[isom] <= -99)) {
                        continue;
                    }

                    if (temperature_stations.get(i).elev <= 0) {
                        continue;
                    }
                    lat = temperature_stations.get(i).lat;
                    lon = temperature_stations.get(i).lon;

                    distlon = (lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv);

                    dist = Math.pow((lat1 - lat), 2) + Math.pow((distlon), 2);

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

                    if (dist == 0.0) {
                        dist = .000001;
                    }

                    dist = 1 / dist;

                    if (k == 4) {
                        temp = (float) ((DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data + (temperature_stations
                                .get(m).max[isom] - temperature_stations.get(i).max[isom])) * dist);
                    }

                    if (k == 5) {
                        temp = (float) ((DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data + (temperature_stations
                                .get(m).min[isom] - temperature_stations.get(i).min[isom])) * dist);
                    }

                    fdata = fdata + temp;

                    fdist = fdist + dist;

                    l++;

                }

            }

            if (l == 0) {
                fdist = 1;
            }
            fvalu = fdata / fdist;
            DailyQcUtils.tdata[j].tstn[m].tlevel2[k].estimate = (short) fvalu;

            fdif = Math.abs(DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data
                    - fvalu);

            DailyQcUtils.tdata[j].tstn[m].tlevel2[k].estimate = (short) fvalu;

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 0
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 1
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 5
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 6
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 2) {
                continue;
            }

            /*
             * set the quality code as Questionable if the maximum temperature
             * is lower than a minimum temperature
             */

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data < DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data) {
                DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 3;
            }
            if (fdif > DailyQcUtils.tdata[j].stddev) {
                DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 3;
            } else {

                good++;
                if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 3) {
                    DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 8;
                }

            }
        }

    }

    private void qcT6Loop(int j, int k,
            ArrayList<Station> temperature_stations, int numTstations) {
        int isom = DailyQcUtils.isom;
        int mpe_dqc_max_temp_neighbors = DailyQcUtils.mpe_dqc_max_temp_neighbors;
        int max_tstations = numTstations;
        int m, i, l, ii;
        float a, b;
        double lat1, lon1, fdist, fdata, lat, lon;
        double distlon, dist, df, fdif;
        float conv = .0174f;
        float temp = 0;

        for (m = 0; m < max_tstations; m++) {

            if (temperature_stations.get(m).elev <= 0) {
                continue;
            }

            lat1 = temperature_stations.get(m).lat;
            lon1 = temperature_stations.get(m).lon;

            if ((temperature_stations.get(m).max[isom] <= -99)
                    || (temperature_stations.get(m).min[isom] <= -99)) {
                continue;
            }

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data == -99) {
                continue;
            }

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].qual == 1
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[5].qual == 1) {
                continue;
            }

            fdist = 0.0;
            fdata = 0.0;
            l = 0;

            for (ii = 0; ii < mpe_dqc_max_temp_neighbors; ii++) {

                i = temperature_stations.get(m).index[ii];

                /* don't estimate unless good or forced good */

                if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 0
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 8
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 6
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 3
                        && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                    continue;
                }

                /* don't use missing temperature_stationss */

                if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data == -99
                        || DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a < -98) {
                    continue;
                }

                if (temperature_stations.get(i).elev <= 0) {
                    continue;
                }

                lat = temperature_stations.get(i).lat;
                lon = temperature_stations.get(i).lon;

                distlon = (Math.abs(lon1) - Math.abs(lon))
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

                temp = (float) (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a * dist);

                fdata = fdata + temp;

                fdist = fdist + dist;

                l++;

                if (l == 5) {
                    break;
                }

            }

            if (l < 5) {

                fdist = 0.0;
                fdata = 0.0;
                l = 0;

                for (i = 0; i < max_tstations; i++) {

                    if (i == m) {
                        continue;
                    }

                    if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 0
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 8
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 6
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 3
                            && DailyQcUtils.tdata[j].tstn[i].tlevel2[k].qual != 2) {
                        continue;
                    }

                    /* dont use missing temperature_stations */

                    if (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].data == -99
                            || DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a < -98) {
                        continue;
                    }

                    if (temperature_stations.get(i).elev <= 0) {
                        continue;
                    }

                    lat = temperature_stations.get(i).lat;
                    lon = temperature_stations.get(i).lon;

                    distlon = (lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv);

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

                    temp = (float) (DailyQcUtils.tdata[j].tstn[i].tlevel2[k].a * dist);

                    fdata = fdata + temp;

                    fdist = fdist + dist;

                    l++;

                }

            }
            if (l == 0) {
                fdist = 1;
            }

            a = (float) (fdata / fdist);

            b = a
                    * (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data - DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data)
                    + DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data;

            DailyQcUtils.tdata[j].tstn[m].tlevel2[k].estimate = (short) b;

            fdif = Math.abs(DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data - b);

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 0
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 1
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 5
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 6
                    || DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 2) {
                continue;
            }

            /*
             * check if the 6h period temperature data is out of daily minimum
             * or maximum temperature, set the quality code as Questionable
             */

            if (DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data > -99
                    && DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data > -99) {

                if ((DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data > DailyQcUtils.tdata[j].tstn[m].tlevel2[4].data)
                        || (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].data < DailyQcUtils.tdata[j].tstn[m].tlevel2[5].data)) {

                    DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 3;
                }
            }
            if (fdif > DailyQcUtils.tdata[j].stddev) {
                DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 3;
            } else {

                good++;
                if (DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual == 3) {
                    DailyQcUtils.tdata[j].tstn[m].tlevel2[k].qual = 8;
                }
            }
        }
    }
}
