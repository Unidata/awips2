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

import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;

/**
 * Converts point precipitation data in struct tdata to hrap grid. The closest
 * tstations are precalculated for each grid point - this speeds computations
 * for data sets with many precipitation points. If there are no good
 * precipitation points for a grid point, then a recalculation is made using all
 * precipitation points. 1/R**2 interpolation is used. If requested, the final
 * interpolation is scaled using seasonal isohyets. The grid is saved as a disk
 * file and used later for a raster or vector (HRAP) plot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	--------------------------
 * Mar 11, 2009            snaples     	Initial creation
 * Apr 16, 2012			   mgamazaychik	DR9602 - made changes to how dist2 is calculated
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RenderT {

    ReadQPFGrids rqg = new ReadQPFGrids();

    Pcp pcp = DailyQcUtils.pcp;

    public void render_t(int pcpn_day, int pcpn_time, int pcpn_time_step,
            int numTstations, ArrayList<Station> temperature_stations,
            Hrap_Grid hrap_grid, Tdata[] tdata, int[] pcp_in_use) {

        int isom = DailyQcUtils.isom;
        int mpe_dqc_max_temp_neighbors = DailyQcUtils.mpe_dqc_max_temp_neighbors;
        int i, j, h, hh;
        int time_pos = 0;
        int htotal = 0;
        ;
        double distance, dist1, dist2, dist, value;
        double temp;
        float conv = .0174f;
        float df;
        int total1 = 0;
        int total2 = 0;

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else if (pcpn_time_step == 1) {
            time_pos = 4;
        } else if (pcpn_time_step == 2) {
            time_pos = 5;
        }

        // logMessage("time pos %d\n",time_pos);

        for (i = 0; i < hrap_grid.maxi; i++) {
            for (j = 0; j < hrap_grid.maxj; j++) {

                if (hrap_grid.owner[i][j] == -1) {

                    pcp.value[i][j] = -9999;
                    continue;

                }

                if ((pcpn_time_step == 1 && hrap_grid.max[isom][i][j] <= -999)
                        || (pcpn_time_step == 2 && hrap_grid.min[isom][i][j] <= -999)) {

                    pcp.value[i][j] = -9999;
                    continue;

                }

                value = 0.0;
                distance = 0.0;
                htotal = 0;

                for (h = 0; h < mpe_dqc_max_temp_neighbors; h++) {

                    hh = hrap_grid.gage[i][j].tindex[h];

                    if (tdata[pcpn_day].tstn[hh].tlevel2[time_pos].data == -99) {
                        /* logMessage("no data\n"); */
                        continue;
                    }

                    if (tdata[pcpn_day].tstn[hh].tlevel2[time_pos].qual != 0
                            && tdata[pcpn_day].tstn[hh].tlevel2[time_pos].qual != 8
                            && tdata[pcpn_day].tstn[hh].tlevel2[time_pos].qual != 6
                            && tdata[pcpn_day].tstn[hh].tlevel2[time_pos].qual != 3
                            && tdata[pcpn_day].tstn[hh].tlevel2[time_pos].qual != 2) {
                        continue;
                    }

                    if (temperature_stations.get(hh).elev <= 0) {
                        continue;
                    }

                    if ((pcpn_time_step == 1 && temperature_stations.get(hh).max[isom] <= -99)
                            || (pcpn_time_step == 2 && temperature_stations
                                    .get(hh).min[isom] <= -99)) {
                        continue;
                    }

                    dist1 = hrap_grid.coord[i][j].lat
                            - temperature_stations.get(hh).lat;
                    /*
                     * DR9602 - Added Math.abs function to get the positive longitude
                     */
                    dist2 = (hrap_grid.coord[i][j].lon - Math.abs(temperature_stations
                    		.get(hh).lon))
                            * Math.cos((hrap_grid.coord[i][j].lat + temperature_stations
                            		.get(hh).lat) / 2 * conv);

                    dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                    dist = Math.pow(dist, .5) * 60;

                    if (dist < .00001) {
                        dist = .00001;
                    }

                    df = 50 * Math.abs(hrap_grid.elev[i][j]
                            - temperature_stations.get(hh).elev) / 5280;

                    dist = dist + df;

                    dist = 1 / dist;

                    temp = tdata[pcpn_day].tstn[hh].tlevel2[time_pos].data;

                    if (pcpn_time_step == 1) {
                        float hmax = hrap_grid.max[isom][i][j];
                        float tmax = temperature_stations.get(hh).max[isom];
                        temp = (temp + (hmax / 10 - tmax)) * dist;
                    }

                    if (pcpn_time_step == 2) {
                    	/*
                    	 * DR9602 - made changes to improve readability of the code
                    	 */
                    	float hmin = hrap_grid.min[isom][i][j];
                        float tmin = temperature_stations.get(hh).min[isom];
                        temp = (temp + (hmin / 10 - tmin)) * dist;
                    }

                    value = value + temp;

                    distance = distance + dist;

                    htotal++;

                    if (htotal == 5) {

                        total1++;
                        break;

                    }

                }

                if (htotal < 5) {

                    total2++;
                    value = 0.0;
                    distance = 0.0;
                    htotal = 0;

                    for (h = 0; h < numTstations; h++) {

                        if (tdata[pcpn_day].tstn[h].tlevel2[time_pos].data == -99) {
                            continue;
                        }

                        if (tdata[pcpn_day].tstn[h].tlevel2[time_pos].qual != 0
                                && tdata[pcpn_day].tstn[h].tlevel2[time_pos].qual != 8
                                && tdata[pcpn_day].tstn[h].tlevel2[time_pos].qual != 6
                                && tdata[pcpn_day].tstn[h].tlevel2[time_pos].qual != 3
                                && tdata[pcpn_day].tstn[h].tlevel2[time_pos].qual != 2) {
                            continue;
                        }

                        if (temperature_stations.get(h).elev <= 0) {
                            continue;
                        }

                        if ((pcpn_time_step == 1 && temperature_stations.get(h).max[isom] <= -99)
                                || (pcpn_time_step == 2 && temperature_stations
                                        .get(h).min[isom] <= -99)) {
                            continue;
                        }

                        dist1 = hrap_grid.coord[i][j].lat 
                               - temperature_stations.get(h).lat;
                        /*
                         * DR9602 - Added Math.abs function to get the positive longitude
                         */
                        dist2 = (hrap_grid.coord[i][j].lon - Math.abs(temperature_stations
                        		.get(h).lon))
                                * Math.cos( ( hrap_grid.coord[i][j].lat + temperature_stations
                                		.get(h).lat ) / 2 * conv);

                        dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                        dist = Math.pow(dist, .5) * 60;

                        if (dist < .00001) {
                            dist = .00001;
                        }

                        df = 50 * Math.abs(hrap_grid.elev[i][j]
                                - temperature_stations.get(h).elev) / 5280;

                        dist = dist + df;

                        dist = 1 / dist;

                        temp = tdata[pcpn_day].tstn[h].tlevel2[time_pos].data;
                        /*
                         * DR9602 - made changes to improve readability of the code
                         */
                        if (pcpn_time_step == 1) {
                        	float hmax = hrap_grid.max[isom][i][j];
                            float tmax = temperature_stations.get(h).max[isom];
                            temp = (temp + (hmax / 10 - tmax)) * dist;;
                        }

                        if (pcpn_time_step == 2) {
                        	float hmin = hrap_grid.min[isom][i][j];
                            float tmin = temperature_stations.get(h).min[isom];
                            temp = (temp + (hmin / 10 - tmin)) * dist;
                        }

                        value = value + temp;

                        distance = distance + dist;

                        htotal++;

                    }

                }

                if (htotal == 0) {
                    pcp.value[i][j] = -9999;
                } else {
                    pcp.value[i][j] = (int) ((value / distance) * 100.0);
                }

            }

        }

        if (pcpn_time_step == 0) {
            time_pos = 150 + pcpn_day * 4 + 3 - pcpn_time;
        } else if (pcpn_time_step == 1) {
            time_pos = 190 + pcpn_day;
        } else if (pcpn_time_step == 2) {
            time_pos = 200 + pcpn_day;
        }

        // logMessage("time pos %d\n",time_pos);

        pcp_in_use[time_pos] = 1;

        rqg.write_file("pcp", time_pos, pcp);

        // logMessage("totals are %d %d\n",total1,total2);

    }

}
