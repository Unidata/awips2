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
import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;

/**
 * Converts point precipitation data in struct pdata to hrap grid. The 20
 * closest stations are precalculated for each grid point - this speeds
 * computations for data sets with many precipitation points. If there are no
 * good precipitation points for a grid point, then a recalculation is made
 * using all precipitation points. 1/R**2 interpolation is used. If requested,
 * the final interpolation is scaled using seasonal isohyets. The grid is saved
 * as a disk file and used later for a raster or vector (HRAP) plot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RenderZ {

    Pcp pcp = DailyQcUtils.pcp;

    public void render_z(int pcpn_day, int pcpn_time, int pcpn_time_step,
            int numZstations, ArrayList<Station> freezing_stations,
            Hrap_Grid hrap_grid, Zdata[] zdata, int[] pcp_in_use) {

        int i, j, h, hh, time_pos, htotal;
        double distance, dist1, dist2, dist, value;
        double temp;
        int totals[] = new int[5];

        for (i = 0; i < 5; i++) {
            totals[i] = 0;
        }

        time_pos = pcpn_time;

        // for (j = hrap_grid.maxj - 1; j >= 0; j--) {

        for (i = 0; i < hrap_grid.maxi; i++) {
            for (j = 0; j < hrap_grid.maxj; j++) {

                if (hrap_grid.owner[j][i] == -1) {

                    pcp.value[j][i] = -9999;
                    continue;

                }

                value = 0.0;
                distance = 0.0;
                htotal = 0;

                for (h = 0; h < 5; h++) {

                    hh = hrap_grid.gage[j][i].zindex[h];

                    if (zdata[pcpn_day].zstn[hh].zlevel2[time_pos].data < 0) {
                        continue;
                    }

                    if (zdata[pcpn_day].zstn[hh].zlevel2[time_pos].qual != 5
                            && zdata[pcpn_day].zstn[hh].zlevel2[time_pos].qual != 8
                            && zdata[pcpn_day].zstn[hh].zlevel2[time_pos].qual != 2) {
                        continue;
                    }

                    dist1 = (i + (hrap_grid.hrap_minx - freezing_stations
                            .get(hh).hrap_x));
                    dist2 = (j + (hrap_grid.hrap_miny - freezing_stations
                            .get(hh).hrap_y));

                    dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                    if (dist < .00001) {
                        dist = .00001;
                    }

                    dist = 1 / dist;

                    temp = zdata[pcpn_day].zstn[hh].zlevel2[time_pos].data
                            * dist;

                    value = value + temp;

                    distance = distance + dist;

                    htotal++;

                }

                if (htotal < 4) {

                    value = 0.0;
                    distance = 0.0;
                    htotal = 0;

                    for (h = 0; h < numZstations; h++) {

                        if (zdata[pcpn_day].zstn[h].zlevel2[time_pos].data < 0) {
                            continue;
                        }

                        if (zdata[pcpn_day].zstn[h].zlevel2[time_pos].qual != 5
                                && zdata[pcpn_day].zstn[h].zlevel2[time_pos].qual != 8
                                && zdata[pcpn_day].zstn[h].zlevel2[time_pos].qual != 2) {
                            continue;
                        }

                        dist1 = (i + (hrap_grid.hrap_minx - freezing_stations
                                .get(h).hrap_x));
                        dist2 = (j + (hrap_grid.hrap_miny - freezing_stations
                                .get(h).hrap_y));

                        dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                        if (dist < .00001) {
                            dist = .00001;
                        }

                        dist = 1 / dist;

                        temp = zdata[pcpn_day].zstn[h].zlevel2[time_pos].data
                                * dist;

                        value = value + temp;

                        distance = distance + dist;

                        htotal++;

                    }

                }

                if (htotal == 0) {
                    pcp.value[j][i] = -9999;
                }

                else {
                    pcp.value[j][i] = (int) ((value / distance) * 100);
                }

            }

        }
        time_pos = 100 + pcpn_day * 4 + 3 - pcpn_time;

        pcp_in_use[time_pos] = 1;

        ReadQPFGrids rqg = new ReadQPFGrids();
        rqg.write_file("pcp", time_pos, pcp);
    }
}
