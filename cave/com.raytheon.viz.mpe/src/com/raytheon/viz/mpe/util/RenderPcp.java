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

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * DESCRIPTION: Maps precipitation station values to an HRAP grid. Produces the
 * DailyQC version of the GageOnly analysis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009            snaples     Initial creation
 * May 02, 2011   8962     snaples     Added render24hrPcpUsingFour6hr() method
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class RenderPcp {

    private static boolean first = true;

    Pcp pcp = DailyQcUtils.pcp;

    /*
     * QC codes
     */
    private static final int MISSING = -1;

    private static final int STANDARD = 0;

    private static final int FAILED = 1;

    private static final int MANUAL = 2;

    private static final int QUESTIONABLE = 3;

    private static final int PARTIAL = 4;

    private static final int ESTIMATED = 5;

    private static final int TIMEDISTRIBUTED = 6;

    private static final int VERIFIED = 8;

    /**
     * Converts point precipitation data in struct pdata to hrap grid. The 20
     * closest stations are precalculated for each grid point - this speeds
     * computations for data sets with many precipitation points. If there are
     * no good precipitation points for a grid point, then a recalculation is
     * made using all precipitation points. 1/R**2 interpolation is used. If
     * requested, the final interpolation is scaled using seasonal isohyets. The
     * grid is saved as a disk file and used later for a raster or vector (HRAP)
     * plot.
     * 
     * @param m
     * @param k
     * @param mk
     * @param numPstations
     * @param precip_stations
     * @param hrap_grid
     * @param pdata
     * @param pcp_in_use
     */
    public void render_pcp(int pcpn_day, int pcpn_time, int pcpn_time_step,
            int numPstations, ArrayList<Station> precip_stations,
            Hrap_Grid hrap_grid, Pdata[] pdata, int[] pcp_in_use) {

        int isom = DailyQcUtils.isom;
        int method = DailyQcUtils.method;
        int mpe_dqc_max_precip_neighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;
        int i, j, h, hh, time_pos, htotal;
        double distance, dist1, dist2, dist, value;
        double temp;
        int totals[] = new int[5];

        for (i = 0; i < 5; i++) {
            totals[i] = 0;
        }

        /*
         * pcpn_time_step is a function parameter. It specifies whether to
         * interpolate 6hr or 24hr HRAP grid
         */
        /*
         * pcpn_time is a function parameter. It specifies which 6hr HRAP grid
         * to generate. It takes 0,1,2,or 3 to represent different 6hr period of
         * a day.
         */
        /*
         * time_pos is assigned a value of 0,1,2,or 3 (for 6hr data) or 4 (for
         * 24hr data). This value is used in
         * pdata[pcpn_day].stn[hh].frain[time_pos].data to control whether to
         * retrieve 6hr data or 24hr data.
         */
        if (pcpn_time_step == 0) {
            time_pos = pcpn_time; // for 6 hour data: 0,1,2,3.
        } else {
            time_pos = 4; // for 24 hour data

            /*
             * in case the 24hr grid rendering is required, we check
             * 24hr_grid_gen_method_token() to determine how to generate the
             * grid. New Post OB9.2
             */

            if (getTokenValue24hrGridGenMeth() == 1) {
                render24hrPcpUsingFour6hr(pcpn_day, pcpn_time, numPstations,
                        precip_stations, hrap_grid, pdata, pcp_in_use);
                return;
            }
        }

        /* begin to interpolate value for each bin in the HRAP grid */
        /*
         * to interpolate, two quantities are needed first: value and distance.
         * They are calculated by using the neighboring stations.
         */
        for (i = 0; i < hrap_grid.maxi; i++) {
            for (j = 0; j < hrap_grid.maxj; j++) {
                /*
                 * Check if the grid cell is covered by an HSA. If not, then do
                 * not estimate precipitation for it.
                 */
                if (hrap_grid.owner[i][j] == -1) {
                    pcp.value[i][j] = 0;
                    continue;
                }

                value = 0.0;
                distance = 0.0;
                htotal = 0;

                /*
                 * the following for loop is to calculate two quantities: value
                 * and distance, which later on, are used to interpret the HRAP
                 * grid.
                 */
                /*
                 * It uses neighbor stations of a HRAP grid bin to calculate
                 * value and distance.
                 */
                /* for each neighbor station of the grid bin, do the following */

                /* For each of the closest stations. */
                for (h = 0; h < mpe_dqc_max_precip_neighbors; h++) {
                    hh = hrap_grid.gage[i][j].index[h];
                    // hh is index of stations
                    if (pdata[pcpn_day].stn[hh].frain[time_pos].data < 0) {
                        /* No precip data. */
                        continue;
                    } // frain refers to level 2 data; rrain refers to level 1

                    if (method == 2 && precip_stations.get(hh).isoh[isom] <= 0) {
                        continue;
                    }

                    if (pdata[pcpn_day].stn[hh].frain[time_pos].qual != 0
                            && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 8
                            && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 6
                            && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 3
                            && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 4
                            && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 2) {
                        /* The station has a bad qc flag. Do not use it. */
                        continue;
                    }

                    /*
                     * Convert the coordinates of the grid and station in
                     * lat/lon into distance.
                     */
                    dist1 = (i + (hrap_grid.hrap_minx - precip_stations.get(hh).hrap_x));
                    dist2 = (j + (hrap_grid.hrap_miny - precip_stations.get(hh).hrap_y));

                    dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                    if (dist < .00001) {
                        dist = .00001;
                    }

                    dist = 1 / dist;

                    temp = pdata[pcpn_day].stn[hh].frain[time_pos].data * dist;

                    if (method == 2 && precip_stations.get(hh).isoh[isom] > 0) {
                        temp = temp * hrap_grid.isoh[isom][i][j]
                                / (precip_stations.get(hh).isoh[isom] * 25.4);
                    }

                    value = value + temp;

                    distance = distance + dist;

                    htotal++;

                    if (htotal == 10) {
                        break;
                    }

                }
                /*
                 * end for loop (h = 0; h < mpe_dqc_max_precip_neighbors;h++)
                 * the above for loop is for each neighbor station.
                 */
                /*
                 * the above for loop is to calculate value and distance, which
                 * later on, are used to interpret the HRAP grid
                 */

                /*
                 * the resulting htotal is the valid number of neighbor stations
                 * that are used to calculate value and distance, which later
                 * on, are used to interpret the HRAP grid
                 */

                /*
                 * if there is not enough valid neighbor station, such as htotal
                 * <4, the code below handle this situation. Basically, the code
                 * recalculate the value and distance using all the stations --
                 * see the for (h = 0; h < max_stations; h++) loop below.
                 */
                if (htotal < 4) {

                    value = 0.0;
                    distance = 0.0;
                    htotal = 0;

                    for (h = 0; h < numPstations; h++) {
                        if (pdata[pcpn_day].stn[h].frain[time_pos].data < 0) {
                            continue;
                        }

                        if (pdata[pcpn_day].stn[h].frain[time_pos].qual != 0
                                && pdata[pcpn_day].stn[h].frain[time_pos].qual != 8
                                && pdata[pcpn_day].stn[h].frain[time_pos].qual != 6
                                && pdata[pcpn_day].stn[h].frain[time_pos].qual != 3
                                && pdata[pcpn_day].stn[h].frain[time_pos].qual != 4
                                && pdata[pcpn_day].stn[h].frain[time_pos].qual != 2) {
                            continue;
                        }

                        if (method == 2
                                && precip_stations.get(h).isoh[isom] <= 0) {
                            continue;
                        }

                        dist1 = (i + (hrap_grid.hrap_minx - precip_stations
                                .get(h).hrap_x));
                        dist2 = (j + (hrap_grid.hrap_miny - precip_stations
                                .get(h).hrap_y));

                        dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                        /*
                         * if distance from grid box to station is >
                         * mpe_dqc_grid_max_distbins,
                         */
                        /* then do not use station */
                        if (dist < .00001) {
                            dist = .00001;
                        }

                        else if (Math.sqrt(dist) > DailyQcUtils.mpe_dqc_grid_max_dist) {
                            continue;
                        }
                        dist = 1 / dist;

                        temp = pdata[pcpn_day].stn[h].frain[time_pos].data
                                * dist;

                        if (method == 2
                                && precip_stations.get(h).isoh[isom] > 0) {
                            temp = temp
                                    * hrap_grid.isoh[isom][i][j]
                                    / (precip_stations.get(h).isoh[isom] * 25.4);
                        }

                        value = value + temp;

                        distance = distance + dist;

                        htotal++;

                    }

                }/* end the handling of special case : if (htotal < 4), */
                /* which means there is no enough neighboring stations */

                if (htotal == 0) {
                    pcp.value[i][j] = 0;
                } else {
                    pcp.value[i][j] = (int) (value / distance * 100.0);
                }
                // if (htotal != 0) {
                // pcp.value[i][j] += (int) (value / distance * 100);
                // }
                if (pcp.value[i][j] < .01) {
                    pcp.value[i][j] = 0;
                }
            }
        } // end of the ( k = 0; k<4; k++) loop, which interpolates 4 6hr HRAP
          // grid.

        /* final adjustment of the pcp->value */
        // for (i = 0; i < hrap_grid.maxi; i++) {
        // hrap_grid comes from the function parameter
        // for (j = 0; j < hrap_grid.maxj; j++) {
        //
        // if (pcp.value[i][j] < .01) {
        // pcp.value[i][j] = 0;
        // }
        // }
        // }

        if (pcpn_time_step == 0) {
            time_pos = pcpn_day * 4 + 3 - pcpn_time;
        } else {
            time_pos = 40 + pcpn_day;
        }
        // notice that time_pos is a variable defined inside the function, and
        // its value do not need to be limited as 0,1,2,3,or 4. Here it is
        // assigned with bigger numbers.
        // time_pos = 40 + pcpn_day;

        // pcp_in_use[i] = 1 -- grids rendered via Render button OR Save Level2
        // option
        // = -1 --grids for this time period not rendered (initialization value)
        pcp_in_use[time_pos] = 1;

        ReadQPFGrids rqp = new ReadQPFGrids();
        rqp.write_file("pcp", time_pos, pcp);
    }

    /*
     * get the token value of token mpe_24hr_grid_gen_method. This token will
     * determine how we generate the 24hr grid. We can either use the 24hr gage
     * values, which is the old way, or we can use four 6hr gage values and add
     * them together.
     */
    /* there is some problem with the static */

    /**
     * @param pcpn_day
     * @param pcpn_time
     * @param numPstations
     * @param precip_stations
     * @param hrap_grid
     * @param pdata
     * @param pcp_in_use
     */
    private void render24hrPcpUsingFour6hr(int pcpn_day, int pcpn_time,
            int numPstations, ArrayList<Station> precip_stations,
            Hrap_Grid hrap_grid, Pdata[] pdata, int[] pcp_in_use) {

        int i, j, k, h, hh, time_pos, htotal;
        double distance, dist1, dist2, dist, value;
        double temp;
        int isom = DailyQcUtils.isom;
        int method = DailyQcUtils.method;
        int totals[] = new int[5];
        int all_total = 0;
        int neighbor_total = 0;

        for (i = 0; i < 5; i++) {
            totals[i] = 0;
        }

        /*
         * pcpn_time_step is function parameter. It specifies whether to
         * interpolate 6hr or 24hr HRAP grid
         */
        /*
         * pcpn_time is a function parameter. It specifies which 6hr HRAP grid
         * to generate. It takes 0,1,2,or 3 to represent different 6hr period of
         * a day.
         */
        /*
         * time_pos is assigned a value of 0,1,2,or 3 (for 6hr data) or 4 (for
         * 24hr data). This value is used in
         * pdata[pcpn_day].stn[hh].frain[time_pos].data to control whether to
         * retrieve 6hr data or 24hr data.
         */

        /* initialization of the pcp->value */
        for (i = 0; i < hrap_grid.maxi; i++) { /*
                                                * hrap_grid comes from the
                                                * function parameter
                                                */
            for (j = 0; j < hrap_grid.maxj; j++) {
                pcp.value[i][j] = 0;
            }
        }

        /*
         * begin to interpolate 4 6hr grids. At the end of each iteration, the
         * calculated interpolation value is added to pcp->value[i][j].
         */
        /*
         * time_pos is assigned a value of 0,1,2,or 3 (for four 6hr data). This
         * value is used in pdata[pcpn_day].stn[hh].frain[time_pos].data to
         * retrieve 6hr data.
         */
        for (k = 0; k < 4; k++) {
            time_pos = k; /* for 6 hour data: 0, 1,2,3. */

            /* begin to interpolate value for each bin in the HRAP grid */
            /*
             * to interpolate, two quantities are needed first: value and
             * distance. They are calculated by using the neighboring stations.
             */

            for (i = 0; i < hrap_grid.maxi; i++) { /*
                                                    * hrap_grid comes from the
                                                    * function parameter
                                                    */
                for (j = 0; j < hrap_grid.maxj; j++) {
                    /*
                     * Check if the grid cell is covered by an HSA. If not, then
                     * do not estimate precipitation for it.
                     */
                    if (hrap_grid.owner[i][j] == -1) {
                        pcp.value[i][j] = 0;
                        continue;
                    }

                    value = 0.0;
                    distance = 0.0;
                    htotal = 0;

                    /*
                     * the following for loop is to calculate two quantities:
                     * value and distance, which later on, are used to interpret
                     * the HRAP grid.
                     */
                    /*
                     * It uses neighbor stations of a HRAP grid bin to calculate
                     * value and distance.
                     */
                    /*
                     * for each neighbor station of the grid bin, do the
                     * following
                     */
                    int mpe_dqc_max_precip_neighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;

                    for (h = 0; h < mpe_dqc_max_precip_neighbors; h++) {
                        hh = hrap_grid.gage[i][j].index[h];/*
                                                            * hh is index of
                                                            * stations
                                                            */

                        if (pdata[pcpn_day].stn[hh].frain[time_pos].data < 0) {
                            /* No precip data. */
                            continue;
                        } /*
                           * frain refers to level 2 data; rain refers to level
                           * 1
                           */

                        /*
                         * generate something for output later on if in debug
                         * mode
                         */
                        // if (debug_level >= 1) {
                        // if (pdata[pcpn_day].stn[hh].frain[time_pos].qual
                        // == MISSING)
                        // {
                        // missing_total++;
                        // }
                        //
                        // if (pdata[pcpn_day].stn[hh].frain[time_pos].qual
                        // == ESTIMATED)
                        // {
                        // estimated_total++;
                        // }
                        //
                        // if (pdata[pcpn_day].stn[hh].frain[time_pos].qual
                        // == FAILED)
                        // {
                        // failed_total++;
                        // }
                        //
                        // if (method == 2 && station[hh].isoh[isom] <= 0)
                        // {
                        // climo_total++;
                        // }
                        // } /* end if (debug_level >= 1) */

                        if (DailyQcUtils.method == 2
                                && precip_stations.get(hh).isoh[isom] <= 0) {
                            continue;
                        }

                        if (pdata[pcpn_day].stn[hh].frain[time_pos].qual != 0
                                && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 8
                                && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 6
                                && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 3
                                && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 4
                                && pdata[pcpn_day].stn[hh].frain[time_pos].qual != 2) {
                            /* The station has a bad qc flag. Do not use it. */
                            continue;
                        }

                        /*
                         * Convert the coordinates of the grid and station in
                         * lat/lon into distance.
                         */
                        dist1 = (i + hrap_grid.hrap_minx - precip_stations
                                .get(hh).hrap_x);
                        dist2 = (j + hrap_grid.hrap_miny - precip_stations
                                .get(hh).hrap_y);

                        dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                        if (dist < .00001) {
                            dist = .00001;
                        }

                        dist = 1 / dist;

                        temp = pdata[pcpn_day].stn[hh].frain[time_pos].data
                                * dist;

                        if (method == 2
                                && precip_stations.get(hh).isoh[isom] > 0) {
                            temp = temp
                                    * hrap_grid.isoh[isom][i][j]
                                    / (precip_stations.get(hh).isoh[isom] * 25.4);
                        }

                        value = value + temp;

                        distance = distance + dist;

                        htotal++;

                        if (htotal == 10) {
                            break;
                        }

                    }
                    /*
                     * end for loop (h = 0; h <
                     * mpe_dqc_max_precip_neighbors;h++)
                     */
                    /* the above for loop is for each neighbor station. */
                    /*
                     * the above for loop is to calculate value and distance,
                     * which later on, are used to interpret the HRAP grid
                     */

                    /*
                     * the resulting htotal is the valid number of neighbor
                     * stations that are used to calculate value and distance,
                     * which later on, are used to interpret the HRAP grid
                     */

                    /*
                     * if there is not enough valid neighbor station, such as
                     * htotal <4, the code below handle this situation.
                     * Basically, the code recalculate the value and distance
                     * using all the stations -- see the for (h = 0; h <
                     * max_stations; h++) loop below.
                     */
                    if (htotal < 4) {

                        value = 0.0;
                        distance = 0.0;
                        htotal = 0;

                        for (h = 0; h < numPstations; h++) {
                            if (pdata[pcpn_day].stn[h].frain[time_pos].data < 0) {
                                continue;
                            }

                            if (pdata[pcpn_day].stn[h].frain[time_pos].qual != 0
                                    && pdata[pcpn_day].stn[h].frain[time_pos].qual != 8
                                    && pdata[pcpn_day].stn[h].frain[time_pos].qual != 6
                                    && pdata[pcpn_day].stn[h].frain[time_pos].qual != 3
                                    && pdata[pcpn_day].stn[h].frain[time_pos].qual != 4
                                    && pdata[pcpn_day].stn[h].frain[time_pos].qual != 2) {
                                continue;
                            }

                            if (method == 2
                                    && precip_stations.get(h).isoh[isom] <= 0) {
                                continue;
                            }

                            dist1 = (i + hrap_grid.hrap_minx - precip_stations
                                    .get(h).hrap_x);
                            dist2 = (j + hrap_grid.hrap_miny - precip_stations
                                    .get(h).hrap_y);

                            dist = Math.pow(dist1, 2) + Math.pow(dist2, 2);

                            if (dist < .00001) {
                                dist = .00001;
                            }

                            dist = 1 / dist;

                            temp = pdata[pcpn_day].stn[h].frain[time_pos].data
                                    * dist;

                            if (method == 2
                                    && precip_stations.get(h).isoh[isom] > 0) {
                                temp = temp
                                        * hrap_grid.isoh[isom][i][j]
                                        / (precip_stations.get(h).isoh[isom] * 25.4);
                            }

                            value = value + temp;

                            distance = distance + dist;

                            htotal++;

                        }
                        neighbor_total++;
                    } /* end the handling of special case : if (htotal < 4), */
                    /* which means there is no enough neighboring stations */

                    /*
                     * add the interpreted value for the bin of the HRAP_grid
                     */
                    /*
                     * if (htotal == 0) { pcp->value[i][j] += 0; } else {
                     * pcp->value[i][j] += (int) (value / distance * 100); }
                     */
                    if (htotal != 0) {
                        pcp.value[i][j] += (int) (value / distance * 100);
                    }

                    /*
                     * if (pcp->value[i][j] < .01) { pcp->value[i][j] = 0; }
                     */
                    all_total++;

                } /* end of for loop (j = 0; j < hrap_grid->maxj; j++) */
            } /* end of for loop (i = 0; i < hrap_grid->maxi; i++) */
            /* At this moment, the interpretation of HRAP grid is done */

        }/*
          * end of the ( k = 0; k<4; k++) loop, which interpolates 4 6hr HRAP
          * grid.
          */

        /* final adjustment of the pcp->value */
        for (i = 0; i < hrap_grid.maxi; i++) { /*
                                                * hrap_grid comes from the
                                                * function parameter
                                                */
            for (j = 0; j < hrap_grid.maxj; j++) {

                if (pcp.value[i][j] < .01) {
                    pcp.value[i][j] = 0;
                }
            }
        }

        /* time_pos = pcpn_day * 4 + 3 - pcpn_time; */
        time_pos = 40 + pcpn_day;

        /*
         * pcp_in_use[i] = 1 -- grids rendered via Render button OR Save Level2
         * option = -1 --grids for this time period not rendered (initialization
         * value)
         */
        pcp_in_use[time_pos] = 1;

        /*
         * results of grid rendering routines (render_t, render_t6, render_pcp,
         * render_z) are written to scratch files using the write_file routine.
         * scratch file is stored in the scratch directory.
         */
        ReadQPFGrids rqp = new ReadQPFGrids();
        rqp.write_file("pcp", time_pos, pcp);

    }

    int getTokenValue24hrGridGenMeth() {

        int token_of_24hr_grid_gen_method = 0;

        /* int token_of_24hr_grid_gen_method = 0; */

        if (first == true) {
            String token_name_of_24hr_grid_gen_method_token = "mpe_dqc_24hr_precip_grid_meth";

            /* char strTokenValue[50] = { '\0' }; */
            String DQC24hrPrecipMeth;

            // char message[GAGEQC_MESSAGE_LEN] = { '\0' };

            AppsDefaults appsDefaults = AppsDefaults.getInstance();
            DQC24hrPrecipMeth = appsDefaults
                    .getToken(token_name_of_24hr_grid_gen_method_token);
            // sprintf(message, "\nSTATUS: token value of \"%s\" : %s\n",
            // token_name_of_24hr_grid_gen_method_token, strTokenValue);
            // logMessage(message);

            if (DQC24hrPrecipMeth != null && DQC24hrPrecipMeth.length() > 0) {
                /* we use the token ACCUM_6HR and USE_24HR */
                if (DQC24hrPrecipMeth.equalsIgnoreCase("ACCUM_6HR")) {
                    token_of_24hr_grid_gen_method = 1;
                }
            }

            first = false;
        }

        return token_of_24hr_grid_gen_method;
    }
}
