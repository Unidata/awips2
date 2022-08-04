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

import java.io.BufferedWriter;
import java.util.Date;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.Disagg6Hr.Dist;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_1hr;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_6hr;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class DisaggPointMethod {

    DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    Station[] disagg_station_6hr = Disagg6Hr.disagg_station_6hr;

    Values_1hr[] disaggValues = Disagg6Hr.disaggValues;

    double[][][] QPEgrids = Disagg6Hr.QPEgrids;

    int val6hreq0 = Disagg6Hr.val6hreq0;

    int val6hrgt0 = Disagg6Hr.val6hrgt0;

    int disagg_db_factor = Disagg6Hr.disagg_db_factor;

    Values_6hr[] values6hr = Disagg6Hr.values6hr;

    BufferedWriter disagg_log_fd = Disagg6Hr.disagg_log_fd;

    String[] obsdate = Disagg6Hr.obsdate;

    Date[] obsdate_date_t = Disagg6Hr.obsdate_date_t;

    double[] total_6hr;

    double[][] totals_1hr;

    int mpe_dqc_max_precip_neighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;

    Dist[] dist_6hr_to_1hr = Disagg6Hr.dist_6hr_to_1hr;

    private int sorted_array_size = Compute1HrNeighborList.sorted_array_size;

    private int[] sorted_list_1hr = Compute1HrNeighborList.sorted_list_1hr;

    private Values_1hr[] valuesReadIn = Compute1HrNeighborList.valuesReadIn;

    private Station[] disagg_station_1hr = Compute1HrNeighborList.disagg_station_1hr;

    public void disaggPointMethod() {

        int i, j, k, l, m, n, p;

        float fdist = 0.f;
        float fdata = 0.f;
        float[] fvalue = new float[6];
        float val_1hr = -9999.f;
        short prism_1hr = -9999;
        short prism_6hr = -9999;
        float dist; // 6hr to 1hr
        float padj;
        float scale = 1.f;
        float stotal = 0.f;
        int num_missing_periods = 0;
        int num_disagg_stations = Disagg6Hr.num_disagg_stations;
        int num_days_to_qc = DailyQcUtils.qcDays;
//        int num_1hrs_reported = 0;
        boolean go_to_next_neighbor = false;
        boolean next_6hr_station = false;
        int index = -1;

        for (i = 0; i < num_disagg_stations; i++) {
            for (j = 0; j < num_days_to_qc; j++) {
                index = j * num_disagg_stations + i;

                if (values6hr[index].ID
                        .equals(Disagg6Hr.DISAGG_MISSING_STATION_SYMBOL)) {
                    continue;
                }

                for (k = 0; k < 4; k++)// 4 6hr periods
                {

                    // If the quality code is the following don't disagg
                    // if()
                    // {
                    // }

                    for (n = 0; n < 6; n++) {
                        fvalue[n] = -9999.f;

                        // block that checks if the six hr station has a
                        // missing
                        // report
                        // if it is a missing report, don't bother disagging
                        // the
                        // station
                        if (values6hr[index].value[k] < 0.) {
                            disaggValues[index].dqc_day = j;
                            for (l = 0; l < 6; l++) {
                                disaggValues[index].HourlyValues[6 * k + l] = -9999.f;
                            }
                            next_6hr_station = true;
                            break;
                        }
                        // block that handles a six hr station with a val =
                        // 0
                        // all 1hr periods get a value of '0'
                        else if (values6hr[index].value[k] == 0.) {
                            disaggValues[index].dqc_day = j;
                            for (l = 0; l < 6; l++) {
                                disaggValues[index].HourlyValues[6 * k + l] = 0.f;
                            }
                            next_6hr_station = true;
                            break;
                        }
                        // block that handles a six hr station with a non
                        // missing, non zero report
                        else {
                            for (p = 0; p < 12; p++) {
                                if (disagg_station_6hr[index].isoh[p] != -1) {
                                    // at the moment we are using the prism
                                    // value for the
                                    // first month dqc run time spans into.
                                    // in
                                    // some cases
                                    // it could span into 2 months
                                    prism_6hr = (short) disagg_station_6hr[index].isoh[p];
                                    break;
                                }
                            }
                            for (l = 0; l < mpe_dqc_max_precip_neighbors; l++) {
                                dist = (float) dist_6hr_to_1hr[i].distances_to_neighbors[l];
                                if (dist == 0) {
                                    dist = 0.000001f;
                                }

                                for (m = 0; m < sorted_array_size; m++) {
                                    if (disagg_station_6hr[index].index[l] == sorted_list_1hr[m]) {
                                        val_1hr = valuesReadIn[j
                                                * sorted_array_size + m].HourlyValues[6
                                                * k + n];
                                        if (val_1hr < 0.0) {
                                            go_to_next_neighbor = true;
                                            break;
                                        }
                                        for (p = 0; p < 12; p++) {
                                            if (disagg_station_1hr[sorted_list_1hr[m]].isoh[p] != -1) {
                                                // at the moment we are
                                                // using
                                                // the prism value for the
                                                // first month dqc run time
                                                // spans into. in some cases
                                                // it could span into 2
                                                // months
                                                prism_1hr = (short) disagg_station_1hr[sorted_list_1hr[m]].isoh[p];
                                                break;
                                            }
                                        }
                                        break;
                                    }
                                }
                                if (go_to_next_neighbor) {
                                    go_to_next_neighbor = false;
                                    continue;
                                }
//                                num_1hrs_reported++;
                                padj = val_1hr * (prism_6hr / prism_1hr);
                                fdist = (float) (fdist + (1 / Math.pow(dist, 2)));
                                fdata = (float) (fdata + (padj / Math.pow(dist,
                                        2)));
                            }
                            // Quit if number of reported 1hrs is less
                            // than desired. This piece is being commented
                            // at the moment.
                            /*
                             * if(num_1hrs_reported <= 15) { num_1hrs_reported =
                             * 0; continue; }
                             */
                            fvalue[n] = fdata / fdist;
                        }
                    }

                    stotal = 0.0f;
                    num_missing_periods = 0;
                    for (l = 0; l < 6; l++) {
                        if (fvalue[l] >= 0) {
                            stotal = stotal + fvalue[l];
                        } else {
                            num_missing_periods++;
                        }
                    }

                    disaggValues[index].dqc_day = j;
                    if (num_missing_periods > 0) {
                        // write to log file
                        // set all 1hr values to missing
                        for (l = 0; l < 6; l++) {
                            disaggValues[index].HourlyValues[6 * k + l] = -9999.f;
                        }
                    } else {
                        scale = values6hr[index].value[k] / stotal;
                        for (l = 0; l < 6; l++) {
                            if (fvalue[l] != -9999.) {
                                disaggValues[index].HourlyValues[6 * k + l] = fvalue[l]
                                        * scale * disagg_db_factor;
                            }
                        }
                    }
                    if (next_6hr_station) {
                        next_6hr_station = false;
                        continue;
                    }
                }
            }
        }
    }

}
