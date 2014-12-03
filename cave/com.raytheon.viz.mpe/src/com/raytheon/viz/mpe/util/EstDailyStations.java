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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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

public class EstDailyStations {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EstDailyStations.class);
    
    DailyQcUtils dqc = DailyQcUtils.getInstance();

    /* j is the day of the data. */
    public void estimate_daily_stations(int j,
            ArrayList<Station> precip_stations, int numPstations) {

        int dqc_neig = dqc.mpe_dqc_max_precip_neighbors;
        int isom = dqc.isom;
        int isohyets_used = dqc.isohyets_used;
        int method = dqc.method;
        int m, k, i, l, ii;
        int dqc_min_good = dqc.mpe_dqc_min_good_stations;
        float conv = .0174f;
        double distlon;
        double lat1;
        double lon1;
        double fdist;
        double fdata;
        double fvalue[] = new double[4];
        double lat;
        double lon;
        double testdist;
        double fmult;
        double ftotal;
        double isoh = 0.;
        double isoh1 = 0.;
        double padj;
        double stotal;
        int max_stations = numPstations;
        int closest_good_gage_index = -9999;
        int num_missing;
        int details = dqc.mpe_td_details_set;
        int mpe_td_new_algorithm_set = dqc.mpe_td_new_algorithm_set;

        if (dqc.pdata[j].data_time == null) {
            return;
        }
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(dqc.pdata[j].data_time);

        /* this routine will estimate 6 hourly periods when 24 hour rain exists */

        for (m = 0; m < max_stations; m++) {
            /* dont estimate missing 24 hour stations */
            if (dqc.pdata[j].stn[m].frain[4].data < 0
                    || dqc.pdata[j].stn[m].frain[4].qual == 4) {
                continue;
            }

            /* search for a missing 6 hourly period */

            for (k = 0; k < 4; k++) {

                if (dqc.pdata[j].stn[m].frain[k].data >= 0
                        && dqc.pdata[j].stn[m].frain[k].qual == 2) {
                    continue;
                }

                if (dqc.pdata[j].stn[m].frain[k].qual == 1) {
                    break;
                }

                if (dqc.pdata[j].stn[m].rrain[k].data < 0
                        && dqc.pdata[j].stn[m].frain[k].qual != 2) {
                    break;
                }

            }

            if (k == 4) {
                continue;
            }

            /* dont estimate stations forced good, bad or estimated */

            if (dqc.pdata[j].stn[m].frain[4].qual == 1
                    || dqc.pdata[j].stn[m].frain[4].qual == 5) {
                continue;
            }

            /* at least one missing 6 hourly period found */

            lat1 = precip_stations.get(m).lat;
            lon1 = precip_stations.get(m).lon;

            /* get isohyet */

            if (isohyets_used != 0) {
                isoh1 = precip_stations.get(m).isoh[isom];
            }

            /* first */
            for (k = 0; k < 4; k++) {

                fdist = 0.0;
                fdata = 0.0;

                l = 0;

                for (ii = 0; ii < dqc_neig; ii++) {

                    i = precip_stations.get(m).index[ii];

                    /* dont estimate unless good or forced good */

                    if (dqc.pdata[j].stn[i].frain[k].qual != 0
                            && dqc.pdata[j].stn[i].frain[k].qual != 8
                            && dqc.pdata[j].stn[i].frain[k].qual != 3
                            && dqc.pdata[j].stn[i].frain[k].qual != 2) {
                        continue;
                    }

                    /* dont use missing stations */

                    if (dqc.pdata[j].stn[i].frain[k].data < 0) {
                        continue;
                    }

                    lat = precip_stations.get(i).lat;
                    lon = precip_stations.get(i).lon;

                    if (isohyets_used != 0) {
                        isoh = precip_stations.get(i).isoh[isom];
                    }

                    distlon = (lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv);

                    testdist = Math.pow((lat1 - lat), 2)
                            + Math.pow((distlon), 2);

                    if (testdist == 0.0) {
                        testdist = .000001;
                    }

                    testdist = 1 / testdist;

                    if (method == 2 && isoh > 0 && isoh1 > 0) {
                        padj = dqc.pdata[j].stn[i].frain[k].data
                                * isoh1 / isoh;
                    } else {
                        padj = dqc.pdata[j].stn[i].frain[k].data;
                    }

                    fdist = testdist + fdist;
                    fdata = padj * testdist + fdata;
                    l++;

                }

                if ((l < dqc_min_good)
                        && (dqc.mpe_td_new_algorithm_set == 0)) {

                    if (details == 1) {
                        String buf = String
                                .format("TD estimate uses whole station list for gage %s at %04d%02d%02d  k=%d",
                                        precip_stations.get(m).hb5,
                                        cal.get(Calendar.YEAR),
                                        cal.get(Calendar.MONTH) + 1,
                                        cal.get(Calendar.DAY_OF_MONTH), k);

                        try {
                            dqc.td_fpwr.write(buf);
                            dqc.td_fpwr.newLine();
                        } catch (IOException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }

                    fdist = 0.0;
                    fdata = 0.0;

                    l = 0;

                    for (i = 0; i < max_stations; i++) {

                        if (i == m) {
                            continue;
                        }

                        if (dqc.pdata[j].stn[i].frain[k].qual != 0
                                && dqc.pdata[j].stn[i].frain[k].qual != 8
                                && dqc.pdata[j].stn[i].frain[k].qual != 3
                                && dqc.pdata[j].stn[i].frain[k].qual != 2) {
                            continue;
                        }

                        if (dqc.pdata[j].stn[i].frain[k].data < 0) {
                            continue;
                        }

                        lat = precip_stations.get(i).lat;
                        lon = precip_stations.get(i).lon;

                        if (isohyets_used != 0) {
                            isoh = precip_stations.get(i).isoh[isom];
                        }

                        testdist = Math.pow((lat1 - lat), 2)
                                + Math.pow((lon1 - lon), 2);

                        if (testdist == 0.0) {
                            testdist = .0001;
                        }

                        testdist = 1 / testdist;

                        if (method == 2 && isoh > 0 && isoh1 > 0) {
                            padj = dqc.pdata[j].stn[i].frain[k].data
                                    * isoh1 / isoh;
                        } else {
                            padj = dqc.pdata[j].stn[i].frain[k].data;
                        }

                        fdist = testdist + fdist;
                        fdata = padj * testdist + fdata;
                        l++;
                    }

                }

                else if (l != 0) {
                    if (details == 1) {
                        String buf = String
                                .format("TD estimate uses neighbor list for gage %s at %04d%02d%02d  k=%d",
                                        precip_stations.get(m).hb5,
                                        cal.get(Calendar.YEAR),
                                        cal.get(Calendar.MONTH) + 1,
                                        cal.get(Calendar.DAY_OF_MONTH), k);

                        try {
                            dqc.td_fpwr.write(buf);
                            dqc.td_fpwr.newLine();

                            buf = String.format("Gage name (value, prism)");
                            dqc.td_fpwr.write(buf);
                            dqc.td_fpwr.newLine();

                            for (ii = 0; ii < dqc_neig; ii++) {
                                i = precip_stations.get(m).index[ii];

                                if (i == m) {
                                    continue;
                                }
                                if (dqc.pdata[j].stn[i].frain[k].qual != 0
                                        && dqc.pdata[j].stn[i].frain[k].qual != 8
                                        && dqc.pdata[j].stn[i].frain[k].qual != 3
                                        && dqc.pdata[j].stn[i].frain[k].qual != 2) {
                                    continue;
                                }

                                if (dqc.pdata[j].stn[i].frain[k].data < 0) {
                                    continue;
                                }

                                buf = String
                                        .format("  %s(%f,%f)",
                                                precip_stations.get(i).hb5,
                                                dqc.pdata[j].stn[i].frain[k].data,
                                                precip_stations.get(i).isoh[isom]);

                                dqc.td_fpwr.write(buf);
                                dqc.td_fpwr.newLine();
                            }
                            // fvalue[k] = fdata / fdist;

                        } catch (IOException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                } else {
                    /*
                     * no any good gage is found from neighbor list, look for
                     * the closest good gage in the whole station list
                     */
                    if (details == 1) {
                        String buf = String
                                .format("TD estimate uses the closest good gage from station list for gage %s at %04d%02d%02d  k=%d",
                                        precip_stations.get(m).hb5,
                                        cal.get(Calendar.YEAR),
                                        cal.get(Calendar.MONTH) + 1,
                                        cal.get(Calendar.DAY_OF_MONTH), k);
                        try {
                            dqc.td_fpwr.write(buf);
                            dqc.td_fpwr.newLine();

                            buf = String.format("Gage name (value, prism)");
                            dqc.td_fpwr.write(buf);
                            dqc.td_fpwr.newLine();
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    }

                    fdist = 99999999;

                    for (i = 0; i < max_stations; i++) {
                        if (i == m) {
                            continue;
                        }

                        if (dqc.pdata[j].stn[i].frain[k].qual != 0
                                && dqc.pdata[j].stn[i].frain[k].qual != 8
                                && dqc.pdata[j].stn[i].frain[k].qual != 3
                                && dqc.pdata[j].stn[i].frain[k].qual != 2) {
                            continue;
                        }

                        if (dqc.pdata[j].stn[i].frain[k].data < 0) {
                            continue;
                        }

                        lat = precip_stations.get(i).lat;
                        lon = precip_stations.get(i).lon;

                        testdist = Math.pow((lat1 - lat), 2)
                                + Math.pow((lon1 - lon), 2);

                        if (testdist == 0.0) {
                            testdist = .0001;
                        }

                        if (testdist < fdist) {
                            fdist = testdist;
                            closest_good_gage_index = i;

                        }
                    }

                    if (details == 1 && closest_good_gage_index != -9999) {
                        String buf = String
                                .format("  %s(%f,%f)",
                                        precip_stations
                                                .get(closest_good_gage_index).hb5,
                                        dqc.pdata[j].stn[closest_good_gage_index].frain[k].data,
                                        precip_stations
                                                .get(closest_good_gage_index).isoh[isom]);

                        try {
                            dqc.td_fpwr.write(buf);
                            dqc.td_fpwr.newLine();
                        } catch (IOException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    }
                }
                if (l != 0) {
                    fvalue[k] = fdata / fdist;
                } else {
                    if (mpe_td_new_algorithm_set == 0) {
                        fvalue[k] = -9999;
                    } else {
                        if (closest_good_gage_index != -9999) {
                            fvalue[k] = dqc.pdata[j].stn[closest_good_gage_index].frain[k].data;
                        } else {
                            fvalue[k] = -9999;
                        }
                    }
                }
            }

            if (fvalue[0] == -9999 || fvalue[1] == -9999 || fvalue[2] == -9999
                    || fvalue[3] == -9999) {

                for (k = 0; k < 4; k++) {

                    dqc.pdata[j].stn[m].frain[k].qual = 6;
                    dqc.pdata[j].stn[m].frain[k].data = dqc.pdata[j].stn[m].frain[4].data / 4;

                }

                continue;

            }

            ftotal = 0.0;
            stotal = 0.0;
            num_missing = 0;

            for (k = 0; k < 4; k++) {

                if ((dqc.pdata[j].stn[m].rrain[k].data >= 0 && dqc.pdata[j].stn[m].frain[k].qual != 1)
                        || (dqc.pdata[j].stn[m].frain[k].qual == 2)) {
                    stotal = stotal
                            + dqc.pdata[j].stn[m].frain[k].data;
                } else {

                    num_missing++;

                    ftotal = ftotal + fvalue[k];

                }

            }

            stotal = dqc.pdata[j].stn[m].frain[4].data - stotal;

            if (stotal < 0) {
                stotal = 0;
            }

            if (ftotal == 0.0) {
                fmult = 0;
            } else {
                fmult = stotal / ftotal;
            }

            for (k = 0; k < 4; k++) {

                if ((dqc.pdata[j].stn[m].rrain[k].data >= 0 && dqc.pdata[j].stn[m].frain[k].qual != 1)
                        || (dqc.pdata[j].stn[m].frain[k].qual == 2)) {
                    continue;
                }

                if (ftotal != 0) {

                    dqc.pdata[j].stn[m].frain[k].data = (float) (fvalue[k] * fmult);
                    dqc.pdata[j].stn[m].frain[k].qual = 6;

                }

                else {

                    dqc.pdata[j].stn[m].frain[k].data = (float) (stotal / num_missing);
                    dqc.pdata[j].stn[m].frain[k].qual = 6;

                }

            }

        }
    }
}
