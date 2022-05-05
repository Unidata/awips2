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
 * Used to estimate partial MPE stations. This routine will estimate 6 hourly
 * periods when 24 hour rain exists.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            snaples     Initial creation
 * Mar 10, 2017  19625     snaples     Fixed erroneous estimates.
 * Oct 03, 2017 6407       bkowal      Eliminated warnings.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class EstPartStations {

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public void estimate_partial_stations(int j, List<Station> precip_stations,
            int numPstations) {
        int dqc_neig = DailyQcUtils.mpe_dqc_max_precip_neighbors;
        int isom = DailyQcUtils.isom;
        boolean isohyets_used = dqc.isohyets_used;
        int method = dqc.method;
        int m, k, i, l, ii;
        int dqc_min_good = dqc.mpe_dqc_min_good_stations;
        double lat1;
        double lon1;
        double fdist;
        double fdata;
        double fvalue[] = new double[4];
        double lat;
        double lon;
        double testdist;
        double ftotal;
        double isoh = 0.;
        double isoh1 = 0.;
        double padj;
        int max_stations = numPstations;

        if (DailyQcUtils.pdata[j].data_time == null) {
            return;
        }

        for (m = 0; m < max_stations; m++) {
            /* search for a good 6 hourly period */
            for (k = 0; k < 4; k++) {

                if (DailyQcUtils.pdata[j].stn[m].rrain[k].data >= 0
                        && (DailyQcUtils.pdata[j].stn[m].frain[k].qual == 0
                                || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 8
                                || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 3
                                || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 2)) {
                    break;
                }
            }

            if (k == 4) {
                /* all 6 hourly periods missing or set bad */
                /* need to re-estimate 24 hour data */

                if (DailyQcUtils.pdata[j].stn[m].frain[4].data >= 0
                        && DailyQcUtils.pdata[j].stn[m].frain[4].qual == 4) {
                    DailyQcUtils.pdata[j].stn[m].frain[4].qual = 0;
                    DailyQcUtils.pdata[j].stn[m].frain[4].data = -1;
                }

                continue;

            }

            /* dont estimate if 24 hour station available */

            if (DailyQcUtils.pdata[j].stn[m].frain[4].data >= 0
                    && DailyQcUtils.pdata[j].stn[m].frain[4].qual != 4
                    && DailyQcUtils.pdata[j].stn[m].frain[4].qual != 5) {
                continue;
            }

            /* at least one missing 6 hourly period found */

            lat1 = precip_stations.get(m).lat;
            lon1 = precip_stations.get(m).lon;

            /* get isohyet */

            if (isohyets_used) {
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

                    if (DailyQcUtils.pdata[j].stn[i].frain[k].qual != 0
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 8
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 3
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 6
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 2) {
                        continue;
                    }

                    /* dont use missing stations */

                    if (DailyQcUtils.pdata[j].stn[i].frain[k].data < 0) {
                        continue;
                    }

                    lat = precip_stations.get(i).lat;
                    lon = precip_stations.get(i).lon;

                    if (isohyets_used) {
                        isoh = precip_stations.get(i).isoh[isom];
                    }

                    testdist = Math.pow((lat1 - lat), 2)
                            + Math.pow((lon1 - lon), 2);
                    if (testdist == 0.0) {
                        testdist = .000001;
                    }

                    if (method == 2 && isoh > 0 && isoh1 > 0) {
                        padj = DailyQcUtils.pdata[j].stn[i].frain[k].data
                                * (isoh1 / isoh);
                    } else {
                        padj = DailyQcUtils.pdata[j].stn[i].frain[k].data;
                    }

                    fdist += 1 / testdist;
                    fdata += padj / testdist;
                    l++;
                }

                if (l < dqc_min_good) {
                    fdist = 0.0;
                    fdata = 0.0;

                    l = 0;

                    for (i = 0; i < max_stations; i++) {
                        if (i == m) {
                            continue;
                        }

                        if (DailyQcUtils.pdata[j].stn[i].frain[k].qual != 0
                                && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 8
                                && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 3
                                && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 6
                                && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 2) {
                            continue;
                        }

                        if (DailyQcUtils.pdata[j].stn[i].frain[k].data < 0) {
                            continue;
                        }

                        lat = precip_stations.get(i).lat;
                        lon = precip_stations.get(i).lon;

                        if (isohyets_used) {
                            isoh = precip_stations.get(i).isoh[isom];
                        }

                        testdist = Math.pow((lat1 - lat), 2)
                                + Math.pow((lon1 - lon), 2);

                        if (testdist == 0.0) {
                            testdist = .0001;
                        }

                        if (method == 2 && isoh > 0 && isoh1 > 0) {
                            padj = DailyQcUtils.pdata[j].stn[i].frain[k].data
                                    * isoh1 / isoh;
                        } else {
                            padj = DailyQcUtils.pdata[j].stn[i].frain[k].data;
                        }

                        fdist += 1 / testdist;
                        fdata += padj / testdist;
                        l++;
                    }

                }

                if (l != 0) {
                    fvalue[k] = fdata / fdist;
                } else {
                    fvalue[k] = -9999;
                }

            }

            /* have good and estimated periods */

            if (fvalue[0] == -9999 || fvalue[1] == -9999 || fvalue[2] == -9999
                    || fvalue[3] == -9999) {
                continue;
            }

            ftotal = 0;

            for (k = 0; k < 4; k++) {
                if (DailyQcUtils.pdata[j].stn[m].rrain[k].data >= 0
                        && (DailyQcUtils.pdata[j].stn[m].frain[k].qual == 0
                                || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 8
                                || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 3
                                || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 2)) {
                    ftotal = ftotal
                            + DailyQcUtils.pdata[j].stn[m].rrain[k].data;
                } else {
                    ftotal = ftotal + fvalue[k];

                    DailyQcUtils.pdata[j].stn[m].frain[k].data = (float) fvalue[k];
                    DailyQcUtils.pdata[j].stn[m].frain[k].qual = 5;
                }
            }

            DailyQcUtils.pdata[j].stn[m].frain[k].data = (float) ftotal;
            DailyQcUtils.pdata[j].stn[m].frain[k].qual = 4;
        }
    }
}