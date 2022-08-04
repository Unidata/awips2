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
 * Quality Control for MPE station data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            snaples     Initial creation
 * Jul 26, 2017 6148       bkowal      Size arrays based on number of stations and possible neighbors. No longer
 *                                     hardcode to 3000.
 * Oct 03, 2017 6407       bkowal      Eliminated warnings.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class QCStations {
    private int good;

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public void quality_control_stations(int j, List<Station> precip_stations,
            int numPstations) {

        int k = 0;

        for (k = 4; k >= 0; k--) {
            int ogood = 0;

            if (DailyQcUtils.pdata[j].used[k] == 0) {
                continue;
            }

            good = 0;

            qcLoop(j, k, precip_stations, numPstations);
            while (good > ogood) {
                ogood = good;
                good = 0;
                qcLoop(j, k, precip_stations, numPstations);
            }
        }

    }

    private void qcLoop(int j, int k, List<Station> precip_stations,
            int numPstations) {
        int m = 0;
        int i = 0;
        int l = 0;
        int h = 0;
        double lat1, lat;
        double lon1, lon;
        float conv = .0174f;
        double distlon;
        double fdist;
        double fdata;
        double isoh = 0.;
        double isoh1 = 0.;
        int maxl;
        float valdif;
        double fvalu, fstd, fdif;
        int dqc_neig = DailyQcUtils.mpe_dqc_max_precip_neighbors;
        int isom = DailyQcUtils.isom;
        boolean isohyets_used = dqc.isohyets_used;
        int method = dqc.method;
        double testdist[] = new double[numPstations * dqc_neig];
        double padj[] = new double[numPstations * dqc_neig];
        int max_stations = numPstations;

        for (m = 0; m < max_stations; m++) {
            lat1 = precip_stations.get(m).lat;
            lon1 = precip_stations.get(m).lon;

            /* get isohyet */

            if (isohyets_used) {
                isoh1 = precip_stations.get(m).isoh[isom];
            }

            /* if data is missing dont quality control */

            if (DailyQcUtils.pdata[j].stn[m].frain[k].data < 0) {
                continue;
            }

            l = 0;
            for (int ii = 0; ii < dqc_neig; ii++) {

                i = precip_stations.get(m).index[ii];

                /* only use good or forced good gages to estimate others */

                if (DailyQcUtils.pdata[j].stn[i].frain[k].qual != 0
                        && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 8
                        && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 6
                        && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 3
                        && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 4) {
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

                distlon = Math
                        .abs((lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv));

                testdist[l] = Math.pow((lat1 - lat), 2)
                        + Math.pow((distlon), 2);
                if (testdist[l] == 0.0) {
                    testdist[l] = .000001;
                }

                if (method == 2 && isoh > 0 && isoh1 > 0) {
                    padj[l] = DailyQcUtils.pdata[j].stn[i].frain[k].data
                            * (isoh1 / isoh);
                } else {
                    padj[l] = DailyQcUtils.pdata[j].stn[i].frain[k].data;
                }

                l++;

            }

            if (l < 5) {
                for (i = 0; i < max_stations; i++) {
                    if (i == m) {
                        continue;
                    }

                    /* only use good or forced good gages to estimate others */

                    if (DailyQcUtils.pdata[j].stn[i].frain[k].qual != 0
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 8
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 6
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 3
                            && DailyQcUtils.pdata[j].stn[i].frain[k].qual != 4) {
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

                    distlon = (lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv);

                    testdist[l] = Math.pow((lat1 - lat), 2)
                            + Math.pow((distlon), 2);

                    if (testdist[l] == 0.0) {
                        testdist[l] = .000001;
                    }

                    if (method == 2 && isoh > 0 && isoh1 > 0) {
                        padj[l] = DailyQcUtils.pdata[j].stn[i].frain[k].data
                                * isoh1 / isoh;
                    } else {
                        padj[l] = DailyQcUtils.pdata[j].stn[i].frain[k].data;
                    }

                    l++;
                }

            }

            /* get weighted mean */

            maxl = l;

            fdata = 0;
            fdist = 0;

            for (l = 0; l < maxl; l++) {
                fdata = fdata + (padj[l] / testdist[l]);
                fdist = fdist + (1 / testdist[l]);
            }

            /* fvalu is estimated number */

            if (maxl == 0) {
                fdist = 1;
            }

            fvalu = fdata / fdist;

            DailyQcUtils.pdata[j].stn[m].frain[k].estimate = (float) fvalu;

            fstd = 0.0;

            for (l = 0; l < maxl; l++) {
                fstd = fstd + Math.abs(padj[l] - fvalu) / testdist[l];
            }

            fstd = fstd / fdist;

            /* if estimated data 0 */

            if (fstd == 0.0 && (DailyQcUtils.pdata[j].stn[m].frain[k].qual == 0
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 1
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 5
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 6
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 4
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 2)) {
                DailyQcUtils.pdata[j].stn[m].frain[k].stddev = -9999;
                continue;
            } else if (fstd == 0.0
                    && DailyQcUtils.pdata[j].stn[m].frain[k].data <= .1) {

                DailyQcUtils.pdata[j].stn[m].frain[k].stddev = 0;
                DailyQcUtils.pdata[j].stn[m].frain[k].qual = 8;

                good++;

                continue;
            } else if (fstd == 0.0) {
                DailyQcUtils.pdata[j].stn[m].frain[k].stddev = -9999;
                DailyQcUtils.pdata[j].stn[m].frain[k].qual = 3;

                if (k == 4) {
                    for (h = 0; h < 4; h++) {
                        if (DailyQcUtils.pdata[j].stn[m].frain[h].qual != 6) {
                            DailyQcUtils.pdata[j].stn[m].frain[h].qual = 3;
                        }
                    }
                }
                continue;
            }

            /* perhaps change this to be more robust */
            if (fstd < .05) {
                fstd = .05;
            } else if (fstd > .20) {
                fstd = .20;
            }

            fdif = Math.abs(DailyQcUtils.pdata[j].stn[m].frain[k].data - fvalu);
            fdif = fdif / fstd;

            valdif = (float) Math
                    .abs(DailyQcUtils.pdata[j].stn[m].frain[k].data - fvalu);

            /* standard deviation check */
            /* check only if difference between actual and estimated > .1 */

            DailyQcUtils.pdata[j].stn[m].frain[k].estimate = (float) fvalu;
            DailyQcUtils.pdata[j].stn[m].frain[k].stddev = (float) fdif;

            if (DailyQcUtils.pdata[j].stn[m].frain[k].qual == 0
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 1
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 5
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 6
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 4
                    || DailyQcUtils.pdata[j].stn[m].frain[k].qual == 2) {
                continue;
            }

            if (fdif > DailyQcUtils.pdata[j].stddev && valdif > .10) {
                DailyQcUtils.pdata[j].stn[m].frain[k].qual = 3;
            } else {
                good++;
                if (DailyQcUtils.pdata[j].stn[m].frain[k].qual == 3) {
                    DailyQcUtils.pdata[j].stn[m].frain[k].qual = 8;
                }

            }
        }
    }
}
