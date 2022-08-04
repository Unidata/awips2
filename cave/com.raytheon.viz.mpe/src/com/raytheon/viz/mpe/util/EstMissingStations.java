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

import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * Used to estimate missing MPE stations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            snaples     Initial creation
 * Dec 22, 2014  #16952    lbousaidi   fixed the eratic estimate values.
 * Jun 26, 2015  17397     snaples     fixed another instance of erratic estimates.
 * Oct 03, 2017  6407      bkowal      Eliminated warnings.
 * Aug 17, 2018   7390     smanoj      Fix for future data periods should remain "M"
 * Aug 31, 2018   7388     smanoj      Estimate done for "m" sites when Render
 * Jan 07, 2019   7703     smanoj      Fix an issue with missing sites.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class EstMissingStations {

    /* j is the day number. */

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public void estimate_missing_stations(int j, List<Station> precip_stations,
            int max_stations, Pdata[] pdata) {
        int mpe_dqc_max_precip_neighbors = DailyQcUtils.mpe_dqc_max_precip_neighbors;
        int mpe_dqc_min_good_stations = dqc.mpe_dqc_min_good_stations;
        int isom = DailyQcUtils.isom;
        boolean isohyets_used = dqc.isohyets_used;
        int method = dqc.method;
        int m, i, h, l, ii;
        double lat1, lon1, fdist, fdata, lat, lon, testdist, isoh = 0.,
                isoh1 = 0., padj, distlon;
        double fvalue[] = new double[4];
        float conv = .0174f;
        double fvalue24 = 0., fvalue06, fmult, stotal;
        int num_missing;

        //Time period for estimating missing stations when there is no 24 hour data.
        int numTimePeriods = 4;
        if(dqc.curHr12_18 ==1){
            numTimePeriods = 1;
        }else if (dqc.curHr18_00 ==1){
            numTimePeriods = 2;
        }else if (dqc.curHr00_06 == 1){
            numTimePeriods = 3;
        }

        for (m = 0; m < max_stations; m++) {
            /*
             * Force 24/6 hr value to be the estimated value when set to bad by
             * the forecaster in the Edit Precipitation window. (added OB9.x)
             */
            for (h = 0; h < 5; h++) {
                if (pdata[j].stn[m].frain[h].qual == 1) {
                    pdata[j].stn[m].frain[h].data = pdata[j].stn[m].frain[h].estimate;
                    pdata[j].stn[m].frain[h].qual = 5;
                }
            }

            /* only estimate missing data */

            if (pdata[j].stn[m].frain[4].data >= 0
                    && pdata[j].stn[m].frain[4].qual != 5
                    && pdata[j].stn[m].frain[4].qual != 1) {
                continue;
            }

            for (h = 4; h >= 0; h--) {
                if (pdata[j].used[h] == 0) {
                    fvalue24 = -1;
                    continue;
                }

                lat1 = precip_stations.get(m).lat;
                lon1 = precip_stations.get(m).lon;

                /* get isohyet */

                if (isohyets_used) {
                    isoh1 = precip_stations.get(m).isoh[isom];
                }

                fdist = 0.0;
                fdata = 0.0;
                l = 0;

                for (ii = 0; ii < mpe_dqc_max_precip_neighbors; ii++) {
                    i = precip_stations.get(m).index[ii];

                    /* dont estimate unless good or forced good */

                    if (pdata[j].stn[i].frain[h].qual != 0
                            && pdata[j].stn[i].frain[h].qual != 8
                            && pdata[j].stn[i].frain[h].qual != 6
                            && pdata[j].stn[i].frain[h].qual != 3
                            && pdata[j].stn[i].frain[h].qual != 2) {
                        continue;
                    }

                    /* dont use missing stations */

                    if (pdata[j].stn[i].frain[h].data < 0) {
                        continue;
                    }

                    lat = precip_stations.get(i).lat;
                    lon = precip_stations.get(i).lon;

                    if (isohyets_used) {
                        isoh = precip_stations.get(i).isoh[isom];
                    }

                    distlon = (lon1 - lon)
                            * Math.cos(((lat1 + lat) / 2) * conv);

                    testdist = Math.pow((lat1 - lat), 2) + Math.pow(distlon, 2);

                    if (testdist == 0.0) {
                        testdist = .0001;
                    }

                    if (method == 2 && isoh > 0 && isoh1 > 0) {
                        padj = pdata[j].stn[i].frain[h].data * isoh1 / isoh;
                    } else {
                        padj = pdata[j].stn[i].frain[h].data;
                    }

                    fdist += 1 / testdist;
                    fdata += padj / testdist;

                    l++;

                }

                if (l < mpe_dqc_min_good_stations) {
                    fdist = 0.0;
                    fdata = 0.0;
                    l = 0;

                    for (i = 0; i < max_stations; i++) {
                        if (i == m) {
                            continue;
                        }

                        /* dont estimate unless good or forced good */

                        if (pdata[j].stn[i].frain[h].qual != 0
                                && pdata[j].stn[i].frain[h].qual != 8
                                && pdata[j].stn[i].frain[h].qual != 6
                                && pdata[j].stn[i].frain[h].qual != 3
                                && pdata[j].stn[i].frain[h].qual != 2) {
                            continue;
                        }

                        /* dont use missing stations */

                        if (pdata[j].stn[i].frain[h].data < 0) {
                            continue;
                        }

                        lat = precip_stations.get(i).lat;
                        lon = precip_stations.get(i).lon;

                        if (isohyets_used) {
                            isoh = precip_stations.get(i).isoh[isom];
                        }

                        distlon = (lon1 - lon)
                                * Math.cos(((lat1 + lat) / 2) * conv);

                        testdist = Math.pow((lat1 - lat), 2)
                                + Math.pow(distlon, 2);

                        if (testdist == 0.0) {
                            testdist = .0001;
                        }

                        if (method == 2 && isoh > 0 && isoh1 > 0) {
                            padj = pdata[j].stn[i].frain[h].data * isoh1 / isoh;
                        } else {
                            padj = pdata[j].stn[i].frain[h].data;
                        }

                        fdist += 1 / testdist;
                        fdata += padj / testdist;

                        l++;
                    }
                }

                /* 24 hourly has stations */

                if (h == 4 && l > 0) {
                    fvalue24 = fdata / fdist;
                } else if (h == 4) {
                    fvalue24 = -1;
                } else if (l > 0) {
                    fvalue[h] = fdata / fdist;
                } else {
                    fvalue[h] = -1;
                }
            }

            /* have all 24 hour data */

            /* 24 hourly data available */

            if (fvalue24 >= 0) {
                fvalue06 = 0.0;
                stotal = 0.0;
                num_missing = 0;

                for (h = 0; h < 4; h++) {
                    /*
                     * use good, forced good and questionable data to estimate
                     */

                    /*
                     * need to caculate partial total to ensure that 24 hourly
                     * and 6 hourly data match
                     */

                    if ((pdata[j].stn[m].frain[h].qual == 0
                            || pdata[j].stn[m].frain[h].qual == 8
                            || pdata[j].stn[m].frain[h].qual == 3
                            || pdata[j].stn[m].frain[h].qual == 2)
                            && pdata[j].stn[m].frain[h].data >= 0) {
                        stotal = stotal + pdata[j].stn[m].frain[h].data;
                    } else {
                        num_missing++;
                        fvalue06 = fvalue06 + fvalue[h];
                    }
                }

                /*
                 * stotal will now be difference between 24 hour estimate and 6
                 * hourly partial total
                 */

                stotal = fvalue24 - stotal;

                if (stotal <= 0) {
                    stotal = 0;
                }

                if (fvalue06 == 0) {
                    fmult = 0;
                } else {
                    /*
                     * fmult is that ratio of actual summed (missing) 6 hourly
                     * rain to estimated 6 hourly rain
                     */
                    fmult = stotal / fvalue06;
                }

                /*
                 * now rescale the estimates so they equal the 24 hour estimate
                 */

                for (h = 0; h < 4; h++) {
                    if ((pdata[j].stn[m].frain[h].qual != 0
                            && pdata[j].stn[m].frain[h].qual != 8
                            && pdata[j].stn[m].frain[h].qual != 3
                            && pdata[j].stn[m].frain[h].qual != 2)
                            || pdata[j].stn[m].frain[h].data < 0) {

                        if (fvalue06 != 0) {
                            pdata[j].stn[m].frain[h].data = (float) (fvalue[h]
                                    * fmult);
                        } else {
                            pdata[j].stn[m].frain[h].data = (float) (stotal
                                    / num_missing);
                        }

                        pdata[j].stn[m].frain[h].qual = 5;
                    }
                }

                pdata[j].stn[m].frain[4].qual = 5;
                pdata[j].stn[m].frain[4].data = (float) fvalue24;

            }

            /*
             * no 24 hour data - estimate done for "m" sites when Render Grids +
             * MAPs
             */
            else {

                for (h = 0; h < numTimePeriods; h++) {
                    if (pdata[j].stn[m].frain[h].qual != 0
                            && pdata[j].stn[m].frain[h].qual != 8
                            && pdata[j].stn[m].frain[h].qual != 3
                            && pdata[j].stn[m].frain[h].qual != 2) {
                        pdata[j].stn[m].frain[h].data = (float) fvalue[h];
                        pdata[j].stn[m].frain[h].qual = 5;
                    }
                }
            }
        }
    }
}
