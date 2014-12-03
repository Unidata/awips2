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
 * Mar 11, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GetZLevel {

    DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    public void get_zlevel(int j, ArrayList<Station> precip_stations,
            ArrayList<Station> freezing_stations, int max_stations,
            int max_zstations) {

        int m, i, h, l, ii;
        double lat1, lon1, fdist, fdata, fvalue, lat, lon, testdist, padj, distlon;
        float conv = .0174f;
        int h1, h2, j1, j2;

        if (max_zstations == 0) {
            for (m = 0; m < max_stations; m++) {
                for (h = 0; h < 4; h++) {
                    dqc.pdata[j].stn[m].frzlvl[h] = -99;
                }
            }
            return;
        }

        for (m = 0; m < max_stations; m++) {
            /* only estimate missing data */
            for (h = 0; h < 4; h++) {
                if (h == 3) {
                    if (j == 0) {
                        j1 = j;
                        j2 = j;
                        h1 = h;
                        h2 = h;
                    } else {
                        j1 = j;
                        j2 = j - 1;
                        h1 = h;
                        h2 = 0;
                    }
                } else {
                    j1 = j;
                    j2 = j;
                    h1 = h;
                    h2 = h + 1;
                }

                lat1 = precip_stations.get(m).lat;
                lon1 = precip_stations.get(m).lon;

                fdist = 0.0;
                fdata = 0.0;
                l = 0;

                for (ii = 0; ii < 5; ii++) {

                    i = precip_stations.get(m).zindex[ii];

                    /* dont estimate unless good or forced good */

                    if (dqc.zdata[j1].zstn[i].zlevel2[h1].qual != 0
                            && dqc.zdata[j1].zstn[i].zlevel2[h1].qual != 8
                            && dqc.zdata[j1].zstn[i].zlevel2[h1].qual != 3
                            && dqc.zdata[j2].zstn[i].zlevel2[h2].qual != 0
                            && dqc.zdata[j2].zstn[i].zlevel2[h2].qual != 8
                            && dqc.zdata[j2].zstn[i].zlevel2[h2].qual != 3) {
                        continue;
                    }

                    /* dont use missing stations */

                    if (dqc.zdata[j1].zstn[i].zlevel2[h1].data < 0
                            || dqc.zdata[j2].zstn[i].zlevel2[h2].data < 0) {
                        continue;
                    }

                    lat = freezing_stations.get(i).lat;
                    lon = freezing_stations.get(i).lon;

                    distlon = (lon1 - lon) * Math.cos((lat1 + lat) / 2 * conv);

                    testdist = Math.pow((lat1 - lat), 2) + Math.pow(distlon, 2);

                    if (testdist == 0.0) {
                        testdist = .0001;
                    }

                    padj = (dqc.zdata[j1].zstn[i].zlevel2[h1].data + dqc.zdata[j2].zstn[i].zlevel2[h2].data) / 2;
                    fdist = 1 / testdist + fdist;
                    fdata = padj / testdist + fdata;
                    l++;
                }

                if (l < 5) {
                    fdist = 0.0;
                    fdata = 0.0;
                    l = 0;
                    for (i = 0; i < max_zstations; i++) {

                        if (dqc.zdata[j1].zstn[i].zlevel2[h1].qual != 0
                                && dqc.zdata[j1].zstn[i].zlevel2[h1].qual != 8
                                && dqc.zdata[j1].zstn[i].zlevel2[h1].qual != 3
                                && dqc.zdata[j2].zstn[i].zlevel2[h2].qual != 0
                                && dqc.zdata[j2].zstn[i].zlevel2[h2].qual != 8
                                && dqc.zdata[j2].zstn[i].zlevel2[h2].qual != 3) {
                            continue;
                        }

                        /* dont use missing stations */

                        if (dqc.zdata[j1].zstn[i].zlevel2[h1].data < 0
                                || dqc.zdata[j2].zstn[i].zlevel2[h2].data < 0) {
                            continue;
                        }

                        lat = freezing_stations.get(i).lat;
                        lon = freezing_stations.get(i).lon;

                        distlon = (lon1 - lon)
                                * Math.cos((lat1 + lat) / 2 * conv);

                        testdist = Math.pow((lat1 - lat), 2)
                                + Math.pow(distlon, 2);

                        if (testdist == 0.0) {
                            testdist = .0001;
                        }

                        padj = (dqc.zdata[j1].zstn[i].zlevel2[h1].data + dqc.zdata[j2].zstn[i].zlevel2[h2].data) / 2;

                        fdist = 1 / testdist + fdist;
                        fdata = padj / testdist + fdata;
                        l++;
                    }
                }

                if (l == 0) {
                    fvalue = -99;
                } else {
                    fvalue = fdata / fdist * 1000;
                }
                dqc.pdata[j].stn[m].frzlvl[h] = (int) fvalue;
            }
        }
    }
}
