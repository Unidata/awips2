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
 * Checks consistency of data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            snaples     Initial creation
 * May 21, 2018   7131     mduff       Updated to accommodate changes to other classes and cleanup.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class CheckConsistency {

    public void check_consistency(int j, List<Station> precip_stations,
            int numPstations) {

        int dqc_neig = DailyQcUtils.mpe_dqc_max_precip_neighbors;
        int max_stations = numPstations;
        float rtotal;

        for (int k = 0; k < max_stations; k++) {
            DailyQcUtils.pdata[j].stn[k].tcons = 1;

            if (DailyQcUtils.pdata[j].stn[k].frain[4].data < 0) {
                continue;
            }

            rtotal = 0;
            for (int m = 0; m < 4; m++) {
                if (DailyQcUtils.pdata[j].stn[k].frain[m].data >= 0) {
                    rtotal += DailyQcUtils.pdata[j].stn[k].frain[m].data;
                }

            }

            if (Math.abs(rtotal
                    - DailyQcUtils.pdata[j].stn[k].frain[4].data) > .01) {
                DailyQcUtils.pdata[j].stn[k].tcons = -1;
            }
        }

        for (int k = 0; k < max_stations; k++) {
            for (int m = 0; m < 5; m++) {
                if (DailyQcUtils.pdata[j].stn[k].frain[m].qual == 1
                        || DailyQcUtils.pdata[j].stn[k].frain[m].data < 0) {
                    continue;
                }

                DailyQcUtils.pdata[j].stn[k].scons[m] = 1;
                for (int ii = 0; ii < dqc_neig; ii++) {
                    int i = precip_stations.get(k).index[ii];

                    if ((precip_stations.get(i).lat == precip_stations
                            .get(k).lat)
                            && (precip_stations.get(i).lon == precip_stations
                                    .get(k).lon)
                            && (DailyQcUtils.pdata[j].stn[i].frain[m].qual != 1)
                            && (DailyQcUtils.pdata[j].stn[i].frain[m].data >= 0)
                            && (DailyQcUtils.pdata[j].stn[i].frain[m].data != DailyQcUtils.pdata[j].stn[k].frain[m].data)) {

                        DailyQcUtils.pdata[j].stn[k].scons[m] = -1;
                    }
                }
            }
        }
    }
}
