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
package com.raytheon.viz.mpe.ui.actions;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.GroupEditStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcTempOptionsDialog;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GroupEditTempStns {

    public void group_edit_temp_stations(ReferencedCoordinate rcoord) {
        int time_pos = 0;
        int i, m, k;
        float lat, lon;
        double testdist, maxdist;
        int isave;
        int max_stations = DailyQcUtils.temperature_stations.size();
        Coordinate coord = new Coordinate();

        try {
            coord = rcoord.asLatLon();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        if (MPEDisplayManager.pcpn_time_step == 0) {
            time_pos = DailyQcUtils.pcpn_time;
        } else if (MPEDisplayManager.pcpn_time_step == 1) {
            time_pos = 4;
        } else if (MPEDisplayManager.pcpn_time_step == 2) {
            time_pos = 5;
        }

        isave = -1;
        maxdist = 9999;

        /*
         * Loop over all stations. Determine which station is the closest to the
         * mouse click point.
         */
        for (i = 0; i < max_stations; i++) {
            if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[i].tlevel2[time_pos].data == -99) {
                continue;
            }

            if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[i].tlevel2[time_pos].data < DailyQcUtils.temperature_filter_value) {
                continue;
            }

            /* Retrieve the latitude and longitude of this station. */
            lat = DailyQcUtils.temperature_stations.get(i).lat;
            lon = DailyQcUtils.temperature_stations.get(i).lon;

            for (m = 0; m < DailyQcUtils.tsmax; m++) {

                if ((DailyQcUtils.temperature_stations.get(i).parm.substring(3,
                        5).equalsIgnoreCase(DailyQcUtils.ts[m].abr))
                        && DailyQcUtils.dflag[m + 1] == 1) {
                    break;
                }

            }

            if (m == DailyQcUtils.tsmax) {
                continue;
            }

            for (m = 0; m < 9; m++) {

                if (m == DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[i].tlevel2[time_pos].qual
                        && DailyQcUtils.qflag[m] == 1) {
                    break;
                }

            }

            if (m == 9) {
                continue;
            }

            testdist = Math.pow((coord.x - lon), 2)
                    + Math.pow((coord.y - lat), 2);
            testdist = Math.pow(testdist, .5);

            if (testdist < maxdist) {
                isave = i;
                maxdist = testdist;
            }

        }

        if (isave == -1) {
            return;
        }

        if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[isave].tlevel2[time_pos].qual != GroupEditStationsDialog.group_qual) {
            QcTempOptionsDialog.renderGridsBtn.setEnabled(true);
        }

        DailyQcUtils.tdata[DailyQcUtils.pcpn_day].tstn[isave].tlevel2[time_pos].qual = (short) GroupEditStationsDialog.group_qual;

        if (MPEDisplayManager.pcpn_time_step == 0) {
            time_pos = 150 + DailyQcUtils.pcpn_day * 4 + DailyQcUtils.pcpn_time;
        } else if (MPEDisplayManager.pcpn_time_step == 1) {
            time_pos = 190 + DailyQcUtils.pcpn_day;
        } else if (MPEDisplayManager.pcpn_time_step == 2) {
            time_pos = 200 + DailyQcUtils.pcpn_day;
        }

        DailyQcUtils.pcp_in_use[time_pos] = -1;

        DailyQcUtils.tdata[DailyQcUtils.pcpn_day].used[DailyQcUtils.pcpn_time] = 2;

        if (MPEDisplayManager.pcpn_time_step == 1
                || MPEDisplayManager.pcpn_time_step == 2) {
            for (k = 0; k < 4; k++) {

                time_pos = 150 + DailyQcUtils.pcpn_day * 4 + k;

                DailyQcUtils.pcp_in_use[time_pos] = -1;

                if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].used[k] != 0) {
                    DailyQcUtils.tdata[DailyQcUtils.pcpn_day].used[k] = 2;
                }
            }
        }
        return;
    }
}
