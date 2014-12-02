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
    
    private DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    public void group_edit_temp_stations(ReferencedCoordinate rcoord) {
        int time_pos = 0;
        int i, m, k;
        float lat, lon;
        double testdist, maxdist;
        int isave;
        int max_stations = dqc.temperature_stations.size();
        Coordinate coord = new Coordinate();

        try {
            coord = rcoord.asLatLon();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        if (MPEDisplayManager.pcpn_time_step == 0) {
            time_pos = dqc.pcpn_time;
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
            if (dqc.tdata[dqc.pcpn_day].tstn[i].tlevel2[time_pos].data == -99) {
                continue;
            }

            if (dqc.tdata[dqc.pcpn_day].tstn[i].tlevel2[time_pos].data < dqc.temperature_filter_value) {
                continue;
            }

            /* Retrieve the latitude and longitude of this station. */
            lat = dqc.temperature_stations.get(i).lat;
            lon = dqc.temperature_stations.get(i).lon;

            for (m = 0; m < dqc.tsmax; m++) {

                if ((dqc.temperature_stations.get(i).parm.substring(3,
                        5).equalsIgnoreCase(dqc.ts[m].abr))
                        && dqc.dflag[m + 1] == 1) {
                    break;
                }

            }

            if (m == dqc.tsmax) {
                continue;
            }

            for (m = 0; m < 9; m++) {

                if (m == dqc.tdata[dqc.pcpn_day].tstn[i].tlevel2[time_pos].qual
                        && dqc.qflag[m] == 1) {
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

        if (dqc.tdata[dqc.pcpn_day].tstn[isave].tlevel2[time_pos].qual != GroupEditStationsDialog.group_qual) {
            QcTempOptionsDialog.renderGridsBtn.setEnabled(true);
        }

        dqc.tdata[dqc.pcpn_day].tstn[isave].tlevel2[time_pos].qual = (short) GroupEditStationsDialog.group_qual;

        if (MPEDisplayManager.pcpn_time_step == 0) {
            time_pos = 150 + dqc.pcpn_day * 4 + dqc.pcpn_time;
        } else if (MPEDisplayManager.pcpn_time_step == 1) {
            time_pos = 190 + dqc.pcpn_day;
        } else if (MPEDisplayManager.pcpn_time_step == 2) {
            time_pos = 200 + dqc.pcpn_day;
        }

        dqc.pcp_in_use[time_pos] = -1;

        dqc.tdata[dqc.pcpn_day].used[dqc.pcpn_time] = 2;

        if (MPEDisplayManager.pcpn_time_step == 1
                || MPEDisplayManager.pcpn_time_step == 2) {
            for (k = 0; k < 4; k++) {

                time_pos = 150 + dqc.pcpn_day * 4 + k;

                dqc.pcp_in_use[time_pos] = -1;

                if (dqc.tdata[dqc.pcpn_day].used[k] != 0) {
                    dqc.tdata[dqc.pcpn_day].used[k] = 2;
                }
            }
        }
        return;
    }
}
