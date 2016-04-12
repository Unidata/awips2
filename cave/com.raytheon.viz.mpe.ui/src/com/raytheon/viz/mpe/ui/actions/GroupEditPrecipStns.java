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
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.GroupEditStationsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcPrecipOptionsDialog;
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
 * May 06, 2011   #8994    jpiatt      Added set precipitation value as zero
 * Sep 04, 2014    283     cgobs       Fixed possible selection of filtered-out gages
 * Dec 2015        17388   ptilles     Add test for mpe_dqc_6hr_24hr_set_bad token value
 * Mar 15, 2016    18427   lbousaidi   Code Improvements for DR 18384 (Vlab 13938)
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GroupEditPrecipStns {
	
    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(GroupEditPrecipStns.class);
	
    public void group_edit_precip_stations(ReferencedCoordinate rcoord) {
        int time_pos;
        int i, m, k;
        float lat, lon;
        double testdist, maxdist;
        int isave;
        int max_stations = DailyQcUtils.precip_stations.size();
        Coordinate coord = new Coordinate();

        try {
            coord = rcoord.asLatLon();
        } catch (Exception e) {
            // TODO Auto-generated catch block
        	 statusHandler
             .error("Failed to convert ReferencedCoordinate to Coordinate.",
                     e);
        }

        if (MPEDisplayManager.pcpn_time_step == 0) {
            time_pos = DailyQcUtils.pcpn_time;
        } else {
            time_pos = 4;

        }

        isave = -1;
        maxdist = 9999;

        /*
         * Loop over all stations. Determine which station is the closest to the
         * mouse click point.
         */
        for (i = 0; i < max_stations; i++) {
            if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[i].frain[time_pos].data < 0) {
                continue;
            }

            // precip filter
            if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[i].frain[time_pos].data < QcPrecipOptionsDialog
                    .getPointFilterValue()) {
                continue;
            }

            // reverse precip filter
            if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[i].frain[time_pos].data > QcPrecipOptionsDialog
                    .getPointFilterReverseValue()) {
                continue;
            }

            // elevation filter
            if (DailyQcUtils.precip_stations.get(i).elev < DailyQcUtils.elevation_filter_value) {
                continue;
            }

            /* Retrieve the latitude and longitude of this station. */
            lat = DailyQcUtils.precip_stations.get(i).lat;
            lon = DailyQcUtils.precip_stations.get(i).lon;

            if (DailyQcUtils.precip_stations.get(i).tip == 0
                    && DailyQcUtils.gage_char[0] == -1) {
                continue;
            }

            if (DailyQcUtils.precip_stations.get(i).tip == 1
                    && DailyQcUtils.gage_char[1] == -1) {
                continue;
            }

            for (m = 0; m < DailyQcUtils.tsmax; m++) {

                if ((DailyQcUtils.precip_stations.get(i).parm.substring(3, 5)
                        .equalsIgnoreCase(DailyQcUtils.ts[m].abr))
                        && DailyQcUtils.dflag[m + 1] == 1) {
                    break;
                }

            }

            if (m == DailyQcUtils.tsmax) {
                continue;
            }

            for (m = 0; m < 9; m++) {

                if (m == DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[i].frain[time_pos].qual
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

        if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[time_pos].qual != GroupEditStationsDialog.group_qual) {
            QcPrecipOptionsDialog.renderGridsBtn.setEnabled(true);
        }

        DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[time_pos].qual = (short) GroupEditStationsDialog.group_qual;

        if (GroupEditStationsDialog.group_qual == 1
                && time_pos == 4
                && DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].sflag[time_pos] == 1) {

            DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[time_pos].data = DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].rrain[time_pos].data;

            DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].sflag[time_pos] = -1;

        }

        if (time_pos == 4
                && (GroupEditStationsDialog.group_qual == 1
                        || GroupEditStationsDialog.group_qual == 0 || GroupEditStationsDialog.group_qual == 8)) {

            for (k = 0; k < 4; k++) {
                DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[k].qual = (short) GroupEditStationsDialog.group_qual;
            }

        }

        /*
         * if the button "set precipitation value as 0.0" is selected, set     
         * the gage value as 0.0, also change the quality code to be       
         * manual. If 24 hour value is 0.0, then set all 6 hour values to 0.0   
         * and QC codes to "Manual"
         */
        if (GroupEditStationsDialog.group_qual == 2) {
            DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[time_pos].data = 0.0f;
            DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[time_pos].qual = 2;
            if ((DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[4].data - 0.0) < 0.0001) {
                for (k = 0; k < 4; k++) {
                    DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[k].data = 0.0f;
                    DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[k].qual = 2;
                }
            }
        }

        /* 6 hour data set bad */
        /* check value of mpe_dqc_6hr_24hr_set_bad token */
        /* if token = ON, then change 24hr QC code to Bad for this station */
        /* if token = OFF, then do not change 24hr QC code for this station */

        boolean mpe_dqc_6hr_24hr_set_bad = AppsDefaults.getInstance()
                .getBoolean("mpe_dqc_6hr_24hr_set_bad", true);

        if (time_pos != 4
                && GroupEditStationsDialog.group_qual == 1
                && DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[4].qual != 5
                && DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[4].qual != 4) {
            if (mpe_dqc_6hr_24hr_set_bad) {
                DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stn[isave].frain[4].qual = (short) GroupEditStationsDialog.group_qual;
            }
        }

        for (k = 0; k < 5; k++) {
            if (k < 4) {
                time_pos = DailyQcUtils.pcpn_day * 4 + k;
            } else {
                time_pos = 40 + DailyQcUtils.pcpn_day;
            }

            DailyQcUtils.pcp_in_use[time_pos] = -1;

            if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].used[k] != 0) {
                DailyQcUtils.pdata[DailyQcUtils.pcpn_day].used[k] = 2;
            }
        }
        return;
    }
}
