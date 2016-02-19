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

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.QcPrecipOptionsDialog;
import com.raytheon.viz.mpe.ui.dialogs.QcTempOptionsDialog;
import com.raytheon.viz.mpe.util.BadTValues;
import com.raytheon.viz.mpe.util.BadValues;
import com.raytheon.viz.mpe.util.CheckConsistency;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.EstDailyStations;
import com.raytheon.viz.mpe.util.EstDailyTStations;
import com.raytheon.viz.mpe.util.EstPartStations;
import com.raytheon.viz.mpe.util.QCStations;
import com.raytheon.viz.mpe.util.QCTStations;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            snaples     Initial creation
 * Feb 5, 2015  17101      snaples     Updated max_stations to use size of dqc.precip_stations.
 * Jun 25, 2015 17397      snaples     Removed send_expose in apply_group method to prevent screen from blanking.
 * Jan 07, 2015 5235       bkowal      Do not clear the QC grid after applying a temperature group update.
 *                                     Eliminated all warnings.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GroupEditCalls {

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    int group_edit = 0;

    int pcpn_day = DailyQcUtils.pcpn_day;

    OtherPrecipOptions opo = new OtherPrecipOptions();

    int pcpn_time_step = MPEDisplayManager.pcpn_time_step;

    int pcp_flag = DailyQcUtils.pcp_flag;

    int points_flag = DailyQcUtils.points_flag;

    int grids_flag = DailyQcUtils.grids_flag;

    int map_flag = DailyQcUtils.map_flag;

    int pcpn_time = DailyQcUtils.pcpn_time;

    int max_stations = DailyQcUtils.precip_stations.size();

    int max_tstations = DailyQcUtils.temperature_stations.size();

    public void apply_group()

    {
        int k, time_pos;

        QcPrecipOptionsDialog.groupEditBtn.setEnabled(false);

        for (k = 0; k < 5; k++) {

            if (k < 4) {
                time_pos = pcpn_day * 4 + k;
            } else {
                time_pos = 40 + pcpn_day;
            }

            DailyQcUtils.pcp_in_use[time_pos] = -1;

            if (DailyQcUtils.pdata[pcpn_day].used[k] != 0) {
                DailyQcUtils.pdata[pcpn_day].used[k] = 2;
            }
        }

        QcPrecipOptionsDialog.dataSet.clear();
        QcPrecipOptionsDialog.dataSet.add(0,
                QcPrecipOptionsDialog.dataType.get(0));
        QcPrecipOptionsDialog.dataSet.add(1,
                QcPrecipOptionsDialog.dataType.get(7));
        String[] a = new String[QcPrecipOptionsDialog.dataSet.size()];
        QcPrecipOptionsDialog.setDataSetCombo(QcPrecipOptionsDialog.dataSet
                .toArray(a));

        if (pcpn_time_step == 0) {
            time_pos = pcp_flag;
        } else {
            time_pos = 40 + pcpn_day;
        }

        if (points_flag == 1 && DailyQcUtils.pcp_in_use[time_pos] == -1) {
            k = 0;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == -1) {
            k = 0;
        } else if (points_flag == -1 && grids_flag == 1 && map_flag == -1) {
            k = 1;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == 1) {
            k = 2;
        } else if (points_flag == 1 && grids_flag == 1 && map_flag == -1) {
            k = 3;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == 1) {
            k = 4;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == -1) {
            k = 5;
        }

        QcPrecipOptionsDialog.selectDataSetVal(k);

        BadValues bv = new BadValues();
        bv.update_bad_values(pcpn_day);

        /*
         * do not estimate daily and partial point precipitation from each other
         * if run DQC on partial time frame and pcpn_day=0
         */

        if (pcpn_day == 0
                && (dqc.curHr00_06 == 1 || dqc.curHr06_12 == 1 || dqc.curHr18_00 == 1)) {
        } else {
            EstDailyStations eds = new EstDailyStations();
            eds.estimate_daily_stations(pcpn_day, DailyQcUtils.precip_stations,
                    max_stations);

            EstPartStations eps = new EstPartStations();
            eps.estimate_partial_stations(pcpn_day,
                    DailyQcUtils.precip_stations, max_stations);
        }

        QCStations qcs = new QCStations();
        qcs.quality_control_stations(pcpn_day, DailyQcUtils.precip_stations,
                max_stations);

        CheckConsistency cc = new CheckConsistency();
        cc.check_consistency(pcpn_day, DailyQcUtils.precip_stations,
                max_stations);

        bv.restore_bad_values(pcpn_day, DailyQcUtils.precip_stations,
                max_stations);
        group_edit = 0;

        QcPrecipOptionsDialog.groupEditBtn.setEnabled(true);
        return;
    }

    public void apply_tgroup() {

        int k = 0, time_pos = 0;

        QcTempOptionsDialog.groupEditBtn.setEnabled(false);

        if (pcpn_time_step == 1) {

            if (DailyQcUtils.tdata[pcpn_day].used[4] != 0) {
                DailyQcUtils.tdata[pcpn_day].used[4] = 2;
            }

            DailyQcUtils.pcp_in_use[190 + pcpn_day] = -1;

        }

        else if (pcpn_time_step == 2) {

            if (DailyQcUtils.tdata[pcpn_day].used[5] != 0) {
                DailyQcUtils.tdata[pcpn_day].used[5] = 2;
            }

            DailyQcUtils.pcp_in_use[200 + pcpn_day] = -1;

        }

        else {

            if (DailyQcUtils.tdata[pcpn_day].used[DailyQcUtils.pcpn_time] != 0) {
                DailyQcUtils.tdata[pcpn_day].used[DailyQcUtils.pcpn_time] = 2;
            }

            DailyQcUtils.pcp_in_use[150 + DailyQcUtils.pcp_flag + 1] = -1;

        }

        if (pcpn_time_step == 1 || pcpn_time_step == 2) {

            for (k = 0; k < 4; k++) {

                time_pos = 150 + pcpn_day * 4 + k;

                DailyQcUtils.pcp_in_use[time_pos] = -1;

                if (DailyQcUtils.tdata[pcpn_day].used[k] != 0) {
                    DailyQcUtils.tdata[pcpn_day].used[k] = 2;
                }

            }

        }

        QcTempOptionsDialog.dataSet.clear();
        QcTempOptionsDialog.dataSet.add(0, QcTempOptionsDialog.dataType.get(0));
        QcTempOptionsDialog.dataSet.add(1, QcTempOptionsDialog.dataType.get(7));
        String[] a = new String[QcTempOptionsDialog.dataSet.size()];
        QcTempOptionsDialog.dataDispCbo.setItems(QcTempOptionsDialog.dataSet
                .toArray(a));

        if (pcpn_time_step == 0) {
            time_pos = 150 + DailyQcUtils.pcp_flag + 1;
        } else if (pcpn_time_step == 1) {
            time_pos = 190 + DailyQcUtils.pcpn_day;
        } else if (pcpn_time_step == 2) {
            time_pos = 200 + DailyQcUtils.pcpn_day;
        }

        if (points_flag == 1 && DailyQcUtils.pcp_in_use[time_pos] == -1) {
            k = 0;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == -1) {
            k = 0;
        } else if (points_flag == -1 && grids_flag == 1 && map_flag == -1) {
            k = 1;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == 1) {
            k = 2;
        } else if (points_flag == 1 && grids_flag == 1 && map_flag == -1) {
            k = 3;
        } else if (points_flag == 1 && grids_flag == -1 && map_flag == 1) {
            k = 4;
        } else if (points_flag == -1 && grids_flag == -1 && map_flag == -1) {
            k = 5;
        }

        QcTempOptionsDialog.dataDispCbo.select(k);

        BadTValues btv = new BadTValues();
        btv.update_bad_tvalues(DailyQcUtils.pcpn_day);

        EstDailyTStations edt = new EstDailyTStations();
        edt.estimate_daily_tstations(DailyQcUtils.pcpn_day,
                DailyQcUtils.temperature_stations, max_tstations);

        QCTStations qct = new QCTStations();
        qct.quality_control_tstations(DailyQcUtils.pcpn_day,
                DailyQcUtils.temperature_stations, max_tstations);

        btv.restore_bad_tvalues(DailyQcUtils.pcpn_day,
                DailyQcUtils.temperature_stations, max_tstations);

        group_edit = 0;

        QcTempOptionsDialog.groupEditBtn.setEnabled(true);
        return;
    }
}
