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

import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.widgets.Button;

import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.QcFreezeOptionsDialog;
import com.raytheon.viz.mpe.util.DailyQcUtils;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul  7, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class OtherFreezeOptions {

    private static final int MAX_GAGEQC_DAYS = 10;

    int time_pos = 0;

    public static int abmode = 2;

    public void set_freeze_arrow_sensitivity() {

        int num_qc_days = DailyQcUtils.qcDays;
        int pcp_flag = DailyQcUtils.pcp_flag;
        Button up_arrow = QcFreezeOptionsDialog.upTimeBtn;
        Button down_arrow = QcFreezeOptionsDialog.dnTimeBtn;

        /* 6 or 24 hour mode? */
        if (MPEDisplayManager.pcpn_time_step == 0) {
            if (pcp_flag + 1 >= num_qc_days * 4) {
                /* Grey out the down arrow. */
                down_arrow.setEnabled(false);
            } else {
                /* Make sure that the down arrow is available. */
                down_arrow.setEnabled(true);
            }

            /* determine the up arrow status */
            if (DailyQcUtils.curHr18_00 == 1) {
                if (pcp_flag - 3 <= 0) {
                    /* Grey out the up arrow. */
                    up_arrow.setEnabled(false);
                } else {
                    /* Make sure the up arrow is available. */
                    up_arrow.setEnabled(true);
                }
            }

            else if (DailyQcUtils.curHr00_06 == 1) {
                if (pcp_flag - 2 <= 0) {
                    /* Grey out the up arrow. */
                    up_arrow.setEnabled(false);
                } else {
                    /* Make sure the up arrow is available. */
                    up_arrow.setEnabled(true);
                }
            } else if (DailyQcUtils.curHr06_12 == 1) {
                if (pcp_flag - 1 <= 0) {
                    /* Grey out the up arrow. */
                    up_arrow.setEnabled(false);
                } else {
                    /* Make sure the up arrow is available. */
                    up_arrow.setEnabled(true);
                }
            } else {

                if (pcp_flag - 1 < 0) {
                    /* Grey out the up arrow. */
                    up_arrow.setEnabled(false);
                } else {
                    /* Make sure the up arrow is available. */
                    up_arrow.setEnabled(true);
                }
            }
        } else {

            if (pcp_flag + 4 >= num_qc_days * 4) {
                /* Grey out the down arrow. */
                down_arrow.setEnabled(false);
            } else {
                /* Make sure that the down arrow is available. */
                down_arrow.setEnabled(true);
            }

            if (pcp_flag - 4 < 0) {
                /* Grey out the up arrow. */
                up_arrow.setEnabled(false);
            } else {
                up_arrow.setEnabled(true);
            }
        }

    }

    public void chg_freeze_time(int data) {

        int i = 0;

        /* backward or forward */
        if (data == 0) {
            DailyQcUtils.pcp_flag--;
        } else if (data == 1) {
            DailyQcUtils.pcp_flag++;
        }

        if (DailyQcUtils.pcp_flag < 0) {
            DailyQcUtils.pcp_flag = 0;
        }

        if (DailyQcUtils.pcp_flag >= MAX_GAGEQC_DAYS * 4) {
            DailyQcUtils.pcp_flag = (MAX_GAGEQC_DAYS * 4) - 1;
        }

        DailyQcUtils.pcpn_day = DailyQcUtils.pcp_flag / 4;

        DailyQcUtils.pcpn_time = 3 - (DailyQcUtils.pcp_flag - DailyQcUtils.pcpn_day * 4);
        time_pos = 100 + DailyQcUtils.pcp_flag;

        QcFreezeOptionsDialog.dataSet.clear();
        QcFreezeOptionsDialog.dataSet.addAll(QcFreezeOptionsDialog.dataType);
        String[] a = new String[QcFreezeOptionsDialog.dataSet.size()];
        QcFreezeOptionsDialog.dataDispCbo
                .setItems(QcFreezeOptionsDialog.dataSet.toArray(a));

        if (DailyQcUtils.pcp_in_use[time_pos] == -1) {
            QcFreezeOptionsDialog.dataSet.clear();
            QcFreezeOptionsDialog.dataSet.add(0,
                    QcFreezeOptionsDialog.dataType.get(0));
            QcFreezeOptionsDialog.dataSet.add(1,
                    QcFreezeOptionsDialog.dataType.get(7));
            a = new String[QcFreezeOptionsDialog.dataSet.size()];
            QcFreezeOptionsDialog.dataDispCbo
                    .setItems(QcFreezeOptionsDialog.dataSet.toArray(a));
        }

        if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.pcp_in_use[time_pos] == -1) {
            i = 0;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == -1
                && DailyQcUtils.contour_flag == -1) {
            i = 0;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == 1 && DailyQcUtils.map_flag == -1) {
            i = 1;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == 1) {
            i = 2;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == 1 && DailyQcUtils.map_flag == -1) {
            i = 3;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == 1) {
            i = 4;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.contour_flag == 1) {
            i = 5;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.contour_flag == 1) {
            i = 6;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == -1) {
            i = 7;
        }

        QcFreezeOptionsDialog.dataDispCbo.select(i);

        if (DailyQcUtils.pcp_in_use[time_pos] == -1
                && DailyQcUtils.zdata[DailyQcUtils.pcpn_day].used[DailyQcUtils.pcpn_time] != 0) {
            QcFreezeOptionsDialog.renderGridsBtn.setEnabled(true);
        } else {
            QcFreezeOptionsDialog.renderGridsBtn.setEnabled(false);
        }

        Calendar tget = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        long offset = (DailyQcUtils.pcpn_day * 86400);
        tget.setTime(DailyQcUtils.btime.getTime());
        tget.add(Calendar.SECOND, (int) -offset);
        DailyQcUtils.isom = tget.get(Calendar.MONTH);

        /*
         * Set the sensitivity of the freeze time step arrows based on the
         * current selected time.
         */
        OtherPrecipOptions op = new OtherPrecipOptions();
        set_freeze_arrow_sensitivity();
        op.send_expose();
    }

    public void change_abmode(int data) {
        OtherPrecipOptions op = new OtherPrecipOptions();
        abmode = data;
        op.refresh_exposure();
    }

}
