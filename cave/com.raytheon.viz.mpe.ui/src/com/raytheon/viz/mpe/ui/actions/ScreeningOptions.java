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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.viz.mpe.util.BadTValues;
import com.raytheon.viz.mpe.util.BadValues;
import com.raytheon.viz.mpe.util.CheckConsistency;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;
import com.raytheon.viz.mpe.util.EstDailyStations;
import com.raytheon.viz.mpe.util.EstDailyTStations;
import com.raytheon.viz.mpe.util.EstPartStations;
import com.raytheon.viz.mpe.util.QCStations;
import com.raytheon.viz.mpe.util.QCTStations;
import com.raytheon.viz.mpe.util.ReadPrecipStationList;
import com.raytheon.viz.mpe.util.ReadTemperatureStationList;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ScreeningOptions {

    OtherPrecipOptions op = new OtherPrecipOptions();

    public void screening_Options(int client_data) {

        int pcpn_day = DailyQcUtils.pcpn_day;
        Pdata[] pdata = DailyQcUtils.pdata;
        Tdata[] tdata = DailyQcUtils.tdata;

        ReadPrecipStationList rp = new ReadPrecipStationList();
        ReadTemperatureStationList rt = new ReadTemperatureStationList();
        int num_temp_stations = rt.getNumTstations();
        int num_precip_stations = rp.getNumPstations();

        ArrayList<Station> precip_stations = DailyQcUtils.precip_stations;
        ArrayList<Station> temp_stations = DailyQcUtils.temperature_stations;
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        final Cursor prevCursor = shell.getCursor();
        final Cursor waitCursor = new Cursor(Display.getDefault(),
                SWT.CURSOR_WAIT);

        if (client_data == 0) {
            pdata[pcpn_day].stddev = 5.0f;
        } else if (client_data == 1) {
            pdata[pcpn_day].stddev = 3.0f;
        } else if (client_data == 2) {
            pdata[pcpn_day].stddev = 1.0f;
        }

        if (client_data == 0) {
            tdata[pcpn_day].stddev = 15.0f;
        } else if (client_data == 1) {
            tdata[pcpn_day].stddev = 10.0f;
        } else if (client_data == 2) {
            tdata[pcpn_day].stddev = 5.0f;
        }

        shell.setCursor(waitCursor);

        /*
         * do not estimate daily and partial point precipitation from each other
         * if run DQC on partial time frame and pcpn_day=0
         */

        if (pcpn_day == 0
                && (DailyQcUtils.curHr00_06 == 1
                        || DailyQcUtils.curHr06_12 == 1 || DailyQcUtils.curHr18_00 == 1)) {
        } else {
            EstDailyStations ed = new EstDailyStations();
            ed.estimate_daily_stations(pcpn_day, precip_stations,
                    num_precip_stations);

            EstPartStations ep = new EstPartStations();
            ep.estimate_partial_stations(pcpn_day, precip_stations,
                    num_precip_stations);
        }
        QCStations qcs = new QCStations();
        qcs.quality_control_stations(pcpn_day, precip_stations,
                num_precip_stations);

        CheckConsistency cc = new CheckConsistency();
        cc.check_consistency(pcpn_day, precip_stations, num_precip_stations);

        BadValues bv = new BadValues();
        bv.restore_bad_values(pcpn_day, precip_stations, num_precip_stations);

        EstDailyTStations edt = new EstDailyTStations();
        edt.estimate_daily_tstations(pcpn_day, temp_stations, num_temp_stations);

        QCTStations qct = new QCTStations();
        qct.quality_control_tstations(pcpn_day, temp_stations,
                num_temp_stations);

        BadTValues btv = new BadTValues();
        btv.restore_bad_tvalues(pcpn_day, temp_stations, num_temp_stations);

        shell.setCursor(prevCursor);
        op.refresh_exposure();
    }
}
