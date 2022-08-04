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
package com.raytheon.viz.mpe.ui.dialogs;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.viz.app.launcher.handlers.AppLauncherHandler;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.GetClimateSource;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.util.BadTValues;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.EstDailyTStations;
import com.raytheon.viz.mpe.util.QCTStations;
import org.locationtech.jts.geom.Coordinate;

/**
 * Dialog box to Edit Temperature Stations in Daily QC.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 13, 2009           snaples   Initial creation
 * Dec 08, 2015  5179     bkowal    Ensure the grid remains displayed when this
 *                                  dialog is closed. Eliminate warnings and e
 *                                  print stack trace.
 * Feb 22, 2016  18599    snaples   Removed send_expose call in
 *                                  changeCustomFile.
 * Sep 21, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4
 * Oct 27, 2016  5969     randerso  Add support for locating hydroapps on the
 *                                  correct monitor
 * Mar 02, 2017  6147     dgilling  Code cleanup.
 * Jun 29, 2017  6147     bkowal    Fix the order of the quality code labels.
 * Nov 16, 2017  6525     bkowal    Ensure the display is refreshed after "Apply" is clicked.
 * Dec 15, 2017  6547     bkowal    Correctly recall the previous location of the dialog on the screen.
 * Jan 24, 2017  6547     bkowal    Immediately refresh display after changes are Applied.
 * May 10, 2017  7131     mduff     Changed parent class and other cleanup.
 * Aug  6, 2018  7098     tgurney   Save dialog position when closing the dialog
 * Feb 06, 2019  7131     tgurney   Add user-friendly message if no stations found
 *
 * </pre>
 *
 * @author snaples
 */

public class EditTempStationsDialog extends AbstractEditStationsDialog {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditTempStationsDialog.class);

    private static final String[] STATION_QUAL_VALUES_1 = { "Manual",
            "Reset to Original" };

    private static final String[] STATION_QUAL_VALUES_2 = { "Verified",
            "Questionable", "Screened (Forced)", "Bad" };

    private static final String[] LB_NAMES = { "upper left", "upper right",
            "lower left", "lower right" };

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private String[] eval = new String[6];

    private int reset_value = 0;

    private int new_qual = 0;

    private Text[] sv = new Text[6];

    private Label[] sc = new Label[6];

    private int time_pos = 0;

    private int pcpn_time_step = MPEDisplayManager.pcpn_time_step;

    private int pcpn_time = DailyQcUtils.pcpn_time;

    private StringBuilder tstnData = new StringBuilder();

    protected Button snotel;

    private int initial_qual = 2;

    public Button[] lsbuttons;

    public Text editVal;

    protected boolean snow = false;

    private int isave = -1;

    private double maxdist = 9999;

    private int tsmax = DailyQcUtils.tsmax;

    private int isom = DailyQcUtils.isom;

    private List<Station> tempStationList = DailyQcUtils.temperature_stations;

    private int max_stations = tempStationList.size();

    private int initial_pos;

    private int pcpn_day = DailyQcUtils.pcpn_day;

    private Coordinate coord = new Coordinate();

    private static Point savedDialogLocation = null;

    public EditTempStationsDialog(Shell parentShell,
            ReferencedCoordinate rcoord) {
        super(parentShell.getDisplay(), SWT.DIALOG_TRIM | SWT.MIN,
                CAVE.DO_NOT_BLOCK);
        setText("Edit Temperature Stations");
        if (rcoord != null) {
            try {
                coord = rcoord.asLatLon();
            } catch (Exception e) {
                statusHandler.error(
                        "Failed to convert ReferencedCoordinate to Coordinate.",
                        e);
            }
        }
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        if (EditTempStationsDialog.savedDialogLocation != null) {
            this.getShell().setLocation(savedDialogLocation);
        }
    }

    @Override
    public boolean shouldClose() {
        EditTempStationsDialog.savedDialogLocation = shell.getLocation();
        return super.shouldClose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else if (pcpn_time_step == 1) {
            time_pos = 4;
        } else if (pcpn_time_step == 2) {
            time_pos = 5;
        }

        int m = 0;
        for (int i = 0; i < max_stations; i++) {

            if (DailyQcUtils.tdata[pcpn_day].tstn[i].tlevel2[time_pos].data == -999) {
                continue;
            }

            if ((DailyQcUtils.tdata[pcpn_day].tstn[i].tlevel2[time_pos].data > QcTempOptionsDialog
                    .getPointFilterReverseValue())
                    && (DailyQcUtils.tdata[pcpn_day].tstn[i].tlevel2[time_pos].data < 110.0)) {
                continue;
            }

            if ((tempStationList.get(i).elev > 0)
                    && (tempStationList.get(i).elev < dqc
                            .getPointElevationFilterValue())) {
                continue;
            }
            if (DailyQcUtils.tdata[pcpn_day].tstn[i].tlevel2[time_pos].data < QcTempOptionsDialog
                    .getPointFilterValue()) {
                continue;
            }

            float lat = tempStationList.get(i).lat;
            float lon = tempStationList.get(i).lon;

            for (m = 0; m < tsmax; m++) {
                char kd = tempStationList.get(i).parm.charAt(4);
                if ((kd == DailyQcUtils.ts[m].abr.charAt(1)
                        && DailyQcUtils.dflag[m + 1] == 1)) {
                    break;
                }
            }

            if (m == tsmax) {
                continue;
            }

            for (m = 0; m < 9; m++) {
                if (m == DailyQcUtils.tdata[pcpn_day].tstn[i].tlevel2[time_pos].qual
                        && DailyQcUtils.qflag[m] == 1) {
                    break;
                }
            }

            if (m == 9) {
                continue;
            }

            Coordinate ll = new Coordinate();
            ll.x = lon;
            ll.y = lat;

            ReferencedCoordinate rc = new ReferencedCoordinate(ll);
            Coordinate gridCell = null;
            try {
                gridCell = rc.asGridCell(HRAP.getInstance().getGridGeometry(),
                        PixelInCell.CELL_CORNER);
            } catch (Exception e) {
                statusHandler.error(
                        "Failed to convert ReferencedCoordinate to Coordinate.",
                        e);
            }
            int x1 = (short) gridCell.x;
            int y1 = (short) gridCell.y;

            rc = new ReferencedCoordinate(coord);
            Coordinate hw = null;
            try {
                hw = rc.asGridCell(HRAP.getInstance().getGridGeometry(),
                        PixelInCell.CELL_CORNER);
            } catch (Exception e) {
                statusHandler.error(
                        "Failed to convert ReferencedCoordinate to Coordinate.",
                        e);
            }
            int win_x = (int) hw.x;
            int win_y = (int) hw.y;

            double testdist = Math.pow((win_x - (float) x1), 2)
                    + Math.pow((win_y - (float) y1), 2);
            testdist = Math.pow(testdist, .5);

            if (testdist < maxdist) {
                isave = i;
                maxdist = testdist;
            }
        }
        if (isave == -1) {
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Composite labelComp = new Composite(shell, SWT.NONE);
            GridLayout dataCompLayout = new GridLayout(1, false);
            labelComp.setLayout(dataCompLayout);
            labelComp.setLayoutData(gd);
            new Label(labelComp, SWT.NONE).setText("No stations found.");
            return;
        }

        reset_value = 0;
        initial_qual = DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].qual;
        new_qual = initial_qual;

        tstnData.append(tempStationList.get(isave).hb5);
        tstnData.append(" ");
        tstnData.append(tempStationList.get(isave).parm);
        tstnData.append("\n");
        tstnData.append(tempStationList.get(isave).name);
        tstnData.append("\n");
        tstnData.append(String.format("%d", tempStationList.get(isave).elev));
        tstnData.append(" ft    ");
        tstnData.append("\n");
        tstnData.append(String.format("Lat: %5.2f Lon: %5.2f",
                tempStationList.get(isave).lat,
                tempStationList.get(isave).lon));
        tstnData.append("\n");
        if (tempStationList.get(isave).max[isom] > -99) {
            GetClimateSource gc = new GetClimateSource();
            String tClimateSource = gc
                    .getClimateSource(tempStationList.get(isave).cparm);

            tstnData.append(String.format(
                    "monthly average high %5.1f low %5.1f source: %s\n",
                    tempStationList.get(isave).max[isom],
                    tempStationList.get(isave).min[isom], tClimateSource));
        }
        if (DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].data > -50) {
            tstnData.append(String.format("estimate %d ",
                    DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].estimate));
        }

        createTstationDataComp();
        createStnQualComp();
        createStnLocComp();
        createStnConComp();
        createButtonComp();
    }

    /**
     * Create the data options group and controls.
     */
    private void createTstationDataComp() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite dataComp = new Composite(shell, SWT.NONE);
        GridLayout dataCompLayout = new GridLayout(1, false);
        dataComp.setLayout(dataCompLayout);
        dataComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label hb5Lbl = new Label(dataComp, SWT.LEFT);
        hb5Lbl.setText(tstnData.toString());
        hb5Lbl.setLayoutData(gd);

        editVal = new Text(dataComp, SWT.LEFT | SWT.SINGLE | SWT.BORDER);
        String mbuf = null;
        if (DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].data < -50) {
            mbuf = "M";
            editVal.setText(mbuf);
        } else {
            mbuf = String.format("%d",
                    (int) DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].data);
            editVal.setText(mbuf.trim());

        }
        editVal.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                String ev = editVal.getText();
                eval[time_pos] = ev;
                sv[time_pos].setText(eval[time_pos]);
            }
        });
    }

    /**
     * Create the data options group and controls.
     */
    private void createStnQualComp() {

        Group stnQualGroup = new Group(shell, SWT.NONE);
        stnQualGroup.setText(" Station quality ");
        GridLayout gl = new GridLayout(1, false);
        stnQualGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnQualGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        Composite stnQualComp = new Composite(stnQualGroup, SWT.NONE);
        GridLayout stnQualCompLayout = new GridLayout(2, true);
        stnQualCompLayout.marginWidth = 0;
        stnQualCompLayout.marginHeight = 0;
        stnQualComp.setLayout(stnQualCompLayout);
        stnQualComp.setLayoutData(gd);

        boolean naflag = false;
        if (initial_qual < 0
                || DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].data < -500) {
            naflag = true;
        }

        if (initial_qual == 2) {
            for (int i = 0; i < STATION_QUAL_VALUES_1.length; i++) {
                Button b = new Button(stnQualComp, SWT.RADIO);
                b.setText(STATION_QUAL_VALUES_1[i]);
                b.setData(Integer.valueOf(i));
                if (i == 0) {
                    b.setSelection(true);
                }
                b.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button source = (Button) e.getSource();
                        if (source.getSelection()) {
                            resetStationQuality((Integer) source.getData());
                        }
                    }
                });
            }
        } else if (initial_qual != 5) {
            for (int i = 0; i < STATION_QUAL_VALUES_2.length; i++) {
                Button b = new Button(stnQualComp, SWT.RADIO);
                b.setText(STATION_QUAL_VALUES_2[i]);
                b.setData(Integer.valueOf(i));
                if (dqc.func[i] == initial_qual && !naflag) {
                    b.setSelection(true);
                } else {
                    b.setSelection(false);
                }
                if (naflag) {
                    b.setEnabled(false);
                }
                if ((initial_qual == 3) && (i == 0)) {
                    b.setEnabled(false);
                }

                b.addSelectionListener(new SelectionAdapter() {

                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        Button source = (Button) e.getSource();
                        if (source.getSelection()) {
                            changeStationQuality((Integer) source.getData());
                        }
                    }

                });
            }
        }
    }

    /**
     * Create the data options group and controls.
     */
    private void createStnLocComp() {

        if (tempStationList.get(isave).xadd == -1
                && tempStationList.get(isave).yadd == -1) {
            initial_pos = 0;
        } else if (tempStationList.get(isave).xadd == 0
                && tempStationList.get(isave).yadd == -1) {
            initial_pos = 2;
        } else if (tempStationList.get(isave).xadd == -1
                && tempStationList.get(isave).yadd == 0) {
            initial_pos = 1;
        } else if (tempStationList.get(isave).xadd == 0
                && tempStationList.get(isave).yadd == 0) {
            initial_pos = 3;
        }
        Group stnLocGroup = new Group(shell, SWT.NONE);
        stnLocGroup.setText(" Station Location ");
        GridLayout gl = new GridLayout(1, false);
        stnLocGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnLocGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        Composite stnLocComp = new Composite(stnLocGroup, SWT.NONE);
        GridLayout stnLocCompLayout = new GridLayout(2, true);
        stnLocCompLayout.marginWidth = 0;
        stnLocCompLayout.marginHeight = 0;
        stnLocComp.setLayout(stnLocCompLayout);
        stnLocComp.setLayoutData(gd);

        lsbuttons = new Button[4];
        for (int i = 0; i < lsbuttons.length; i++) {
            final Button b = new Button(stnLocComp, SWT.RADIO);
            b.setText(LB_NAMES[i]);
            b.setData(i);
            if (i == initial_pos) {
                b.setSelection(true);
            } else {
                b.setSelection(false);
            }
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    changeStationLocation((Integer) b.getData());
                }
            });
            lsbuttons[i] = b;
        }

    }

    /**
     * Create the data options group and controls.
     */
    private void createStnConComp() {

        Group stnConGroup = new Group(shell, SWT.NONE);
        stnConGroup.setText(" Station Consistency ");
        GridLayout gl = new GridLayout(1, false);
        stnConGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnConGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        Composite stnConComp = new Composite(stnConGroup, SWT.NONE);
        GridLayout stnConCompLayout = new GridLayout(2, true);
        stnConCompLayout.marginWidth = 5;
        stnConCompLayout.marginHeight = 3;
        stnConComp.setLayout(stnConCompLayout);
        stnConComp.setLayoutData(gd);

        for (int i = 0; i < 6; i++) {

            String muf;
            final int ii;
            sc[i] = new Label(stnConComp, SWT.LEFT);
            sc[i].setText(dqc.T_TIME_FILE[dqc.dqcTimeStringIndex][i]);
            sv[i] = new Text(stnConComp, SWT.LEFT | SWT.BORDER);
            if (DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[i].data < -99) {
                muf = "M";
                sv[i].setText(muf);
            } else {
                muf = String.format("%d",
                        (int) DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[i].data);
                sv[i].setText(muf.trim());
            }
            eval[i] = sv[i].getText();
            ii = i;
            sv[i].addModifyListener(new ModifyListener() {
                @Override
                public void modifyText(ModifyEvent e) {
                    eval[ii] = sv[ii].getText();
                }
            });
        }

    }

    private void createButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnGpComp = new Composite(shell, SWT.NONE);
        GridLayout btnGpCompLayout = new GridLayout(3, false);
        btnGpComp.setLayout(btnGpCompLayout);
        btnGpComp.setLayoutData(gd);

        GridData bd = new GridData(110, 25);
        Button applyBtn = new Button(btnGpComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(bd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleApplyAction();
            }
        });

        Button closeBtn = new Button(btnGpComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(bd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
        Button graphBtn = new Button(btnGpComp, SWT.PUSH);
        graphBtn.setText("Graph");
        graphBtn.setLayoutData(bd);
        graphBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                AppLauncherHandler alh = new AppLauncherHandler();
                String lid = tempStationList.get(isave).hb5;
                String dataType = tempStationList.get(isave).parm.trim();
                char[] dataTypeN = dataType.toCharArray();

                /*
                 * For temperature, use the shef extremum code 'X' for the daily
                 * maximum temperature, 'N' for the daily minimum temperature
                 * and 'Z' for the regular hourly temperature. Note if 6hr mode
                 * is selected, still display hourly temperature timeseries
                 * since currently pdc_pp does not generate the 6hr temperature
                 * timeseries, also user still can figure out 6hr temperature
                 * from the hourly temperature
                 */

                if (pcpn_time_step == 0) {
                    /* hourly temperature */
                    dataTypeN[5] = 'Z';
                } else if (pcpn_time_step == 1) {
                    /* maximum temperature */
                    dataTypeN[5] = 'X';
                } else {
                    /* minimum temperature */
                    dataTypeN[5] = 'N';
                }

                dataType = new String(dataTypeN);

                final String TSL_BUNDLE_LOC = "bundles/run-TimeSeriesLite.xml";
                try {
                    statusHandler.info("Launching TSL " + lid + ", "
                            + dataType  + " ...");
                    alh.execute(getShell(), TSL_BUNDLE_LOC, lid,
                            dataType );
                } catch (ExecutionException e1) {
                    statusHandler.error("Failed to launch TSL " + lid + ", "
                            + dataType + ".", e1);
                }
            }
        });
    }

    private void handleApplyAction() {
        changeCustomFile(isave);
        new OtherPrecipOptions().refresh_exposure();
    }

    protected void resetStationQuality(Integer data) {
        int k;

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else if (pcpn_time_step == 1) {
            time_pos = 4;
        } else if (pcpn_time_step == 2) {
            time_pos = 5;
        }

        if (data == 1) {

            for (k = 0; k < 6; k++) {

                DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].qual = DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel1[k].qual;

                DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].data = DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel1[k].data;

            }

            reset_value = 1;
            new_qual = DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel1[time_pos].qual;

        } else {
            reset_value = 0;
        }

    }

    protected void changeStationQuality(Integer data) {
        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else {
            time_pos = 4;
        }

        new_qual = dqc.func[data];
    }

    protected void changeStationLocation(Integer data) {
        if (data == 0) {
            tempStationList.get(isave).xadd = -1;
            tempStationList.get(isave).yadd = -1;
        }

        else if (data == 2) {
            tempStationList.get(isave).xadd = 0;
            tempStationList.get(isave).yadd = -1;
        }

        else if (data == 1) {
            tempStationList.get(isave).xadd = -1;
            tempStationList.get(isave).yadd = 0;
        }

        else if (data == 3) {
            tempStationList.get(isave).xadd = 0;
            tempStationList.get(isave).yadd = 0;
        }

        return;
    }

    protected void changeCustomFile(int data) {

        String pathName = getStationListPath(DailyQcUtils.currentQcArea);
        String tstation_list_custom_file = pathName + "_label_position";
        int time_pos = 0;
        float val = 0;
        int idif;
        String cstr;
        Button rpbutton = QcTempOptionsDialog.renderGridsBtn;

        int pcp_flag = DailyQcUtils.pcp_flag;
        int grids_flag = DailyQcUtils.grids_flag;
        int points_flag = DailyQcUtils.points_flag;
        int map_flag = DailyQcUtils.map_flag;

        if (pcpn_time_step == 0) {
            time_pos = pcpn_time;
        } else if (pcpn_time_step == 1) {
            time_pos = 4;
        } else if (pcpn_time_step == 2) {
            time_pos = 5;
        }

        try (BufferedWriter out = new BufferedWriter(
                new FileWriter(tstation_list_custom_file))) {
            for (int i = 0; i < max_stations; i++) {
                String rec = String.format("%s %s %d %d\n",
                        tempStationList.get(i).hb5, tempStationList.get(i).parm,
                        tempStationList.get(i).xadd,
                        tempStationList.get(i).yadd);
                out.write(rec);
            }
            out.close();
        } catch (IOException e) {
            statusHandler.error(
                    "Failed to write file: " + tstation_list_custom_file + ".",
                    e);
            return;
        }

        cstr = editVal.getText();
        int p = cstr.indexOf('M');
        if (p == -1) {
            val = Float.parseFloat(cstr);
        }
        cstr = null;

        /* use manually entered data */

        idif = (int) Math.abs(val
                - DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].data);

        if (idif > 1 && p == -1 && reset_value == 0) {

            DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].data = val;
            DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].qual = 2;

        }

        else {

            DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[time_pos].qual = (short) new_qual;

        }

        /*
         * Add logic to read consistency fields and update the other temperature
         * fields if necessary here.
         */
        for (int k = 0; k < 6; k++) {

            if (k != time_pos) {
                cstr = eval[k];
                val = 0;
                p = cstr.indexOf('M');
                if (p == -1) {
                    val = Float.parseFloat(cstr);
                }

                idif = (int) Math.abs(val
                        - DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].data);

                if (p != -1) {
                    DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].data = -99;
                    DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].qual = -99;
                } else {
                    if (idif > 1) {
                        DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].data = val;
                        DailyQcUtils.tdata[pcpn_day].tstn[isave].tlevel2[k].qual = 2;
                    }
                }
                cstr = null;
            }
        }
        if (DailyQcUtils.tdata[pcpn_day].used[time_pos] != 0) {
            DailyQcUtils.tdata[pcpn_day].used[time_pos] = 2;
        }

        if (pcpn_time_step == 0) {
            time_pos = 150 + pcp_flag;
        } else if (pcpn_time_step == 1) {
            time_pos = 190 + pcpn_day;
        } else if (pcpn_time_step == 2) {
            time_pos = 200 + pcpn_day;
        }

        for (int k = 0; k < 4; k++) {

            time_pos = 150 + pcpn_day * 4 + k;

            DailyQcUtils.pcp_in_use[time_pos] = -1;

            if (DailyQcUtils.tdata[pcpn_day].used[k] != 0) {
                DailyQcUtils.tdata[pcpn_day].used[k] = 2;
            }
        }

        QcTempOptionsDialog.dataSet.clear();
        QcTempOptionsDialog.dataSet.add(0, QcTempOptionsDialog.dataType.get(0));
        QcTempOptionsDialog.dataSet.add(1, QcTempOptionsDialog.dataType.get(7));
        String[] a = new String[QcTempOptionsDialog.dataSet.size()];
        QcTempOptionsDialog.dataDispCbo
                .setItems(QcTempOptionsDialog.dataSet.toArray(a));

        if (pcpn_time_step == 0) {
            time_pos = 150 + pcp_flag;
        } else if (pcpn_time_step == 1) {
            time_pos = 190 + pcpn_day;
        } else if (pcpn_time_step == 2) {
            time_pos = 200 + pcpn_day;
        }

        int k = 0;
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

        rpbutton.setEnabled(true);

        BadTValues bv = new BadTValues();
        bv.update_bad_tvalues(pcpn_day);

        EstDailyTStations eds = new EstDailyTStations();
        eds.estimate_daily_tstations(pcpn_day, tempStationList, max_stations);

        QCTStations qcs = new QCTStations();
        qcs.quality_control_tstations(pcpn_day, tempStationList, max_stations);

        bv.restore_bad_tvalues(pcpn_day, tempStationList, max_stations);

        new OtherPrecipOptions().refresh_exposure();
    }

    private String getStationListPath(String qcArea) {
        String station_dir = DailyQcUtils.mpe_station_list_dir;
        String dir;

        if (qcArea != null) {
            if (station_dir.length() > 0) {
                dir = station_dir + "/" + qcArea + "_tstation_list";
            } else {
                dir = qcArea;
            }
        } else {
            if (station_dir.length() > 0) {
                dir = station_dir + "/" + qcArea + "_tstation_list";
            } else {
                dir = qcArea;
            }
        }
        return dir;
    }
}
