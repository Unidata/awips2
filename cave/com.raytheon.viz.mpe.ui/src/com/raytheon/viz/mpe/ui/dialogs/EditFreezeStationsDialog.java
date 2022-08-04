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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
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
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.ReadFreezingStationList;
import org.locationtech.jts.geom.Coordinate;

/**
 * Dialog box to Edit Freezing Stations in Daily QC.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 07, 2009           snaples   Initial creation
 * Sep 21, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4
 * Dec 15, 2017  6547     bkowal    Correctly recall the previous location of the dialog on the screen.
 * May 10, 2017  7131     mduff     Changed parent class and other cleanup.
 * Aug  6, 2018  7098     tgurney   Save dialog position when closing the dialog
 * Feb 06, 2019  7131     tgurney   Add user-friendly message if no stations found
 *
 * </pre>
 *
 * @author snaples
 */

public class EditFreezeStationsDialog extends AbstractEditStationsDialog {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EditFreezeStationsDialog.class);

    private static final String[] LB_NAMES = { "upper left", "upper right",
            "lower left", "lower right" };

    private static final String[] Q2B_NAMES = { "Manual", "Reset to Original" };

    private static final String[] Q45B_NAMES = { "Verified", "Calculated",
            "Bad" };

    private Font font;

    private String[] eval = new String[6];

    private int reset_value = 0;

    private int new_qual = 0;

    private int time_pos = 0;

    private int pcpn_time_step = MPEDisplayManager.pcpn_time_step;

    private int pcpn_time = DailyQcUtils.pcpn_time;

    private StringBuilder zstnData = new StringBuilder();

    private int initial_qual = 2;

    public Button[] qsbuttons;

    public Button[] lsbuttons;

    public Text editVal;

    protected boolean snow = false;

    private int isave = -1;

    private double maxdist = 9999;

    private java.util.List<Station> station = DailyQcUtils.freezing_stations;

    private ReadFreezingStationList rz = new ReadFreezingStationList();

    private int max_stations = rz.getNumZstations();

    private int initial_pos;

    private int pcpn_day = DailyQcUtils.pcpn_day;

    private static int hmflag = 0;

    private Coordinate coord = new Coordinate();

    private static Point savedDialogLocation = null;

    protected EditFreezeStationsDialog(Shell parentShell) {
        this(parentShell, null);
    }

    public EditFreezeStationsDialog(Shell parentShell,
            ReferencedCoordinate rcoord) {
        super(parentShell.getDisplay(), SWT.DIALOG_TRIM | SWT.MIN,
                CAVE.DO_NOT_BLOCK);
        setText("Edit Freeze Stations");
        if (rcoord != null) {
            try {
                coord = rcoord.asLatLon();
            } catch (Exception e) {
                statusHandler.error("Error transforming coordinate.", e);
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
        if (EditFreezeStationsDialog.savedDialogLocation != null) {
            this.getShell().setLocation(savedDialogLocation);
        }
    }

    @Override
    public boolean shouldClose() {
        EditFreezeStationsDialog.savedDialogLocation = shell.getLocation();
        return super.shouldClose();
    }

    @Override
    protected void disposed() {
        super.disposed();
        if (font != null) {
            font.dispose();
        }
    }

    @Override
    protected void initializeComponents(Shell shell) {
        for (int i = 0; i < max_stations; i++) {

            if (DailyQcUtils.zdata[pcpn_day].zstn[i].zlevel2[time_pos].data < 0) {
                continue;
            }

            float lat = station.get(i).lat;
            float lon = station.get(i).lon;

            Coordinate ll = new Coordinate();
            ll.x = lon;
            ll.y = lat;

            time_pos = DailyQcUtils.pcpn_time;
            ReferencedCoordinate rc = new ReferencedCoordinate(ll);
            Coordinate gridCell = null;
            try {
                gridCell = rc.asGridCell(HRAP.getInstance().getGridGeometry(),
                        PixelInCell.CELL_CORNER);
            } catch (Exception e) {
                statusHandler.error("Error transforming data.", e);
            }
            int x1 = (short) gridCell.x;
            int y1 = (short) gridCell.y;

            rc = new ReferencedCoordinate(coord);
            Coordinate hw = null;
            try {
                hw = rc.asGridCell(HRAP.getInstance().getGridGeometry(),
                        PixelInCell.CELL_CORNER);
            } catch (TransformException | FactoryException
                    | HrapConversionException e) {
                statusHandler.error(
                        "Error converting reference coordinate to grid cell.",
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
        initial_qual = DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].qual;
        new_qual = initial_qual;

        /*
         * Updated to allow editing of time distributed station as in OB 9.x if
         * (initial_qual == 6) {
         */
        zstnData.append(station.get(isave).hb5);
        zstnData.append(" ");
        zstnData.append(station.get(isave).parm);
        zstnData.append("\n");
        zstnData.append(station.get(isave).name);
        zstnData.append("\n");

        createTstationDataComp();
        createStnQualComp();
        createStnLocComp();
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
        hb5Lbl.setText(zstnData.toString());
        hb5Lbl.setLayoutData(gd);

        String mbuf = "";
        editVal = new Text(dataComp, SWT.LEFT | SWT.SINGLE | SWT.BORDER);
        if (DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].data < 0) {
            mbuf = "M";
            editVal.setText(mbuf);
        } else {
            mbuf = String.format("%5.2f",
                    DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].data);
            editVal.setText(mbuf.trim());

        }
        editVal.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                String ev = editVal.getText();
                eval[time_pos] = ev;
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
        // GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite stnQualComp = new Composite(stnQualGroup, SWT.NONE);
        GridLayout stnQualCompLayout = new GridLayout(2, true);
        stnQualCompLayout.marginWidth = 0;
        stnQualCompLayout.marginHeight = 0;
        stnQualComp.setLayout(stnQualCompLayout);
        stnQualComp.setLayoutData(gd);
        int naflag = 0;
        if (initial_qual < 0
                || DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].data < 0) {
            naflag = 1;
        }

        if (initial_qual == 2) {
            qsbuttons = new Button[2];
            for (int i = 0; i < qsbuttons.length; i++) {
                final Button b = new Button(stnQualComp, SWT.RADIO);
                b.setText(Q2B_NAMES[i]);
                b.setData(i);
                b.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        resetStationQuality((Integer) b.getData());
                    }
                });
                qsbuttons[i] = b;
            }
            qsbuttons[0].setSelection(true);

        }

        qsbuttons = new Button[3];
        for (int i = 0; i < qsbuttons.length; i++) {
            final Button b = new Button(stnQualComp, SWT.RADIO);
            b.setText(Q45B_NAMES[i]);
            b.setData(i);

            /*
             * If the station value is calculated, then only show the calculated
             * toggle button.
             */
            if (initial_qual == 5 && i != 1) {
                continue;
            }

            /*
             * If the station is not estimated do not show the estimated toggle
             * button.
             */
            if (initial_qual != 5 && i == 1) {
                continue;
            }

            /* Set the verified button. */
            if (i == 0 && initial_qual == 8 && naflag != 1) {
                b.setEnabled(true);
            } else if (i == 1 && initial_qual == 5 && naflag != 1) {
                b.setEnabled(true);
            } else if (i == 2 && initial_qual == 1 && naflag != 1) {
                b.setEnabled(true);
            } else {
                b.setEnabled(false);
            }
            if (naflag != 1) {
                b.setSelection(true);
            } else {
                b.setSelection(false);
            }

            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    changeStationQuality((Integer) b.getData());
                }
            });
            qsbuttons[i] = b;
        }

    }

    /**
     * Create the data options group and controls.
     */
    private void createStnLocComp() {

        if (station.get(isave).xadd == -1 && station.get(isave).yadd == -1) {
            initial_pos = 0;
        } else if (station.get(isave).xadd == 0
                && station.get(isave).yadd == -1) {
            initial_pos = 2;
        } else if (station.get(isave).xadd == -1
                && station.get(isave).yadd == 0) {
            initial_pos = 1;
        } else if (station.get(isave).xadd == 0
                && station.get(isave).yadd == 0) {
            initial_pos = 3;
        }
        Group stnLocGroup = new Group(shell, SWT.NONE);
        stnLocGroup.setText(" Station Location ");
        GridLayout gl = new GridLayout(1, false);
        stnLocGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stnLocGroup.setLayoutData(gd);

        // Create a container to hold the label and the combo box.
        // GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
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
                changeCustomFile(isave);
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
    }

    protected void resetStationQuality(Integer data) {
        int k;

        time_pos = pcpn_time;

        if (data == 1) {

            for (k = 0; k < 5; k++) {

                DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[k].qual = DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel1[k].qual;

                DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[k].data = DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel1[k].data;

            }

            reset_value = 1;
            new_qual = DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel1[time_pos].qual;

        } else {
            reset_value = 0;
        }

    }

    protected void changeStationQuality(Integer data) {
        // logMessage ("thru station_quality %d\n", (int) data);
        hmflag++;
        if (hmflag == 1) {
            return;
        }

        hmflag = 0;

        if (data == 0) {
            new_qual = 8;
        } else if (data == 1) {
            new_qual = 5;
        } else if (data == 2) {
            new_qual = 1;
        }
    }

    protected void changeStationLocation(Integer data) {
        if (data == 0) {
            station.get(isave).xadd = -1;
            station.get(isave).yadd = -1;
        }

        else if (data == 2) {
            station.get(isave).xadd = 0;
            station.get(isave).yadd = -1;
        }

        else if (data == 1) {
            station.get(isave).xadd = -1;
            station.get(isave).yadd = 0;
        }

        else if (data == 3) {
            station.get(isave).xadd = 0;
            station.get(isave).yadd = 0;
        }

        return;
    }

    protected void changeCustomFile(int data) {

        String pathName = getStationListPath(DailyQcUtils.currentQcArea);
        String zstation_list_custom_file = pathName + "_label_position";
        int i;
        int time_pos = 0;
        float val = 0;
        int idif;
        String cstr;
        int k, p;
        int[] pcp_in_use = DailyQcUtils.pcp_in_use;
        Button rpbutton = QcFreezeOptionsDialog.renderGridsBtn;

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
                new FileWriter(zstation_list_custom_file))) {
            for (i = 0; i < max_stations; i++) {
                String rec = String.format("%s %s %d %d\n", station.get(i).hb5,
                        station.get(i).parm, station.get(i).xadd,
                        station.get(i).yadd);
                out.write(rec);
            }
            out.close();
        } catch (IOException e) {
            statusHandler.error(
                    "Error writing file " + zstation_list_custom_file, e);
            return;
        }

        cstr = editVal.getText();
        p = cstr.indexOf('M');
        if (p == -1) {
            val = Float.parseFloat(cstr);
        }
        cstr = null;

        /* use manually entered data */

        idif = (int) Math.abs(val
                - DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].data);

        if (idif > .01 && p == -1 && reset_value == 0) {

            DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].data = val;
            DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].qual = 2;

        }

        else {

            DailyQcUtils.zdata[pcpn_day].zstn[isave].zlevel2[time_pos].qual = (short) new_qual;

        }

        /*
         * If there is edit for freezing level, force to go back to Point and
         * Render Grids+MAPS for all 4 periods in full day
         */
        for (k = 0; k < 4; k++) {
            time_pos = 100 + pcpn_day * 4 + k;
            pcp_in_use[time_pos] = -1;
        }

        for (k = 0; k < 5; k++) {
            if (DailyQcUtils.zdata[pcpn_day].used[k] != 0) {
                DailyQcUtils.zdata[pcpn_day].used[k] = 2;
            }
        }

        QcFreezeOptionsDialog.dataSet.clear();
        QcFreezeOptionsDialog.dataSet.add(0,
                QcFreezeOptionsDialog.dataType.get(0));
        QcFreezeOptionsDialog.dataSet.add(1,
                QcFreezeOptionsDialog.dataType.get(7));
        String[] a = new String[QcFreezeOptionsDialog.dataSet.size()];
        QcFreezeOptionsDialog.dataDispCbo
                .setItems(QcFreezeOptionsDialog.dataSet.toArray(a));

        time_pos = 100 + pcp_flag;

        if (points_flag == 1 && pcp_in_use[time_pos] == -1) {
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

        QcFreezeOptionsDialog.dataDispCbo.select(k);

        rpbutton.setEnabled(true);

        OtherPrecipOptions op = new OtherPrecipOptions();
        op.send_expose();
        return;

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
