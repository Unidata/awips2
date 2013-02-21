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

import java.util.ArrayList;
import java.util.Date;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.mpe.ui.actions.GageQcSelect;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.ui.actions.OtherTempOptions;
import com.raytheon.viz.mpe.ui.actions.SaveLevel2Data;
import com.raytheon.viz.mpe.ui.actions.ScreeningOptions;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Ts;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2008            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class QcTempOptionsDialog extends AbstractMPEDialog {

    public static Combo maxminTimeCbo;

    public static Combo dataDispCbo;

    /**
     * Font used for controls.
     */
    private Font font;

    public static Button upTimeBtn;

    public static Button dnTimeBtn;

    public static Button renderGridsBtn;

    public static Button groupEditBtn;

    public static Combo pcpTypeCbo;

    private Button nexChk;

    private Button allChk;

    public static Combo pntDispCbo;

    public static Combo pntScnCbo;

    private static Scale pntFilter;

    private static Scale pntRevFilter;

    private static Scale pntElFilter;

    private Label pfvalueLabel;

    private Label prvalueLabel;

    private Label pevalueLabel;

    public static boolean isOpen = false;

    private static boolean isfinished = true;

    private int dqc_good = 0;

    public static DrawDQCStations ddqc;

    public static ArrayList<String> dataType = new ArrayList<String>();

    public static ArrayList<String> dataSet = new ArrayList<String>();

    OtherPrecipOptions opo = new OtherPrecipOptions();

    OtherTempOptions oto = new OtherTempOptions();

    Tdata[] tdata = new Tdata[0];

    Ts[] ts;

    private int time_pos;

    public static Button[] tsbuttons = null;

    public static Button[] qsbuttons = new Button[10];

    final GageQcSelect gq = new GageQcSelect();

    int i = 0;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public QcTempOptionsDialog(Shell parent) {
        super(parent);
    }

    private int getOpts() {
        int ik = 0;
        if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.pcp_in_use[time_pos] == -1) {
            ik = 0;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == -1
                && DailyQcUtils.contour_flag == -1) {
            ik = 0;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == 1 && DailyQcUtils.map_flag == -1) {
            ik = 1;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == 1) {
            ik = 2;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == 1 && DailyQcUtils.map_flag == -1) {
            ik = 3;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == 1) {
            ik = 4;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.contour_flag == 1) {
            ik = 5;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.contour_flag == 1) {
            ik = 6;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == -1 && DailyQcUtils.map_flag == -1) {
            ik = 7;
        }
        return ik;

    }

    /**
     * Open method used to display the Point data Control dialog.
     * 
     * @return Null.
     */
    public Object open() {
        Shell parent = this.getParent();
        Display display = parent.getDisplay();
        MPEDisplayManager displayMgr = MPEDisplayManager.getCurrent();
        Date prevDate = displayMgr.getCurrentEditDate();
        Date currDate = ChooseDataPeriodDialog.prevDate;
        String QcArea = ChooseDataPeriodDialog.prevArea;
        AppsDefaults appDefaults = AppsDefaults.getInstance();
        DisplayFieldData df = displayMgr.getDisplayFieldType();
        if (currDate == null) {
            currDate = prevDate;
        }
        if (QcArea == null) {
            QcArea = appDefaults.getToken("mpe_site_id");
        }
        int qcDays = displayMgr.getDqcDays();
        // checks to see if area or date has changed since last data load
        DailyQcUtils dqcu = new DailyQcUtils();
        // reloads data if changed
        // returns 0 for failed, 1 for new area, 2 for Ok
        dqc_good = dqcu.qcDataHasChanged(prevDate, currDate, QcArea, qcDays,
                false);
        if (dqc_good == 1) {
            SaveLevel2Data s2 = new SaveLevel2Data();
            dqc_good = s2.check_new_area(currDate, QcArea, qcDays);
            if (dqc_good == 0) {
                dqc_good = dqcu.qcDataReload(currDate, QcArea, qcDays, false);
            }
        }
        if (dqc_good == 0) {
            return 0;
        }
        dataType.clear();
        dataType.add("Points");
        dataType.add("Grids");
        dataType.add("MATs");
        dataType.add("Points+Grids");
        dataType.add("Points+MATs");
        dataType.add("Contours");
        dataType.add("Points+Contours");
        dataType.add("None");
        dataSet.addAll(dataType);
        if (displayMgr.isQpf()) {
            QcPrecipOptionsDialog.destroy(false);
            displayMgr.setQpf(false);
        }
        if (displayMgr.isZflag()) {
            QcFreezeOptionsDialog.destroy(false);
        }

        displayMgr.setMaxmin(true);
        ddqc = DrawDQCStations.getInstance();

        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS);

        shell.setText("QC Temperature Options");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);
        if (MPEDisplayManager.pcpn_time_step != 0) {
            MPEDisplayManager.pcpn_time_step = 0;
        }
        // Initialize all of the controls and layouts
        this.initializeComponents();

        shell.pack();

        shell.open();
        displayMgr.setMaxmin(true);
        isOpen = true;
        isfinished = false;
        oto.chg_maxmin_time(maxminTimeCbo.getSelectionIndex() + 2);
        opo.send_expose();
        while (!shell.isDisposed()) {
            if (dqc_good == 0) {
                displayMgr.setMaxmin(false);
                isOpen = false;
                ddqc.destroy();
                shell.dispose();
            }
            if (isOpen == false) {
                displayMgr.setMaxmin(false);
                ddqc.destroy();
                shell.dispose();
            }
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        ddqc.destroy();
        displayMgr.setMaxmin(false);
        isfinished = true;
        isOpen = false;
        font.dispose();
        SaveLevel2Data s2 = new SaveLevel2Data();
        s2.send_dbase_new_area();
        DailyQcUtils dc = new DailyQcUtils();
        dc.clearData();
        displayMgr.displayFieldData(df);
        removePerspectiveListener();
        if (MPEDisplayManager.getCurrent() != null) {
            display.asyncExec(new Runnable() {
                public void run() {
                    ChooseDataPeriodDialog dialog = new ChooseDataPeriodDialog(
                            getParent().getShell());
                    dialog.open();
                }
            });
        }

        return s2;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        tdata = DailyQcUtils.tdata;
        DailyQcUtils.points_flag = 1;
        DailyQcUtils.grids_flag = -1;
        DailyQcUtils.map_flag = -1;
        DailyQcUtils.contour_flag = -1;
        if (MPEDisplayManager.pcpn_time_step != 1) {
            MPEDisplayManager.pcpn_time_step = 1;
        }
        if (tdata == null || (tdata.length <= 0)) {
            Date currDate = ChooseDataPeriodDialog.prevDate;
            String QcArea = ChooseDataPeriodDialog.prevArea;
            int qcDays = MPEDisplayManager.getCurrent().getDqcDays();
            // checks to see if area or date has changed since last data load
            DailyQcUtils dqcu = new DailyQcUtils();
            dqc_good = dqcu.qcDataReload(currDate, QcArea, qcDays, false);
            tdata = DailyQcUtils.tdata;

        }
        dataSet.clear();
        dataSet.addAll(dataType);

        DailyQcUtils.pcp_flag = 3;
        DailyQcUtils.pcpn_day = 0;
        DailyQcUtils.pcpn_time = 0;

        for (i = 0; i < 8; i++) {

            if (MPEDisplayManager.pcpn_time_step == 0) {
                time_pos = 150 + DailyQcUtils.pcp_flag;
            } else if (MPEDisplayManager.pcpn_time_step == 1) {
                time_pos = 190 + DailyQcUtils.pcpn_day;
            } else if (MPEDisplayManager.pcpn_time_step == 2) {
                time_pos = 200 + DailyQcUtils.pcpn_day;
            }

            if ((i != 0 && i != 7) && DailyQcUtils.pcp_in_use[time_pos] == -1) {
                dataSet.remove(dataSet.indexOf(dataType.get(i)));
            }
        }

        ts = DailyQcUtils.ts;
        this.createDataOptionsGroup();
        this.createPointTypeGroup();
        this.createPointQualityGroup();
        this.createPointSetComp();
        this.createPointControlComp();

    }

    /**
     * Create the data options group and controls.
     */
    private void createDataOptionsGroup() {
        int i = 0;
        Group dataOptionsGroup = new Group(shell, SWT.NONE);
        dataOptionsGroup.setText(" Data Options ");
        GridLayout groupLayout = new GridLayout(1, false);
        dataOptionsGroup.setLayout(groupLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataOptionsGroup.setLayoutData(gd);
        final Shell shell = this.getParent();
        final Cursor prevCursor = shell.getCursor();
        final Cursor waitCursor = new Cursor(Display.getDefault(),
                SWT.CURSOR_WAIT);

        // Create a container to hold the label and the combo box.
        Composite maxmTimeComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout maxmTimeCompLayout = new GridLayout(3, false);
        maxmTimeCompLayout.marginHeight = 0;
        maxmTimeCompLayout.marginWidth = 0;
        maxmTimeComp.setLayout(maxmTimeCompLayout);

        Label maxmTimeLbl = new Label(maxmTimeComp, SWT.CENTER);
        maxmTimeLbl.setText("6 Hour/MaxMin:");

        GridData sd = new GridData(140, SWT.DEFAULT);
        maxminTimeCbo = new Combo(maxmTimeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        maxminTimeCbo.setTextLimit(30);
        maxminTimeCbo.setLayoutData(sd);
        maxminTimeCbo.add("6 Hourly");
        maxminTimeCbo.add("Maximum");
        maxminTimeCbo.add("Minimum");
        maxminTimeCbo.select(MPEDisplayManager.pcpn_time_step);
        maxminTimeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                oto.chg_maxmin_time(maxminTimeCbo.getSelectionIndex() + 2);
            }
        });
        if (DailyQcUtils.qcDays == 1
                && (DailyQcUtils.curHr18_00 == 1
                        || DailyQcUtils.curHr00_06 == 1 || DailyQcUtils.curHr06_12 == 1)) {
            maxminTimeCbo.setEnabled(false);
        } else {
            maxminTimeCbo.setEnabled(true);
        }

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(maxmTimeComp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.HORIZONTAL);
        timeArrowsComp.setLayout(timeArrowRl);

        RowData rd = new RowData(25, 25);
        upTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.UP);
        upTimeBtn.setLayoutData(rd);
        upTimeBtn.setEnabled(false);
        upTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                oto.chg_maxmin_time(0);
            }
        });

        rd = new RowData(25, 25);
        dnTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnTimeBtn.setLayoutData(rd);
        dnTimeBtn.setEnabled(false);
        dnTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                oto.chg_maxmin_time(1);
            }
        });

        GridData dd = new GridData(208, SWT.DEFAULT);

        String[] a = new String[dataSet.size()];
        dataDispCbo = new Combo(dataOptionsGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        dataDispCbo.setTextLimit(30);
        dataDispCbo.setLayoutData(dd);
        dataDispCbo.removeAll();
        dataDispCbo.setItems(dataSet.toArray(a));

        i = getOpts();

        int ii = dataSet.indexOf((dataType.get(i)));
        dataDispCbo.select(ii);
        dataDispCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.display_pcpn_options(dataDispCbo.getSelectionIndex());
            }
        });

        Composite renderComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout renderCompLayout = new GridLayout(2, false);
        renderCompLayout.marginHeight = 0;
        renderCompLayout.marginWidth = 0;
        renderComp.setLayout(renderCompLayout);

        gd = new GridData(153, 25);
        renderGridsBtn = new Button(renderComp, SWT.PUSH);
        renderGridsBtn.setText("Render Grids+MATs");
        renderGridsBtn.setLayoutData(gd);
        if (DailyQcUtils.pcp_in_use[time_pos] == -1
                && DailyQcUtils.tdata[i].used[4] != 0) {
            renderGridsBtn.setEnabled(true);
        } else {
            renderGridsBtn.setEnabled(false);
        }
        renderGridsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                opo.render_options(3);
                shell.setCursor(prevCursor);
                renderGridsBtn.setEnabled(false);
            }
        });

        GridData bd = new GridData(110, 25);
        groupEditBtn = new Button(renderComp, SWT.PUSH);
        groupEditBtn.setText("Group Edit");
        groupEditBtn.setLayoutData(bd);
        groupEditBtn.addSelectionListener(new SelectionAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                GroupEditStationsDialog groupDialog = new GroupEditStationsDialog(
                        shell, 1);
                groupDialog.open();
            }
        });
    }

    /**
     * Create the data options group and controls.
     */
    private void createPointTypeGroup() {
        Group pointTypeGroup = new Group(shell, SWT.NONE);
        pointTypeGroup.setText(" Point Type ");
        GridLayout gl = new GridLayout(1, false);
        pointTypeGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pointTypeGroup.setLayoutData(gd);

        Composite pointTypeComp = new Composite(pointTypeGroup, SWT.NONE);
        GridLayout pointTypeCompLayout = new GridLayout(3, false);
        pointTypeCompLayout.marginHeight = 0;
        pointTypeCompLayout.marginWidth = 0;
        pointTypeComp.setLayout(pointTypeCompLayout);

        // ------------------------------------------------------
        // Setup the composite of point type check boxes
        // ------------------------------------------------------
        Composite chkBxComp = new Composite(pointTypeComp, SWT.NONE);
        GridLayout chkBxCompGl = new GridLayout(3, false);
        chkBxComp.setLayout(chkBxCompGl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        chkBxComp.setLayoutData(gd);

        int m;
        tsbuttons = new Button[DailyQcUtils.tsmax + 2];

        nexChk = new Button(chkBxComp, SWT.CHECK);
        nexChk.setEnabled(false);
        nexChk.setText("NEXRAD");
        nexChk.setData(0);
        nexChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                gq.source_Select((Integer) nexChk.getData());
            }
        });
        tsbuttons[0] = nexChk;
        for (m = 0; m < DailyQcUtils.tsmax; m++) {
            final Button bname = new Button(chkBxComp, SWT.CHECK);
            bname.setText(ts[m].name);
            bname.setData(m + 1);
            bname.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    gq.source_Select(((Integer) bname.getData()));
                }
            });
            tsbuttons[m + 1] = bname;
        }

        allChk = new Button(chkBxComp, SWT.CHECK);
        allChk.setText("ALL");
        allChk.setData(m + 1);
        allChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                gq.source_Select(((Integer) allChk.getData()));
            }
        });
        tsbuttons[m + 1] = allChk;
        for (int i = 0; i < DailyQcUtils.tsmax + 2; i++) {

            if (i == 0) {
                /*
                 * Do not show the NEXRAD option. Nexrad data are now displayed
                 * through the MPE portion of MPE Editor.
                 */
                DailyQcUtils.dflag[i] = -1;
                continue;
            } else {
                DailyQcUtils.dflag[i] = 1;
            }

            if (DailyQcUtils.dflag[i] == 1) {
                tsbuttons[i].setSelection(true);
            } else {
                tsbuttons[i].setSelection(false);
            }

        }

    }

    /**
     * Create the data options group and controls.
     */
    private void createPointQualityGroup() {
        Group pointQualGroup = new Group(shell, SWT.NONE);
        pointQualGroup.setText(" Point Quality ");
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        pointQualGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pointQualGroup.setLayoutData(gd);

        int i;
        int qflag[] = DailyQcUtils.qflag;

        for (i = 0; i < 10; i++) {
            qflag[i] = 1;
        }

        // qflag[5] = -1;

        boolean mpe_show_missing_gage_set = false;
        if (DailyQcUtils.mpe_show_missing_gage.length() > 0) {
            if ((DailyQcUtils.mpe_show_missing_gage.equalsIgnoreCase("All"))
                    || (DailyQcUtils.mpe_show_missing_gage
                            .equalsIgnoreCase("Reported"))) {
                mpe_show_missing_gage_set = true;
            } else {
                mpe_show_missing_gage_set = false;
            }

        } else {
            mpe_show_missing_gage_set = false;
        }

        if (mpe_show_missing_gage_set == true) {
            qflag[7] = 1;
        } else {
            qflag[7] = -1;
        }

        Composite pointQualComp = new Composite(pointQualGroup, SWT.NONE);
        GridLayout pointQualCompLayout = new GridLayout(2, false);
        pointQualCompLayout.marginHeight = 0;
        pointQualCompLayout.marginWidth = 0;
        pointQualComp.setLayout(pointQualCompLayout);

        // ------------------------------------------------------
        // Setup the composite of point quality check boxes
        // ------------------------------------------------------
        Composite qualCkBxComp = new Composite(pointQualComp, SWT.NONE);
        GridLayout qualCkBxCompGl = new GridLayout(2, false);
        qualCkBxComp.setLayout(qualCkBxCompGl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        qualCkBxComp.setLayoutData(gd);

        Composite ltCkBxComp = new Composite(qualCkBxComp, SWT.NONE);
        GridLayout lrCkBxLo = new GridLayout(1, false);
        Composite rtCkBxComp = new Composite(qualCkBxComp, SWT.NONE);
        ltCkBxComp.setLayout(lrCkBxLo);
        rtCkBxComp.setLayout(lrCkBxLo);

        String[] qbnames = { "Verified", "Screened", "Time Dist", "Manual",
                "Questionable", "Partial", "Estimated", "Bad", "Missing", "All" };

        for (i = 0; i < qsbuttons.length / 2; i++) {

            final Button b = new Button(ltCkBxComp, SWT.CHECK);
            b.setText(qbnames[i]);
            b.setData(i);
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    gq.quality_Select_Temperature(((Integer) b.getData()));
                }
            });
            qsbuttons[i] = b;
        }
        for (i = qsbuttons.length / 2; i < qsbuttons.length; i++) {
            final Button b = new Button(rtCkBxComp, SWT.CHECK);
            b.setText(qbnames[i]);
            b.setData(i);
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    gq.quality_Select_Temperature(((Integer) b.getData()));
                }
            });
            qsbuttons[i] = b;
            qsbuttons[5].setEnabled(false);
        }
        for (i = 0; i < 10; i++) {

            if (i == 5) {
                continue;
            }
            if (qflag[DailyQcUtils.funct[i]] == 1) {
                qsbuttons[i].setSelection(true);
            } else {
                qsbuttons[i].setSelection(false);
            }
        }
    }

    private void createPointSetComp() {
        Composite pntSetComp = new Composite(shell, SWT.NONE);
        GridLayout pntSetCompGl = new GridLayout(2, false);
        pntSetComp.setLayout(pntSetCompGl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pntSetComp.setLayoutData(gd);

        Label pntDispLbl = new Label(pntSetComp, SWT.CENTER);
        pntDispLbl.setText("Point display:");

        DailyQcUtils.plot_view = 4;

        pntDispCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntDispCbo.setTextLimit(30);
        pntDispCbo.setLayoutData(gd);
        pntDispCbo.add("Handbook 5");
        pntDispCbo.add("PC");
        pntDispCbo.add("Name");
        pntDispCbo.add("Data");
        pntDispCbo.add("RFS");
        pntDispCbo.add("Elevation");
        pntDispCbo.select(3);
        pntDispCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                gq.change_Plot(pntDispCbo.getSelectionIndex() + 1);
            }
        });

        Label pntScnLbl = new Label(pntSetComp, SWT.CENTER);
        pntScnLbl.setText("Point screening:");

        int i;
        final ScreeningOptions so = new ScreeningOptions();
        if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].stddev == 15.0) {
            i = 0;
        } else if (DailyQcUtils.tdata[DailyQcUtils.pcpn_day].stddev == 10.0) {
            i = 1;
        } else {
            i = 2;
        }

        pntScnCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntScnCbo.setTextLimit(30);
        pntScnCbo.setLayoutData(gd);
        pntScnCbo.add("Coarse");
        pntScnCbo.add("Medium");
        pntScnCbo.add("Fine");
        pntScnCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                so.screening_Options(pntScnCbo.getSelectionIndex());
            }
        });
        pntScnCbo.select(i);

    }

    private void createPointControlComp() {
        Composite pntControlComp = new Composite(shell, SWT.NONE);
        GridLayout pntControlCompGl = new GridLayout(2, false);
        pntControlComp.setLayout(pntControlCompGl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pntControlComp.setLayoutData(gd);

        /**
         * Point filter scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pntFilterLbl = new Label(pntControlComp, SWT.NONE);
        pntFilterLbl.setText("Point filter  (degrees F):");
        pntFilterLbl.setLayoutData(gd);

        pfvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pfvalueLabel.setLayoutData(gd);
        pfvalueLabel.setAlignment(SWT.LEFT);
        pfvalueLabel.setText("-50");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntFilter = new Scale(pntControlComp, SWT.BORDER);
        pntFilter.setMinimum(0);
        pntFilter.setMaximum(175);
        pntFilter.setSelection(0);
        pntFilter.setLayoutData(gd);
        pntFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pntFilter.getSelection() - 50;
                pfvalueLabel.setText(String.format("%3d", sel));
            }
        });
        pntFilter.addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                opo.refresh_exposure();
            }

        });
        pfvalueLabel
                .setText(String.format("%3d", pntFilter.getSelection() - 50));

        /**
         * Point reverse filter scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pntRevFilterLbl = new Label(pntControlComp, SWT.NONE);
        pntRevFilterLbl.setText("Point reverse filter  (degrees F):");
        pntRevFilterLbl.setLayoutData(gd);

        prvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prvalueLabel.setLayoutData(gd);
        prvalueLabel.setAlignment(SWT.LEFT);
        prvalueLabel.setText("125");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntRevFilter = new Scale(pntControlComp, SWT.BORDER);
        pntRevFilter.setMinimum(0);
        pntRevFilter.setMaximum(175);
        pntRevFilter.setIncrement(1);
        pntRevFilter.setSelection(0);
        pntRevFilter.setLayoutData(gd);
        pntRevFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = 125 - pntRevFilter.getSelection();
                prvalueLabel.setText(String.format("%3d", sel));
            }
        });
        pntRevFilter
                .addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
                    @Override
                    public void mouseUp(MouseEvent e) {
                        opo.refresh_exposure();
                    }

                });
        prvalueLabel.setText(String.format("%3d",
                125 - pntRevFilter.getSelection()));

        /**
         * Point elevation scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pntElFilterLbl = new Label(pntControlComp, SWT.NONE);
        pntElFilterLbl.setText("Point elevation  (feet):");
        pntElFilterLbl.setLayoutData(gd);

        pevalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pevalueLabel.setLayoutData(gd);
        pevalueLabel.setAlignment(SWT.LEFT);
        pevalueLabel.setText("0");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntElFilter = new Scale(pntControlComp, SWT.BORDER);
        pntElFilter.setMinimum(0);
        pntElFilter.setMaximum(15000);
        pntElFilter.setIncrement(100);
        pntElFilter.setSelection(0);
        pntElFilter.setLayoutData(gd);
        pntElFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pntElFilter.getSelection();
                pevalueLabel.setText(String.format("%d", sel));
            }
        });
        pntElFilter.addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                int sel = pntElFilter.getSelection();
                DailyQcUtils.elevation_filter_value = sel;
                opo.refresh_exposure();
            }

        });
        pevalueLabel.setText(String.format("%d", pntElFilter.getSelection()));

        OtherPrecipOptions.change_pcpn_flag = -1;
        OtherPrecipOptions.change_rpcpn_flag = -1;
        OtherPrecipOptions.change_topo_flag = -1;
        OtherPrecipOptions.change_frz_flag = -1;
        OtherPrecipOptions.change_maxmin_flag = 1;

        pntFilter.setSelection(0);
        pntRevFilter.setSelection(0);
        DailyQcUtils.elevation_filter_value = pntElFilter.getSelection();

        opo.send_expose();
        OtherTempOptions oto = new OtherTempOptions();
        oto.set_temp_arrow_sensitivity();

    }

    public static float getPointFilterValue() {
        float sel = pntFilter.getSelection() - 50.0f;
        return sel;
    }

    public static float getPointFilterReverseValue() {
        float sel = 125.0f - pntRevFilter.getSelection();
        return sel;
    }

    public static void destroy(boolean t) {
        isOpen = t;
    }

    public static boolean isFinished() {
        return isfinished;
    }
}
