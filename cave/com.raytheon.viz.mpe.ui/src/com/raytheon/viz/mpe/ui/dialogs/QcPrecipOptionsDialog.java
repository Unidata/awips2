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
import com.raytheon.viz.mpe.ui.actions.SaveLevel2Data;
import com.raytheon.viz.mpe.ui.actions.ScreeningOptions;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
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

public class QcPrecipOptionsDialog extends AbstractMPEDialog {

    public static Combo selsix24Cbo;

    private static Combo dataDispCbo;

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

    public static Combo pntCharCbo;

    public static Combo pntDispCbo;

    public static Combo pntScnCbo;

    public static Combo pntTConCbo;

    public static Combo pntSConCbo;

    private static Scale pntFilter;

    private static Scale pntRevFilter;

    private static Scale pntElFilter;

    private static Scale pxTempFilter;

    private Label pfvalueLabel;

    private Label prvalueLabel;

    private Label pevalueLabel;

    private Label pxvalueLabel;

    public static boolean isOpen = false;

    private static boolean isfinished = true;

    private static int dqc_good = 0;

    public static DrawDQCStations ddqc;

    public static ArrayList<String> dataType = new ArrayList<String>();

    public static ArrayList<String> dataSet = new ArrayList<String>();

    OtherPrecipOptions opo = new OtherPrecipOptions();

    int[] pcp_in_use;

    Pdata[] pdata = new Pdata[0];

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
    public QcPrecipOptionsDialog(Shell parent) {
        super(parent);
    }

    private int getOpts() {
        int ik = 0;
        if (DailyQcUtils.points_flag == 1 && pcp_in_use[time_pos] == -1) {
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
        dataType.add("MAPs");
        dataType.add("Points+Grids");
        dataType.add("Points+MAPs");
        dataType.add("Contours");
        dataType.add("Points+Contours");
        dataType.add("None");
        dataSet.addAll(dataType);
        if (displayMgr.isMaxmin()) {
            QcTempOptionsDialog.destroy(false);
            displayMgr.setMaxmin(false);
        }
        if (displayMgr.isZflag()) {
            QcFreezeOptionsDialog.destroy(false);
            displayMgr.setZflag(false);
        }

        displayMgr.setQpf(true);
        ddqc = DrawDQCStations.getInstance();

        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS);

        shell.setText("QC Precipitation Options");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        this.initializeComponents();

        shell.pack();

        shell.open();
        displayMgr.setQpf(true);
        isOpen = true;
        isfinished = false;
        opo.chg_precip_time(selsix24Cbo.getSelectionIndex() + 2);
        opo.send_expose();
        while (!shell.isDisposed()) {
            if (dqc_good == 0) {
                displayMgr.setQpf(false);
                isOpen = false;
                ddqc.destroy();
                shell.dispose();
            }
            if (isOpen == false) {
                displayMgr.setQpf(false);
                ddqc.destroy();
                shell.dispose();
            }
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        ddqc.destroy();
        displayMgr.setQpf(false);
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
        pdata = DailyQcUtils.pdata;
        DailyQcUtils.points_flag = 1;
        DailyQcUtils.grids_flag = -1;
        DailyQcUtils.map_flag = -1;
        DailyQcUtils.contour_flag = -1;
        if (pdata == null || (pdata.length <= 0)) {
            Date currDate = ChooseDataPeriodDialog.prevDate;
            String QcArea = ChooseDataPeriodDialog.prevArea;
            int qcDays = MPEDisplayManager.getCurrent().getDqcDays();
            // checks to see if area or date has changed since last data load
            DailyQcUtils dqcu = new DailyQcUtils();
            dqc_good = dqcu.qcDataReload(currDate, QcArea, qcDays, false);
            pdata = DailyQcUtils.pdata;
        }
        pcp_in_use = DailyQcUtils.pcp_in_use;
        dataSet.clear();
        dataSet.addAll(dataType);

        DailyQcUtils.pcp_flag = 3;
        DailyQcUtils.pcpn_day = 0;
        DailyQcUtils.pcpn_time = 0;

        for (i = 0; i < 8; i++) {

            if (MPEDisplayManager.pcpn_time_step == 0) {
                time_pos = DailyQcUtils.pcp_flag;
            } else {
                time_pos = 40 + DailyQcUtils.pcpn_day;
            }

            if ((i != 0 && i != 7) && pcp_in_use[time_pos] == -1) {
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

        if (MPEDisplayManager.pcpn_time_step != 1) {
            MPEDisplayManager.pcpn_time_step = 1;
        }

        // Create a container to hold the label and the combo box.
        Composite six24Comp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout six24CompLayout = new GridLayout(3, false);
        six24CompLayout.marginHeight = 0;
        six24CompLayout.marginWidth = 0;
        six24Comp.setLayout(six24CompLayout);

        Label six24Lbl = new Label(six24Comp, SWT.CENTER);
        six24Lbl.setText("6/24 Hour:");

        GridData sd = new GridData(140, SWT.DEFAULT);
        selsix24Cbo = new Combo(six24Comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        selsix24Cbo.setTextLimit(30);
        selsix24Cbo.setLayoutData(sd);
        selsix24Cbo.add("6 Hour");
        selsix24Cbo.add("24 Hour");
        selsix24Cbo.select(MPEDisplayManager.pcpn_time_step);
        selsix24Cbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.chg_precip_time(selsix24Cbo.getSelectionIndex() + 2);
            }
        });
        if (DailyQcUtils.qcDays == 1
                && (DailyQcUtils.curHr18_00 == 1
                        || DailyQcUtils.curHr00_06 == 1 || DailyQcUtils.curHr06_12 == 1)) {
            selsix24Cbo.setEnabled(false);
        } else {
            selsix24Cbo.setEnabled(true);
        }

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(six24Comp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.HORIZONTAL);
        timeArrowsComp.setLayout(timeArrowRl);

        RowData rd = new RowData(25, 25);
        upTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.UP);
        upTimeBtn.setLayoutData(rd);
        upTimeBtn.setEnabled(false);
        upTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.chg_precip_time(0);
            }
        });

        rd = new RowData(25, 25);
        dnTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnTimeBtn.setLayoutData(rd);
        dnTimeBtn.setEnabled(false);
        dnTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.chg_precip_time(1);
            }
        });

        GridData dd = new GridData(208, SWT.DEFAULT);

        // int pd = DailyQcUtils.hrgt12z == 1 ? 1 : 0;
        // if (pcp_flag == -1) {
        // /*
        // * Define the pcp_flag based on whether or not there is a partial
        // * DQC day. This also depends on whether the time step is 6 or 24
        // * hours. Initially this will be 24.
        // */
        // if (pd == 1) {
        // pcp_flag = 4;
        // pcpn_day = 1;
        // } else {
        // pcp_flag = 0;
        // pcpn_day = 0;
        // }
        //
        // }

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
        renderGridsBtn.setText("Render Grids+MAPs");
        renderGridsBtn.setLayoutData(gd);
        if (pcp_in_use[time_pos] == -1 && pdata[i].used[4] != 0) {
            renderGridsBtn.setEnabled(true);
        } else {
            renderGridsBtn.setEnabled(false);
        }
        renderGridsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                opo.render_options(0);
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
                        shell, 0);
                groupDialog.open();
            }
        });

        Composite pcpTypeComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout pcpTypeCompLayout = new GridLayout(2, false);
        pcpTypeCompLayout.marginHeight = 0;
        pcpTypeCompLayout.marginWidth = 0;
        pcpTypeComp.setLayout(pcpTypeCompLayout);

        Label pcpLbl = new Label(pcpTypeComp, SWT.CENTER);
        pcpLbl.setText("Precip Type:");

        gd = new GridData(190, SWT.DEFAULT);
        pcpTypeCbo = new Combo(pcpTypeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pcpTypeCbo.setTextLimit(30);
        pcpTypeCbo.setLayoutData(gd);
        pcpTypeCbo.add("Rain/Snow");
        pcpTypeCbo.add("All");
        pcpTypeCbo.select(OtherPrecipOptions.rsmode);
        pcpTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                opo.change_rsmode(pcpTypeCbo.getSelectionIndex());
            }
        });
        pcpTypeCbo.setEnabled(false);
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

        qflag[5] = -1;

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
                    gq.quality_Select(((Integer) b.getData()));
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
                    gq.quality_Select(((Integer) b.getData()));
                }
            });
            qsbuttons[i] = b;
        }
        for (i = 0; i < 10; i++) {

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

        Label pntCharLbl = new Label(pntSetComp, SWT.CENTER);
        pntCharLbl.setText("Point character:");
        DailyQcUtils.gage_char[0] = 1;
        DailyQcUtils.gage_char[1] = 1;

        gd = new GridData(160, SWT.DEFAULT);
        pntCharCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntCharCbo.setTextLimit(30);
        pntCharCbo.setLayoutData(gd);
        pntCharCbo.add("Tip");
        pntCharCbo.add("Weigh");
        pntCharCbo.add("Tip+Weigh");
        pntCharCbo.select(2);
        pntCharCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                gq.change_Character(pntCharCbo.getSelectionIndex());
            }
        });

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
        pntDispCbo.add("Dev");
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
        if (pdata[DailyQcUtils.pcpn_day].stddev == 5.0) {
            i = 0;
        } else if (pdata[DailyQcUtils.pcpn_day].stddev == 3.0) {
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

        Label pntTConLbl = new Label(pntSetComp, SWT.CENTER);
        pntTConLbl.setText("Point Tconsistency:");

        if (OtherPrecipOptions.tcmode == 0) {
            i = 0;
        } else if (OtherPrecipOptions.tcmode == 1) {
            i = 1;
        } else {
            i = 2;
        }

        pntTConCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntTConCbo.setTextLimit(30);
        pntTConCbo.setLayoutData(gd);
        pntTConCbo.add("Consistent");
        pntTConCbo.add("Inconsistent");
        pntTConCbo.add("All");
        pntTConCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                opo.change_TCmode(pntTConCbo.getSelectionIndex());
            }
        });
        pntTConCbo.select(i);

        Label pntSConLbl = new Label(pntSetComp, SWT.CENTER);
        pntSConLbl.setText("Point Sconsistency:");

        if (OtherPrecipOptions.dcmode == 0) {
            i = 0;
        } else if (OtherPrecipOptions.dcmode == 1) {
            i = 1;
        } else {
            i = 2;
        }

        pntSConCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntSConCbo.setTextLimit(30);
        pntSConCbo.setLayoutData(gd);
        pntSConCbo.add("Consistent");
        pntSConCbo.add("Inconsistent");
        pntSConCbo.add("All");
        pntSConCbo.select(i);
        pntSConCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                opo.change_DCmode(pntSConCbo.getSelectionIndex());
            }
        });
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
        pntFilterLbl.setText("Point filter  (inches):");
        pntFilterLbl.setLayoutData(gd);

        pfvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pfvalueLabel.setLayoutData(gd);
        pfvalueLabel.setAlignment(SWT.LEFT);
        pfvalueLabel.setText("0.00");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntFilter = new Scale(pntControlComp, SWT.BORDER);
        pntFilter.setMinimum(0);
        pntFilter.setMaximum(500);
        pntFilter.setSelection(0);
        pntFilter.setLayoutData(gd);
        pntFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pntFilter.getSelection();
                pfvalueLabel.setText(String.format("%4.2f", sel / 100.0f));
            }
        });
        pntFilter.addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                opo.refresh_exposure();
            }

        });

        /**
         * Point reverse filter scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pntRevFilterLbl = new Label(pntControlComp, SWT.NONE);
        pntRevFilterLbl.setText("Point reverse filter  (inches):");
        pntRevFilterLbl.setLayoutData(gd);

        prvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prvalueLabel.setLayoutData(gd);
        prvalueLabel.setAlignment(SWT.LEFT);
        prvalueLabel.setText("20.00");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntRevFilter = new Scale(pntControlComp, SWT.BORDER);
        pntRevFilter.setMinimum(0);
        pntRevFilter.setMaximum(2000);
        pntRevFilter.setIncrement(10);
        pntRevFilter.setSelection(0);
        pntRevFilter.setLayoutData(gd);
        pntRevFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = 2000 - pntRevFilter.getSelection();
                prvalueLabel.setText(String.format("%4.2f", sel / 100.0f));
            }
        });
        pntRevFilter
                .addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
                    @Override
                    public void mouseUp(MouseEvent e) {
                        opo.refresh_exposure();
                    }

                });

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

        /**
         * Pxtemp scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pxTempFilterLbl = new Label(pntControlComp, SWT.NONE);
        pxTempFilterLbl.setText("Pxtemp  (deg C):");
        pxTempFilterLbl.setLayoutData(gd);

        pxvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pxvalueLabel.setLayoutData(gd);
        pxvalueLabel.setAlignment(SWT.LEFT);
        pxvalueLabel.setText("1.00");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pxTempFilter = new Scale(pntControlComp, SWT.BORDER);
        pxTempFilter.setMinimum(0);
        pxTempFilter.setMaximum(400);
        pxTempFilter.setIncrement(10);
        pxTempFilter.setSelection(200);
        pxTempFilter.setLayoutData(gd);
        pxTempFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pxTempFilter.getSelection() - 100;
                pxvalueLabel.setText(String.format("%4.2f", sel / 100.0f));
            }
        });
        pxTempFilter
                .addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
                    @Override
                    public void mouseUp(MouseEvent e) {
                        int sel = pxTempFilter.getSelection() - 100;
                        DailyQcUtils.pxtemp = (float) sel / 100;
                        DailyQcUtils.dmvalue = (int) (DailyQcUtils.pxtemp * 100 * 3.28 / .55);
                        opo.refresh_exposure();
                    }
                });

        OtherPrecipOptions.change_pcpn_flag = 1;
        OtherPrecipOptions.change_rpcpn_flag = -1;
        OtherPrecipOptions.change_topo_flag = -1;
        OtherPrecipOptions.change_frz_flag = -1;
        OtherPrecipOptions.change_maxmin_flag = -1;

        // initialize the gage filter values
        pntFilter.setSelection(0);
        pntRevFilter.setSelection(0);
        DailyQcUtils.elevation_filter_value = pntElFilter.getSelection();
        DailyQcUtils.pxtemp = (pxTempFilter.getSelection() - 100) / 100;
        DailyQcUtils.dmvalue = (int) (DailyQcUtils.pxtemp * 100 * 3.28 / .55);

        opo.refresh_exposure();
        opo.set_precip_arrow_sensitivity();
    }

    public static void setDataSetCombo(String[] a) {
        dataDispCbo.setItems(dataSet.toArray(a));
    }

    public static void selectDataSetVal(int i) {
        int ii = dataSet.indexOf((dataType.get(i)));
        dataDispCbo.select(ii);
    }

    public static float getPointFilterValue() {
        float sel = pntFilter.getSelection() / 100.0f;
        return sel;
    }

    public static float getPointFilterReverseValue() {
        float sel = (2000 - pntRevFilter.getSelection()) / 100.0f;
        return sel;
    }

    public static void destroy(boolean t) {
        isOpen = t;
    }

    public static boolean isFinished() {
        return isfinished;
    }
}
