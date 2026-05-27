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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Cursor;
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
import com.raytheon.viz.mpe.ui.actions.GroupEditCalls;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.ui.actions.SaveLevel2Data;
import com.raytheon.viz.mpe.ui.actions.ScreeningOptions;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.ui.dialogs.DialogUtil;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * QC Precip Options Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Nov 12, 2008           snaples    Initial creation
 * Mar 07, 2013  15657    lbousaidi  fixed DQC slider and added listener to the
 *                                   Keys when pressed.
 * Sep 11, 2013  2353     lvenable   Fixed cursor memory leak.
 * Mar 10, 2015  14575    snaples    Added additional status flag.
 * Jul 09, 2015  14618    snaples    Cleaned up code issues.
 * Sep 11, 2015  17988    snaples    Fixed issue with wait cursor not showing
 *                                   when Rendering Grids.
 * Nov 18, 2015  18093    snaples    Fixed problem with arrows being disabled
 *                                   when new day rollover >18Z occurs.
 * Jan 15, 2016  5054     randerso   Use proper parent shell
 * Apr 05, 2016  18350    snaples    Added method call to dqc.destroy to close
 *                                   instance of DQC Utils when exiting.
 * Apr 11, 2016  5512     bkowal     Fix GUI sizing issues. Cleanup.
 * May 04, 2016  5624     skorolev   Fixed UELE for QC Precipitation dialog.
 * May 04, 2016  5054     dgilling   Fix dialog parenting for SaveLevel2Data
 *                                   when closing this dialog.
 * Sep 21, 2016  5901     randerso   Fix dialog centering issue introduced in
 *                                   Eclipse 4
 * Mar 02, 2017  6164     bkowal     Updated to extend {@link
 *                                   AbstractDailyQCDialog}.
 * Aug 11, 2017  6148     bkowal     Cleanup. Implement {@link
 *                                   IGroupEditHandler}.
 * Dec 15, 2017  6547     bkowal     Correctly assign the current date and the
 *                                   previous date.
 * May 10, 2018  7131     mduff      Added child dialogs so they can be closed
 *                                   if this is closed, cleanup.
 * May 10, 2018  7184     bkowal     Always allow the user to switch between 6hr
 *                                   and 24hr data.
 * May 16, 2018  6926     tgurney    Point elevation slider, update the display
 *                                   when value is changed with arrow keys
 * Jul 17, 2018  7375     smanoj     Fixed widget dispose issue when exiting
 *                                   CAVE.
 * Aug 09, 2018  7098     tgurney    Move child dialog tracking to superclass
 * Jul 02, 2019  7131     randerso   Fix point filter sliders to allow
 *                                   increments of 0.01 inches.
 * Jan 04, 2021  18448    dhaines    Prevent nullpointer when closing cave and
 *                                   there is stuff to save to the db in DailyQC
 *                                   and the QC Precip Options Window is still
 *                                   open.                                  
 * 
 * </pre>
 *
 * @author snaples
 */

public class QcPrecipOptionsDialog extends AbstractDailyQCDialog
        implements IGroupEditHandler {

    private static Combo selsix24Cbo;

    private static Combo dataDispCbo;

    public static Button upTimeBtn;

    public static Button dnTimeBtn;

    public static Button renderGridsBtn;

    public static Button groupEditBtn;

    public static Combo pcpTypeCbo;

    private Combo pntCharCbo;

    private Combo pntDispCbo;

    public static Combo pntScnCbo;

    private Combo pntTConCbo;

    private Combo pntSConCbo;

    private int dqc_good = 0;

    private DrawDQCStations ddq;

    private DailyQcUtils dqc;

    public static List<String> dataType = new ArrayList<>();

    public static List<String> dataSet = new ArrayList<>();

    private OtherPrecipOptions opo = new OtherPrecipOptions();

    private int time_pos;

    public static Button[] tsbuttons = null;

    public static Button[] qsbuttons = new Button[10];

    private final GageQcSelect gq = new GageQcSelect();

    /**
     * Constructor.
     *
     * @param parent
     *            Parent shell.
     */
    public QcPrecipOptionsDialog(Shell parent) {
        super(parent);
        MPEDisplayManager.getCurrent().setQpf(true);
        dqc = DailyQcUtils.getInstance();
        ddq = DrawDQCStations.getInstance();
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
                && DailyQcUtils.grids_flag == 1
                && DailyQcUtils.map_flag == -1) {
            ik = 1;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == -1
                && DailyQcUtils.map_flag == 1) {
            ik = 2;
        } else if (DailyQcUtils.points_flag == 1 && DailyQcUtils.grids_flag == 1
                && DailyQcUtils.map_flag == -1) {
            ik = 3;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.grids_flag == -1
                && DailyQcUtils.map_flag == 1) {
            ik = 4;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.contour_flag == 1) {
            ik = 5;
        } else if (DailyQcUtils.points_flag == 1
                && DailyQcUtils.contour_flag == 1) {
            ik = 6;
        } else if (DailyQcUtils.points_flag == -1
                && DailyQcUtils.grids_flag == -1
                && DailyQcUtils.map_flag == -1) {
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
        Date currDate = displayMgr.getCurrentEditDate();
        Date prevDate = ChooseDataPeriodDialog.prevHydDate;
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
        // reloads data if changed
        // returns 0 for failed, 1 for new area, 2 for Ok
        dqc_good = dqc.qcDataHasChanged(prevDate, currDate, QcArea, qcDays,
                false);
        if (dqc_good == 1) {
            SaveLevel2Data s2 = new SaveLevel2Data(getShell());
            dqc_good = s2.check_new_area(currDate, QcArea, qcDays);
            if (dqc_good == 0) {
                dqc_good = dqc.qcDataReload(currDate, QcArea, qcDays, false);
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
            displayMgr.getQcTempDialog().close();
            displayMgr.setMaxmin(false);
        }
        if (displayMgr.isZflag()) {
            displayMgr.getQcFreezeDialog().close();
            displayMgr.setZflag(false);
        }

        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS);

        shell.setText("QC Precipitation Options");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        shell.setLayout(mainLayout);
        
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                SaveLevel2Data s2 = new SaveLevel2Data(getParent());
                s2.send_dbase_new_area();
            }
        });
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    mgr.removePespectiveDialog(QcPrecipOptionsDialog.this);
                }
                revertDisplayModeToPrevious();
            }
        });

        // Initialize all of the controls and layouts
        this.initializeComponents();

        shell.pack();

        DialogUtil.centerOnParentShell(parent, shell);

        shell.open();

        displayMgr.setQcPrecipDialog(this);
        displayMgr.setQpf(true);
        DailyQcUtils.qpf_flag = true;
        opo.chg_precip_time(selsix24Cbo.getSelectionIndex() + 2);
        opo.send_expose();
        listenToRevertDisplay(shell);
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        ddq.destroy();
        displayMgr.setQpf(false);
        DailyQcUtils.qpf_flag = false;
        displayMgr.displayFieldData(df);
        removePerspectiveListener();
        closeChildDlgs();

        if (!getParent().isDisposed()) {
            if (MPEDisplayManager.getCurrent() != null) {
                display.asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        ChooseDataPeriodDialog dialog = new ChooseDataPeriodDialog(
                                getParent().getShell());
                        dialog.open();
                    }
                });
            }
        }
        return 0;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        DailyQcUtils.points_flag = 1;
        DailyQcUtils.grids_flag = -1;
        DailyQcUtils.map_flag = -1;
        DailyQcUtils.contour_flag = -1;
        if (DailyQcUtils.pdata == null || (DailyQcUtils.pdata.length <= 0)) {
            Date currDate = ChooseDataPeriodDialog.getCurrentHydroEditDate();
            String QcArea = ChooseDataPeriodDialog.prevArea;
            int qcDays = MPEDisplayManager.getCurrent().getDqcDays();
            // checks to see if area or date has changed since last data load
            dqc_good = dqc.qcDataReload(currDate, QcArea, qcDays, false);
        }
        dataSet.clear();
        dataSet.addAll(dataType);

        DailyQcUtils.pcp_flag = 3;
        DailyQcUtils.pcpn_day = 0;
        DailyQcUtils.pcpn_time = 0;

        for (int i = 0; i < 8; i++) {
            if (MPEDisplayManager.pcpn_time_step == 0) {
                time_pos = DailyQcUtils.pcp_flag;
            } else {
                time_pos = 40 + DailyQcUtils.pcpn_day;
            }

            if ((i != 0 && i != 7) && DailyQcUtils.pcp_in_use[time_pos] == -1) {
                dataSet.remove(dataSet.indexOf(dataType.get(i)));
            }
        }

        // ts = DailyQcUtils.ts;
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
        dataOptionsGroup.setText("Data Options");
        GridLayout groupLayout = new GridLayout(1, false);
        dataOptionsGroup.setLayout(groupLayout);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataOptionsGroup.setLayoutData(gd);
        final Shell shell = this.getParent();
        final Cursor prevCursor = shell.getCursor();
        final Cursor waitCursor = Display.getDefault()
                .getSystemCursor(SWT.CURSOR_WAIT);

        if (MPEDisplayManager.pcpn_time_step != 1) {
            MPEDisplayManager.pcpn_time_step = 1;
        }

        // Create a container to hold the label and the combo box.
        Composite six24Comp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout six24CompLayout = new GridLayout(3, false);
        six24CompLayout.marginHeight = 0;
        six24CompLayout.marginWidth = 0;
        six24Comp.setLayout(six24CompLayout);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        six24Comp.setLayoutData(gd);

        Label six24Lbl = new Label(six24Comp, SWT.CENTER);
        six24Lbl.setText("6/24 Hour:");

        GridData sd = new GridData(SWT.FILL, SWT.CENTER, true, false);
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

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(six24Comp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.HORIZONTAL);
        timeArrowsComp.setLayout(timeArrowRl);

        RowData rd = new RowData(SWT.DEFAULT, SWT.DEFAULT);
        upTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.UP);
        upTimeBtn.setLayoutData(rd);
        upTimeBtn.setEnabled(false);
        upTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.chg_precip_time(0);
            }
        });

        rd = new RowData(SWT.DEFAULT, SWT.DEFAULT);
        dnTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnTimeBtn.setLayoutData(rd);
        dnTimeBtn.setEnabled(false);
        dnTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.chg_precip_time(1);
            }
        });

        GridData dd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        String[] a = new String[dataSet.size()];
        dataDispCbo = new Combo(dataOptionsGroup,
                SWT.DROP_DOWN | SWT.READ_ONLY);
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

        final Composite renderComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout renderCompLayout = new GridLayout(2, false);
        renderCompLayout.marginHeight = 0;
        renderCompLayout.marginWidth = 0;
        renderComp.setLayout(renderCompLayout);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        renderComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        renderGridsBtn = new Button(renderComp, SWT.PUSH);
        renderGridsBtn.setText("Render Grids+MAPs");
        renderGridsBtn.setLayoutData(gd);
        if (DailyQcUtils.pcp_in_use[time_pos] == -1
                && DailyQcUtils.pdata[i].used[4] != 0) {
            renderGridsBtn.setEnabled(true);
        } else {
            renderGridsBtn.setEnabled(false);
        }
        renderGridsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                renderComp.setCursor(waitCursor);
                opo.render_options(0);
                renderGridsBtn.setEnabled(false);
                renderComp.setCursor(prevCursor);
            }
        });

        GridData bd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        groupEditBtn = new Button(renderComp, SWT.PUSH);
        groupEditBtn.setText("Group Edit");
        groupEditBtn.setLayoutData(bd);
        final IGroupEditHandler handler = this;
        groupEditBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                GroupEditStationsDialog groupDialog = new GroupEditStationsDialog(
                        shell, handler);
                groupDialog.open();
            }
        });

        Composite pcpTypeComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout pcpTypeCompLayout = new GridLayout(2, false);
        pcpTypeCompLayout.marginHeight = 0;
        pcpTypeCompLayout.marginWidth = 0;
        pcpTypeComp.setLayout(pcpTypeCompLayout);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pcpTypeComp.setLayoutData(gd);

        Label pcpLbl = new Label(pcpTypeComp, SWT.CENTER);
        pcpLbl.setText("Precip Type:");

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        pcpTypeCbo = new Combo(pcpTypeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pcpTypeCbo.setTextLimit(30);
        pcpTypeCbo.setLayoutData(gd);
        pcpTypeCbo.add("Rain/Snow");
        pcpTypeCbo.add(ALL);
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
        pointTypeGroup.setText("Point Type");
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

        Button nexChk = new Button(chkBxComp, SWT.CHECK);
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
            bname.setText(DailyQcUtils.ts[m].name);
            bname.setData(m + 1);
            bname.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    gq.source_Select(((Integer) bname.getData()));
                }
            });
            tsbuttons[m + 1] = bname;
        }

        Button allChk = new Button(chkBxComp, SWT.CHECK);
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
        pointQualGroup.setText("Point Quality");
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
        if (dqc.mpe_show_missing_gage.length() > 0) {
            if ((ALL.equalsIgnoreCase(dqc.mpe_show_missing_gage)) || ("Reported"
                    .equalsIgnoreCase(dqc.mpe_show_missing_gage))) {
                mpe_show_missing_gage_set = true;
            } else {
                mpe_show_missing_gage_set = false;
            }

        } else {
            mpe_show_missing_gage_set = false;
        }

        if (mpe_show_missing_gage_set) {
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
                "Questionable", "Partial", "Estimated", "Bad", "Missing", ALL };

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

            if (qflag[dqc.funct[i]] == 1) {
                qsbuttons[i].setSelection(true);
            } else {
                qsbuttons[i].setSelection(false);
            }
        }
    }

    private void createPointSetComp() {
        Composite pntSetComp = new Composite(shell, SWT.NONE);
        GridLayout pntSetCompGl = new GridLayout(2, true);
        pntSetComp.setLayout(pntSetCompGl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pntSetComp.setLayoutData(gd);

        Label pntCharLbl = new Label(pntSetComp, SWT.CENTER);
        pntCharLbl.setText("Point character:");
        DailyQcUtils.gage_char[0] = 1;
        DailyQcUtils.gage_char[1] = 1;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
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

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
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
        if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stddev == 5.0) {
            i = 0;
        } else if (DailyQcUtils.pdata[DailyQcUtils.pcpn_day].stddev == 3.0) {
            i = 1;
        } else {
            i = 2;
        }

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
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

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        pntTConCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntTConCbo.setTextLimit(30);
        pntTConCbo.setLayoutData(gd);
        pntTConCbo.add("Consistent");
        pntTConCbo.add("Inconsistent");
        pntTConCbo.add(ALL);
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

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        pntSConCbo = new Combo(pntSetComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pntSConCbo.setTextLimit(30);
        pntSConCbo.setLayoutData(gd);
        pntSConCbo.add("Consistent");
        pntSConCbo.add("Inconsistent");
        pntSConCbo.add(ALL);
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

        Label pfvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pfvalueLabel.setLayoutData(gd);
        pfvalueLabel.setAlignment(SWT.LEFT);
        pfvalueLabel.setText("0.00");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Scale pntFilter = new Scale(pntControlComp, SWT.BORDER);
        pntFilter.setMinimum(0);
        pntFilter.setMaximum(500);
        pntFilter.setIncrement(1);
        pntFilter.setPageIncrement(1);
        pntFilter.setSelection(0);
        pntFilter.setLayoutData(gd);
        pntFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pntFilter.getSelection();
                float filterVal = sel / 100.0f;
                pfvalueLabel.setText(String.format("%4.2f", filterVal));
                dqc.setPrecipFilterValue(filterVal);
            }
        });
        pntFilter.addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                opo.refresh_exposure();
            }

        });

        /**
         * Add a key listener for up and down arrows to move up and down through
         * the filter scale
         */

        pntFilter.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_RIGHT) {
                    opo.refresh_exposure();
                } else if (e.keyCode == SWT.ARROW_LEFT) {
                    opo.refresh_exposure();
                }

            }

        });

        /**
         * Point reverse filter scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pntRevFilterLbl = new Label(pntControlComp, SWT.NONE);
        pntRevFilterLbl.setText("Point reverse filter  (inches):");
        pntRevFilterLbl.setLayoutData(gd);

        Label prvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prvalueLabel.setLayoutData(gd);
        prvalueLabel.setAlignment(SWT.LEFT);
        prvalueLabel.setText("20.00");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Scale pntRevFilter = new Scale(pntControlComp, SWT.BORDER);
        pntRevFilter.setMinimum(0);
        pntRevFilter.setMaximum(2000);
        pntRevFilter.setIncrement(1);
        pntRevFilter.setPageIncrement(1);
        pntRevFilter.setSelection(0);
        pntRevFilter.setLayoutData(gd);
        pntRevFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = 2000 - pntRevFilter.getSelection();
                float filterVal = sel / 100.0f;
                prvalueLabel.setText(String.format("%4.2f", filterVal));
                dqc.setPrecipReverseFilterValue(filterVal);
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
         * Add a key listener for up and down arrows to move up and down through
         * the reverse filter scale
         */
        pntRevFilter.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_RIGHT) {
                    opo.refresh_exposure();
                } else if (e.keyCode == SWT.ARROW_LEFT) {
                    opo.refresh_exposure();
                }

            }

        });

        /**
         * Point elevation scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pntElFilterLbl = new Label(pntControlComp, SWT.NONE);
        pntElFilterLbl.setText("Point elevation  (feet):");
        pntElFilterLbl.setLayoutData(gd);

        Label pevalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pevalueLabel.setLayoutData(gd);
        pevalueLabel.setAlignment(SWT.LEFT);
        pevalueLabel.setText("0");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Scale pntElFilter = new Scale(pntControlComp, SWT.BORDER);
        pntElFilter.setMinimum(0);
        pntElFilter.setMaximum(15_000);
        pntElFilter.setIncrement(100);
        pntElFilter.setSelection(0);
        pntElFilter.setLayoutData(gd);
        pntElFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pntElFilter.getSelection();
                pevalueLabel.setText(String.format("%d", sel));
                dqc.setPointElevationFilterValue(sel);

            }
        });
        pntElFilter.addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                int sel = pntElFilter.getSelection();
                DailyQcUtils.getInstance().setPointElevationFilterValue(sel);
                opo.refresh_exposure();
            }

        });
        pntElFilter.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.ARROW_LEFT
                        || e.keyCode == SWT.ARROW_RIGHT) {
                    int sel = pntElFilter.getSelection();
                    DailyQcUtils.getInstance()
                            .setPointElevationFilterValue(sel);
                    opo.refresh_exposure();
                }
            }
        });

        /**
         * Pxtemp scale
         */
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label pxTempFilterLbl = new Label(pntControlComp, SWT.NONE);
        pxTempFilterLbl.setText("Pxtemp  (deg C):");
        pxTempFilterLbl.setLayoutData(gd);

        Label pxvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pxvalueLabel.setLayoutData(gd);
        pxvalueLabel.setAlignment(SWT.LEFT);
        pxvalueLabel.setText("1.00");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Scale pxTempFilter = new Scale(pntControlComp, SWT.BORDER);
        pxTempFilter.setMinimum(0);
        pxTempFilter.setMaximum(400);
        pxTempFilter.setIncrement(10);
        pxTempFilter.setSelection(200);
        pxTempFilter.setLayoutData(gd);
        pxTempFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = pxTempFilter.getSelection() - 100;
                float value = sel / 100.0f;
                pxvalueLabel.setText(String.format("%4.2f", value));
            }
        });
        pxTempFilter
                .addMouseListener(new org.eclipse.swt.events.MouseAdapter() {
                    @Override
                    public void mouseUp(MouseEvent e) {
                        int sel = pxTempFilter.getSelection() - 100;
                        dqc.pxtemp = (float) sel / 100;
                        dqc.dmvalue = (int) (dqc.pxtemp * 100 * 3.28 / .55);
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
        dqc.setPointElevationFilterValue(pntElFilter.getSelection());
        dqc.pxtemp = (pxTempFilter.getSelection() - 100) / 100;
        dqc.dmvalue = (int) (dqc.pxtemp * 100 * 3.28 / .55);

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

    @Override
    public void handleGroupEdit() {
        new GroupEditCalls().apply_group();
    }

    public void reexposeDqc() {
        this.ddq.reexposeDQC();
    }

    public void reloadDqc() {
        this.ddq.reloadDQC();
    }
}
