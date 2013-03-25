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
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
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
import com.raytheon.viz.mpe.ui.actions.OtherFreezeOptions;
import com.raytheon.viz.mpe.ui.actions.OtherPrecipOptions;
import com.raytheon.viz.mpe.ui.actions.SaveLevel2Data;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Ts;
import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul, 7 2009             snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class QcFreezeOptionsDialog extends AbstractMPEDialog {

    public static Combo freezeTimeCbo;

    public static Combo dataDispCbo;

    /**
     * Font used for controls.
     */
    private Font font;

    public static Button upTimeBtn;

    public static Button dnTimeBtn;

    public static Button renderGridsBtn;

    public static Button groupEditBtn;

    public static Combo filterZTypeCbo;

    public static Combo pntDispCbo;

    public static Combo pntScnCbo;

    private static Scale pntFilter;

    private static Scale pntRevFilter;

    private Label pfvalueLabel;

    private Label prvalueLabel;

    public static boolean isOpen = false;

    private static boolean isfinished = true;

    private int dqc_good = 0;

    public static DrawDQCStations ddqc;

    public static ArrayList<String> dataType = new ArrayList<String>();

    public static ArrayList<String> dataSet = new ArrayList<String>();

    OtherPrecipOptions opo = new OtherPrecipOptions();

    OtherFreezeOptions ozo = new OtherFreezeOptions();

    Zdata[] zdata = new Zdata[0];

    Ts[] ts;

    private int time_pos;

    public static Button[] tsbuttons = null;

    public static Button[] qsbuttons = new Button[10];

    final GageQcSelect gq = new GageQcSelect();

    int i = 0;

    private Label pxvalueLabel;

    private static Scale pxTempFilter;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public QcFreezeOptionsDialog(Shell parent) {
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
        dataType.add("MAZs");
        dataType.add("Points+Grids");
        dataType.add("Points+MAZs");
        dataType.add("Contours");
        dataType.add("Points+Contours");
        dataType.add("None");
        dataSet.addAll(dataType);
        if (displayMgr.isQpf()) {
            QcPrecipOptionsDialog.destroy(false);
            displayMgr.setQpf(false);
        }
        if (displayMgr.isMaxmin()) {
            QcTempOptionsDialog.destroy(false);
            displayMgr.setMaxmin(false);
        }

        displayMgr.setZflag(true);
        ddqc = DrawDQCStations.getInstance();

        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS);

        shell.setText("QC Freeze Options");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        shell.setLayout(mainLayout);

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    mgr.removePespectiveDialog(QcFreezeOptionsDialog.this);
                }
            }
        });

        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);
        if (MPEDisplayManager.pcpn_time_step != 0) {
            MPEDisplayManager.pcpn_time_step = 0;
            DailyQcUtils.pcpn_time = 0;
            DailyQcUtils.pcp_flag = 3 + DailyQcUtils.pcpn_day * 4;
        }
        // Initialize all of the controls and layouts
        this.initializeComponents();

        shell.pack();

        shell.open();
        displayMgr.setZflag(true);
        isOpen = true;
        isfinished = false;
        ozo.chg_freeze_time(2);
        opo.send_expose();
        while (!shell.isDisposed()) {
            if (dqc_good == 0) {
                displayMgr.setZflag(false);
                isOpen = false;
                ddqc.destroy();
                shell.dispose();
            }
            if (isOpen == false) {
                displayMgr.setZflag(false);
                ddqc.destroy();
                shell.dispose();
            }
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        ddqc.destroy();
        displayMgr.setZflag(false);
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
        zdata = DailyQcUtils.zdata;
        DailyQcUtils.points_flag = 1;
        DailyQcUtils.grids_flag = -1;
        DailyQcUtils.map_flag = -1;
        DailyQcUtils.contour_flag = -1;
        if (zdata == null || (zdata.length <= 0)) {
            Date currDate = ChooseDataPeriodDialog.prevDate;
            String QcArea = ChooseDataPeriodDialog.prevArea;
            int qcDays = MPEDisplayManager.getCurrent().getDqcDays();
            // checks to see if area or date has changed since last data load
            DailyQcUtils dqcu = new DailyQcUtils();
            dqc_good = dqcu.qcDataReload(currDate, QcArea, qcDays, false);
            zdata = DailyQcUtils.zdata;
            if (MPEDisplayManager.pcpn_time_step != 0) {
                MPEDisplayManager.pcpn_time_step = 0;
                DailyQcUtils.pcpn_time = 0;
                DailyQcUtils.pcp_flag = 3 + DailyQcUtils.pcpn_day * 4;
            }

        }
        dataSet.clear();
        dataSet.addAll(dataType);

        DailyQcUtils.pcp_flag = 3;
        DailyQcUtils.pcpn_day = 0;
        DailyQcUtils.pcpn_time = 0;

        for (i = 0; i < 8; i++) {

            time_pos = 100 + DailyQcUtils.pcp_flag;

            if ((i != 0 && i != 7) && DailyQcUtils.pcp_in_use[time_pos] == -1) {
                dataSet.remove(dataSet.indexOf(dataType.get(i)));
            }
        }

        ts = DailyQcUtils.ts;
        this.createDataOptionsGroup();
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
        Composite freezeTimeComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout freezeTimeCompLayout = new GridLayout(3, false);
        freezeTimeCompLayout.marginHeight = 0;
        freezeTimeCompLayout.marginWidth = 0;
        freezeTimeComp.setLayout(freezeTimeCompLayout);

        Label freezeTimeLbl = new Label(freezeTimeComp, SWT.CENTER);
        freezeTimeLbl.setText(" 6 Hour ");

        // Add the time arrow buttons
        Composite timeArrowsComp = new Composite(freezeTimeComp, SWT.NONE);
        RowLayout timeArrowRl = new RowLayout(SWT.HORIZONTAL);
        timeArrowsComp.setLayout(timeArrowRl);

        RowData rd = new RowData(25, 25);
        upTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.UP);
        upTimeBtn.setLayoutData(rd);
        upTimeBtn.setEnabled(false);
        upTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ozo.chg_freeze_time(0);
            }
        });

        rd = new RowData(25, 25);
        dnTimeBtn = new Button(timeArrowsComp, SWT.ARROW | SWT.DOWN);
        dnTimeBtn.setLayoutData(rd);
        dnTimeBtn.setEnabled(false);
        dnTimeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ozo.chg_freeze_time(1);
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

        time_pos = 100 + DailyQcUtils.pcp_flag;
        int ii = dataSet.indexOf((dataType.get(i)));
        dataDispCbo.select(ii);
        dataDispCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                opo.display_pcpn_options(dataDispCbo.getSelectionIndex());
            }
        });

        Composite renderComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout renderCompLayout = new GridLayout(1, false);
        renderCompLayout.marginHeight = 0;
        renderCompLayout.marginWidth = 0;
        renderComp.setLayout(renderCompLayout);

        gd = new GridData(153, 25);
        renderGridsBtn = new Button(renderComp, SWT.PUSH);
        renderGridsBtn.setText("Render Grids+MAZs");
        renderGridsBtn.setLayoutData(gd);
        if (DailyQcUtils.pcp_in_use[time_pos] == -1
                && DailyQcUtils.zdata[i].used[DailyQcUtils.pcpn_time] != 0) {
            renderGridsBtn.setEnabled(true);
        } else {
            renderGridsBtn.setEnabled(false);
        }
        renderGridsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setCursor(waitCursor);
                opo.render_options(2);
                shell.setCursor(prevCursor);
                renderGridsBtn.setEnabled(false);
            }
        });

        Composite filterTypeComp = new Composite(dataOptionsGroup, SWT.NONE);
        GridLayout filterTypeCompLayout = new GridLayout(2, false);
        filterTypeCompLayout.marginHeight = 0;
        filterTypeCompLayout.marginWidth = 0;
        filterTypeComp.setLayout(filterTypeCompLayout);

        Label pcpLbl = new Label(filterTypeComp, SWT.CENTER);
        pcpLbl.setText("Filter Z:");

        gd = new GridData(190, SWT.DEFAULT);
        filterZTypeCbo = new Combo(filterTypeComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        filterZTypeCbo.setTextLimit(30);
        filterZTypeCbo.setLayoutData(gd);
        filterZTypeCbo.add("Above");
        filterZTypeCbo.add("Below");
        filterZTypeCbo.add("All");
        filterZTypeCbo.select(OtherFreezeOptions.abmode);
        filterZTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ozo.change_abmode(filterZTypeCbo.getSelectionIndex());
            }
        });
        filterZTypeCbo.setEnabled(false);
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
        pntDispCbo.select(3);
        pntDispCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                gq.change_Plot(pntDispCbo.getSelectionIndex() + 1);
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
        pntFilterLbl.setText("Point filter  (1000s ft):");
        pntFilterLbl.setLayoutData(gd);

        pfvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        pfvalueLabel.setLayoutData(gd);
        pfvalueLabel.setAlignment(SWT.LEFT);
        pfvalueLabel.setText("0.0");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntFilter = new Scale(pntControlComp, SWT.BORDER);
        pntFilter.setMinimum(0);
        pntFilter.setMaximum(200);
        pntFilter.setSelection(0);
        pntFilter.setLayoutData(gd);
        pntFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = (pntFilter.getSelection());
                pfvalueLabel.setText(String.format("%3.1f", sel / 10.0));
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
        pntRevFilterLbl.setText("Point reverse filter (1000s ft):");
        pntRevFilterLbl.setLayoutData(gd);

        prvalueLabel = new Label(pntControlComp, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        prvalueLabel.setLayoutData(gd);
        prvalueLabel.setAlignment(SWT.LEFT);
        prvalueLabel.setText("20.0");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        pntRevFilter = new Scale(pntControlComp, SWT.BORDER);
        pntRevFilter.setMinimum(0);
        pntRevFilter.setMaximum(200);
        pntRevFilter.setIncrement(1);
        pntRevFilter.setSelection(0);
        pntRevFilter.setLayoutData(gd);
        pntRevFilter.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int sel = (200 - pntRevFilter.getSelection());
                prvalueLabel.setText(String.format("%3.1f", sel / 10.0));
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

        OtherPrecipOptions.change_pcpn_flag = -1;
        OtherPrecipOptions.change_rpcpn_flag = -1;
        OtherPrecipOptions.change_topo_flag = -1;
        OtherPrecipOptions.change_frz_flag = 1;
        OtherPrecipOptions.change_maxmin_flag = -1;

        // initialize the gage filter values
        // DailyQcUtils.elevation_filter_value = pntElFilter.getSelection();
        pntFilter.setSelection(0);
        pntRevFilter.setSelection(0);
        DailyQcUtils.pxtemp = (pxTempFilter.getSelection() - 100) / 100;
        DailyQcUtils.dmvalue = (int) (DailyQcUtils.pxtemp * 100 * 3.28 / .55);

        opo.send_expose();
        OtherFreezeOptions ozo = new OtherFreezeOptions();
        ozo.set_freeze_arrow_sensitivity();

    }

    public static float getPointFilterValue() {
        float sel = pntFilter.getSelection() / 10.0f;
        return sel;
    }

    public static float getPointFilterReverseValue() {
        float sel = 200 - pntRevFilter.getSelection() / 10.0f;
        return sel;
    }

    public static void destroy(boolean t) {
        isOpen = t;
    }

    public static boolean isFinished() {
        return isfinished;
    }

}
