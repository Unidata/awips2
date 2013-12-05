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
package com.raytheon.uf.viz.monitor.scan.tables;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.TimeZone;
import java.util.TimerTask;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.monitor.scan.config.AbsConfigMgr;
import com.raytheon.uf.common.monitor.scan.config.DmdConfigMgr;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.DmdDisplayFilterDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.DrawSettings;
import com.raytheon.uf.viz.monitor.scan.commondialogs.EditCreateTrendDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IAttributeUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IDisplayFilterUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IThresholdUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAlarmThreshDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAlarmTimeLimitDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAttributesDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANColorThreshDlg;
import com.raytheon.uf.viz.monitor.scan.data.ScanDataGenerator;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager.AlarmType;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Main dialog for the SCAN DMD table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2010            lvenable     Initial creation
 * 
 * 03/15/2012	13939	   Mike Duff    For a SCAN Alarms issue
 * Apr 29, 2013 #1945      lvenable    Improved SCAN performance, reworked
 *                                     some bad code, and some code cleanup.
 * 06 Jun 2013  #2065      lvenable    Added code to alert the user to use the clear
 *                                     button if they want to close the dialog.
 * Jul 24, 2013  2218      mpduff      Change method signature.
 * Jul 26, 2013 #2143      skorolev    Changes for non-blocking dialogs.
 * Aug 15, 2013  2143      mpduff      Added some isDisposed() checks.
 * 04 Dec 2013  #2592      lvenable    Update how the checkboxes are handled
 *                                     (background/foreground colors) since the Redhat
 *                                     6 upgrade causes the check in the checkbox to be
 *                                     colored the same as the background.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANDmdTableDlg extends AbstractTableDlg implements
        IAttributeUpdate, IThresholdUpdate, IDisplayFilterUpdate,
        IRequestTimeHeightData {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SCANDmdTableDlg.class);

    /**
     * Flag indicating if the dialog should be killed.
     */
    private boolean killDialog = false;

    /**
     * Configuration button.
     */
    private Button configBtn;

    /**
     * Rank button.
     */
    private Button rankBtn;

    /**
     * Attributes button.
     */
    private Button attribBtn;

    /**
     * Link to Frame check box.
     */
    private Button linkToFrameChk;

    /**
     * CWA filter check box.
     */
    private Button cwaFilterChk;

    /**
     * Vertical check box.
     */
    private Button vertChk;

    /**
     * Tips check box.
     */
    private Button tipsChk;

    /**
     * Alarm button.
     */
    private Button alarmBtn;

    /**
     * Elevation label (displays current elevation).
     */
    private Label elevationLbl;

    /**
     * Time label displaying the scan time.
     */
    private Label timeLbl;

    /**
     * DMD table composite containing the data table.
     */
    private SCANDmdTableComp dmdTableComp;

    /**
     * Data to be displayed in the table.
     */
    private final SCANTableData tableData;

    /**
     * Popup menu for the File button.
     */
    private Menu filePopupMenu;

    /**
     * Configuration popup menu.
     */
    private Menu configPopupMenu;

    /**
     * Rank popup menu.
     */
    private Menu rankPopupMenu;

    /**
     * Attribute dialog.
     */
    private SCANAttributesDlg attributeDlg;

    /**
     * Color threshold dialog.
     */
    private SCANColorThreshDlg colorThresholdDlg;

    /**
     * Alarm threshold dialog.
     */
    private SCANAlarmThreshDlg alarmThreshDlg;

    /**
     * Alarm time limit dialog.
     */
    private SCANAlarmTimeLimitDlg alarmTimeLimitDlg;

    /**
     * Create/Edit trend dialog.
     */
    private EditCreateTrendDlg editTrendDlg;

    /**
     * Filter dialog.
     */
    private DmdDisplayFilterDlg displayFilterDlg;

    /**
     * Alarm dialog
     */
    private SCANAlarmsDlg alarmsDlg;

    /**
     * Define active trend set menu item.
     */
    private MenuItem defineActiveTrendSetMI;

    /**
     * Define active trend set menu.
     */
    private Menu defineActiveTrendSetMenu;

    /**
     * The current trend selected.
     */
    private String selectedTrend = "default";

    /**
     * Color of the elevation label.
     */
    private Color elevationLabelColor;

    /**
     * current time.
     */
    private Date currentTime = null;

    /**
     * Current elevation.
     */
    private Double currentElevation = null;

    /**
     * Current elevation.
     */
    private Double currentTrueElevation = null;

    /**
     * Time Height Graph options.
     */
    private DrawSettings drawSettings = new DrawSettings();

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param site
     *            Site name.
     * @param tableData
     *            Data displayed in the table.
     */
    public SCANDmdTableDlg(Shell parentShell, String site,
            SCANTableData tableData) {
        super(parentShell);
        this.site = site;
        this.tableData = tableData;
        mgr = SCANAlarmAlertManager.getInstance(site);
        open();
    }

    /**
     * Set the table type.
     */
    @Override
    protected void setTableType() {
        scanTable = ScanTables.DMD;
    }

    /**
     * Initialize the components.
     */
    @Override
    protected void initComponents() {
        elevationLabelColor = new Color(display, 250, 220, 165);

        createTopControls();
        createDmdTable();

        createFilePopupMenu();
        createConfigurationsPopupMenu();
        newUpRankPopUpMenu();
        createRankPopupMenu(rankPopupMenu, rankBtn);

        scanCfg = SCANConfig.getInstance();
        AbsConfigMgr absCfgMgr = scanCfg.getAbsConfigMgr(scanTable);
        String[] attributes = absCfgMgr.getAlarmAttributes();
        for (int i = 0; i < attributes.length; i++) {
            int val = absCfgMgr.getAbsoluteValue(attributes[i]);
            mgr.updateScheduledAlarm(site, scanTable, attributes[i],
                    AlarmType.AbsVal, val);
        }
    }

    private void addAlarmTimer() {
        if (dmdTableComp.timer != null) {
            timerTask = new TimerTask() {
                @Override
                public void run() {
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
                        public void run() {
                            if (shell.isDisposed()) {
                                dmdTableComp.timer.cancel();
                                mgr.clearScheduledAlarms(site, scanTable);
                            } else if (alarmBtn.isVisible()) {
                                setBlinkColor();
                                alarmBtn.setBackground(blinkColor);
                            }
                        }
                    });
                }
            };
            dmdTableComp.timer.schedule(timerTask, 0, 1000);
        }
    }

    /**
     * Create the control at the top of the dialog.
     */
    private void createTopControls() {
        SCANConfig scanCfg = SCANConfig.getInstance();
        DmdConfigMgr dmdConfigMgr = (DmdConfigMgr) scanCfg
                .getAbsConfigMgr(scanTable);

        Composite controlComp = new Composite(shell, SWT.PUSH);
        GridLayout gl = new GridLayout(12, false);
        gl.horizontalSpacing = 3;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(100, SWT.DEFAULT);
        fileBtn = new Button(controlComp, SWT.PUSH);
        fileBtn.setText("File");
        displayFileButtonToolTip();
        fileBtn.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        fileBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        fileBtn.setLayoutData(gd);
        fileBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetButtonForegroundColor(fileBtn);
                Point controlLoc = display.map(fileBtn, null, e.x, e.y
                        + fileBtn.getSize().y);
                filePopupMenu.setLocation(controlLoc);
                filePopupMenu.setVisible(true);
            }
        });
        setupButtonMouseListeners(fileBtn);

        fileBtn.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                displayFileButtonToolTip();
            }
        });

        gd = new GridData();
        configBtn = new Button(controlComp, SWT.PUSH);
        configBtn.setText("Configurations");
        configBtn
                .setBackground(scanCfg.getScanColor(ScanColors.Configurations));
        configBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        configBtn.setLayoutData(gd);
        configBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetButtonForegroundColor(configBtn);
                Point controlLoc = display.map(configBtn, null, e.x, e.y
                        + configBtn.getSize().y);
                configPopupMenu.setLocation(controlLoc);
                configPopupMenu.setVisible(true);
            }
        });
        setupButtonMouseListeners(configBtn);

        gd = new GridData(100, SWT.DEFAULT);
        rankBtn = new Button(controlComp, SWT.PUSH);
        rankBtn.setText("Rank: " + scanCfg.getDefaultRank(ScanTables.DMD));
        rankBtn.setBackground(scanCfg.getScanColor(ScanColors.Rank));
        rankBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        rankBtn.setLayoutData(gd);
        rankBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetButtonForegroundColor(rankBtn);
                Point controlLoc = display.map(rankBtn, null, e.x, e.y
                        + rankBtn.getSize().y);
                rankPopupMenu.setLocation(controlLoc);
                rankPopupMenu.setVisible(true);
            }
        });
        setupButtonMouseListeners(rankBtn);

        gd = new GridData();
        attribBtn = new Button(controlComp, SWT.PUSH);
        attribBtn.setText("Attributes");
        attribBtn.setBackground(scanCfg.getScanColor(ScanColors.Attributes));
        attribBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        attribBtn.setLayoutData(gd);
        attribBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetButtonForegroundColor(attribBtn);
                displayAttributesDialog();
            }
        });
        setupButtonMouseListeners(attribBtn);

        /*
         * Link to frame
         */
        linkToFrameChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.LinkToFrame),
                display.getSystemColor(SWT.COLOR_WHITE), "Link to Frame ",
                true, null);

        linkToFrameChk.setSelection(dmdConfigMgr.getScanDmdCfgXML()
                .getLinkToFrame());

        linkToFrameChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleLinkToFrame();
            }
        });

        /*
         * CWA Filter
         */
        StringBuilder tipText = new StringBuilder();
        tipText.append("Activate to remove from the SCAN table all cells and\n");
        tipText.append("DMDs that are outside your CWA.\n\n");
        tipText.append("Deactivate to include in the SCAN table all cells and\n");
        tipText.append("DMDs detected by radar.");

        cwaFilterChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.CWAFilter),
                display.getSystemColor(SWT.COLOR_WHITE), "CWA Filter ", true,
                tipText.toString());

        cwaFilterChk.setSelection(dmdConfigMgr.getScanDmdCfgXML()
                .getFilterOption());
        cwaFilterChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCWAFilterAction();
            }
        });

        /*
         * Vertical - tech blocked
         */
        vertChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Vert),
                display.getSystemColor(SWT.COLOR_WHITE), "Vert ", true, null);

        vertChk.setSelection(dmdConfigMgr.getScanDmdCfgXML().getFilterOption());
        vertChk.setEnabled(false);

        /*
         * Tool tips
         */
        tipsChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Tips),
                display.getSystemColor(SWT.COLOR_WHITE), "Tips ", true, null);

        tipsChk.setSelection(dmdConfigMgr.getScanDmdCfgXML().getTipsOption());

        tipsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                SCANConfig.getInstance().setShowTips(scanTable,
                        tipsChk.getSelection());
                dmdTableComp.updateColumnTips();
            }
        });

        /*
         * Alarm button
         */
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        alarmBtn = new Button(controlComp, SWT.PUSH);
        alarmBtn.setText("Alarm");
        alarmBtn.setBackground(display.getSystemColor(SWT.COLOR_RED));
        alarmBtn.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        alarmBtn.setLayoutData(gd);
        alarmBtn.setVisible(false);
        alarmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (alarmsDlg == null || alarmsDlg.isDisposed()) {
                    alarmsDlg = new SCANAlarmsDlg(shell, ScanTables.DMD, site);
                    alarmsDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            if (!alarmBtn.isDisposed()
                                    && mgr.getAlertedAlarms(site, scanTable)
                                            .isEmpty()) {
                                turnOffAlarm();
                            }
                        }

                    });
                    alarmsDlg.open();
                } else {
                    alarmsDlg.close();
                }

            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 135;
        elevationLbl = new Label(controlComp, SWT.CENTER | SWT.BORDER);
        elevationLbl.setText("* No Elevation *");
        elevationLbl.setBackground(elevationLabelColor);
        elevationLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 160;
        timeLbl = new Label(controlComp, SWT.RIGHT);
        timeLbl.setText("* No Date/Time *");
        timeLbl.setLayoutData(gd);
    }

    /**
     * Create the DMD table.
     */
    private void createDmdTable() {
        dmdTableComp = new SCANDmdTableComp(shell, tableData, this, this, this,
                site);
    }

    /**
     * New up the rank popup menu.
     */
    private void newUpRankPopUpMenu() {
        if (rankPopupMenu != null) {
            rankPopupMenu.dispose();
        }
        rankPopupMenu = new Menu(rankBtn);
    }

    /**
     * Create the File popup menu.
     */
    private void createFilePopupMenu() {
        filePopupMenu = new Menu(fileBtn);

        MenuItem retDefaultMI = new MenuItem(filePopupMenu, SWT.NONE);
        retDefaultMI.setText("Retrieve Default DMD Configuration");
        retDefaultMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                retrieveDefaultConfig();
            }
        });

        MenuItem retDmdConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        retDmdConfMI.setText("Retrieve DMD Configuration...");
        retDmdConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                retrieveExistingConfig();
            }
        });

        MenuItem saveDmdConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveDmdConfMI.setText("Save DMD Configuration");
        saveDmdConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveCurrentConfiguration();
            }
        });

        MenuItem saveAsDmdConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveAsDmdConfMI.setText("Save DMD Configuration As...");
        saveAsDmdConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveConfigurationAs();
            }
        });

        fileBtn.setMenu(filePopupMenu);
    }

    /**
     * Create the Configurations popup menu.
     */
    private void createConfigurationsPopupMenu() {
        configPopupMenu = new Menu(configBtn);

        /*
         * D2D Display menu
         */
        MenuItem d2dDisplayMI = new MenuItem(configPopupMenu, SWT.NONE);
        d2dDisplayMI.setText("D2D Display...");
        d2dDisplayMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayFilterDialog();
            }
        });

        /*
         * Alarm Thresholds menu
         */
        MenuItem alarmThreshMI = new MenuItem(configPopupMenu, SWT.NONE);
        alarmThreshMI.setText("Alarm Thresholds...");
        alarmThreshMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayAlarmThresholdDialog();
            }
        });

        /*
         * Trend Sets menu and sub menus
         */
        MenuItem trendSetsMI = new MenuItem(configPopupMenu, SWT.CASCADE);
        trendSetsMI.setText("Trend Sets");

        Menu trendSetsMenu = new Menu(shell, SWT.DROP_DOWN);
        trendSetsMI.setMenu(trendSetsMenu);

        MenuItem createEditTrendMI = new MenuItem(trendSetsMenu, SWT.NONE);
        createEditTrendMI.setText("Create/Edit Trend Set...");
        createEditTrendMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayCreateEditTrendDialog();
            }
        });

        defineActiveTrendSetMI = new MenuItem(trendSetsMenu, SWT.CASCADE);
        defineActiveTrendSetMI.setText("Define Active Trend Set");

        updateDefineActiveTrendMenu();

        /*
         * Alarm Time Setup
         */
        MenuItem alarmTimeSetupMI = new MenuItem(configPopupMenu, SWT.NONE);
        alarmTimeSetupMI.setText("Alarm Time Setup...");
        alarmTimeSetupMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayAlarmTimeLimitDialog();
            }
        });

        /*
         * Box Colors
         */
        MenuItem boxColorsMI = new MenuItem(configPopupMenu, SWT.NONE);
        boxColorsMI.setText("Box Colors...");
        boxColorsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayColorThresholdDialog();
            }
        });

        configBtn.setMenu(configPopupMenu);
    }

    /**
     * Display the Create/Edit trend dialog.
     */
    private void displayCreateEditTrendDialog() {
        if (editTrendDlg == null || editTrendDlg.isDisposed()) {
            editTrendDlg = new EditCreateTrendDlg(shell, scanTable);
            editTrendDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    updateDefineActiveTrendMenu();
                    unregisterDialog(editTrendDlg);
                    editTrendDlg = null;
                }
            });
            registerDialog(editTrendDlg);
            editTrendDlg.open();
        } else {
            editTrendDlg.bringToTop();
        }
    }

    /**
     * Display the alert time limit dialog.
     */
    private void displayAlarmTimeLimitDialog() {
        if (alarmTimeLimitDlg == null || alarmTimeLimitDlg.isDisposed()) {
            alarmTimeLimitDlg = new SCANAlarmTimeLimitDlg(shell, scanTable,
                    this.site);
            alarmTimeLimitDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    unregisterDialog(alarmTimeLimitDlg);
                    alarmTimeLimitDlg = null;
                }

            });
            registerDialog(alarmTimeLimitDlg);
            alarmTimeLimitDlg.open();
        } else {
            alarmTimeLimitDlg.bringToTop();
        }
    }

    /**
     * Display the alarm threshold dialog.
     */
    private void displayAlarmThresholdDialog() {
        if (alarmThreshDlg == null || alarmThreshDlg.isDisposed()) {
            alarmThreshDlg = new SCANAlarmThreshDlg(site, shell, scanTable);
            alarmThreshDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    unregisterDialog(alarmThreshDlg);
                    alarmThreshDlg = null;
                }
            });
            registerDialog(alarmThreshDlg);
            alarmThreshDlg.open();
        } else {
            alarmThreshDlg.bringToTop();
        }
    }

    /**
     * Display the filter dialog.
     */
    private void displayFilterDialog() {
        if (displayFilterDlg == null || displayFilterDlg.isDisposed()) {
            displayFilterDlg = new DmdDisplayFilterDlg(shell, this);
            displayFilterDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    unregisterDialog(displayFilterDlg);
                    displayFilterDlg = null;
                }
            });
            registerDialog(displayFilterDlg);
            displayFilterDlg.open();
        } else {
            displayFilterDlg.bringToTop();
        }
    }

    /**
     * Update the elevation.
     */
    private void updateElevation() {
        if (currentElevation == null) {
            elevationLbl.setText("*** NO TIME ***");
            return;
        }

        elevationLbl.setText("Elevation: "
                + String.format("%3.1f", currentTrueElevation));
    }

    /**
     * Update the time.
     */
    private void updateTime() {
        if (currentTime == null) {
            timeLbl.setText("*** NO TIME ***");
            return;
        }

        SimpleDateFormat dateFmt = new SimpleDateFormat("E MMM dd HH:mm yyyy");
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        timeLbl.setText(dateFmt.format(currentTime));
    }

    /**
     * Update the define active trend menu.
     */
    private void updateDefineActiveTrendMenu() {
        if (defineActiveTrendSetMenu != null) {
            MenuItem[] items = defineActiveTrendSetMenu.getItems();

            for (MenuItem mi : items) {
                if (mi.getSelection() == true) {
                    selectedTrend = mi.getText();
                }
            }

            defineActiveTrendSetMenu.dispose();
            defineActiveTrendSetMenu = null;
        }

        defineActiveTrendSetMenu = new Menu(shell, SWT.DROP_DOWN);
        defineActiveTrendSetMI.setMenu(defineActiveTrendSetMenu);

        SCANConfig scancfg = SCANConfig.getInstance();
        LinkedHashMap<String, String> trendMap = scancfg.getTrendConfigMgr(
                scanTable).getTrendSetMap();

        if (trendMap.containsKey(selectedTrend) == false) {
            selectedTrend = "default";
        }

        Set<String> keys = trendMap.keySet();

        for (String key : keys) {
            MenuItem menuItem = new MenuItem(defineActiveTrendSetMenu,
                    SWT.RADIO);
            menuItem.setText(key);

            if (key.compareTo(selectedTrend) == 0) {
                menuItem.setSelection(true);
            }
            menuItem.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    MenuItem mi = (MenuItem) e.getSource();
                    selectedTrend = mi.getText();
                }
            });
        }
    }

    /**
     * Handle the rank menu event.
     * 
     * @param event
     *            Selection event.
     */
    @Override
    protected void handleRankMenuEvent(SelectionEvent event) {
        String rank = ((MenuItem) event.getSource()).getText();

        String colName = rank;

        if (rank.compareTo(SCANConfig.getInstance().getDefaultName()) == 0) {
            colName = SCANConfig.getInstance().getDefaultRank(this.scanTable);
        }

        rankBtn.setText("Rank: " + colName);
        int colIndex = SCANConfig.getInstance().getColumnIndex(scanTable,
                colName);
        dmdTableComp.sortTableColumnByIndex(colIndex);
    }

    /**
     * Handle the CWA filter action.
     */
    private void handleCWAFilterAction() {
        SCANConfig.getInstance().setCWAFilter(scanTable,
                cwaFilterChk.getSelection());
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);
    }

    /**
     * Handle link to frame action.
     */
    private void handleLinkToFrame() {
        SCANConfig.getInstance().setLinkToFrame(scanTable,
                linkToFrameChk.getSelection());
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);
    }

    /**
     * Display the attributes dialog.
     */
    private void displayAttributesDialog() {
        if ((attributeDlg == null || attributeDlg.isDisposed())
                || (attributeDlg.getParent().isDisposed() == true)) {
            attributeDlg = new SCANAttributesDlg(shell, scanTable, this);
            attributeDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    unregisterDialog(attributeDlg);
                    attributeDlg = null;
                }

            });
            registerDialog(attributeDlg);
            attributeDlg.open();
        } else {
            attributeDlg.bringToTop();
        }
    }

    /**
     * Display the color threshold dialog.
     */
    private void displayColorThresholdDialog() {
        if (colorThresholdDlg == null || colorThresholdDlg.isDisposed()) {
            colorThresholdDlg = new SCANColorThreshDlg(shell, scanTable, this);
            colorThresholdDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    unregisterDialog(colorThresholdDlg);
                    colorThresholdDlg = null;
                }

            });
            registerDialog(colorThresholdDlg);
            colorThresholdDlg.open();
        } else {
            colorThresholdDlg.bringToTop();
        }
    }

    /**
     * Shell dispose action.
     */
    @Override
    protected void shellDisposeAction() {
        /*
         * This dialog will be disposed of when the Clear button on the D2D
         * display is clicked. The shellDisposeDialog needs to be called to set
         * the killDialog flag so the shell can be disposed.
         */
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                if (killDialog == false) {
                    e.doit = false;
                    displayCloseInstructions("SCAN DMD Table dialog");
                }
            }
        });
    }

    /**
     * Shell dispose dialog.
     */
    @Override
    public void shellDisposeDialog() {
        killDialog = true;

        close();
        elevationLabelColor.dispose();
    }

    /**
     * Set the title bar text.
     */
    @Override
    protected void setShellText() {
        shell.setText(this.site + " DMD Table");
    }

    /**
     * Set the Rank button to display which column is sorted.
     */
    @Override
    public void sortedColumn(String columnName) {
        if (columnName.compareTo(DMDTable.STRANK.getColName()) == 0) {
            rankBtn.setText("Rank: "
                    + SCANConfig.getInstance().getDefaultName());
            return;
        }

        rankBtn.setText("Rank: " + columnName);
    }

    /**
     * Method called when the data are update/changed.
     */
    @Override
    public void notify(IMonitorEvent me) {
        if (me.getSource() instanceof IMonitor) {
            ScanMonitor scan = (ScanMonitor) me.getSource();

            // If scan is null return since nothing will be done.
            if (scan == null) {
                return;
            }

            Date time = getScanTime(scan);

            if ((time != null) && scan.isInstantiated()) {
                ScanDataGenerator sdg = new ScanDataGenerator(site);
                ScanTableData<?> data = null;
                if (getLinkToFrame(ScanTables.DMD.name()) == false) {
                    if (scan.getLatestElevation() == null) {
                        scan.setLatestElevation(scan.getDmdTilt(site));
                    }
                    data = scan.getDmdTableData(site, time,
                            scan.getLatestElevation());
                } else {
                    data = scan.getTableData(ScanTables.DMD, site, time);
                }

                if (data == null) {
                    return;
                }
                dmdTableComp.setVcp(data.getVcp());
                dmdTableComp.setTableData(sdg.generateDMDData(data));
                setShellText();

                if ((currentTime != null)
                        && (currentElevation != null)
                        && currentTime.equals(scan.getMostRecent(
                                scanTable.name(), site).getRefTime())
                        && (currentElevation != scan.getDmdTilt(site))) {
                    // do nothing
                } else {
                    mgr.clearIdents(site, scanTable);
                }

                currentTime = time;
                currentElevation = scan.getDmdTilt(site);
                currentTrueElevation = data.getTrueAngle();

                updateTime();
                updateElevation();

                dmdTableComp.updateAllTrendGraphs();

                /*
                 * If a time-height graph is displayed then update it.
                 */
                if (dmdTableComp.timeHeightDisplayed() == true) {
                    dmdTableComp.updateTimeHeightGraph(scan, site, time);
                }

                // closes the alarm dialog if new data comes in or user switches
                // frame
                if (getLinkToFrame(scanTable.name())
                        || currentTime.equals(scan.getMostRecent(
                                scanTable.name(), site).getRefTime())) {
                    if ((alarmsDlg != null) && alarmsDlg.isOpen()) {
                        alarmsDlg.close();
                    }
                }

                Date date = scan.getMostRecent(scanTable.name(), site)
                        .getRefTime();
                if ((EditorUtil.getActiveVizContainer() != null)
                        && !(EditorUtil.getActiveVizContainer()
                                .getLoopProperties().isLooping())
                        && (mgr.getScheduledAlarms(site, scanTable).size() > 0)
                        && currentTime.equals(date)
                        && !scanCfg.getAlarmsDisabled(scanTable)) {
                    dmdTableComp.checkBlink(sdg, date);
                    if (mgr.getAlertedAlarms(site, scanTable).size() > 0) {
                        alarmBtn.setVisible(true);
                        addAlarmTimer();
                    } else {
                        alarmBtn.setVisible(false);
                    }
                } else {
                    mgr.removeAlertedAlarms(site, scanTable);
                    alarmBtn.setVisible(false);
                }
            }
        }
    }

    /**
     * Fire a recenter call on the selected ident.
     */
    @Override
    public void centerByIdent(String ident) {
        fireRecenter(ident, scanTable, site);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.ITableAction#centerByStormId(
     * java.lang.String)
     */
    @Override
    public void centerByStormId(String stormId) {
        // no op
    }

    /**
     * Get the current date.
     */
    @Override
    public Date getCurrentDate() {
        return currentTime;
    }

    /**
     * Get the Trend Set name.
     */
    @Override
    public String getTrendSetName() {
        return selectedTrend;
    }

    /**
     * @return the scanTableComp
     */
    public SCANDmdTableComp getScanTableComp() {
        return dmdTableComp;
    }

    /**
     * Attributes have changed (column shown/hidden in the table) so update the
     * attribute configuration and the table.
     */
    @Override
    public void attributeUpdates(boolean[] visibleAttrs) {
        SCANConfig scanCfg = SCANConfig.getInstance();
        scanCfg.setVisibleColumns(scanTable, visibleAttrs);
        dmdTableComp.columnVisiblityChanged(visibleAttrs);
        newUpRankPopUpMenu();
        createRankPopupMenu(rankPopupMenu, rankBtn);
    }

    /**
     * Thresholds have change so update the configuration and the table.
     */
    @Override
    public void thresholdsUpdated(String attrName, double upper, double mid,
            double lower) {
        SCANConfig scanCfg = SCANConfig.getInstance();
        scanCfg.setThresholds(scanTable, attrName, upper, mid, lower);

        dmdTableComp.updateThresholds(attrName);
        dmdTableComp.redrawAllTrendGraphs();
        dmdTableComp.redrawTimeHeightGraph();
        fireThresholdUpdate(new IMonitorThresholdEvent(this));
        if (displayFilterDlg != null) {
            if (!displayFilterDlg.isDisposed()) {
                displayFilterDlg.refresh();
            }
        }
    }

    /**
     * Display filter has updated so update the configuration and the table.
     */
    @Override
    public void displayFilterUpdated() {
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);
        dmdTableComp.updateTableColumnImages();
        if (colorThresholdDlg != null) {
            if (!colorThresholdDlg.isDisposed()) {
                colorThresholdDlg.refresh();
            }
        }
    }

    /**
     * Update the controls and reload the table data to reflect the new
     * configuration changes.
     */
    @Override
    protected void updateAfterConfigLoad() {
        /*
         * Fire a configuration updated - this will load in new table data as
         * well.
         */
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);

        linkToFrameChk.setSelection(scanCfg.getLinkToFrame(scanTable));
        tipsChk.setSelection(scanCfg.showTips(scanTable));
        cwaFilterChk.setSelection(scanCfg.getCWAFilter(scanTable));
        rankBtn.setText("Rank: " + scanCfg.getDefaultRank(scanTable));

        dmdTableComp.newConfigLoaded();

        Iterator<IMonitor> iter = getMonitorControlListeners().iterator();
        while (iter.hasNext()) {
            ((ScanMonitor) iter.next()).configurationLoaded(scanTable, site);
        }
    }

    /**
     * Display the trend sets graph.
     */
    @Override
    public void displayTrendSetGraphs(String ident) {
        dmdTableComp.displayTrendSetGraphFromMap(ident);
    }

    /**
     * Request time-height data.
     */
    @Override
    public TreeMap<Long, DMDTableDataRow> requestTimeHeightData(
            DMDTable tableCol, String dmdIdent) {
        handleLinkToFrame();

        ScanMonitor scanMonitor = this.getScanMonitor();
        return scanMonitor.getTimeHeightGraphData(site, tableCol, dmdIdent,
                currentTime);
    }

    @Override
    // Update the threshold in the composite contained in this dialog for the
    // desired attribute in the dialog.
    public void updateThresh(String attr) {
        this.dmdTableComp.updateThresholds(attr);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #getDialogTime()
     */
    @Override
    public Date getDialogTime() {
        return currentTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData
     * #cellValid(java.lang.String)
     */
    @Override
    public boolean cellValid(String ident) {
        String[] idents = this.dmdTableComp.getIdentList();
        for (String id : idents) {
            if (id.equals(ident)) {
                return true;
            }
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #getDrawSettings()
     */
    @Override
    public DrawSettings getDrawSettings() {
        return this.drawSettings;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTimeHeightData
     * #setDrawSettings
     * (com.raytheon.uf.viz.monitor.scan.commondialogs.DrawSettings)
     */
    @Override
    public void setDrawSettings(DrawSettings drawSettings) {
        this.drawSettings = drawSettings;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData
     * #requestTrendGraphDataObject
     * (com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables,
     * java.lang.String, java.lang.String)
     */
    @Override
    public TrendGraphData requestTrendGraphData(ScanTables type, String field,
            String ident) {
        return ScanMonitor.getInstance().getGraphData(type, site, field, ident);
    }

    /**
     * Alarm Selection
     * 
     * @param ident
     */
    public void alarmSelection(String ident) {
        dmdTableComp.alarmSelection(ident);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#turnOffAlarm()
     */
    @Override
    public void turnOffAlarm() {
        if (alarmBtn != null && !alarmBtn.isDisposed()) {
            alarmBtn.setVisible(false);
        }
        mgr.setRing(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#turnOnAlarm()
     */
    @Override
    public void turnOnAlarm() {
        if (alarmBtn != null && !alarmBtn.isDisposed()) {
            alarmBtn.setVisible(true);
        }
        mgr.setRing(true);
    }

}
