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
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.AbsConfigMgr;
import com.raytheon.uf.common.monitor.scan.config.CellConfigMgr;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.MESOTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.EditCreateTrendDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IAttributeUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IStormCellDisplayUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IThresholdUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAlarmThreshDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAlarmTimeLimitDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAttributesDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANColorThreshDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANUnwarnedDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.StormCellIdDisplayDlg;
import com.raytheon.uf.viz.monitor.scan.data.ScanDataGenerator;
import com.raytheon.uf.viz.monitor.scan.data.UnwarnedCell;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager.AlarmType;
import com.raytheon.uf.viz.monitor.scan.tables.SCANAlarmAlertManager.AlertedAlarms;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class displays the CELL table dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2009 #3039      lvenable    Initial creation
 * 
 * 03/15/2012	13939	   mpduff      For a SCAN Alarms issue
 * Apr 26, 2013 #1945      lvenable    Improved SCAN performance, reworked
 *                                     some bad code, and some code cleanup.
 * 06 Jun 2013  #2065      lvenable    Added code to alert the user to use the clear
 *                                     button if they want to close the dialog.
 * Jul 24, 2013 #2218      mpduff      Method signature changed.
 * Jul 26, 2013 #2143      skorolev    Changes for non-blocking dialogs.
 * 04 Dec 2013  #2592      lvenable    Update how the checkboxes are handled
 *                                     (background/foreground colors) since the Redhat
 *                                     6 upgrade causes the check in the checkbox to be
 *                                     colored the same as the background.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANCellTableDlg extends AbstractTableDlg implements
        IAttributeUpdate, IThresholdUpdate, IStormCellDisplayUpdate {

    private boolean killDialog = false;

    /*
     * Action buttons at the top of the dialog.
     */
    private Button configBtn;

    private Button rankBtn;

    private Button attribBtn;

    private Button tablesBtn;

    private Button linkToFrameChk;

    private Button cwaFilterChk;

    private Button unwarnedChk;

    private Button vertChk;

    private Button tipsChk;

    private Button alarmBtn;

    private Label timeLbl;

    /**
     * SCAN CELL table composite
     */
    private SCANCellTableComp scanTableComp;

    /**
     * Data to be displayed in the table.
     */
    private SCANTableData tableData;

    /*
     * Popup menus.
     */
    private Menu filePopupMenu;

    private Menu configPopupMenu;

    private Menu rankPopupMenu;

    private Menu tablePopupMenu;

    /*
     * The define trend sets menu and menu item.
     */
    private MenuItem defineActiveMI;

    private Menu defineActiveMenu;

    /**
     * The selected trend set.
     */
    private String selectedTrend = "default";

    /**
     * Current time.
     */
    private Date currentTime = null;

    /*
     * The following are sub-dialogs launched from this dialog.
     */
    private SCANAttributesDlg attributeDlg = null;

    private SCANColorThreshDlg colorThresholdDlg = null;

    private StormCellIdDisplayDlg scidDlg = null;

    private SCANAlarmThreshDlg alarmThreshDlg = null;

    private SCANAlarmTimeLimitDlg alarmTimeLimitDlg = null;

    private SCANUnwarnedDlg unwarnedAlarmDlg = null;

    private EditCreateTrendDlg editTrendDlg = null;

    private SCANAlarmsDlg alarmDlg = null;

    /** Date format for the time label. */
    private final SimpleDateFormat dateFmt = new SimpleDateFormat(
            "E MMM dd HH:mm yyyy");

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param site
     * @param tableData
     */
    public SCANCellTableDlg(Shell parentShell, String site,
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
        scanTable = ScanTables.CELL;
    }

    /**
     * Initialize the components.
     */
    @Override
    protected void initComponents() {
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        createTopControls();
        createCellTable();
        createFilePopupMenu();
        createConfigurationsPopupMenu();
        newUpRankPopUpMenu();
        createRankPopupMenu(rankPopupMenu, rankBtn);
        createTablesPopupMenu();

        scanCfg = SCANConfig.getInstance();
        AbsConfigMgr absCfgMgr = scanCfg.getAbsConfigMgr(scanTable);
        String[] attributes = absCfgMgr.getAlarmAttributes();
        for (int i = 0; i < attributes.length; i++) {
            int val = absCfgMgr.getAbsoluteValue(attributes[i]);
            mgr.updateScheduledAlarm(site, scanTable, attributes[i],
                    AlarmType.AbsVal, val);
            val = absCfgMgr.getRateOfChange(attributes[i]);
            mgr.updateScheduledAlarm(site, scanTable, attributes[i],
                    AlarmType.RateOfChange, val);
        }
    }

    /**
     * Add an alarm timer.
     */
    private void addAlarmTimer() {
        if (scanTableComp.timer != null) {
            timerTask = new TimerTask() {
                @Override
                public void run() {
                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
                        public void run() {
                            if (shell.isDisposed()) {
                                scanTableComp.timer.cancel();
                                mgr.clearScheduledAlarms(site, scanTable);
                            } else if (alarmBtn.isVisible()) {
                                setBlinkColor();
                                alarmBtn.setBackground(blinkColor);
                            }
                        }
                    });
                }
            };
            scanTableComp.timer.schedule(timerTask, 0, 1000);
        }
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls() {
        CellConfigMgr cellCfgMgr = (CellConfigMgr) scanCfg
                .getAbsConfigMgr(scanTable);

        Composite controlComp = new Composite(shell, SWT.PUSH);
        GridLayout gl = new GridLayout(12, false);
        gl.horizontalSpacing = 3;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(60, SWT.DEFAULT);
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

        configBtn = new Button(controlComp, SWT.PUSH);
        configBtn.setText("Configurations");
        configBtn
                .setBackground(scanCfg.getScanColor(ScanColors.Configurations));
        configBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
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
        rankBtn.setText("Rank: " + scanCfg.getDefaultRank(ScanTables.CELL));
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

        attribBtn = new Button(controlComp, SWT.PUSH);
        attribBtn.setText("Attributes");
        attribBtn.setBackground(scanCfg.getScanColor(ScanColors.Attributes));
        attribBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        attribBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetButtonForegroundColor(attribBtn);
                displayAttributesDialog();
            }
        });
        setupButtonMouseListeners(attribBtn);

        tablesBtn = new Button(controlComp, SWT.PUSH);
        tablesBtn.setText("Tables");
        tablesBtn.setBackground(scanCfg.getScanColor(ScanColors.Default));
        tablesBtn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        tablesBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                resetButtonForegroundColor(tablesBtn);
                Point controlLoc = display.map(tablesBtn, null, e.x, e.y
                        + tablesBtn.getSize().y);
                tablePopupMenu.setLocation(controlLoc);
                tablePopupMenu.setVisible(true);
            }
        });
        setupButtonMouseListeners(tablesBtn);

        /*
         * Link to Frame
         */
        linkToFrameChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.LinkToFrame),
                display.getSystemColor(SWT.COLOR_WHITE), "Link to Frame ",
                true, null);

        linkToFrameChk.setSelection(cellCfgMgr.getScanCellCfgXML()
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
        tipText.append("Activate to remove from the SCAN table all cells,\n");
        tipText.append("MESOs, and TVS's that are outside your CWA.\n\n");
        tipText.append("Deactivate to include in the SCAN table all cells,\n");
        tipText.append("MESOs, and TVS's detected by radar.");

        cwaFilterChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.CWAFilter),
                display.getSystemColor(SWT.COLOR_WHITE), "CWA Filter ", true,
                tipText.toString());

        cwaFilterChk.setSelection(cellCfgMgr.getScanCellCfgXML()
                .getFilterOption());
        cwaFilterChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCWAFilterAction();
            }
        });

        /*
         * Unwarned
         */
        unwarnedChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Attributes),
                display.getSystemColor(SWT.COLOR_WHITE), "Unwarned ", true,
                null);

        unwarnedChk.setSelection(cellCfgMgr.getScanCellCfgXML()
                .getFilterOption());

        unwarnedChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // unwarnedChk.setSelection(true);
                displayUnwarnedAlarmDialog();
            }
        });

        // Create/Recreate the unwarned config data since the ScanConfig is a
        // singleton and the data clears out when the CELL table dialog get
        // re-created.
        scanCfg.createUnwarnedConfig();

        /*
         * Vertical - tech blocked
         */
        vertChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Vert),
                display.getSystemColor(SWT.COLOR_WHITE), "Vert ", true, null);

        vertChk.setSelection(cellCfgMgr.getScanCellCfgXML().getFilterOption());
        vertChk.setEnabled(false);

        /*
         * Tool tips
         */
        tipsChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Tips),
                display.getSystemColor(SWT.COLOR_WHITE), "Tips ", true, null);

        tipsChk.setSelection(cellCfgMgr.getScanCellCfgXML().getTipsOption());

        tipsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                scanCfg.setShowTips(scanTable, tipsChk.getSelection());
                scanTableComp.updateColumnTips();
            }
        });

        /*
         * Alarm button
         */
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        alarmBtn = new Button(controlComp, SWT.PUSH);
        alarmBtn.setText("Alarm");
        alarmBtn.setBackground(display.getSystemColor(SWT.COLOR_RED));
        alarmBtn.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        alarmBtn.setLayoutData(gd);
        alarmBtn.setVisible(false);
        alarmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if(alarmDlg==null || alarmDlg.isDisposed()){
                alarmDlg = new SCANAlarmsDlg(shell, ScanTables.CELL, site);
                alarmDlg.setCloseCallback(new ICloseCallback(){

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (!alarmBtn.isDisposed()
                                && (mgr.getAlertedAlarmCount(site, scanTable) == 0)) {
                            turnOffAlarm();
                        }
                    }
                    
                });
                alarmDlg.open();
                } else {
                    alarmDlg.bringToTop();
                }
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.widthHint = 160;
        timeLbl = new Label(controlComp, SWT.RIGHT);
        timeLbl.setText("*** No Date/Time ***");
        timeLbl.setLayoutData(gd);
    }

    @Override
    public void turnOffAlarm() {
        if (alarmBtn != null && !alarmBtn.isDisposed()) {
            alarmBtn.setVisible(false);
        }
        mgr.setRing(false);
    }

    @Override
    public void turnOnAlarm() {
        if (alarmBtn != null && !alarmBtn.isDisposed()) {
            alarmBtn.setVisible(true);
        }
        mgr.setRing(true);
    }

    /**
     * Create the CELL table.
     */
    private void createCellTable() {
        scanTableComp = new SCANCellTableComp(shell, tableData, this, this,
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
        retDefaultMI.setText("Retrieve Default CELL Configuration");
        retDefaultMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                retrieveDefaultConfig();
            }
        });

        MenuItem retCellConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        retCellConfMI.setText("Retrieve CELL Configuration...");
        retCellConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                retrieveExistingConfig();
            }
        });

        MenuItem saveAllConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveAllConfMI.setText("Save All Configurations");
        saveAllConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveAllConfigs();
            }
        });

        MenuItem saveCellConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveCellConfMI.setText("Save CELL Configuration");
        saveCellConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveCurrentConfiguration();
            }
        });

        MenuItem saveAsCellConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveAsCellConfMI.setText("Save CELL Configuration As...");
        saveAsCellConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveConfigurationAs();
            }
        });

        fileBtn.setMenu(filePopupMenu);
    }

    /**
     * Create the Configuration popup menu.
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
                displayStormCellDialog();
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

        defineActiveMI = new MenuItem(trendSetsMenu, SWT.CASCADE);
        defineActiveMI.setText("Define Active Trend Set");

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
     * Create the Tables popup menu.
     */
    private void createTablesPopupMenu() {
        tablePopupMenu = new Menu(tablesBtn);

        /*
         * MESO item
         */
        MenuItem mesoMI = new MenuItem(tablePopupMenu, SWT.NONE);
        mesoMI.setText("MESO Table");
        mesoMI.setData("meso");
        mesoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayMesoTableDialog();
            }
        });

        /*
         * TVS item
         */
        MenuItem tvsMI = new MenuItem(tablePopupMenu, SWT.NONE);
        tvsMI.setText("TVS Table");
        tvsMI.setData("tvs");
        tvsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayTvsTableDialog();
            }
        });

        tablesBtn.setMenu(tablePopupMenu);
    }

    /**
     * Handle the link to frame action.
     */
    private void handleLinkToFrame() {
        scanCfg.setLinkToFrame(scanTable, linkToFrameChk.getSelection());
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);
    }

    /**
     * Handle the Rank action.
     * 
     * @param event
     */
    @Override
    protected void handleRankMenuEvent(SelectionEvent event) {
        String rank = ((MenuItem) event.getSource()).getText();

        String colName = rank;
        int colIndex = -1;
        if (rank.compareTo(SCANConfig.getInstance().getDefaultName()) == 0) {
            colName = SCANConfig.getInstance().getDefaultRank(this.scanTable);
        }

        if ((colName == null) || (colName.length() == 0)) {
            rankBtn.setText("Rank: Default");
        } else {
            rankBtn.setText("Rank: " + colName);
            colIndex = SCANConfig.getInstance().getColumnIndex(scanTable,
                    colName);
        }

        scanTableComp.sortTableColumnByIndex(colIndex);
    }

    /**
     * Handle the CWA filter action.
     */
    private void handleCWAFilterAction() {
        scanCfg.setCWAFilter(scanTable, cwaFilterChk.getSelection());
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);
    }

    /**
     * Display the Attributes dialog.
     */
    private void displayAttributesDialog() {
        if (attributeDlg == null
                || attributeDlg.getParent().isDisposed() == true) {
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
        if (colorThresholdDlg == null) {
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
     * Display the Storm Cell dialog.
     */
    private void displayStormCellDialog() {
        if (scidDlg == null || scidDlg.isDisposed()) {
            scidDlg = new StormCellIdDisplayDlg(shell, this);
            scidDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    unregisterDialog(scidDlg);
                    scidDlg = null;
                }

            });
            registerDialog(scidDlg);
            scidDlg.open();
        } else {
            scidDlg.bringToTop();
        }
    }

    /**
     * Display the Alarm threshold dialog.
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
     * Display the Alarm Time Limit dialog.
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
     * Display the Unwarned Alarm dialog.
     */
    private void displayUnwarnedAlarmDialog() {
        if (unwarnedAlarmDlg == null || unwarnedAlarmDlg.isDisposed()) {
            unwarnedAlarmDlg = new SCANUnwarnedDlg(shell);
            unwarnedAlarmDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    // The check box is a toggle... so capture the toggled
                    // state.
                    Boolean isEnabled = unwarnedChk.getEnabled();
                    if (returnValue instanceof Boolean) {
                        Boolean okSelected = (Boolean) returnValue;
                        if (okSelected) {
                            if ((scanCfg.getUnwarnedConfig().getUnwarnedTor() == false)
                                    && (scanCfg.getUnwarnedConfig()
                                            .getUnwarnedSvr() == false)) {
                                unwarnedChk.setSelection(false);
                            } else {
                                unwarnedChk.setSelection(true);
                            }
                        } else if ((scanCfg.getUnwarnedConfig()
                                .getUnwarnedTor() == true)
                                && (scanCfg.getUnwarnedConfig()
                                        .getUnwarnedSvr() == true)) {
                            unwarnedChk.setSelection(true);
                        } else {
                            // If the checkbox should not change state, reverse
                            // the effect
                            // of the toggle behavior inherent to the checkbox.
                            unwarnedChk.setSelection(!isEnabled);
                        }
                    }
                    unregisterDialog(unwarnedAlarmDlg);
                    unwarnedAlarmDlg = null;

                    IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(
                            SCANCellTableDlg.this);
                    SCANCellTableDlg.this.fireConfigUpdate(imce);
                }
            });

            registerDialog(unwarnedAlarmDlg);
            unwarnedAlarmDlg.open();
        } else {
            unwarnedAlarmDlg.bringToTop();
        }
    }

    /**
     * Display the Create and Edit Trend Set dialog.
     */
    private void displayCreateEditTrendDialog() {
        if (editTrendDlg == null || editTrendDlg.isDisposed()) {
            editTrendDlg = new EditCreateTrendDlg(shell, scanTable);
            editTrendDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    updateDefineActiveTrendMenu();
                    editTrendDlg = null;
                    unregisterDialog(editTrendDlg);
                }
            });
            registerDialog(editTrendDlg);
            editTrendDlg.open();
        } else {
            editTrendDlg.bringToTop();
        }
    }

    /**
     * Display the MESO table dialog.
     */
    private void displayMesoTableDialog() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {

                    ((ScanMonitor) iter.next()).launchDialog(shell, site,
                            ScanTables.MESO);

                }
            }
        });
    }

    /**
     * Display the TVS table dialog.
     */
    private void displayTvsTableDialog() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {

                    ((ScanMonitor) iter.next()).launchDialog(shell, site,
                            ScanTables.TVS);

                }
            }
        });
    }

    /**
     * Update the time label.
     */
    private void updateTimeLabel() {
        if (currentTime == null) {
            timeLbl.setText("*** NO TIME ***");
            return;
        }

        timeLbl.setText(dateFmt.format(currentTime));
    }

    /**
     * Update the Define Active Trend menu. This will always create a new menu
     * as the trend sets may have changed.
     */
    private void updateDefineActiveTrendMenu() {
        if (defineActiveMenu != null) {
            MenuItem[] items = defineActiveMenu.getItems();

            for (MenuItem mi : items) {
                if (mi.getSelection() == true) {
                    selectedTrend = mi.getText();
                }
            }

            defineActiveMenu.dispose();
            defineActiveMenu = null;
        }

        defineActiveMenu = new Menu(shell, SWT.DROP_DOWN);
        defineActiveMI.setMenu(defineActiveMenu);

        LinkedHashMap<String, String> trendMap = scanCfg.getTrendConfigMgr(
                scanTable).getTrendSetMap();

        if (trendMap.containsKey(selectedTrend) == false) {
            selectedTrend = "default";
        }

        Set<String> keys = trendMap.keySet();

        for (String key : keys) {
            MenuItem menuItem = new MenuItem(defineActiveMenu, SWT.RADIO);
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
     * Save all configurations.
     */
    private void saveAllConfigs() {
        /*
         * TODO : save the CELL, MESO, and TVS configs...
         * 
         * should probably call into the MESO & TVS dialogs if they are up and
         * have them call save configuration
         */
        MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Information");
        mb.setMessage("The Save All Configurations is not functional in the legacy system.");
        mb.open();
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
                    displayCloseInstructions("SCAN Cell Table dialog");
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
    }

    /**
     * Set the shell text.
     */
    @Override
    protected void setShellText() {
        shell.setText(this.site + " CELL Table");
    }

    /**
     * Update the table with new data.
     * 
     * @param scan
     *            Scan Monitor.
     * @param time
     *            The new time.
     * @param sdg
     *            Scan Data Generator.
     */
    private void updateTable(ScanMonitor scan, Date time, ScanDataGenerator sdg) {
        tableData = sdg.generateCellData(scan.getTableData(scanTable, site,
                time));
        scanTableComp.setTableData(tableData);
    }

    /**
     * Notify when the table data needs to be updated.
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

            if ((time != null)) {

                ScanDataGenerator sdg = new ScanDataGenerator(site);

                // check for un-warned checks
                if (unwarnedChk.getSelection()) {
                    Map<String, UnwarnedCell> warnings = null;
                    warnings = scan.retrieveWarnings(site, time, scanCfg
                            .getUnwarnedConfig().getUnwarnedSvr(), scanCfg
                            .getUnwarnedConfig().getUnwarnedTor());

                    sdg.setUnwarnedCells(warnings);
                }

                if (getLinkToFrame(scanTable.name())) {
                    currentTime = scan.getDialogTime(scanTable, site);
                    updateTimeLabel();
                    updateTable(scan, time, sdg);
                } else {
                    if (currentTime == null || !currentTime.equals(time)) {
                        currentTime = time;
                        updateTimeLabel();
                        updateTable(scan, time, sdg);
                    }
                }

                scan.fireMonitorEvent(SCANMesoTableDlg.class.getName());
                scan.fireMonitorEvent(SCANTvsTableDlg.class.getName());

                // Update the trend graphs and trend sets graph.
                scanTableComp.updateAllTrendGraphs();

                // closes the alarm dialog if new data comes in or user switches
                // frame
                Date scanMostRecentTime = null;
                DataTime dataTime = scan.getMostRecent(scanTable.name(), site);
                if (dataTime != null) {
                    scanMostRecentTime = dataTime.getRefTime();
                }

                if (scanMostRecentTime != null) {
                    if (getLinkToFrame(scanTable.name())
                            || currentTime.equals(scanMostRecentTime)) {
                        if ((alarmDlg != null) && alarmDlg.isOpen()) {
                            alarmDlg.close();
                        }
                    }
                    if (((scanTable == ScanTables.CELL) || (scanTable == ScanTables.DMD))
                            && !(EditorUtil.getActiveVizContainer()
                                    .getLoopProperties().isLooping())
                            && (mgr.getScheduledAlarms(site, scanTable).size() > 0)
                            && currentTime.equals(scanMostRecentTime)
                            && !scanCfg.getAlarmsDisabled(scanTable)) {
                        scanTableComp.checkBlink(sdg, scanMostRecentTime);
                        if (mgr.getAlertedAlarms(site, scanTable).size() > 0) {
                            boolean displayAlarmBtn = false;
                            for (AlertedAlarms alarm : mgr.getAlertedAlarms(
                                    site, scanTable)) {
                                if (!alarm.cleared) {
                                    displayAlarmBtn = true;
                                    break;
                                }
                            }

                            alarmBtn.setVisible(displayAlarmBtn);
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
    }

    /**
     * Set the text of the Rank button.
     * 
     * @param columnName
     *            Column name.
     */
    @Override
    public void sortedColumn(String columnName) {
        rankBtn.setText("Rank: " + columnName);
    }

    /**
     * Method called when attributes have been shown/hidden.
     * 
     * @param visibleAttrs
     *            Array of visibly attributes.
     */
    @Override
    public void attributeUpdates(boolean[] visibleAttrs) {
        scanCfg.setVisibleColumns(scanTable, visibleAttrs);
        scanTableComp.columnVisiblityChanged(visibleAttrs);
        newUpRankPopUpMenu();
        createRankPopupMenu(rankPopupMenu, rankBtn);
    }

    /**
     * Method called when the threshold values have been updated.
     * 
     * @param attrName
     *            Attribute name.
     * @param upper
     *            Upper value.
     * @param mid
     *            Mid value.
     * @param lower
     *            Lower value.
     */
    @Override
    public void thresholdsUpdated(String attrName, double upper, double mid,
            double lower) {
        scanCfg.setThresholds(scanTable, attrName, upper, mid, lower);

        scanTableComp.updateThresholds(attrName);
        scanTableComp.redrawAllTrendGraphs();
        if (attrName.equalsIgnoreCase(MESOTable.MDASR.getColName())) {
            scanCfg.setThresholds(ScanTables.MESO,
                    MESOTable.MDASR.getColName(), upper, mid, lower);

            // Update the threshold in the MESO table.
            Iterator<IMonitor> iter = getMonitorControlListeners().iterator();
            while (iter.hasNext()) {

                ((ScanMonitor) iter.next()).thresholdUpdated(ScanTables.MESO,
                        site, attrName);

            }
        }
        fireThresholdUpdate(new IMonitorThresholdEvent(this));
        if (scidDlg != null) {
            if (!scidDlg.isDisposed()) {
                scidDlg.refresh();
            }
        }
    }

    /**
     * Method called when the Storm Cell data has been updated.
     */
    @Override
    public void stormCellUpdated() {
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);
        scanTableComp.updateTableColumnImages();
        SCANToolTipTextMgr.getInstance().regenerateTableColumnTips(scanTable);
        scanTableComp.updateColumnTips();
        if (colorThresholdDlg != null) {
            if (!colorThresholdDlg.isDisposed()) {
                colorThresholdDlg.refresh();
            }
        }
    }

    /**
     * Recenter the CAVE display to the selected ident.
     * 
     * @param ident
     *            Identifier.
     */
    @Override
    public void centerByIdent(String ident) {
        // nop op
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.scan.tables.ITableAction#centerByStormId(java.lang.String)
     */
    @Override
    public void centerByStormId(String stormId) {
        fireRecenter(stormId, scanTable, site);
    }

    /**
     * Get the current date.
     * 
     * @return The current date.
     */
    @Override
    public Date getCurrentDate() {
        return currentTime;
    }

    /**
     * Get the trend set name.
     * 
     * @return The trend set name.
     */
    @Override
    public String getTrendSetName() {
        return selectedTrend;
    }

    /**
     * @return the scanTableComp
     */
    public SCANCellTableComp getScanTableComp() {
        return scanTableComp;
    }

    /**
     * Update the display after a new configuration has been loaded.
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

        scanTableComp.newConfigLoaded();

        Iterator<IMonitor> iter = getMonitorControlListeners().iterator();
        while (iter.hasNext()) {
            ((ScanMonitor) iter.next()).configurationLoaded(scanTable, site);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#
     * displayTrendSetGraphs(java.lang.String)
     */
    @Override
    public void displayTrendSetGraphs(String ident) {
        scanTableComp.displayTrendSetGraphFromMap(ident);
    }

    @Override
    // Update the threshold in the composite contained in this dialog for the
    // desired attribute in the dialog.
    public void updateThresh(String attr) {
        this.scanTableComp.updateThresholds(attr);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData
     * #cellValid()
     */
    @Override
    public boolean cellValid(String ident) {
        ArrayList<SCANTableRowData> dataList = tableData.getTableRows();
        for (SCANTableRowData data : dataList) {
            if (data.getIdent().equals(ident)) {
                return true;
            }
        }

        return false;
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
}
