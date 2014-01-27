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
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanColors;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.TvsConfigMgr;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IAttributeUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IThresholdUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAlarmTimeLimitDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANAttributesDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANColorThreshDlg;
import com.raytheon.uf.viz.monitor.scan.data.ScanDataGenerator;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * 
 * Dialog for the SCAN TVS table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2013 #1945      lvenable    Code cleanup for SCAN performance.
 * 24 Jul 2013  #2143      skorolev    Changes for non-blocking dialogs.
 * Aug 15, 2013  2143      mpduff      Added some isDisposed() checks.
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
public class SCANTvsTableDlg extends AbstractTableDlg implements
        IAttributeUpdate, IThresholdUpdate {
    private Button configBtn;

    private Button rankBtn;

    private Button attribBtn;

    private Button vertChk;

    private Button tipsChk;

    private Label timeLbl;

    private Menu filePopupMenu;

    private Menu configPopupMenu;

    private Menu rankPopupMenu;

    private SCANTvsTableComp scanTableComp;

    private final SCANTableData tableData;

    private SCANAttributesDlg attributeDlg;

    private SCANAlarmTimeLimitDlg alarmTimeLimitDlg;

    private SCANColorThreshDlg colorThresholdDlg;

    private Date currentTime = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param site
     *            Site name.
     * @param tableData
     *            Tabel data.
     */
    public SCANTvsTableDlg(Shell parentShell, String site,
            SCANTableData tableData) {
        super(parentShell);

        this.site = site;
        this.tableData = tableData;
        open();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#setTableType()
     */
    @Override
    protected void setTableType() {
        scanTable = ScanTables.TVS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#initComponents()
     */
    @Override
    protected void initComponents() {
        createTopControls();
        createTvsTable();

        createFilePopupMenu();
        createConfigurationsPopupMenu();
        newUpRankPopUpMenu();
        createRankPopupMenu(rankPopupMenu, rankBtn);
    }

    /**
     * Create Top Controls.
     */
    private void createTopControls() {
        SCANConfig scanCfg = SCANConfig.getInstance();
        TvsConfigMgr tvsCfgMgr = (TvsConfigMgr) scanCfg
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
        rankBtn.setText("Rank: " + scanCfg.getDefaultRank(ScanTables.TVS));
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
         * Vertical - tech blocked
         */
        vertChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Vert),
                display.getSystemColor(SWT.COLOR_WHITE), "Vert ", true, null);

        vertChk.setSelection(tvsCfgMgr.getScanTvsCfgXML().getFilterOption());
        vertChk.setEnabled(false);

        /*
         * Tool tips
         */
        tipsChk = createCheckLabelComposite(controlComp,
                scanCfg.getScanColor(ScanColors.Tips),
                display.getSystemColor(SWT.COLOR_WHITE), "Tips ", true, null);

        tipsChk.setSelection(tvsCfgMgr.getScanTvsCfgXML().getTipsOption());

        tipsChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                SCANConfig.getInstance().setShowTips(scanTable,
                        tipsChk.getSelection());
                scanTableComp.updateColumnTips();
            }
        });

        /*
         * Time
         */
        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        gd.widthHint = 160;
        timeLbl = new Label(controlComp, SWT.RIGHT);
        timeLbl.setText("*** No Date/Time ***");
        timeLbl.setLayoutData(gd);
    }

    /**
     * Create TVS Table.
     */
    private void createTvsTable() {
        scanTableComp = new SCANTvsTableComp(shell, tableData, this, site);
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
     * Create File Popup Menu.
     */
    private void createFilePopupMenu() {
        filePopupMenu = new Menu(fileBtn);

        MenuItem retDefaultMI = new MenuItem(filePopupMenu, SWT.NONE);
        retDefaultMI.setText("Retrieve Default TVS Configuration");
        retDefaultMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                retrieveDefaultConfig();
            }
        });

        MenuItem retMesoConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        retMesoConfMI.setText("Retrieve TVS Configuration...");
        retMesoConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                retrieveExistingConfig();
            }
        });

        MenuItem saveMesoConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveMesoConfMI.setText("Save TVS Configuration");
        saveMesoConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveCurrentConfiguration();
            }
        });

        MenuItem saveAsMesoConfMI = new MenuItem(filePopupMenu, SWT.NONE);
        saveAsMesoConfMI.setText("Save TVS Configuration As...");
        saveAsMesoConfMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                saveConfigurationAs();
            }
        });

        new MenuItem(filePopupMenu, SWT.SEPARATOR);

        MenuItem exitMI = new MenuItem(filePopupMenu, SWT.NONE);
        exitMI.setText("Close TVS Table");
        exitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shellDisposeDialog();
            }
        });

        fileBtn.setMenu(filePopupMenu);
    }

    /**
     * Create Configurations Popup Menu.
     */
    private void createConfigurationsPopupMenu() {
        configPopupMenu = new Menu(configBtn);

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
     * Display Attributes Dialog.
     */
    private void displayAttributesDialog() {
        if ((attributeDlg == null) || (attributeDlg.getParent().isDisposed())) {
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
     * Display Alarm Time Limit Dialog.
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
     * Display Color Threshold Dialog.
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#handleRankMenuEvent
     * (org.eclipse.swt.events.SelectionEvent)
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
        scanTableComp.sortTableColumnByIndex(colIndex);
    }

    /**
     * Update Time Label.
     */
    private void updateTimeLabel() {
        if (currentTime == null) {
            timeLbl.setText("*** NO TIME ***");
            return;
        }

        SimpleDateFormat dateFmt = new SimpleDateFormat("E MMM dd HH:mm yyyy");
        dateFmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        timeLbl.setText(dateFmt.format(currentTime));
    }

    /**
     * Unregister Dialog From Monitor.
     */
    private void unregisterDialogFromMonitor() {
        this.fireDialogShutdown(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#shellDisposeAction
     * ()
     */
    @Override
    protected void shellDisposeAction() {
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                unregisterDialogFromMonitor();
            }
        });

        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                unregisterDialogFromMonitor();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#shellDisposeDialog
     * ()
     */
    @Override
    public void shellDisposeDialog() {
        close();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#setShellText()
     */
    @Override
    protected void setShellText() {
        if (!shell.isDisposed()) {
            shell.setText(this.site + " TVS Table");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.ITableAction#sortedColumn(java
     * .lang.String)
     */
    @Override
    public void sortedColumn(String columnName) {
        rankBtn.setText("Rank: " + columnName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#notify(com.raytheon
     * .uf.viz.monitor.events.IMonitorEvent)
     */
    @Override
    public void notify(IMonitorEvent me) {
        if (me.getSource() instanceof IMonitor) {
            ScanMonitor scan = (ScanMonitor) me.getSource();

            // If scan is null or the scan table has been disposed then return
            // since nothing will be done.
            if (scan == null || scanTableComp.isDisposed()) {
                return;
            }

            Date time = getScanTime(scan);

            if (time != null) {

                if (getLinkToFrame(scanTable.name())) {
                    currentTime = scan.getDialogTime(scanTable, site);
                    updateTimeLabel();
                    updateTable(scan, time);
                } else {
                    if (currentTime == null || !currentTime.equals(time)) {
                        currentTime = time;
                        updateTimeLabel();
                        updateTable(scan, time);
                    }
                }
            }
        }
    }

    /**
     * Update the table with new data.
     * 
     * @param scan
     *            Scan Monitor.
     * @param time
     *            New time.
     */
    private void updateTable(ScanMonitor scan, Date time) {
        ScanDataGenerator sdg = new ScanDataGenerator(site);
        scanTableComp.setTableData(sdg.generateTVSData(scan.getTableData(
                ScanTables.TVS, site, time)));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.ITableAction#centerByIdent(java
     * .lang.String)
     */
    @Override
    public void centerByIdent(String ident) {
        fireRecenter(ident, ScanTables.TVS, site);
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
        fireRecenter(stormId, ScanTables.CELL, site);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData
     * #getCurrentDate()
     */
    @Override
    public Date getCurrentDate() {
        return currentTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.ITableAction#getTrendSetName()
     */
    @Override
    public String getTrendSetName() {
        // Not used...
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.scan.commondialogs.IAttributeUpdate#
     * attributeUpdates(boolean[])
     */
    @Override
    public void attributeUpdates(boolean[] visibleAttrs) {
        SCANConfig scanCfg = SCANConfig.getInstance();
        scanCfg.setVisibleColumns(scanTable, visibleAttrs);
        scanTableComp.columnVisiblityChanged(visibleAttrs);
        newUpRankPopUpMenu();
        createRankPopupMenu(rankPopupMenu, rankBtn);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.scan.commondialogs.IThresholdUpdate#
     * thresholdsUpdated(java.lang.String, double, double, double)
     */
    @Override
    public void thresholdsUpdated(String attrName, double upper, double mid,
            double lower) {
        SCANConfig scanCfg = SCANConfig.getInstance();
        scanCfg.setThresholds(scanTable, attrName, upper, mid, lower);

        scanTableComp.updateThresholds(attrName);
        fireThresholdUpdate(new IMonitorThresholdEvent(this));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#
     * updateAfterConfigLoad()
     */
    @Override
    protected void updateAfterConfigLoad() {
        /*
         * Fire a configuration updated - this will load in new table data as
         * well.
         */
        IMonitorConfigurationEvent imce = new IMonitorConfigurationEvent(this);
        this.fireConfigUpdate(imce);

        tipsChk.setSelection(scanCfg.showTips(scanTable));
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
        // NOT USED
    }

    /*
     * Update the threshold in the composite contained in this dialog for the
     * desired attribute in the dialog.
     */
    @Override
    public void updateThresh(String attr) {
        this.scanTableComp.updateThresholds(attr);
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
        // Not currently used for TVS
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg#turnOffAlarm()
     */
    @Override
    public void turnOffAlarm() {
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
        mgr.setRing(true);
    }
}
