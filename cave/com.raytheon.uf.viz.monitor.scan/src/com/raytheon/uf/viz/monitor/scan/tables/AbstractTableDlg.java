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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.scan.ScanMonitor;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.ICommonDialogAction;
import com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.LoadSaveConfigDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.LoadSaveConfigDlg.DialogType;

/**
 * Abstract dialog class used for the CALL, DND, MESO, and TVS table dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2009 #3039      lvenable     Initial creation
 * Apr 26, 2013 #1945      lvenable    Some code cleanup.
 * 06 Jun 2013  #2065      lvenable    Added convenience method to alert the user to use the clear
 *                                     button if they want to close the dialog.
 * 04 Dec 2013  #2592      lvenable    Update how the checkboxes are handled
 *                                     (background/foreground colors) since the Redhat
 *                                     6 upgrade causes the check in the checkbox to be
 *                                     colored the same as the background.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class AbstractTableDlg extends Dialog implements IMonitor,
        IMonitorListener, IMonitorControlListener, ITableAction,
        IRequestTrendGraphData {
    /**
     * The display object.
     */
    protected Display display;

    /**
     * Shell object.
     */
    protected Shell shell;

    /**
     * Array listening monitors
     */
    protected ArrayList<IMonitor> controlListeners = new ArrayList<IMonitor>();

    /**
     * The scan table.
     */
    protected ScanTables scanTable;

    /**
     * the site identifier
     */
    protected String site = "";

    /**
     * Array of available dialogs that can be launched. This array is used to
     * close down dialog that are displayed. Dialogs will be closed before
     * loading a configuration.
     */
    protected HashMap<ICommonDialogAction, Object> dialogsMap;

    /**
     * Instance of the SCAN configuration.
     */
    protected SCANConfig scanCfg;

    /**
     * File button that is common to all of the tables.
     */
    protected Button fileBtn;

    /**
     * Tip text for the file button.
     */
    private String fileButtonTipText = null;

    protected Timer timer;

    protected TimerTask timerTask;

    protected int blinkColorInt = SWT.COLOR_RED;

    protected Color blinkColor;

    protected SCANAlarmAlertManager mgr = null;

    /**
     * Abstract constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public AbstractTableDlg(Shell parentShell) {
        super(parentShell);

        scanCfg = SCANConfig.getInstance();
    }

    /**
     * Open the dialog.
     */
    protected void open() {
        Shell parent = getParent();
        display = parent.getDisplay();

        /*
         * The shell is being created off of the display
         */
        shell = new Shell(display, SWT.DIALOG_TRIM | SWT.RESIZE);
        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                shell.dispose();
            }
        });

        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);
        shell.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        initData();

        setShellText();

        setTableType();
        initComponents();
        shellDisposeAction();

        shell.pack();
        shell.setVisible(true);
    }

    /**
     * Initialize the data.
     */
    private void initData() {
        dialogsMap = new HashMap<ICommonDialogAction, Object>();
    }

    /**
     * Get the table type.
     * 
     * @return The SCAN table.
     */
    public ScanTables getTableType() {
        return scanTable;
    }

    /**
     * Register a dialog that is launched by the table. The dialog is put into a
     * map so it can be closed when loading a new configuration.
     * 
     * @param dialog
     *            The dialog to be registered.
     */
    protected void registerDialog(ICommonDialogAction dialog) {
        dialogsMap.put(dialog, null);
    }

    /**
     * Unregister a dialog that is closed. The dialog is removed from a map
     * since it is no longer open..
     * 
     * @param dialog
     *            The dialog to be registered.
     */
    protected void unregisterDialog(ICommonDialogAction dialog) {
        dialogsMap.remove(dialog);
    }

    /**
     * This method will prompt the user if they want to close the open dialogs
     * when they want to load a new configuration.
     * 
     * @return
     */
    private boolean closedOpenDialogs() {
        // Prompt the user to close the dialogs
        if (dialogsMap.isEmpty() == false) {
            StringBuilder sb = new StringBuilder();

            sb.append("There are open dialogs that need to be closed before loading ");
            sb.append("a new configuration.\n\n");
            sb.append("Click 'OK' to close all dialogs and resume loading a configuration.");
            sb.append("\n\nClick 'Cancel' to stop loading a configuration.");

            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Close Dialogs");
            mb.setMessage(sb.toString());
            int result = mb.open();

            // If the user selected Cancel then return.
            if (result == SWT.CANCEL) {
                return false;
            }
        }

        // Loop and close all of the open dialogs;
        Set<ICommonDialogAction> keys = dialogsMap.keySet();

        for (ICommonDialogAction icda : keys) {
            icda.closeDialog();
        }

        dialogsMap.clear();

        return true;
    }

    /**
     * Retrieve the default configuration.
     */
    protected void retrieveDefaultConfig() {
        /*
         * TODO : when loading the default config, the controls on the dialog
         * needs to be updated and a new table data should be loaded
         */
        scanCfg.loadDefaultConfigFileName(scanTable);

        updateAfterConfigLoad();
        updateFileButton();
    }

    /**
     * Retrieve an existing configuration.
     */
    protected void retrieveExistingConfig() {
        /*
         * TODO : when loading an existing config, the controls on the dialog
         * needs to be updated and a new table data should be loaded
         */

        if (closedOpenDialogs() == false) {
            return;
        }

        LoadSaveConfigDlg loadDlg = new LoadSaveConfigDlg(shell,
                DialogType.OPEN, scanTable);
        LocalizationFile fileName = (LocalizationFile) loadDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        scanCfg.loadNewConfigFileName(scanTable, fileName.getFile().getName());

        System.out.println(fileName.getFile().getAbsolutePath());

        updateAfterConfigLoad();
        updateFileButton();
    }

    /**
     * Save the current configuration.
     */
    protected void saveCurrentConfiguration() {
        /*
         * TODO : save the current configuration...
         * 
         * do not need to update the display...
         * 
         * call to configuration manager to save the config...
         */

        /*
         * check if the user is trying to save the default config
         */

        if (scanCfg.currentConfigIsDefault(scanTable) == true) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Overwrite");
            mb.setMessage("Saving will overwrite the default configuration.\n"
                    + "Do you wish to continue?");
            int result = mb.open();

            // If the user selected Cancel then return.
            if (result == SWT.CANCEL) {
                return;
            }
        }

        scanCfg.saveCurrentConfigurationFile(scanTable);
        updateFileButton();
    }

    /**
     * Save the current configuration as a different name.
     */
    protected void saveConfigurationAs() {
        /*
         * TODO : launch the save dialog and then get a name to save the
         * configuration.
         * 
         * do not need to update the display...
         */
        String defCfgName = scanCfg.getDefaultConfigName(scanTable);

        LoadSaveConfigDlg loadDlg = new LoadSaveConfigDlg(shell,
                DialogType.SAVE_AS, scanTable);
        LocalizationFile fileName = (LocalizationFile) loadDlg.open();

        if (fileName == null) {
            System.out.println("FileName is null...");
            return;
        }

        if (defCfgName.compareTo(fileName.getFile().getName()) == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Overwrite");
            mb.setMessage("The Save As name is the same as the default configuration name.  Saving "
                    + "will overwrite the default configuration.\n"
                    + "Do you wish to continue?");
            int result = mb.open();

            // If the user selected Cancel then return.
            if (result == SWT.CANCEL) {
                return;
            }
        }

        scanCfg.saveConfigurationFileAs(scanTable, fileName.getFile().getName());
        updateFileButton();
    }

    protected void resetButtonForegroundColor(Button btn) {
        btn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
    }

    protected void setupButtonMouseListeners(final Button btn) {
        btn.addMouseMoveListener(new MouseMoveListener() {
            @Override
            public void mouseMove(MouseEvent e) {
                btn.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
            }

        });

        btn.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseExit(MouseEvent e) {
                btn.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
            }

            @Override
            public void mouseEnter(MouseEvent e) {
                btn.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
            }
        });
    }

    /**
     * Create the Rank popup menu.
     */
    protected void createRankPopupMenu(Menu rankPopupMenu, Button rankBtn) {

        /*
         * Default menu item
         */
        MenuItem defaultMI = new MenuItem(rankPopupMenu, SWT.NONE);
        defaultMI.setText(scanCfg.getDefaultName());
        defaultMI.setData(scanCfg.getDefaultName());
        defaultMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleRankMenuEvent(event);
            }
        });

        /*
         * Create the remaining rank menus from the configuration
         */
        String[] ranks = scanCfg.getRankColumns(scanTable);
        HashMap<String, Object> availRankMap = scanCfg
                .getAvailRankColumns(scanTable);

        for (String rankStr : ranks) {
            if (availRankMap.containsKey(rankStr)) {
                MenuItem mi = new MenuItem(rankPopupMenu, SWT.NONE);
                mi.setText(rankStr);
                mi.setData(rankStr);
                mi.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        handleRankMenuEvent(event);
                    }
                });
            }
        }

        rankBtn.setMenu(rankPopupMenu);
    }

    /**
     * Update the file buttons tool tip text when the configuration file is
     * changed.
     */
    private void updateFileButton() {
        fileButtonTipText = "Current Config: "
                + scanCfg.getCurrentConfigFileName(scanTable);
        fileBtn.setToolTipText(fileButtonTipText);
    }

    protected void displayFileButtonToolTip() {
        if (fileButtonTipText == null) {
            fileButtonTipText = "Current Config: "
                    + scanCfg.getCurrentConfigFileName(scanTable);
        }
        fileBtn.setToolTipText(fileButtonTipText);
    }

    /**
     * Get the scan time. If link to frame is turned on then get that time with
     * the displayed frame otherwise get the latest time.
     * 
     * @param scanMonitor
     *            The scan monitor.
     * @return The scan time.
     */
    protected Date getScanTime(ScanMonitor scanMonitor) {
        Date time = null;
        if (getLinkToFrame(scanTable.name())) {
            time = scanMonitor.getScanTime(scanTable, site);
        } else {
            DataTime dt = scanMonitor.getMostRecent(scanMonitor,
                    scanTable.name(), site);

            if (dt != null) {
                time = dt.getRefTime();
            }
        }
        return time;
    }

    @Override
    public void addMonitorControlListener(IMonitor monitor) {
        getMonitorControlListeners().add(monitor);

    }

    @Override
    public void removeMonitorContorlListener(IMonitor monitor) {
        getMonitorControlListeners().remove(monitor);
    }

    @Override
    public void fireConfigUpdate(IMonitorConfigurationEvent imce) {
        final IMonitorConfigurationEvent fimce = imce;
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((ScanMonitor) iter.next()).configUpdate(fimce);
                }
            }
        });
    }

    @Override
    public void fireKillMonitor() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((ScanMonitor) iter.next()).nullifyMonitor(site);
                }
            }
        });
    }

    @Override
    public void fireThresholdUpdate(IMonitorThresholdEvent imte) {
        final IMonitorThresholdEvent fimte = imte;
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((ScanMonitor) iter.next()).thresholdUpdate(fimte);
                }
            }
        });
    }

    /**
     * Gets the graph data from the monitor
     * 
     * @param ident
     * @param type
     */
    public TrendGraphData getTrendGraphData(ScanTables type, String field,
            String ident) {
        TrendGraphData tgd = null;
        Iterator<IMonitor> iter = getMonitorControlListeners().iterator();
        while (iter.hasNext()) {
            tgd = ((ScanMonitor) iter.next()).getGraphData(type, site, field,
                    ident);
        }
        return tgd;
    }

    /**
     * Get the SCAN Monitor. The is only one at this time but there could be
     * multiple monitors.
     * 
     * @return The scan monitor.
     */
    protected ScanMonitor getScanMonitor() {
        ArrayList<IMonitor> monitorArray = getMonitorControlListeners();

        if (monitorArray.size() != 0) {
            return (ScanMonitor) monitorArray.get(0);
        }

        return null;
    }

    /**
     * Fires a re-center of the screen for the drawing portion
     * 
     * @param ident
     * @param type
     */
    public void fireRecenter(String ident, ScanTables type, String icao) {
        final String fident = ident;
        final ScanTables ftype = type;
        final String ficao = icao;
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((ScanMonitor) iter.next()).recenter(fident, ftype, ficao);
                }
            }
        });
    }

    /**
     * Fires a repaint of the screen for the drawing portion
     * 
     * @param ident
     * @param type
     */
    public void fireScanPaint() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((ScanMonitor) iter.next()).paintScan();
                }
            }
        });
    }

    /**
     * 
     * @param ident
     * @param type
     */
    public void fireTrendSet(String ident, ScanTables type) {

        if ((type == ScanTables.CELL) || (type == ScanTables.DMD)) {
            displayTrendSetGraphs(ident);
        }

        // TODO: What needs to be done here is to grab the current selected
        // trend set and fire a trend set for the passed in ID and table
        // this will mean firing a get graphData Event back to the monitor
    }

    /**
     * This is a method that will create a composite that contains a checkbox
     * with no text and a label. Since the upgrade to Redhat 6, a checkbox that
     * has its foreground and background color changed can cause the check in
     * the checkbox to become invisible if the foreground color is too light.
     * This method creates an ordinary checkbox with the label and composite
     * background being colored.
     * 
     * @param parentComp
     *            Parent composite.
     * @param bgColor
     *            Background color.
     * @param fgColor
     *            Foreground color.
     * @param labelText
     *            Text for the label.
     * @param colorComposite
     *            Flag indicating if the composite background color should be
     *            set.
     * @param toolTipText
     *            Set the toolTipText
     * @return The checkbox control that is created.
     */
    protected final Button createCheckLabelComposite(Composite parentComp,
            Color bgColor, Color fgColor, String labelText,
            boolean colorComposite, String toolTipText) {

        GridData gd = new GridData();
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        gl.horizontalSpacing = 0;

        Composite chkLblComp = new Composite(parentComp, SWT.NONE);
        chkLblComp.setLayout(gl);
        chkLblComp.setLayoutData(gd);

        if (colorComposite) {
            chkLblComp.setBackground(bgColor);
        }

        gd = new GridData(18, SWT.DEFAULT);
        Button chkBox = new Button(chkLblComp, SWT.CHECK);
        chkBox.setLayoutData(gd);

        Label lbl = new Label(chkLblComp, SWT.NONE);
        lbl.setBackground(bgColor);
        lbl.setForeground(fgColor);
        lbl.setText(" " + labelText);

        chkBox.setToolTipText(toolTipText);
        lbl.setToolTipText(toolTipText);

        return chkBox;
    }

    @Override
    public ArrayList<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    /**
     * Gets the shell
     * 
     * @return
     */
    public Shell getCurrentShell() {
        return shell;
    }

    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IMonitor> iter = getMonitorControlListeners()
                        .iterator();
                while (iter.hasNext()) {
                    ((ScanMonitor) iter.next()).closeDialog(scanTable, site);
                }
            }
        });
    }

    @Override
    public TrendGraphData requestTrendGraphData(ScanTables type, String field,
            String ident) {
        return getTrendGraphData(type, field, ident);
    }

    public void setBlinkColor() {
        if (blinkColorInt == SWT.COLOR_YELLOW) {
            blinkColorInt = SWT.COLOR_RED;
        } else {
            blinkColorInt = SWT.COLOR_YELLOW;
        }
        blinkColor = Display.getDefault().getSystemColor(blinkColorInt);
    }

    /**
     * Indicates that an alert message has arrived.
     * 
     * This method will be invoked in thread that has been tied to the listener.
     * Thus, it is possible to perform longer-running operations inside
     * implementations of this method. Note that only one thread will be
     * allocated per registration and throughput is potentially throttled per
     * listener.
     * 
     * @param alertMessages
     *            an array of alert messages containing both the original
     *            dataURI message and the decoded equivalent
     */
    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        return;
    }

    /**
     * Fires events to listeners of the IMonitorListener interface generally
     * used to notify updates of data to display elements from classes that
     * implement the IMonitor interface.
     */
    @Override
    public void fireMonitorEvent() {
        return;
    }

    /**
     * Fires events to listeners of the IMonitorListener interface generally
     * used to notify updates of data to display elements from classes that
     * implement the IMonitor interface. This method takes the name of the
     * listener class as an argument that can be used for targeted
     * notifications.
     */
    @Override
    public void fireMonitorEvent(String type) {
        return;
    }

    /**
     * Fires events to listeners of the IMonitorListener interface generally
     * used to notify updates of data to display elements from classes that
     * implement the IMonitor interface.
     */
    @Override
    public void fireMonitorEvent(Monitor monitor) {
        return;
    }

    /**
     * Adds this monitor as a listener to the ProductAlerts
     * 
     * @param pluginName
     */
    @Override
    public void initObserver(String pluginName, Monitor monitor) {
        return;
    }

    /**
     * Order the dates
     * 
     * @param type
     * @return
     */
    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        return monitor.getTimeOrderedKeys(monitor, type);
    }

    /**
     * Check the scan table should be linked to the display so both will update
     * at the same time.
     * 
     * @param type
     *            Scan table.
     * @return True to link the table to the display, false otherwise.
     */
    public boolean getLinkToFrame(String type) {
        return SCANConfig.getInstance()
                .getLinkToFrame(ScanTables.valueOf(type));
    }

    /**
     * This pops up a dialog letting the operator know that you must use the
     * Clear button to close the SCAN dialog.
     * 
     * @param dialogBeingClosed
     *            Name of the dialog trying to be closed.
     */
    protected void displayCloseInstructions(String dialogBeingClosed) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
        mb.setText("Close Information");
        mb.setMessage("To close the " + dialogBeingClosed
                + ",\nyou must clear the D2D display.");
        mb.open();
    }

    /*
     * Abstract methods to be implemented by classes extending this class.
     */
    @Override
    public abstract void notify(IMonitorEvent me);

    public abstract void displayTrendSetGraphs(String ident);

    protected abstract void setTableType();

    protected abstract void initComponents();

    protected abstract void setShellText();

    protected abstract void shellDisposeAction();

    protected abstract void updateAfterConfigLoad();

    public abstract void shellDisposeDialog();

    protected abstract void handleRankMenuEvent(SelectionEvent event);

    public abstract void updateThresh(String attr);

    public abstract void turnOffAlarm();

    public abstract void turnOnAlarm();
}
