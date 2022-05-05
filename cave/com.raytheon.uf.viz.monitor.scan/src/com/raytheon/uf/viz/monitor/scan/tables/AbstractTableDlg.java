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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
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
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Abstract dialog class used for the CALL, DND, MESO, and TVS table dialogs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 21, 2009  3039     lvenable  Initial creation
 * Apr 26, 2013  1945     lvenable  Some code cleanup.
 * Jun 06, 2013  2065     lvenable  Added convenience method to alert the user
 *                                  to use the clear button if they want to
 *                                  close the dialog.
 * Jul 24, 2013  2218     mpduff    Changed method signature.
 * Jul 26, 2013  2143     skorolev  Changes for non-blocking dialog.
 * Aug 15, 2013  2143     mpduff    Change how the dialogs close to prevent
 *                                  ConcurrentModificationException.
 * Dec 04, 2013  2592     lvenable  Update how the checkboxes are handled
 *                                  (background/foreground colors) since the
 *                                  Redhat 6 upgrade causes the check in the
 *                                  checkbox to be colored the same as the
 *                                  background.
 * May 10, 2016  5516     randerso  Lots of GUI clean up
 * Mar 27, 2018  6942     tgurney   Set CAVE.INDEPENDENT_SHELL to allow hiding
 *                                  the dialog behind CAVE
 * Jul 16, 2018  6766     randerso  Remove unused method
 * Jul 25, 2018  6748     randerso  Code cleanup.
 * Aug  1, 2018  6567     tgurney   Config save confirmation dialog, include
 *                                  the table name in message
 *
 * </pre>
 *
 * @author lvenable
 */
public abstract class AbstractTableDlg extends CaveSWTDialog
        implements IMonitor, IMonitorListener, IMonitorControlListener,
        ITableAction, IRequestTrendGraphData {
    /** The display object. */
    protected Display display;

    /** Array listening monitors */
    protected List<IMonitor> controlListeners = new ArrayList<>();

    /** The SCAN table type */
    protected ScanTables scanTable;

    /** the site identifier */
    protected String site = "";

    /**
     * Array of available dialogs that can be launched. This array is used to
     * close down dialog that are displayed. Dialogs will be closed before
     * loading a configuration.
     */
    protected HashMap<ICommonDialogAction, Object> dialogsMap;

    /** Instance of the SCAN configuration. */
    protected SCANConfig scanCfg;

    /** File button that is common to all of the tables. */
    protected Button fileBtn;

    /** Tip text for the file button. */
    private String fileButtonTipText = null;

    protected Timer timer;

    protected TimerTask timerTask;

    protected int blinkColorInt = SWT.COLOR_RED;

    protected Color blinkColor;

    protected SCANAlarmAlertManager mgr = null;

    private LoadSaveConfigDlg loadDlg = null;

    private LoadSaveConfigDlg saveDlg = null;

    /**
     * Abstract constructor.
     *
     * @param parentShell
     *            Parent shell.
     */
    public AbstractTableDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);

        scanCfg = SCANConfig.getInstance();
    }

    @Override
    protected Layout constructShellLayout() {

        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return (mainLayout);
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        display = getDisplay();
        initData();

        setShellText();

        setTableType();
    }

    /** Initialize the data. */
    private void initData() {
        dialogsMap = new HashMap<>();
    }

    /** @return The SCAN table type. */
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
     * Prompt the user if they want to close the open dialogs when they want to
     * load a new configuration.
     *
     * @return true if open dialogs were closed
     */
    private boolean closedOpenDialogs() {
        // Prompt the user to close the dialogs
        if (!dialogsMap.isEmpty()) {
            StringBuilder sb = new StringBuilder();

            sb.append(
                    "There are open dialogs that need to be closed before loading ");
            sb.append("a new configuration.\n\n");
            sb.append(
                    "Click 'OK' to close all dialogs and resume loading a configuration.");
            sb.append("\n\nClick 'Cancel' to stop loading a configuration.");

            MessageBox mb = new MessageBox(shell,
                    SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
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
        List<ICommonDialogAction> toClose = new ArrayList<>(keys.size());
        for (ICommonDialogAction icda : keys) {
            toClose.add(icda);
        }

        for (ICommonDialogAction icda : toClose) {
            icda.closeDialog();
        }
        dialogsMap.clear();

        return true;
    }

    /** Retrieve the default configuration. */
    protected void retrieveDefaultConfig() {
        /*
         * TODO : when loading the default config, the controls on the dialog
         * needs to be updated and a new table data should be loaded
         */
        scanCfg.loadDefaultConfigFileName(scanTable);

        updateAfterConfigLoad();
        updateFileButton();
    }

    /** Retrieve an existing configuration. */
    protected void retrieveExistingConfig() {
        /*
         * TODO : when loading an existing config, the controls on the dialog
         * needs to be updated and a new table data should be loaded
         */

        if (!closedOpenDialogs()) {
            return;
        }

        if (loadDlg == null || loadDlg.isDisposed()) {
            loadDlg = new LoadSaveConfigDlg(shell, DialogType.OPEN, scanTable);
            loadDlg.addCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        scanCfg.loadNewConfigFileName(scanTable,
                                fileName.getFile().getName());
                        updateAfterConfigLoad();
                        updateFileButton();
                    } else {
                        return;
                    }
                }
            });
            loadDlg.open();
        } else {
            loadDlg.bringToTop();
        }
    }

    /** Save the current configuration. */
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

        if (scanCfg.currentConfigIsDefault(scanTable)) {
            MessageBox mb = new MessageBox(shell,
                    SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
            mb.setText("Overwrite");
            mb.setMessage("Saving will overwrite the default " + scanTable
                    + " configuration.\n" + "Do you wish to continue?");
            int result = mb.open();

            // If the user selected Cancel then return.
            if (result == SWT.CANCEL) {
                return;
            }
        }

        scanCfg.saveCurrentConfigurationFile(scanTable);
        updateFileButton();
    }

    /** Save the current configuration as a different name. */
    protected void saveConfigurationAs() {
        /*
         * TODO : launch the save dialog and then get a name to save the
         * configuration.
         *
         * do not need to update the display...
         */
        if (saveDlg == null || saveDlg.isDisposed()) {
            saveDlg = new LoadSaveConfigDlg(shell, DialogType.SAVE_AS,
                    scanTable);
            saveDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof LocalizationFile) {
                        String defCfgName = scanCfg
                                .getDefaultConfigName(scanTable);
                        LocalizationFile fileName = (LocalizationFile) returnValue;
                        if (defCfgName
                                .compareTo(fileName.getFile().getName()) == 0) {
                            MessageBox mb = new MessageBox(shell,
                                    SWT.ICON_WARNING | SWT.OK | SWT.CANCEL);
                            mb.setText("Overwrite");
                            mb.setMessage(
                                    "The Save As name is the same as the default configuration name.  Saving "
                                            + "will overwrite the default configuration.\n"
                                            + "Do you wish to continue?");
                            int result = mb.open();

                            // If the user selected Cancel then return.
                            if (result == SWT.CANCEL) {
                                return;
                            }
                        }
                        scanCfg.saveConfigurationFileAs(scanTable,
                                fileName.getFile().getName());
                    }
                    updateFileButton();
                }
            });
            saveDlg.open();
        } else {
            saveDlg.bringToTop();
        }
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
     *
     * @param rankPopupMenu
     * @param rankBtn
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
        Map<String, Object> availRankMap = scanCfg
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
            DataTime dt = scanMonitor.getMostRecent(scanTable.name(), site);

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
                for (IMonitor m : getMonitorControlListeners()) {
                    ((ScanMonitor) m).configUpdate(fimce);
                }
            }
        });
    }

    @Override
    public void fireKillMonitor() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                for (IMonitor m : getMonitorControlListeners()) {
                    ((ScanMonitor) m).nullifyMonitor(site);
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
                for (IMonitor m : getMonitorControlListeners()) {
                    ((ScanMonitor) m).thresholdUpdate(fimte);
                }
            }
        });
    }

    /**
     * @param type
     * @param field
     * @param ident
     * @return graph data from the monitor
     */
    public TrendGraphData getTrendGraphData(ScanTables type, String field,
            String ident) {
        TrendGraphData tgd = null;

        for (IMonitor m : getMonitorControlListeners()) {
            tgd = ((ScanMonitor) m).getGraphData(type, site, field, ident);
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
        List<IMonitor> monitorArray = getMonitorControlListeners();

        if (!monitorArray.isEmpty()) {
            return (ScanMonitor) monitorArray.get(0);
        }

        return null;
    }

    /**
     * Fires a re-center of the screen for the drawing portion
     *
     * @param ident
     * @param type
     * @param icao
     */
    public void fireRecenter(String ident, ScanTables type, String icao) {
        final String fident = ident;
        final ScanTables ftype = type;
        final String ficao = icao;
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                for (IMonitor m : getMonitorControlListeners()) {
                    ((ScanMonitor) m).recenter(fident, ftype, ficao);
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
                for (IMonitor m : getMonitorControlListeners()) {
                    ((ScanMonitor) m).paintScan();
                }
            }
        });
    }

    /**
     * Display trend set graphs (for relevant table types only)
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

        Composite chkLblComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 2;
        gl.horizontalSpacing = 0;
        chkLblComp.setLayout(gl);
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        chkLblComp.setLayoutData(gd);

        Button chkBox = new Button(chkLblComp, SWT.CHECK);
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, true);
        chkBox.setLayoutData(gd);

        if (colorComposite) {
            chkBox.setBackground(bgColor);
            chkLblComp.setBackground(bgColor);
        }

        Label lbl = new Label(chkLblComp, SWT.NONE);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        lbl.setLayoutData(gd);
        lbl.setBackground(bgColor);
        lbl.setForeground(fgColor);
        lbl.setText(labelText);

        chkBox.setToolTipText(toolTipText);
        lbl.setToolTipText(toolTipText);

        return chkBox;
    }

    @Override
    public List<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    /** @return the current shell */
    public Shell getCurrentShell() {
        return shell;
    }

    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                for (IMonitor m : getMonitorControlListeners()) {
                    ((ScanMonitor) m).closeDialog(scanTable, site);
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
     * @param monitor
     */
    @Override
    public void initObserver(String pluginName, Monitor monitor) {
        return;
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

    protected abstract void setShellText();

    protected abstract void updateAfterConfigLoad();

    public abstract void shellDisposeDialog();

    protected abstract void handleRankMenuEvent(SelectionEvent event);

    public abstract void updateThresh(String attr);

    public abstract void turnOffAlarm();

    public abstract void turnOnAlarm();
}
