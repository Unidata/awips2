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
package com.raytheon.uf.viz.alertviz.ui;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.lang.ProcessBuilder.Redirect;
import java.time.Duration;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MenuDetectEvent;
import org.eclipse.swt.events.MenuDetectListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.swt.widgets.Tray;
import org.eclipse.swt.widgets.TrayItem;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertvizJob;
import com.raytheon.uf.viz.alertviz.ConfigContext;
import com.raytheon.uf.viz.alertviz.ConfigurationManager;
import com.raytheon.uf.viz.alertviz.Constants;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.IAlertArrivedCallback;
import com.raytheon.uf.viz.alertviz.IConfigurationChangedListener;
import com.raytheon.uf.viz.alertviz.IRestartListener;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.ui.audio.AlertAudioMgr;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertMessageDlg;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertPopupMessageDlg;
import com.raytheon.uf.viz.alertviz.ui.dialogs.AlertVisConfigDlg;
import com.raytheon.uf.viz.alertviz.ui.dialogs.SimpleLogViewer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * This is the main class for the alert visualization.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Apr 02, 2009           lvenable  TTR fixes.
 * Nov 18, 2010  2235     cjeanbap  Add AlertViz location preservation.
 * Dec 01, 2010  6532     cjeanbap  Add functionality to restart
 *                                  AlertVisualization App.
 * Jan 21, 2011           cjeanbap  Check Status Message for Internal AlertViz
 *                                  problem(s).
 * Jan 24, 2011  1978     cjeanbap  Add Monitor Tooltip functionality.
 * Jan 28, 2011  4617     cjeanbap  Added Monitor Only functionality.
 * Feb 08, 2011  4617     cjeanbap  Fix bug of multiple windows.
 * Feb 14, 2010  4617     cjeanbap  Changed handling of Exceptions filtering.
 * Mar 03, 2011  8059     rferrel   alertArrived can now play system beep.
 * Mar 04, 2011  6532     rferrel   Restart now works
 * May 02, 2011  9067     cjeanbap  Redraw AlertMessageDlg if a Layout or a
 *                                  Monitor was changed.
 * May 03, 2011  9101     cjeanbap  Pass a clone object into AlertVizPython
 *                                  class.
 * May 31, 2011  8058     cjeanbap  Kill sound based on TextMsgBox id.
 * Jan 17, 2012  27       rferrel   Refactored to allow override of
 *                                  createTrayMenuItems
 * Mar 09, 2015  3856     lvenable  Added a check to determine if the timer is
 *                                  running before changing the icon on the
 *                                  timer action.  If it isn't running then set
 *                                  the icon to the default image.
 * Mar 18, 2015  4234     njensen   Remove reference to non-working python
 * Jun 03, 2015  4473     njensen   Updated for new AlertvizJob API
 * Jun 29, 2015  4311     randerso  Reworking AlertViz dialogs to be resizable.
 * Oct 28, 2015  5054     randerso  Call AlertVisualization.dispose() on restart
 *                                  so all the other dispose methods are called.
 * Jan 25, 2016  5054     randerso  Removed dummy parent shell
 * Feb 08, 2016  5312     randerso  Changed to build tray menu on demand
 * Feb 14, 2017  6029     randerso  Make popup appear on monitor with AlertViz
 *                                  bar
 * Mar 24, 2017  16985    dfiedman  Restore Python script functionality
 * Apr 19, 2018  7013     tgurney   Add optional Exit menu item
 * Sep 11, 2018  7456     randerso  Cleaned up AlertPopupMessageDlg constructor.
 * Sep 20, 2018  7457     randerso  Changes to support AlertAudioMgr refactor.
 *                                  Code cleanup.
 * Sep 28, 2018  7455     randerso  Set errorBtn background based on message
 *                                  priority.
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 09, 2018  7457     randerso  Fix audio playback when text is not enabled.
 * Oct 11, 2018  7515     randerso  Don't filter AV_ADMIN messages
 * Oct 15, 2018  7515     randerso  Moved coloring of error button to
 *                                  AlertMessageDlg
 * Nov 02, 2018  7600     randerso  Changes to support standard script files for
 *                                  AlertViz actions.
 * Nov 05, 2018  7509     randerso  Allow only one System Log to be displayed.
 * Nov 13, 2018  7512     randerso  Moved tray icon images out of localization.
 *                                  Added support for custom popup images
 * Jul 23, 2020  8197     randerso  Removed unnecessary HideListener code
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class AlertVisualization implements IAlertArrivedCallback,
        DisposeListener, IConfigurationChangedListener, IRestartListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVisualization.class, "AV_ADMIN", "AV_ADMIN");

    /** Tray icon blink period in milliseconds */
    private static final Duration TRAY_ICON_BLINK_PERIOD = Duration
            .ofSeconds(1);

    private class TrayBlinkTask extends TimerTask {
        @Override
        public void run() {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (trayItem.isDisposed()) {
                        return;
                    }

                    if (blinking) {
                        ++blinkCount;

                        if (blinkCount >= blinkImages.length) {
                            blinkCount = 0;
                        }
                        trayItem.setImage(blinkImages[blinkCount]);
                    } else {
                        trayItem.setImage(alertVizImg);
                    }
                }
            });
        }
    }

    /**
     * The display control.
     */
    private final Display display;

    /**
     * Alert visualization image.
     */
    private Image alertVizImg;

    /**
     * Alert visualization error image.
     */
    private Image alertVizErrorImg;

    /**
     * System tray object.
     */
    private Tray tray;

    /**
     * Tray item.
     */
    private TrayItem trayItem;

    /**
     * Alert message dialog.
     */
    protected AlertMessageDlg alertMessageDlg;

    /**
     * Text blink count variable.
     */
    private int blinkCount = 0;

    private boolean blinking = false;

    /**
     * Array of blink images.
     */
    private Image[] blinkImages;

    /**
     * Timer for the tray to blink the icon.
     */
    private Timer trayBlinkTimer;

    private TimerTask trayBlinkTask;

    /**
     * Configuration data.
     */
    private Configuration configData;

    /**
     * Alert popup dialog.
     */
    private AlertPopupMessageDlg alertPopupDlg;

    /**
     * Configuration dialog.
     */
    private AlertVisConfigDlg configDlg;

    /**
     * Do not disturb flag.
     */
    private boolean doNotDisturb = false;

    /**
     * Show alert Dialog flag
     */
    private boolean showAlertDlg = true;

    private boolean ackAll = false;

    private boolean showPopup = false;

    /**
     * Tool tip.
     */
    private ToolTip toolTip;

    /**
     * Is this running as a standalone application
     */
    protected final boolean runningStandalone;

    private ConfigContext configContext;

    private Configuration prevConfigFile;

    private Integer exitStatus = IApplication.EXIT_OK;

    private boolean canExit;

    private SimpleLogViewer slv = null;

    /**
     * Constructor.
     *
     * @param runningStandalone
     *            True if the application is running stand-alone.
     * @param canExit
     *            True if user is allowed to exit the application
     * @param display
     *            Display object.
     */
    public AlertVisualization(boolean runningStandalone, boolean canExit,
            Display display) {
        this.display = display;
        this.canExit = canExit;
        this.runningStandalone = runningStandalone;
        ConfigurationManager.getInstance().addListener(this);
        if (Boolean.getBoolean("SystemTray")) {
            showAlertDlg = Boolean.getBoolean("ShowAlertVizBar");
            doNotDisturb = true;
        }
        initializeComponents();

        AlertvizJob.getInstance().addAlertArrivedCallback(this);
    }

    /**
     * Dispose method.
     */
    public void dispose() {

        if (alertPopupDlg != null) {
            alertPopupDlg.dispose();
        }

        if (trayBlinkTimer != null) {
            trayBlinkTimer.cancel();
        }

        if (toolTip != null) {
            toolTip.dispose();
        }

        if (alertVizImg != null) {
            alertVizImg.dispose();
        }

        if (alertVizErrorImg != null) {
            alertVizErrorImg.dispose();
        }

        if (alertMessageDlg != null) {
            alertMessageDlg.dispose();
        }

        if (display != null) {
            display.dispose();
        }
    }

    /**
     * Initialize all of the components and data.
     */
    private void initializeComponents() {
        configurationChanged();

        initAlertMessageDialog();
        initTrayControl();
        prevConfigFile = configData.clone();
    }

    /**
     * Initialize the alert message dialog.
     */
    private void initAlertMessageDialog() {
        alertMessageDlg = new AlertMessageDlg(display, showAlertDlg,
                configData);
        display.syncExec(new Runnable() {
            @Override
            public void run() {
                alertMessageDlg.open();
            }
        });
    }

    /**
     * Initialize the tray control.
     */
    private void initTrayControl() {
        blinkImages = new Image[2];

        Bundle bundle = Activator.getDefault().getBundle();
        alertVizImg = IconUtil.getImage(bundle, "AlertVizIcon.png", display);
        alertVizErrorImg = IconUtil.getImage(bundle, "AlertErrorIcon.png",
                display);

        blinkImages[0] = alertVizImg;
        blinkImages[1] = alertVizErrorImg;

        tray = display.getSystemTray();

        if (tray == null) {
            Container.logInternal(Priority.ERROR,
                    "The system tray is not available");
        } else {
            createTray();
        }
    }

    /**
     * Create the tray control.
     */
    private void createTray() {
        trayItem = new TrayItem(tray, SWT.NONE);
        updateToolTip();

        // Right click action
        trayItem.addMenuDetectListener(new MenuDetectListener() {
            @Override
            public void menuDetected(MenuDetectEvent de) {
                Menu trayItemMenu = new Menu(alertMessageDlg.getShell(),
                        SWT.POP_UP);
                createTrayMenuItems(trayItemMenu);
                trayItemMenu.setVisible(true);
            }
        });

        // Left click action
        trayItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (alertPopupDlg != null) {
                    openAlertPopupDialog();
                }
            }
        });

        trayItem.setImage(alertVizImg);
        trayBlinkTimer = new Timer();
    }

    /**
     * Create the tray menu items.
     *
     * @param menu
     */
    protected void createTrayMenuItems(Menu menu) {

        MenuItem showAlertDialogMI = new MenuItem(menu, SWT.CHECK);
        showAlertDialogMI.setText("Show Alert Dialog");
        showAlertDialogMI.setSelection(showAlertDlg);
        showAlertDialogMI.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                if (alertMessageDlg != null) {
                    showAlertDlg = !showAlertDlg;
                    alertMessageDlg.showDialog(showAlertDlg);
                    if (Boolean.getBoolean("SystemTray")) {
                        System.setProperty("ShowAlertVizBar",
                                Boolean.toString(showAlertDlg));
                    }
                }
            }
        });

        if (Boolean.getBoolean("SystemTray")) {
            MenuItem doNotDisturbMI = new MenuItem(menu, SWT.CHECK);
            doNotDisturbMI.setText("Do Not Disturb");
            doNotDisturbMI.setSelection(doNotDisturb);
            doNotDisturbMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MenuItem item = (MenuItem) event.widget;
                    doNotDisturb = item.getSelection();
                }
            });
        }

        new MenuItem(menu, SWT.SEPARATOR);

        MenuItem configTrayMI = new MenuItem(menu, SWT.NONE);
        configTrayMI.setText("Configuration...");
        configTrayMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showConfigDialog();
            }
        });

        MenuItem viewLogMI = new MenuItem(menu, SWT.NONE);
        viewLogMI.setText("System Log...");
        viewLogMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if ((slv == null) || slv.isDisposed()) {
                    slv = new SimpleLogViewer(display);
                    slv.open();
                } else {
                    slv.bringToTop();
                }
            }
        });

        new MenuItem(menu, SWT.SEPARATOR);

        MenuItem showPopupMI = new MenuItem(menu, SWT.NONE);
        showPopupMI.setText("Show Alert Popup Dialog...");
        showPopupMI.setEnabled(showPopup && (alertPopupDlg != null)
                && !alertPopupDlg.isOpen());
        showPopupMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openAlertPopupDialog();
            }
        });

        MenuItem ackAllMI = new MenuItem(menu, SWT.NONE);
        ackAllMI.setText("Acknowledge All Messages");
        ackAllMI.setEnabled(ackAll);
        ackAllMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (alertPopupDlg != null) {
                    alertPopupDlg.showDialog();
                    alertPopupDlg.acknowledgeAllMessages();
                } else {
                    // should never happen
                    Container.logInternal(Priority.ERROR,
                            "alertPopupDlg unexpectedly null");
                }
            }
        });

        if (this.runningStandalone) {
            new MenuItem(menu, SWT.SEPARATOR);
            MenuItem restartMI = new MenuItem(menu, SWT.NONE);
            restartMI.setText("Restart");
            restartMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    if (openRestartOrExitDialog("Restart")) {
                        restart();
                    }
                }
            });

            if (canExit) {
                new MenuItem(menu, SWT.SEPARATOR);
                MenuItem exitMI = new MenuItem(menu, SWT.NONE);
                exitMI.setText("Exit");
                exitMI.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        if (openRestartOrExitDialog("Exit")) {
                            exitStatus = IApplication.EXIT_OK;
                            AlertVisualization.this.dispose();
                        }
                    }
                });
            }
        }
    }

    private boolean openRestartOrExitDialog(String actionText) {
        MessageDialog dialog = new MessageDialog(alertMessageDlg.getShell(),
                "Confirm " + actionText + "!", null,
                "Any unsaved changes will be lost. " + actionText + " anyway?",
                MessageDialog.QUESTION, new String[] {
                        IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL },
                0);

        dialog.create();

        // center dialog on display
        Shell shell = dialog.getShell();
        Point size = shell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        Rectangle bounds = alertMessageDlg.getShell().getMonitor().getBounds();
        shell.setLocation(bounds.x + ((bounds.width - size.x) / 2),
                bounds.y + ((bounds.height - size.y) / 2));
        return dialog.open() == 0;
    }

    /**
     * Start blinking the tray icon.
     */
    private void startBlinkingIcon() {
        blinking = true;
        trayBlinkTask = new TrayBlinkTask();
        trayBlinkTimer.scheduleAtFixedRate(trayBlinkTask, 0,
                TRAY_ICON_BLINK_PERIOD.toMillis());

    }

    /**
     * Stop blinking the tray icon.
     */
    private void stopBlinkingIcon() {
        blinking = false;
        trayBlinkTask.cancel();
        trayItem.setImage(alertVizImg);
    }

    /**
     * Show the Alert Visualization Configuration dialog.
     */
    private void showConfigDialog() {
        if ((configDlg != null) && !configDlg.isDisposed()) {
            configDlg.close();
        }
        configDlg = new AlertVisConfigDlg(display, alertMessageDlg, configData,
                configContext, this, this);
        configDlg.open();
    }

    /**
     * Handle the alert message.
     *
     * @param statMsg
     *            Status message.
     * @param amd
     *            Alert metadata.
     * @param cat
     *            Category.
     * @param gConfig
     *            Global configuration.
     */
    @Override
    public void alertArrived(final StatusMessage statMsg,
            final AlertMetadata amd, final Category cat,
            final TrayConfiguration gConfig) {

        if ((alertMessageDlg == null) || alertMessageDlg.isDisposed()) {
            Container.logInternal(Priority.ERROR,
                    statMsg.getMessage() + "\n" + statMsg.getDetails());
            return;
        }

        if (amd.getAction() != null) {
            runAlertVizAction(statMsg, amd);
        }

        boolean isAvAdminMessage = "AV_ADMIN".equals(statMsg.getCategory())
                || "AV_ADMIN".equals(statMsg.getSourceKey());

        if (isAvAdminMessage) {
            Container.logInternal(statMsg);
            if (statMsg.getPriority().ordinal() <= Priority.PROBLEM.ordinal()) {
                alertMessageDlg.sendToTextMsgLog(statMsg);
            }
        } else {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (Boolean.getBoolean("SystemTray")
                            && !Boolean.getBoolean("ShowAlertVizBar")) {
                        if (amd.isText()) {
                            textBalloonMessage(statMsg, cat);
                        }
                    } else {
                        if (cat.getCategoryName().equals(Constants.MONITOR)
                                || (amd.isText())) {
                            alertMessageDlg.messageHandler(statMsg, amd, cat,
                                    gConfig);
                        }
                    }
                }
            });
        }

        // Handle audio if text is not enabled, otherwise it's handled in the
        // text message box
        if (!amd.isText()) {
            File audioFile = AlertAudioMgr.getAudioFile(amd, statMsg);
            if (audioFile != null || amd.isAudioEnabled()) {
                Duration duration = Duration
                        .ofSeconds(gConfig.getAudioDuration());
                AlertAudioMgr.loopSound(audioFile, duration);
            }
        }

        // Pop-up message
        if (amd.isPopup()) {
            if (alertPopupDlg == null) {
                alertPopupDlg = new AlertPopupMessageDlg(
                        alertMessageDlg.getShell(), gConfig.isExpandedPopup());

                alertPopupDlg.addDisposeListener(this);

                showPopup = true;
                ackAll = true;
                startBlinkingIcon();
            }
            alertPopupDlg.addNewMessage(statMsg, amd);

            updateToolTip();
            if (!doNotDisturb) {
                openAlertPopupDialog();
            }
        }
    }

    private void runAlertVizAction(final StatusMessage statMsg,
            final AlertMetadata amd) {
        String scriptName = StringUtils.trimToNull(amd.getAction());
        if (scriptName != null) {
            try {
                IPathManager pm = PathManagerFactory.getPathManager();
                LocalizationFile locFile = pm
                        .getStaticLocalizationFile(LocalizationUtil
                                .join("alertViz", "actions", scriptName));

                if (locFile == null || !locFile.exists()) {
                    statusHandler.fatal(String.format(
                            "AlertViz script %s not found", scriptName));
                    return;
                }

                File scriptFile = locFile.getFile();
                scriptFile.setExecutable(true);

                Thread thread = new Thread(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            ProcessBuilder pb = new ProcessBuilder(
                                    scriptFile.getAbsolutePath(),
                                    statMsg.getSourceKey(),
                                    statMsg.getCategory(),
                                    Integer.toString(
                                            statMsg.getPriority().ordinal()),
                                    statMsg.getMessage());
                            pb.redirectOutput(Redirect.INHERIT);

                            statusHandler.info(String.format(
                                    "Running AlertViz script %s", locFile));
                            Process process = pb.start();

                            StringBuilder sb = new StringBuilder();
                            try (BufferedReader errorReader = new BufferedReader(
                                    new InputStreamReader(
                                            process.getErrorStream()))) {
                                String line;
                                while ((line = errorReader
                                        .readLine()) != null) {
                                    sb.append(line).append('\n');
                                }
                            }

                            int returnCode = process.waitFor();
                            String stdErr = sb.toString();
                            if (returnCode != 0 || !stdErr.isEmpty()) {
                                statusHandler.fatal(String.format(
                                        "Alertviz script %s returned %d\n%s",
                                        locFile, returnCode, stdErr));
                            }
                        } catch (Throwable e) {
                            statusHandler.fatal(String.format(
                                    "Error executing AlertViz script %s:",
                                    locFile), e);
                        }
                    }
                });

                thread.setName("AlertViz-" + scriptName);
                thread.start();
            } catch (Exception e) {
                statusHandler.fatal(String.format(
                        "Error executing AlertViz script %s:", scriptName), e);
            }
        }
    }

    /**
     * Opens the alert pop-up dialog
     */
    public void openAlertPopupDialog() {
        if (alertPopupDlg != null) {
            alertPopupDlg.showDialog();
        } else {
            // should never happen
            Container.logInternal(Priority.ERROR,
                    "alertPopupDlg unexpectedly null");
        }
    }

    /**
     * Displays a pop-up balloon message
     *
     * @param statMsg
     *            Message to display.
     * @param cat
     *            Category of message.
     */
    private void textBalloonMessage(StatusMessage statMsg, Category cat) {

        if (toolTip != null) {
            toolTip.dispose();
        }

        toolTip = new ToolTip(alertMessageDlg.getShell(),
                SWT.BALLOON | SWT.ICON_WARNING);
        toolTip.setText(cat.getCategoryName());
        toolTip.setMessage(statMsg.getMessage());

        trayItem.setToolTip(toolTip);
        toolTip.setVisible(true);
    }

    /**
     * Adds a tool tip to the tray icon, if messages need to be acknowledged,
     * shows the number, otherwise displays general text.
     */
    private void updateToolTip() {
        if (alertPopupDlg == null) {
            this.trayItem.setToolTipText("AlertViz Menu");
        } else {
            int messages = alertPopupDlg.getNumberOfMessages();
            if (messages == 1) {
                this.trayItem.setToolTipText(
                        "There is " + messages + " message to be acknowledged");
            } else {
                this.trayItem.setToolTipText("There are " + messages
                        + " messages to be acknowledged");
            }
        }
    }

    /**
     * @return the exit status
     */
    public Integer getExitStatus() {
        return exitStatus;
    }

    /**
     * Called when the alertPopupDlg is disposed.
     */
    @Override
    public void widgetDisposed(DisposeEvent e) {
        alertPopupDlg = null;
        stopBlinkingIcon();
        updateToolTip();

        ackAll = false;
        showPopup = false;
    }

    @Override
    public void restart() {
        if (runningStandalone) {
            // Must use EXIT_RELAUNCH. EXIT_RESTART causes the
            // executable to do a restart without returning to
            // the shell/bat script. This fails. Any other value
            // such as Integer(1) the executable attempts to bring
            // up an error screen before exiting with the error code.
            exitStatus = IApplication.EXIT_RELAUNCH;
            this.dispose();
        }
    }

    @Override
    public void configurationChanged() {
        ConfigurationManager.getInstance().resetCustomLocalization();
        configData = ConfigurationManager.getInstance()
                .getCurrentConfiguration();
        configContext = ConfigurationManager.getInstance().getCurrentContext();
        if (alertMessageDlg != null) {
            alertMessageDlg.setConfigData(configData);
            if (configData.isMonitorLayoutChanged(prevConfigFile)) {
                if (alertMessageDlg.reLayout()) {
                    showAlertDlg = true;
                }
                prevConfigFile = configData.clone();
            }
        }
    }

}
