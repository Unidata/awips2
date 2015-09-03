/**
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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.io.File;
import java.io.FileNotFoundException;

import org.eclipse.equinox.app.IApplication;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuDetectEvent;
import org.eclipse.swt.events.MenuDetectListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.swt.widgets.Tray;
import org.eclipse.swt.widgets.TrayItem;
import org.eclipse.ui.themes.ColorUtil;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertVizPython;
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
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.ui.audio.AlertAudioMgr;
import com.raytheon.uf.viz.alertviz.ui.audio.IAudioAction;
import com.raytheon.uf.viz.alertviz.ui.timer.AlertTimer;
import com.raytheon.uf.viz.alertviz.ui.timer.ITimerAction;
import com.raytheon.uf.viz.core.VizApp;

/**
 * This is the main class for the alert visualization.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 18 Nov 2010  2235       cjeanbap    Add AlertViz location preservation.
 * 01 Dec 2010  6532       cjeanbap    Add functionality to restart AlertVisualization App.
 * 21 Jan 2011             cjeanbap    Check Status Message for Internal AlertViz problem(s).
 * 24 Jan 2011  1978       cjeanbap    Add Monitor Tooltip functionality.
 * 28 Jan 2011  4617       cjeanbap    Added Monitor Only functionality.
 * 08 Feb 2011  4617       cjeanbap    Fix bug of multiple windows.
 * 14 Feb 2010  4617       cjeanbap    Changed handling of Exceptions filtering.
 * 03 Mar 2011  8059       rferrel     alertArrived can now play system beep.
 * 04 Mar 2011  6532       rferrel     Restart now works
 * 02 May 2011  9067       cjeanbap    Redraw AlertMessageDlg if a Layout or a Monitor was
 *                                     changed.
 * 03 May 2011  9101       cjeanbap    Pass a clone object into AlertVizPython class.
 * 31 May 2011  8058       cjeanbap    Kill sound based on TextMsgBox id.
 * 17 Jan 2012  27         rferrel     Refactored to allow override of createTrayMenuItems
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertVisualization implements ITimerAction, IAudioAction,
        IAlertArrivedCallback, Listener, IConfigurationChangedListener,
        IRestartListener {
    /**
     * Dialog shell.
     */
    protected Shell shell;

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
     * A pop-up menu for the tray item.
     */
    protected Menu trayItemMenu;

    /**
     * Show alert visualization menu item.
     */
    private MenuItem showAlertDialogMI;

    /**
     * Do not disturb menu item.
     */
    private MenuItem doNotDisturbMI;

    /**
     * Show alert dialog popup menu item
     */
    private MenuItem showPopup;

    /**
     * Acknowledge all alerts menu item
     */
    private MenuItem ackAll;

    /**
     * Alert message dialog.
     */
    private AlertMessageDlg alertMessageDlg;

    /**
     * Text blink count variable.
     */
    private int blinkCount = 0;

    /**
     * Array of blink images.
     */
    private Image[] blinkImages;

    /**
     * Timer for the tray to blink the icon.
     */
    private AlertTimer trayAlertTimer;

    /**
     * Audio manager used to set the audio file and play sounds.
     */
    private AlertAudioMgr audioMgr;

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

    /**
     * Tool tip.
     */
    private ToolTip toolTip;

    /**
     * Is this running as a standalone application
     */
    protected final boolean runningStandalone;

    private Rectangle prevLocation = null;

    private ConfigContext configContext;

    private Configuration prevConfigFile;
    
    private Integer exitStatus = IApplication.EXIT_OK;

    /**
     * Constructor.
     * 
     * @param runningStandalone
     *            True if the application is running stand-alone.
     * @param display
     *            Display object.
     */
    public AlertVisualization(boolean runningStandalone, Display display) {
        this.display = display;
        this.runningStandalone = runningStandalone;
        ConfigurationManager.getInstance().addListener(this);
        /*
        if (Boolean.getBoolean("SystemTray")) {
            showAlertDlg = Boolean.getBoolean("ShowAlertVizBar");
            doNotDisturb = true;
        }
        */
        showAlertDlg = false;
        doNotDisturb = true;
        initShell();
    }

    /**
     * Initialize a main shell.
     */
    private void initShell() {
        shell = new Shell(display);

        initializeComponents();

        AlertvizJob.addAlertArrivedCallback(this);
    }

    /**
     * Dispose method.
     */
    public void dispose() {

        if (alertPopupDlg != null) {
            alertPopupDlg.dispose();
        }

        if (trayAlertTimer != null) {
            cancelTimer();
        }

        if (audioMgr != null) {
            int numTextBoxComp = getNumberOfTextControls();
            for (int i = 0; i < numTextBoxComp; i++) {
                audioMgr.stopTimer(i);
            }
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

        if (shell != null) {
            shell.dispose();
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
        audioMgr = new AlertAudioMgr(display, getNumberOfTextControls());
        trayAlertTimer = new AlertTimer(display, this, 1000);

        initAlertMessageDialog();
        initTrayControl();
        prevConfigFile = configData.clone();
    }

    /**
     * Initialize the alert message dialog.
     */
    private void initAlertMessageDialog() {
        alertMessageDlg = new AlertMessageDlg(shell, this, showAlertDlg,
                configData, audioMgr);
        display.asyncExec(new Runnable() {
            public void run() {
                alertMessageDlg.open();
                alertMessageDlg.showDialog(false);
            }
        });
    }

    /**
     * Initialize the tray control.
     */
    private void initTrayControl() {
        blinkImages = new Image[2];

        alertVizImg = new Image(display, loadAlertVizImage());
        alertVizErrorImg = new Image(display, loadAlertVizErrorImage());

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
        addToolTip();

        trayItemMenu = new Menu(shell, SWT.POP_UP);

        createTrayMenuItems();

        // Right click action
        trayItem.addMenuDetectListener(new MenuDetectListener() {
            public void menuDetected(MenuDetectEvent de) {
                trayItemMenu.setVisible(true);
            }
        });

        // Left click action
        trayItem.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (alertPopupDlg != null
                        && alertPopupDlg.getNumberOfMessages() > 0) {
                    cancelTimer();
                    openAlertPopupDialog();
                }
            }
        });

        trayItem.setImage(alertVizImg);
    }

    /**
     * Create the tray menu items.
     */
    protected void createTrayMenuItems() {

        showAlertDialogMI = new MenuItem(trayItemMenu, SWT.CHECK);
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
            doNotDisturbMI = new MenuItem(trayItemMenu, SWT.CHECK);
            doNotDisturbMI.setText("Do Not Disturb");
            doNotDisturbMI.setSelection(doNotDisturb);
            doNotDisturbMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    doNotDisturb = doNotDisturbMI.getSelection();
                }
            });
        }

        new MenuItem(trayItemMenu, SWT.SEPARATOR);

        MenuItem configTrayMI = new MenuItem(trayItemMenu, SWT.NONE);
        configTrayMI.setText("Configuration...");
        configTrayMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showConfigDialog();
            }
        });

        MenuItem viewLogMI = new MenuItem(trayItemMenu, SWT.NONE);
        viewLogMI.setText("System Log...");
        viewLogMI.addSelectionListener(new SelectionAdapter() {
            boolean open = false;

            SimpleLogViewer slv = null;

            @Override
            public void widgetSelected(SelectionEvent event) {
                if (open) {
                    open = slv.focus(); // Do Nothing! It's open
                } else {
                    open = true;
                    slv = new SimpleLogViewer(shell);
                    slv.setText("System Log");
                    slv.open();
                    open = false;
                }
            }
        });

        new MenuItem(trayItemMenu, SWT.SEPARATOR);

        showPopup = new MenuItem(trayItemMenu, SWT.NONE);
        showPopup.setText("Show Alert Popup Dialog...");
        showPopup.setEnabled(false);
        showPopup.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (alertPopupDlg != null) {
                    openAlertPopupDialog();
                }
            }
        });

        ackAll = new MenuItem(trayItemMenu, SWT.NONE);
        ackAll.setText("Acknowledge All Messages");
        ackAll.setEnabled(false);
        ackAll.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Verify user meant to acknowledge all messages
                boolean rval = MessageDialog.openConfirm(shell, "Confirm",
                        "Acknowledge all messages?");

                if (rval == true) {
                    if (alertPopupDlg != null) {
                        alertPopupDlg.acknowledgeAllMessages(false);
                        alertPopupDlg = null;
                        addToolTip();
                        ackAll.setEnabled(false);
                        showPopup.setEnabled(false);
                    }
                    cancelTimer();
                }
            }
        });

        if (this.runningStandalone) {
            new MenuItem(trayItemMenu, SWT.SEPARATOR);
            MenuItem restartMI = new MenuItem(trayItemMenu, SWT.NONE);
            restartMI.setText("Restart");
            restartMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION
                            | SWT.YES | SWT.NO);
                    mb.setMessage("Any unsaved changes will be lost. Restart anyway?");
                    if (mb.open() == SWT.YES) {
                        restart();
                    }
                }
            });
        }

    }

    /**
     * Start the tray icon blink timer.
     */
    private void startBlinkTrayTimer() {
        trayAlertTimer.startTimer(20000);
    }

    /**
     * Timer action to change the tray icons to emulate blinking.
     */
    @Override
    public void timerAction(boolean ignoreThisArg) {
        if (trayItem.isDisposed()) {
            return;
        }
        ++blinkCount;

        if (blinkCount >= blinkImages.length) {
            blinkCount = 0;
        }

        trayItem.setImage(blinkImages[blinkCount]);
    }

    /**
     * Reset the tray icon when the blink timer is complete.
     */
    @Override
    public void timerCompleted() {
        trayItem.setImage(alertVizImg);
    }

    /**
     * Cancel playing the audio file.
     */
    @Override
    public void cancelAudio() {
        int numTextBoxComp = getNumberOfTextControls();
        for (int i = 0; i < numTextBoxComp; i++) {
            audioMgr.stopTimer(i);
        }
    }

    public void cancelAudio(int numTextMsgBoxId) {
        audioMgr.stopTimer(numTextMsgBoxId);
    }

    /**
     * Load the alert visualization image.
     * 
     * @return Image path.
     */
    private String loadAlertVizImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + "alertViz.png")
                .getAbsolutePath();
        return path;
    }

    /**
     * Load the alert visualization error image.
     * 
     * @return Image path.
     */
    private String loadAlertVizErrorImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + "alertError.png")
                .getAbsolutePath();
        return path;
    }

    /**
     * Show the Alert Visualization Configuration dialog.
     */
    private void showConfigDialog() {
        if (configDlg != null && !configDlg.isDisposed()) {
            configDlg.close();
        }
        configDlg = new AlertVisConfigDlg(shell, alertMessageDlg, configData,
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

        if (shell.isDisposed()) {
            Container.logInternal(Priority.ERROR, statMsg.getMessage() + "\n"
                    + statMsg.getDetails());
            return;
        }

        // Run python script (this is done early since this will not block)
        if (amd.isPythonEnabled() == true) {
            try {
                AlertVizPython.enqueue(statMsg, amd.clone(), gConfig);
            } catch (FileNotFoundException e) {
                Container.logInternal(Priority.ERROR,
                        "AlertVizualization: exception python script not found: "
                                + amd.getPythonScript(), e);
            }
        }

        boolean isGdnAdminMessage = statMsg.getCategory().equals("GDN_ADMIN")
                || statMsg.getSourceKey().equals("GDN_ADMIN");

        if (isGdnAdminMessage) {
            // Container.logInternal(statMsg);
            if ((statMsg.getDetails() != null)
                    && (statMsg.getDetails().contains("Error")
                            || statMsg.getDetails().contains("Exception")
                            || statMsg.getDetails().contains("Throwable") || Container
                            .hasMissing(statMsg))) {
                Source source = configData.lookupSource("GDN_ADMIN");
                RGB backgroundRBG = null;
                if (source == null || source.getConfigurationItem() == null) {
                    backgroundRBG = ColorUtil.getColorValue("COLOR_YELLOW");
                } else {
                    AlertMetadata am = source.getConfigurationItem().lookup(
                            Priority.SIGNIFICANT);
                    backgroundRBG = am.getBackground();
                }
                alertMessageDlg.setErrorLogBtnBackground(backgroundRBG);
                alertMessageDlg.sendToTextMsgLog(statMsg);
            }
        } else {

            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (Boolean.getBoolean("SystemTray")
                            && !Boolean.getBoolean("ShowAlertVizBar")) {
                        if (amd.isText() == true) {
                            textBalloonMessage(statMsg, cat);
                        }
                    } else {
                        if (cat.getCategoryName().equals(Constants.MONITOR)
                                || amd.isText() == true) {
                            alertMessageDlg.messageHandler(statMsg, amd, cat,
                                    gConfig);
                        }

                        if (amd.isAudioEnabled() == true) {
                            alertMessageDlg.audioHandler(cat, gConfig);
                        }
                    }
                }

            });
        }

        // Play audio
        if (amd.isAudioEnabled() == true) {
            int textMsgBoxId = cat.getTextBox() - 1;
            // check for associated text message box; -1 is NONE
            if (textMsgBoxId >= 0) {
                audioMgr.stopTimer(textMsgBoxId);
                int durations = gConfig.getAudioDuration();
                String audioFile = amd.getAudioFile();
                if (audioFile != null) {
                    audioFile = getFullAudioFilePath(audioFile);
                } else {
                    audioFile = statMsg.getAudioFile();
                    if (audioFile != null
                            && (audioFile.trim().length() == 0 || audioFile
                                    .equals("NONE"))) {
                        audioFile = null;
                    }
                    audioFile = getFullAudioFilePath(audioFile);
                }
                audioMgr.setAudioFile(audioFile, durations, textMsgBoxId);
                audioMgr.playLoopSound(textMsgBoxId);
            }
        }

        // Pop-up message
        if (amd.isPopup() == true) {
            if (alertPopupDlg == null || alertPopupDlg.isDisposed() == true) {
                alertPopupDlg = new AlertPopupMessageDlg(shell, statMsg,
                        gConfig.isExpandedPopup(), this, amd.getBackground(),
                        this);
            } else {
                alertPopupDlg.addNewMessage(statMsg, amd);
            }
            startBlinkTrayTimer();
            addToolTip();
            showPopup.setEnabled(true);
            ackAll.setEnabled(true);
            if (doNotDisturb == false) {
                openAlertPopupDialog();
            }
        }
    }

    /**
     * Get full path name to a existing audio file.
     * 
     * @param fname
     *            - name of audio file
     * @return filename when file found otherwise null
     */
    private String getFullAudioFilePath(String fname) {
        if (fname == null) {
            return null;
        }
        String filename = fname.trim();
        if (filename.isEmpty()) {
            return null;
        }
        File file = new File(filename);
        if (!file.exists()) {
            file = PathManagerFactory.getPathManager().getStaticFile(
                    "alertVizAudio/" + file.getName());
            if (file == null || !file.exists()) {
                filename = null;
            } else {
                filename = file.getPath();
            }
        }
        return filename;
    }

    /**
     * Opens the alert pop-up dialog
     */
    public void openAlertPopupDialog() {
        if (alertPopupDlg != null && alertPopupDlg.dialogIsOpen() == true) {
            alertPopupDlg.showDialog(true);
        } else {
            alertPopupDlg.open();
            alertPopupDlg = null;
            cancelTimer();
            addToolTip();
            ackAll.setEnabled(false);
            showPopup.setEnabled(showAlertDlg);
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

        toolTip = new ToolTip(shell, SWT.BALLOON | SWT.ICON_WARNING);
        toolTip.setText(cat.getCategoryName());
        toolTip.setMessage(statMsg.getMessage());

        trayItem.setToolTip(toolTip);
        toolTip.setVisible(true);
    }

    /**
     * Adds a tool tip to the tray icon, if messages need to be acknowledged,
     * shows the number, otherwise displays general text.
     */
    private void addToolTip() {
        if (alertPopupDlg == null) {
            this.trayItem.setToolTipText("AlertViz Menu");
        } else {
            int messages = alertPopupDlg.getNumberOfMessages();
            if (messages == 1) {
                this.trayItem.setToolTipText("There is " + messages
                        + " message to be acknowledged");
            } else {
                this.trayItem.setToolTipText("There are " + messages
                        + " messages to be acknowledged");
            }
        }
    }

    /**
     * Cancels the blinking timer.
     */
    private void cancelTimer() {
        if (trayAlertTimer.timerIsRunning()) {
            trayAlertTimer.cancelTimer();
        }
    }

    public Integer getExitStatus() {
    	return exitStatus;
    }
    
    /**
     * This is the button click event for the alertPopupDialog. This function is
     * called when "Hide Dialog" is clicked.
     */
    @Override
    public void handleEvent(Event event) {
        ackAll.setEnabled(true);
        showPopup.setEnabled(true);
        startBlinkTrayTimer();
        addToolTip();
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
        	display.dispose();
        }
    }

    @Override
    public void configurationChanged() {
        ConfigurationManager.getInstance().resetCustomLocalization();
        configData = ConfigurationManager.getInstance()
                .getCurrentConfiguration();
        configContext = ConfigurationManager.getInstance().getCurrentContext();
        if (alertMessageDlg != null && showAlertDialogMI != null) {
            alertMessageDlg.setConfigData(configData);
            if (configData.isMonitorLayoutChanged(prevConfigFile)) {
                if (alertMessageDlg.reLayout()) {
                    showAlertDialogMI.setEnabled(true);
                }
                prevConfigFile = configData.clone();
            }
            audioMgr = alertMessageDlg.getAlertAudioManager();
        }
        // if (configDlg != null && !configDlg.isDisposed()) {
        // configDlg.restart(requestRestart);
        // }
    }

    /**
     * Set the Alert Popup Message previous location.
     */
    public void setAlertPopupMsgPrvLocation(Rectangle prevLocation) {
        this.prevLocation = prevLocation;
    }

    /**
     * Get the Alert Popup Message to restore the window to its previous
     * location.
     */
    public Rectangle getAlertPopupMsgPrvLocation() {
        return this.prevLocation;
    }

    /**
     * Get the number of text control composites.
     * 
     * @return The number of text control composites.
     */
    private int getNumberOfTextControls() {
        int retVal = 1;

        TrayConfiguration.TrayMode layoutMode = configData
                .getGlobalConfiguration().getMode();

        switch (layoutMode) {
        case H1:
            retVal = 1;
            break;
        case V2:
        case H2:
            retVal = 2;
            break;
        case V3:
            retVal = 3;
            break;
        case V4:
        case Q4:
            retVal = 4;
            break;
        case MO:
            break;
        }

        return retVal;
    }
}