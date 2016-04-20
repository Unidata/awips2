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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ConcurrentSkipListMap;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.alertviz.Activator;
import com.raytheon.uf.viz.alertviz.ConfigContext;
import com.raytheon.uf.viz.alertviz.ConfigurationManager;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ConfigurationMonitor;
import com.raytheon.uf.viz.alertviz.config.MonitorMetadata;
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.ui.audio.AlertAudioMgr;
import com.raytheon.uf.viz.alertviz.ui.audio.IAudioAction;

/**
 * This class displays the dialog showing all of the in-bound messages.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 16 Nov 2010  6144       cjeanbap    Add more detail to Tip info
 * 22 Nov 2010  2235       cjeanbap    Added audio file functionality.
 * 06 Dec 2010  6144       cjeanbap    Updated ToolTipText message.
 * 15 Dec 2010  5149       cjeanbap    Fix Audio bug.
 * 24 Jan 2011  1978       cjeanbap    Add Monitor Tooltip functionality.
 * 28 Jan 2011  4617       cjeanbap    Added Monitor Only functionality.
 * 10 Feb 2011  4617       cjeanbap    Fix NullPointerException if position is null.
 * Mar 2, 2011  5632       cjeanbap    Added sort based on category.
 * Mar 4, 2011  7950       rferrel     Check for null contextName.
 * Mar 9, 2011  8058       rferrel     Associate audio alert with text component.
 * May 2, 2011  9067       cjeanbap    Preserve text component text on relayout().
 * 27 May 2011  9575       cjeanbap    Moved moveLabel to first image in list.
 * 31 May 2011  8058       cjeanbap    Kill sound based on TextMsgBox id.
 * 26 Aug 2013  #2293      lvenable    Fixed color memory leaks.
 * 02 Jun 2015  4473       mschenke    Remember dialog position
 * 28 Oct 2015  5054       randerso    Fix lots of multimonitor display issues.
 * 14 Jan 2016  5054       randerso    Fix the Tips window to display on the correct monitor
 *                                     Removed duplicate parent shell
 * 25 Jan 2016  5054       randerso    Converted to stand alone window
 * 19 Apr 2016  5517       randerso    Fixed saving/restoring location of AlertViz bar
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertMessageDlg implements MouseMoveListener, MouseListener,
        IAudioAction, ILocalizationFileObserver {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertMessageDlg.class, "GDN_ADMIN", "GDN_ADMIN");

    private static final String P_ALERT_MSG_DLG_POSITION = "alertMessageDlg.position";

    private static final ScopedPreferenceStore dialogPrefs = new ScopedPreferenceStore(
            InstanceScope.INSTANCE, Activator.PLUGIN_ID);

    /**
     * Local shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Return value when the shell is disposed.
     */
    private final Boolean returnValue = false;

    /**
     * Label used to move the dialog.
     */
    private Label moveLabel;

    /**
     * Label used to resize the dialog.
     */
    private Label resizeLabel;

    /**
     * Mouse down location
     */
    private Point mouseDownPt;

    /**
     * Show dialog flag.
     */
    private boolean showDialog = true;

    /**
     * Move the dialog flag.
     */
    private boolean moveDialog = false;

    /**
     * Move dialog flag.
     */
    private boolean resizeDialog = false;

    /**
     * ArrayList of TextMsgControlComp objects.
     */
    private ArrayList<TextMsgControlComp> txtMsgCompArray;

    /**
     * Text composite.
     */
    private Composite textComp;

    /**
     * Audio callback.
     */
    private final IAudioAction audioCB;

    /**
     * Configuration data.
     */
    private Configuration configData;

    /**
     * Flag to determine if bar is enabled
     */
    private boolean enabled;

    private AlertAudioMgr alertAudioMgr;

    private Button audioBtn;

    private Image audioPlay;

    private static final String AUDIO_FILE_IMAGE = "audio.png";

    private Button errorBtn;

    /**
     * Error button background color.
     */
    private Color errorBtnBgColor = null;

    private TabControlDlg tabControlDlg;

    private static final String ERROR_FILE_IMAGE = "alertVizErrorLog.png";

    private Boolean opened = false;

    /**
     * Log dialog (TabItem) that is used for the TabControlDialog.
     */
    private TextMsgLog textMsgLog;

    /**
     * Message vector.
     */
    private final Vector<StatusMessage> messageVec = new Vector<StatusMessage>();

    private static final String CATEGORY_MONITOR = "MONITOR";

    private Map<String, AlertMonitor> alertMonitors;

    /**
     * Constructor.
     * 
     * @param display
     *            Parent display.
     * @param audioCB
     *            Audio callback.
     * @param showDialog
     *            Show dialog flag.
     * @param configData
     *            Configuration data.
     * @param alertAudioMgr
     */
    public AlertMessageDlg(Display display, IAudioAction audioCB,
            boolean showDialog, Configuration configData,
            AlertAudioMgr alertAudioMgr) {
        this.display = display;
        this.showDialog = showDialog;

        this.alertAudioMgr = alertAudioMgr;
        this.audioCB = audioCB;

        this.configData = configData;

        if (Boolean.getBoolean("SystemTray")
                && !Boolean.getBoolean("ShowAlertVizBar")) {
            enabled = false;
        }
    }

    /**
     * @return the shell
     */
    public Shell getShell() {
        return shell;
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return True/False.
     */
    public Object open() {
        shell = new Shell(display, SWT.ON_TOP | SWT.NO_TRIM);

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (errorBtnBgColor != null) {
                    errorBtnBgColor.dispose();
                }
            }
        });

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        mainLayout.verticalSpacing = 0;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
        shell.pack();

        // Restore the previous dialog position
        Rectangle rect = restoreDialogPosition();
        Point shellLoc = new Point(rect.x, rect.y);

        if (rect.width > 0 && rect.height > 0) {
            shell.setSize(rect.width, rect.height);
        }
        Point shellSize = shell.getSize();

        // force bar location to be within the display.
        Display d = shell.getDisplay();
        Rectangle dBounds = d.getBounds();
        if (shellLoc.x < dBounds.x) {
            shellLoc.x = dBounds.x;
        } else if ((shellLoc.x + shellSize.x) > (dBounds.x + dBounds.width)) {
            shellLoc.x = (dBounds.x + dBounds.width) - shellSize.x;
        }
        if (shellLoc.y < dBounds.y) {
            shellLoc.y = dBounds.y;
        } else if ((shellLoc.y + shellSize.y) > (dBounds.y + dBounds.height)) {
            shellLoc.y = (dBounds.y + dBounds.height) - shellSize.y;
        }
        shell.setLocation(shellLoc);
        shell.open();

        if (Boolean.getBoolean("SystemTray")
                && !Boolean.getBoolean("ShowAlertVizBar")) {
            enabled = false;
            shell.setVisible(false);
        } else {
            enabled = true;
        }

        return returnValue;
    }

    /**
     * @return true if dialog is disposed
     */
    public boolean isDisposed() {
        return (shell == null) || shell.isDisposed();
    }

    /**
     * Dispose of all the message timers.
     */
    public void dispose() {
        saveDialogPosition(shell.getBounds());

        // Stop all of the message timers.
        for (int i = 0; i < txtMsgCompArray.size(); i++) {
            txtMsgCompArray.get(i).stopTimer();
        }

        if (audioPlay != null) {
            audioPlay.dispose();
        }

        if (textMsgLog != null) {
            textMsgLog.disposeDialog();
        }

        for (AlertMonitor monitor : alertMonitors.values()) {
            monitor.dispose();
        }

        if (shell != null) {
            shell.dispose();
        }

        ConfigurationManager.getInstance().getCustomLocalization()
                .removeFileUpdatedObserver(this);

    }

    /**
     * Register as a window.
     */
    public void registerAsWindow() {
        IContextService svc = (IContextService) PlatformUI.getWorkbench()
                .getService(IContextService.class);
        svc.registerShell(shell, IContextService.TYPE_WINDOW);
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        TrayConfiguration.TrayMode tm = configData.getGlobalConfiguration()
                .getMode();
        boolean monitorOnly = (TrayConfiguration.TrayMode.MO.equals(tm) ? true
                : false);

        this.alertMonitors = createAlertMonitorMap();
        updateMonitorImagesLabels();

        int monitorsCount = getDisplayedMonitorCount();
        int glSize = 6 + monitorsCount;

        if (monitorOnly) {
            glSize = 4 + monitorsCount;
        }

        txtMsgCompArray = new ArrayList<TextMsgControlComp>();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.BORDER);
        GridLayout gl = new GridLayout(glSize, false);
        gl.marginHeight = 0;
        gl.marginWidth = 4;
        gl.verticalSpacing = 0;
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        moveLabel = new Label(mainComp, SWT.NONE);
        moveLabel.setImage(new Image(display, loadHandleImage()));
        moveLabel.setToolTipText("Move");
        moveLabel.addMouseListener(this);
        moveLabel.addMouseMoveListener(this);

        Button infoBtn = new Button(mainComp, SWT.NONE);
        infoBtn.setImage(new Image(display, loadInfoImage()));
        infoBtn.setToolTipText("Info");
        infoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                InfoPopUpText();
            }
        });

        errorBtn = new Button(mainComp, SWT.PUSH);
        errorBtn.setImage(new Image(display, loadErrorImage()));
        errorBtn.setToolTipText("Error Log");
        errorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                errorButtonAction();
            }
        });

        tabControlDlg = TabControlDlg.getInstance(shell);

        audioBtn = new Button(mainComp, SWT.PUSH);
        audioPlay = new Image(display, loadAudioImage(true));
        audioBtn.setImage(audioPlay);
        audioBtn.setToolTipText("Audio Kill");
        audioBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int numTextMsgBoxComp = getNumberOfTextControls();
                for (int i = 0; i < numTextMsgBoxComp; i++) {
                    alertAudioMgr.stopSound(i);
                    audioCB.cancelAudio(i);
                }
            }
        });

        for (AlertMonitor monitor : alertMonitors.values()) {
            monitor.init(mainComp, configData, display);
        }

        if (!monitorOnly) {
            createTextControls(mainComp);

            resizeLabel = new Label(mainComp, SWT.NONE);
            resizeLabel.setImage(new Image(display, loadResizeImage()));
            resizeLabel.setToolTipText("Resize");
            resizeLabel.addMouseListener(this);
            resizeLabel.addMouseMoveListener(this);
        }

        ConfigurationManager.getInstance().getCustomLocalization()
                .addFileUpdatedObserver(this);
    }

    private int getDisplayedMonitorCount() {
        int displayedCount = 0;
        for (AlertMonitor alertMonitor : alertMonitors.values()) {
            displayedCount += (alertMonitor.isOmitted() ? 0 : 1);
        }
        return displayedCount;
    }

    @Override
    public void fileUpdated(final FileUpdatedMessage message) {
        final String title = "AlertViz: Change from other AlertViz Session";
        final String messageText = "Another AlertViz session has made\n"
                + "a change to the Category or Source lists.\n"
                + "Do you want to re-ingest the configuration\n"
                + "file now?  (This may result in\n"
                + "eliminating any changes you might\n"
                + "have made to the configuration\n"
                + "without saving to a file.)  If no, then\n"
                + "you can activate a re-ingest later by\n"
                + "re-starting AlertViz";

        display.asyncExec(new Runnable() {
            @Override
            public void run() {
                ModelessMessageDialog warningPopup = new ModelessMessageDialog(
                        shell, title, messageText);

                if (warningPopup.open() == SWT.YES) {
                    // reload current context
                    ConfigContext currentContext = ConfigurationManager
                            .getInstance().getCurrentContext();
                    ConfigurationManager.getInstance().loadAsCurrent(
                            currentContext);
                }
            }
        });
    }

    /**
     * Create all of the text controls composites.
     * 
     * @param parent
     *            Parent composite.
     */
    private void createTextControls(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        textComp = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(getGridColumnCount(), false);
        gl.marginWidth = 1;
        gl.marginHeight = 1;
        gl.verticalSpacing = 1;
        textComp.setLayout(gl);
        textComp.setLayoutData(gd);

        String prefix = getModePrefix();
        for (int i = 0; i < getNumberOfTextControls(); i++) {
            TextMsgControlComp textMsg = new TextMsgControlComp(textComp, this,
                    configData.getGlobalConfiguration().getLogLength(),
                    getCategoryList(i + 1), i);
            textMsg.setIndexName(prefix, (i + 1));
            txtMsgCompArray.add(textMsg);
        }
    }

    public void setMaxLogSize(final int maxLogSize) {
        if ((txtMsgCompArray != null) && (txtMsgCompArray.size() > 0)
                && (maxLogSize != txtMsgCompArray.get(0).getMaxLogSize())) {
            for (TextMsgControlComp comp : txtMsgCompArray) {
                comp.setMaxLogSize(maxLogSize);
            }
        }
    }

    /**
     * Mouse move callback.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseMove(MouseEvent e) {
        if (mouseDownPt != null) {
            if ((Label) e.getSource() == moveLabel) {
                if (moveDialog == true) {
                    Point dialogLoc = shell.getLocation();
                    dialogLoc.x = dialogLoc.x + (e.x - mouseDownPt.x);
                    dialogLoc.y = dialogLoc.y + (e.y - mouseDownPt.y);
                    shell.setLocation(dialogLoc);
                }
            } else if ((Label) e.getSource() == resizeLabel) {
                if (resizeDialog == true) {
                    int newWidth = shell.getSize().x + (e.x - mouseDownPt.x);

                    if (newWidth < 300) {
                        return;
                    }

                    shell.setSize(newWidth, shell.getSize().y);
                }
            }
        }
    }

    /**
     * Mouse down event.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDown(MouseEvent e) {
        if (e.button != SWT.BUTTON3) {
            mouseDownPt = new Point(e.x, e.y);
            if ((Label) e.getSource() == moveLabel) {
                moveDialog = true;

            } else if ((Label) e.getSource() == resizeLabel) {
                resizeDialog = true;
            }
        }
    }

    /**
     * Mouse up event.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseUp(MouseEvent e) {
        if (e.button != SWT.BUTTON3) {
            mouseDownPt = null;
            if ((Label) e.getSource() == moveLabel) {
                moveDialog = false;

            } else if ((Label) e.getSource() == resizeLabel) {
                resizeDialog = false;
            }
        }
    }

    /**
     * Mouse double click event (not used).
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {
        // NOT USED...
    }

    /**
     * Method to show the dialog.
     * 
     * @param showDialogFlag
     *            Flag indicating if the dialog should be shown.
     */
    public void showDialog(boolean showDialogFlag) {
        showDialog = showDialogFlag;

        shell.setVisible(showDialog);
    }

    /**
     * Check if the dialog is visible.
     * 
     * @return True if the dialog is visible.
     */
    public boolean isVisible() {
        return showDialog;
    }

    /**
     * Kill (dispose) of the dialog.
     */
    public void killDialog() {
        shell.dispose();
    }

    public void resetTabControl() {
        boolean isOpen = (tabControlDlg != null) && !tabControlDlg.isDisposed()
                && tabControlDlg.isOpened();
        reLayout();
        // TODO: Need to determine which tabs to open and populate them.
        // For now let the user reopen and they will be properly populated.
        // if (isOpen) {
        // tabControlDlg = TabControlDlg.getInstance(shell);
        // tabControlDlg.open();
        // }
    }

    /**
     * Re-layout the message composites.
     */
    public boolean reLayout() {
        boolean result = false;

        if (Boolean.getBoolean("SystemTray")
                && !Boolean.getBoolean("ShowAlertVizBar")) {
            enabled = false;
            shell.setVisible(false);
        } else {
            int size = txtMsgCompArray.size();
            String[] prevMessageText = new String[size];
            RGB[] prevBackground = new RGB[size];
            RGB[] prevForeground = new RGB[size];
            dispose();
            for (int i = 0; i < txtMsgCompArray.size(); i++) {
                if ((txtMsgCompArray.get(i) != null)
                        && !txtMsgCompArray.get(i).isDisposed()) {
                    prevMessageText[i] = txtMsgCompArray.get(i)
                            .getMessageText();
                    prevBackground[i] = txtMsgCompArray.get(i)
                            .getBackgroundRGB();
                    prevForeground[i] = txtMsgCompArray.get(i)
                            .getForegroundRGB();
                }
                txtMsgCompArray.get(i).dispose();
            }
            TabControlDlg.dispose();
            txtMsgCompArray.clear();

            killDialog();
            open();
            result = true;
            for (int i = 0; i < txtMsgCompArray.size(); i++) {
                if ((i < prevMessageText.length)
                        && (prevMessageText[i] != null)) {
                    txtMsgCompArray.get(i).setMessageText(prevMessageText[i]);
                    if ((prevBackground[i] != null)
                            && (prevForeground[i] != null)) {
                        txtMsgCompArray.get(i).setMessageTextBackAndForeground(
                                prevBackground[i], prevForeground[i]);
                    }
                }
            }
            shell.update();
            alertAudioMgr = null;
            alertAudioMgr = new AlertAudioMgr(display,
                    getNumberOfTextControls());
        }

        return result;
    }

    /**
     * Cancel the audio file that is being played.
     */
    @Override
    public void cancelAudio() {
        audioCB.cancelAudio();
    }

    /**
     * Cancel the audio file that is being played for an associated Text Message
     * Box.
     */
    @Override
    public void cancelAudio(int textMsgBoxId) {
        audioCB.cancelAudio(textMsgBoxId);
    }

    /**
     * Load the image (information) for moving the dialog.
     * 
     * @return The "Info" image.
     */
    private String loadInfoImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + "trans_i_small.gif")
                .getAbsolutePath();
        return path;
    }

    /**
     * Load the image (handle) for moving the dialog.
     * 
     * @return The "handle" image.
     */
    private String loadHandleImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + "handle.png")
                .getAbsolutePath();
        return path;
    }

    /**
     * Load the image used for resizing the dialog.
     * 
     * @return
     */
    private String loadResizeImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + "resize.png")
                .getAbsolutePath();
        return path;
    }

    /**
     * Get the number of grid columns for the text composites.
     * 
     * @return The number of grid columns.
     */
    private int getGridColumnCount() {
        int retVal = 1;

        TrayConfiguration.TrayMode layoutMode = configData
                .getGlobalConfiguration().getMode();

        switch (layoutMode) {
        case H1:
        case V2:
        case V3:
        case V4:
            retVal = 1;
            break;
        case Q4:
        case H2:
            retVal = 2;
            break;
        case MO:
            break;
        }

        return retVal;
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

    /**
     * Get the prefix of the layoutMode
     * 
     * @return String that is prefix
     */
    private String getModePrefix() {
        String retVal = "";

        TrayConfiguration.TrayMode layoutMode = configData
                .getGlobalConfiguration().getMode();

        switch (layoutMode) {
        case H1:
        case H2:
            retVal = "H";
            break;
        case V2:
        case V3:
        case V4:
            retVal = "V";
            break;
        case Q4:
            retVal = "Q";
            break;
        case MO:
            break;
        }

        return retVal;
    }

    /**
     * Get the list of categories of the specified text composite.
     * 
     * @param textBox
     *            Text box number.
     * @return String array of categories.
     */
    private Category[] getCategoryList(int textBox) {
        ArrayList<String> strArrayList = new ArrayList<String>();

        Map<String, Category> catMap = configData.getCategories();

        Set<String> keySet = catMap.keySet();

        for (String key : keySet) {
            if (textBox == catMap.get(key).getTextBox()) {
                strArrayList.add(key);
            }
        }

        Category[] catArray = new Category[strArrayList.size()];

        for (int i = 0; i < strArrayList.size(); i++) {
            catArray[i] = catMap.get(strArrayList.get(i));
        }

        return catArray;
    }

    /**
     * Handle the incoming message.
     * 
     * @param statMsg
     *            Status message.
     * @param amd
     *            Alert metadata.
     * @param cat
     *            Category information.
     * @param gConfig
     *            Global configuration.
     */
    public void messageHandler(StatusMessage statMsg, AlertMetadata amd,
            Category cat, TrayConfiguration gConfig) {

        if (CATEGORY_MONITOR.equals(statMsg.getCategory())) {
            updateMonitorImageLabel(statMsg);
            return;
        }

        int textBox = cat.getTextBox();

        // If the text box number is 0 then return because we don't
        // care about the message.
        if (textBox == 0) {
            return;
        }

        if (txtMsgCompArray.size() < textBox) {
            statusHandler.error("Trying to display message in text box -- "
                    + textBox);
            return;
        }

        txtMsgCompArray.get(textBox - 1).messageHandler(statMsg, amd, gConfig);
    }

    /**
     * Handle the incoming audio.
     * 
     * @param cat
     *            Category information.
     * @param gConfig
     *            Global configuration.
     */
    public void audioHandler(Category cat, TrayConfiguration gConfig) {
        int textBox = cat.getTextBox();

        // If the text box number is 0 then return because we don't
        // care about the audio.
        if (textBox == 0) {
            return;
        }

        if (txtMsgCompArray.size() < textBox) {
            statusHandler.error("Trying to associate audio with text box -- "
                    + textBox);
            return;
        }

        txtMsgCompArray.get(textBox - 1).setAudioEnabled(true);
    }

    /**
     * Set the configuration data.
     * 
     * @param configData
     *            Configuration data.
     */
    public void setConfigData(Configuration configData) {
        this.configData = configData;
    }

    /**
     * Is the message dlg enabled
     * 
     * @return
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * This method will display Alert Visualization Tips
     * 
     */

    Shell infoTextShell;

    public void InfoPopUpText() {

        if (infoTextShell != null) {
            infoTextShell.open();
            return;
        }

        infoTextShell = new Shell(this.shell.getDisplay());
        infoTextShell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                infoTextShell = null;
            }
        });

        infoTextShell.setMinimumSize(300, 200);
        infoTextShell.setLayout(new GridLayout());
        infoTextShell.setText(" AlertViz Tips! ");

        // create the styled text widget
        StyledText widget = new StyledText(infoTextShell, SWT.MULTI
                | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        widget.setLayoutData(layoutData);

        widget.setText("----------------------------------------------\n"
                + " Here are some basic tips on how to use the AlertViz Main GUI: \n\n"
                + " To MOVE the AlertViz Window:\n"
                + "\t - Button1-click on Move Icon. \n\n"
                + " To ACKNOWLEDGE or CLEAR:\n"
                + "     a TEXT message:\n"
                + "\t - Button1-click the small ''c'' by the text message.\n\n"
                + " To access the Message LOGS:\n"
                + "\t - Button1-click on the button to the right of text message with the tooltip of ''Show logs...'' \n\n"
                + " To close the Message Log Window:\n"
                + "\t - Button1-click on the button to the right of text message with the tooltip of ''Show logs...'' \n"
                + "\t - Button1-click the ''Close'' button.\n\n"
                + " To clear the Message Log:\n"
                + "\t - Button1-click the 'Clear' button in the ''Show logs...'' Window.\n"
                + "\tThis will clear all Categories in the Log Window.  \n\n"
                + " To access the AlertViz ''System Logs...'' \n"
                + "\t- Button3-click on the AlertViz Logo in the System Tray \n\n"
                + " To access the AlertViz CONFIGURATION GUI \n"
                + "\t- Button3-click on the AlertViz Logo in the System Tray \n\n"
                + " To access TIPS in the AlertViz CONFIGURATION GUI \n"
                + "\t- Open Alert Visualization Configuration dialog\n"
                + "\t- Hover mouse cursor over section labels for ToolTipText.\n"
                + "------------------------------------------------------------\n\n"
                + "PRIORITY definitions: \n"
                + "( Note that these generalizations may not be followed for 'Monitors',\n"
                + " as they encode the color-coding of the monitor button into the priority.)\n"
                + "0 - CRITICAL: Information that must be acted upon immediately\n"
                + "    and must not be ignored!!  This would include emergency information.\n\n"
                + "1 - SIGNIFICANT: Due to significant importance, the software developer\n"
                + "    suggests the use of a pop-up, or some other  significant emphasis.\n"
                + "    This may not be an emergency, but is still very important.\n\n"
                + "2 - In between Medium and Significant - perhaps information that is 'WFO-required'\n"
                + "    or linked to policy requirements. \n\n"
                + "3 - MEDIUM: Fairly important, but not crucial.\n\n"
                + "4 - A step above informational. ''Suggested reading'', but not required.\n\n"
                + "5 - Perhaps not even important enough for a notice of any kind.  Informational. \n\n"
                + "------------------------------------------------------------\n\n");

        infoTextShell.layout();
        infoTextShell.pack();

        Rectangle b = this.shell.getBounds();
        Monitor m = this.shell.getMonitor();

        // attempt to position tips window above or below alertviz bar

        // if tips window would be partially off screen it will be forced on
        // screen by eclipse/window manager
        if (b.y < (m.getBounds().height - (b.y + b.height))) {
            // space below alertviz bar > than space above so position below
            infoTextShell.setLocation(b.x, b.y + b.height);
        } else {
            // space above alertviz bar > than space below so position above
            infoTextShell.setLocation(b.x, b.y
                    - infoTextShell.getBounds().height);
        }

        infoTextShell.open();
    }

    /**
     * Load the image (information) for moving the dialog.
     * 
     * @return The "Info" image.
     */
    private String loadAudioImage(boolean play) {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + AUDIO_FILE_IMAGE)
                .getAbsolutePath();
        return path;
    }

    /**
     * Load the image (information) for moving the dialog.
     * 
     * @return The "Info" image.
     */
    private String loadErrorImage() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "alertVizIcons" + File.separatorChar + ERROR_FILE_IMAGE)
                .getAbsolutePath();
        return path;
    }

    /**
     * Action to be taken when the Error button has been pressed.
     */
    private void errorButtonAction() {
        opened = !opened;

        if (errorBtnBgColor != null) {
            errorBtnBgColor.dispose();
        }
        errorBtnBgColor = new Color(display, 237, 233, 227);
        errorBtn.setBackground(errorBtnBgColor);

        if ((tabControlDlg == null) || tabControlDlg.isDisposed()) {
            tabControlDlg = TabControlDlg.getInstance(shell);
        }
        if (textMsgLog == null) {
            String[] categories = new String[] { "Causes", "Catch", "Error",
                    "Exception" };
            textMsgLog = new TextMsgLog(shell, categories, 0, messageVec);
            textMsgLog.setIndex(0);
        }
        if (opened) {
            tabControlDlg.populateClearOptionsCombo(textMsgLog);
            TabItem item = textMsgLog.getTab(tabControlDlg.getTabFolder());
            tabControlDlg.addedTab(textMsgLog);
            item.setText("      Error Log      ");
            item.addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    opened = false;
                }
            });
            if (tabControlDlg.isOpened() == false) {
                tabControlDlg.open();
            }
        } else {
            tabControlDlg.removeTab(textMsgLog);
        }
    }

    public void sendToTextMsgLog(StatusMessage statMsg) {
        if (textMsgLog == null) {
            String[] categories = new String[] { "Causes", "Catch", "Error",
                    "Exception" };
            textMsgLog = new TextMsgLog(shell, categories, 0, messageVec);
            textMsgLog.setIndex(0);
        }
        textMsgLog.addMessage(statMsg);
    }

    public void setErrorLogBtnBackground(RGB background) {

        if (errorBtnBgColor != null) {
            errorBtnBgColor.dispose();
        }

        errorBtnBgColor = new Color(this.display, background);
        errorBtn.setBackground(errorBtnBgColor);
    }

    /**
     * Set each of the default or user-defined monitor images that have been
     * stored in the configuration file.
     * 
     * @param mainComp
     */
    private void updateMonitorImagesLabels() {
        Map<String, Source> sourceMap = configData.getSources();

        for (Source source : sourceMap.values()) {
            String name = source.getName();
            if (source.isMonitor() && (name != null)) {
                String imageFile = source.getConfigurationMonitor()
                        .getMonitorMetadata().getImageFile();
                boolean omitMonitor = source.getConfigurationMonitor()
                        .getMonitorMetadata().getOmit();
                AlertMonitor monitor = alertMonitors.get(name);

                if ((imageFile != null)
                        && (!imageFile.equals("null") && !imageFile.equals(""))) {
                    if (monitor != null) {
                        monitor.setImageName(imageFile);
                    } else {
                        // new monitor
                        monitor = new AlertMonitor(name, imageFile);
                        alertMonitors.put(name, monitor);
                    }
                }
                monitor.setOmit(omitMonitor);
            }
        }
    }

    /**
     * Update the Monitor Image label background based upon the AlertMetadata
     * color set by the user.
     * 
     * @param mess
     *            the status message.
     */
    private void updateMonitorImageLabel(StatusMessage mess) {
        AlertMonitor monitor = alertMonitors.get(mess.getSourceKey());

        if (monitor != null) {
            monitor.updateImage(mess, configData, display);
        }
    }

    private Map<String, AlertMonitor> createAlertMonitorMap() {
        Map<String, AlertMonitor> map = new ConcurrentSkipListMap<String, AlertMonitor>();
        Map<String, Source> sources = configData.getSources();

        for (Source s : sources.values()) {
            if (s.isMonitor()) {
                ConfigurationMonitor config = s.getConfigurationMonitor();
                MonitorMetadata meta = config.getMonitorMetadata();
                AlertMonitor monitor = new AlertMonitor(s.getName(),
                        meta.getImageFile());

                map.put(s.getName(), monitor);
            }
        }
        return map;
    }

    public AlertAudioMgr getAlertAudioManager() {
        return alertAudioMgr;
    }

    private static Rectangle restoreDialogPosition() {
        return new Rectangle(
                dialogPrefs.getInt(P_ALERT_MSG_DLG_POSITION + ".x"),
                dialogPrefs.getInt(P_ALERT_MSG_DLG_POSITION + ".y"),
                dialogPrefs.getInt(P_ALERT_MSG_DLG_POSITION + ".width"),
                dialogPrefs.getInt(P_ALERT_MSG_DLG_POSITION + ".height"));
    }

    private static void saveDialogPosition(Rectangle r) {
        dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".x", r.x);
        dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".y", r.y);
        dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".width", r.width);
        dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".height", r.height);
        try {
            dialogPrefs.save();
        } catch (IOException e) {
            statusHandler.handle(UFStatus.Priority.ERROR,
                    e.getLocalizedMessage(), e);
        }
    }
}