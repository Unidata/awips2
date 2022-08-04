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

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.eclipse.ui.themes.ColorUtil;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.alertviz.InitializeAlertMonitorsRequest;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.uf.viz.alertviz.ConfigContext;
import com.raytheon.uf.viz.alertviz.ConfigurationManager;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ConfigurationMonitor;
import com.raytheon.uf.viz.alertviz.config.MonitorMetadata;
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration.TrayMode;
import com.raytheon.uf.viz.alertviz.ui.Activator;
import com.raytheon.uf.viz.alertviz.ui.audio.AlertAudioMgr;
import com.raytheon.uf.viz.alertviz.ui.dialogs.TextMsgLog.ClearListener;
import com.raytheon.uf.viz.alertviz.ui.util.MessageFormatter;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.DialogUtil;

/**
 * This class displays the dialog showing all of the in-bound messages.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Nov 16, 2010  6144     cjeanbap  Add more detail to Tip info
 * Nov 22, 2010  2235     cjeanbap  Added audio file functionality.
 * Dec 06, 2010  6144     cjeanbap  Updated ToolTipText message.
 * Dec 15, 2010  5149     cjeanbap  Fix Audio bug.
 * Jan 24, 2011  1978     cjeanbap  Add Monitor Tooltip functionality.
 * Jan 28, 2011  4617     cjeanbap  Added Monitor Only functionality.
 * Feb 10, 2011  4617     cjeanbap  Fix NullPointerException if position is
 *                                  null.
 * Mar 02, 2011  5632     cjeanbap  Added sort based on category.
 * Mar 04, 2011  7950     rferrel   Check for null contextName.
 * Mar 09, 2011  8058     rferrel   Associate audio alert with text component.
 * May 02, 2011  9067     cjeanbap  Preserve text component text on relayout().
 * May 27, 2011  9575     cjeanbap  Moved moveLabel to first image in list.
 * May 31, 2011  8058     cjeanbap  Kill sound based on TextMsgBox id.
 * Aug 26, 2013  2293     lvenable  Fixed color memory leaks.
 * Jun 02, 2015  4473     mschenke  Remember dialog position
 * Oct 28, 2015  5054     randerso  Fix lots of multi-monitor display issues.
 * Jan 14, 2016  5054     randerso  Fix the Tips window to display on the
 *                                  correct monitor Removed duplicate parent
 *                                  shell
 * Jan 25, 2016  5054     randerso  Converted to stand alone window
 * Apr 19, 2016  5517     randerso  Fixed saving/restoring location of AlertViz
 *                                  bar
 * May 10, 2016  5517     randerso  Fixed AlertViz bar to initially display on
 *                                  monitor containing cursor after caveData is
 *                                  cleared. Code cleanup.
 * Nov 02, 2016  5980     randerso  Fixed sizing when changing layout
 * Feb 14, 2017  6029     randerso  Ensure AlertMessageDlg cannot be on top of
 *                                  panels, cleanup
 * Aug 15, 2018  6670     randerso  Added request to initialize AlertMonitors at
 *                                  startup.
 * Sep 07, 2018  7446     randerso  Save dialog position when moved. Ensure
 *                                  dialog is always within the client area of
 *                                  the monitor.
 * Sep 20, 2018  7457     randerso  Changes to support new AlertAudioMgr
 *                                  implementation.
 * Sep 27, 2018  7454     randerso  Use a common message format for text display
 *                                  and logs. Code cleanup.
 * Sep 28, 2018  7455     randerso  Set errorBtn to default color when error log
 *                                  button is clicked. Bring TabControlDlg to
 *                                  top if already open.
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 08, 2018  7515     randerso  Adjusted priorities of AlertViz internal
 *                                  errors.
 * Oct 15, 2018  7515     randerso  Moved coloring of error button to
 *                                  AlertMessageDlg
 * Nov 13, 2018  7512     randerso  Moved GUI image files out of localization
 * Dec 06, 2018  7513     randerso  Code cleanup for changes in TabControlDlg.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class AlertMessageDlg implements MouseMoveListener, MouseListener,
        ILocalizationFileObserver, ClearListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertMessageDlg.class, "AV_ADMIN", "AV_ADMIN");

    private static final String P_ALERT_MSG_DLG_POSITION = "alertMessageDlg.position";

    private static final ScopedPreferenceStore dialogPrefs = new ScopedPreferenceStore(
            InstanceScope.INSTANCE,
            com.raytheon.uf.viz.alertviz.Activator.PLUGIN_ID);

    private static final String[] ERROR_LOG_CATEGORIES = new String[] {
            "AV_ADMIN" };

    private enum ImageName {
        AUDIO, ERROR, HANDLE, INFO, RESIZE
    }

    /**
     * Local shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

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
     * Configuration data.
     */
    private Configuration configData;

    private MessageFormatter msgFormat;

    /**
     * Flag to determine if bar is enabled
     */
    private boolean enabled;

    private EnumMap<ImageName, Image> imageMap = new EnumMap<>(ImageName.class);

    private Button errorBtn;

    /**
     * Error button background color.
     */
    private Color errorBtnBgColor = null;

    private Boolean opened = false;

    /**
     * Log dialog (TabItem) that is used for the TabControlDialog.
     */
    private final TextMsgLog errorLog;

    private static final String CATEGORY_MONITOR = "MONITOR";

    private Map<String, AlertMonitor> alertMonitors;

    private Shell infoTextShell;

    private boolean monitorOnly;

    private int minimumWidth;

    /**
     * Constructor.
     *
     * @param display
     *            Parent display.
     * @param showDialog
     *            Show dialog flag.
     * @param configData
     *            Configuration data.
     */
    public AlertMessageDlg(Display display, boolean showDialog,
            Configuration configData) {
        this.display = display;
        this.showDialog = showDialog;

        AlertAudioMgr.initialize();

        this.configData = configData;
        this.msgFormat = new MessageFormatter(
                configData.getGlobalConfiguration());

        if (Boolean.getBoolean("SystemTray")
                && !Boolean.getBoolean("ShowAlertVizBar")) {
            enabled = false;
        }

        errorLog = new TextMsgLog(0, ERROR_LOG_CATEGORIES, msgFormat,
                configData.getGlobalConfiguration().getLogLength());
        errorLog.addClearListener(this);

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
     */
    public void open() {
        shell = new Shell(display, SWT.ON_TOP | SWT.NO_TRIM);
        shell.setText("AlertViz");

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

        // Restore the previous dialog position
        Rectangle dialogBounds = restoreDialogPosition();

        // always set height to the computed height
        dialogBounds.height = shell.getSize().y;

        // if previous width was not set
        if (dialogBounds.width == 0) {
            // use the computed width
            dialogBounds.width = shell.getSize().x;

            // locate dialog in upper left of monitor containing cursor
            Monitor monitor = DialogUtil.getCursorMonitor(display);
            dialogBounds.x = monitor.getClientArea().x;
            dialogBounds.y = monitor.getClientArea().y;
        }

        adjustBounds(getMonitor(dialogBounds.x, dialogBounds.y), dialogBounds);
        shell.setBounds(dialogBounds);
        shell.open();

        if (Boolean.getBoolean("SystemTray")
                && !Boolean.getBoolean("ShowAlertVizBar")) {
            enabled = false;
            shell.setVisible(false);
        } else {
            enabled = true;
        }

        InitializeAlertMonitorsRequest request = new InitializeAlertMonitorsRequest();
        try {
            ThriftClient.sendRequest(request);
        } catch (VizException e) {
            statusHandler.fatal("Error getting initial monitor status", e);
        }
    }

    private Monitor getMonitor(int px, int py) {
        Monitor monitor = null;
        int nearest = Integer.MAX_VALUE;
        for (Monitor m : display.getMonitors()) {
            Rectangle ca = m.getClientArea();
            // find distance to client area
            int dist = distance(ca, px, py);

            if (dist == 0) {
                return m;
            }

            // if less than nearest
            if (dist < nearest) {
                nearest = dist;
                monitor = m;
            }
        }

        return monitor;
    }

    private int distance(Rectangle r, int px, int py) {
        int x = r.x + r.width / 2;
        int y = r.y + r.height / 2;

        int dx = Math.max(Math.abs(px - x) - r.width / 2, 0);
        int dy = Math.max(Math.abs(py - y) - r.height / 2, 0);

        return dx * dx + dy * dy;
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
        AlertAudioMgr.shutDown();

        // Stop the message timer.
        TextMsgControlComp.stopTimer();

        disposeImages();

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
        IContextService svc = PlatformUI.getWorkbench()
                .getService(IContextService.class);
        svc.registerShell(shell, IContextService.TYPE_WINDOW);
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        TrayConfiguration.TrayMode tm = configData.getGlobalConfiguration()
                .getMode();
        monitorOnly = (TrayConfiguration.TrayMode.MO.equals(tm) ? true : false);

        this.alertMonitors = createAlertMonitorMap();
        updateMonitorImagesLabels();

        int monitorsCount = getDisplayedMonitorCount();
        int glSize = 6 + monitorsCount;

        if (monitorOnly) {
            glSize = 4 + monitorsCount;
        }

        txtMsgCompArray = new ArrayList<>();

        loadImages();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.BORDER);
        GridLayout gl = new GridLayout(glSize, false);
        gl.marginHeight = 0;
        gl.marginWidth = 4;
        gl.verticalSpacing = 0;
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        moveLabel = new Label(mainComp, SWT.NONE);
        moveLabel.setImage(imageMap.get(ImageName.HANDLE));
        moveLabel.setToolTipText("Move");
        moveLabel.addMouseListener(this);
        moveLabel.addMouseMoveListener(this);

        Button infoBtn = new Button(mainComp, SWT.NONE);
        infoBtn.setImage(imageMap.get(ImageName.INFO));
        infoBtn.setToolTipText("Info");
        infoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                InfoPopUpText();
            }
        });

        errorBtn = new Button(mainComp, SWT.TOGGLE);
        errorBtn.setImage(imageMap.get(ImageName.ERROR));
        errorBtn.setToolTipText("Error Log");
        errorBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                errorButtonAction();
            }
        });

        Button audioBtn = new Button(mainComp, SWT.PUSH);
        audioBtn.setImage(imageMap.get(ImageName.AUDIO));
        audioBtn.setToolTipText("Audio Kill");
        audioBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                AlertAudioMgr.stopAllSound();
            }
        });

        for (AlertMonitor monitor : alertMonitors.values()) {
            monitor.init(mainComp, configData, display);
        }

        if (monitorOnly) {
            shell.pack();
            minimumWidth = shell.getSize().x;
        } else {
            Composite textComp = createTextControls(mainComp, tm);

            resizeLabel = new Label(mainComp, SWT.NONE);
            resizeLabel.setImage(imageMap.get(ImageName.RESIZE));
            resizeLabel.setToolTipText("Resize");
            resizeLabel.addMouseListener(this);
            resizeLabel.addMouseMoveListener(this);

            shell.pack();
            minimumWidth = shell.getSize().x - textComp.getSize().x;
        }

        ConfigurationManager.getInstance().getCustomLocalization()
                .addFileUpdatedObserver(this);
    }

    private void loadImages() {
        Bundle bundle = Activator.getDefault().getBundle();

        for (ImageName imageName : ImageName.values()) {
            Image image = IconUtil.getImage(bundle,
                    imageName.name().toLowerCase() + ".png",
                    shell.getDisplay());
            imageMap.put(imageName, image);
        }
    }

    private void disposeImages() {
        for (ImageName imageName : ImageName.values()) {
            Image image = imageMap.remove(imageName);
            image.dispose();
        }
    }

    @Override
    public void logCleared() {
        setErrorLogBtnBackground(null);
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
                    ConfigurationManager.getInstance()
                            .loadAsCurrent(currentContext);
                }
            }
        });
    }

    /**
     * Create all of the text controls composites.
     *
     * @param parent
     *            Parent composite.
     * @param tm
     */
    private Composite createTextControls(Composite parent, TrayMode tm) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite textComp = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(tm.getHorizontalBoxes(), false);
        gl.marginWidth = 1;
        gl.marginHeight = 1;
        gl.verticalSpacing = 0;
        textComp.setLayout(gl);
        textComp.setLayoutData(gd);

        for (int i = 0; i < tm.getNumberOfBoxes(); i++) {
            TextMsgControlComp textMsg = new TextMsgControlComp(textComp,
                    tm.getModePrefix(), i + 1,
                    configData.getGlobalConfiguration().getLogLength(),
                    msgFormat, getCategoryList(i + 1));
            txtMsgCompArray.add(textMsg);
        }
        TextMsgControlComp.startTimer();

        return textComp;
    }

    /**
     * Set maxLogSize
     *
     * @param maxLogSize
     */
    public void setMaxLogSize(final int maxLogSize) {
        if (txtMsgCompArray != null) {
            for (TextMsgControlComp comp : txtMsgCompArray) {
                comp.setMaxLogSize(maxLogSize);
            }
        }
        errorLog.setMaxLogSize(maxLogSize);
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
            Rectangle dialogBounds = shell.getBounds();
            if (moveDialog) {
                dialogBounds.x = dialogBounds.x + (e.x - mouseDownPt.x);
                dialogBounds.y = dialogBounds.y + (e.y - mouseDownPt.y);
            } else if (resizeDialog) {
                dialogBounds.width = shell.getSize().x + (e.x - mouseDownPt.x);
            }

            adjustBounds(DialogUtil.getCursorMonitor(display), dialogBounds);
            shell.setBounds(dialogBounds);
            saveDialogPosition(shell.getBounds());
        }
    }

    /**
     * Adjust dialog bounds to be in the valid client area of the monitor and at
     * least minimum width
     *
     * @param dialogBounds
     */
    private void adjustBounds(Monitor monitor, Rectangle dialogBounds) {

        if (monitorOnly) {
            dialogBounds.width = shell.getSize().x;
        } else if (dialogBounds.width < minimumWidth) {
            dialogBounds.width = minimumWidth;
        }

        // force dialog location to be within the client area of the monitor
        Rectangle clientArea = monitor.getClientArea();
        if (dialogBounds.x < clientArea.x) {
            dialogBounds.x = clientArea.x;
        } else if ((dialogBounds.x + dialogBounds.width) > (clientArea.x
                + clientArea.width)) {
            dialogBounds.x = (clientArea.x + clientArea.width)
                    - dialogBounds.width;
        }
        if (dialogBounds.y < clientArea.y) {
            dialogBounds.y = clientArea.y;
        } else if ((dialogBounds.y + dialogBounds.height) > (clientArea.y
                + clientArea.height)) {
            dialogBounds.y = (clientArea.y + clientArea.height)
                    - dialogBounds.height;
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

    /**
     * Re-layout the message composites.
     *
     * @return true if AlertVizBar is enabled and visible
     */
    public boolean reLayout() {
        AlertAudioMgr.shutDown();

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
            TabControlDlg.close();
            txtMsgCompArray.clear();

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
            AlertAudioMgr.initialize();
        }

        return result;
    }

    /**
     * Get the list of categories of the specified text composite.
     *
     * @param textBox
     *            Text box number.
     * @return String array of categories.
     */
    private Category[] getCategoryList(int textBox) {
        List<String> strArrayList = new ArrayList<>();

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

        int textBoxId = cat.getTextBox() - 1;

        // If the text box number is less than 0 then return because we don't
        // care about the message.
        if (textBoxId < 0) {
            return;
        }

        if (textBoxId >= txtMsgCompArray.size()) {
            statusHandler.error(
                    "Trying to display message in text box -- " + textBoxId);
            return;
        }

        TextMsgControlComp textBox = txtMsgCompArray.get(textBoxId);
        textBox.messageHandler(statMsg, amd, gConfig);
    }

    /**
     * Set the configuration data.
     *
     * @param configData
     *            Configuration data.
     */
    public void setConfigData(Configuration configData) {
        this.configData = configData;
        this.msgFormat.setTrayConfig(configData.getGlobalConfiguration());
    }

    /**
     * Is the message dlg enabled
     *
     * @return true if dlg is enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * This method will display Alert Visualization Tips
     *
     */

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
        StyledText widget = new StyledText(infoTextShell,
                SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        widget.setLayoutData(layoutData);

        widget.setText("----------------------------------------------\n"
                + " Here are some basic tips on how to use the AlertViz Main GUI: \n\n"
                + " To MOVE the AlertViz Window:\n"
                + "\t - Button1-click on Move Icon. \n\n"
                + " To ACKNOWLEDGE or CLEAR:\n" + "     a TEXT message:\n"
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
        if (b.y < (m.getClientArea().height - (b.y + b.height))) {
            // space below alertviz bar > than space above so position below
            infoTextShell.setLocation(b.x, b.y + b.height);
        } else {
            // space above alertviz bar > than space below so position above
            infoTextShell.setLocation(b.x,
                    b.y - infoTextShell.getBounds().height);
        }

        infoTextShell.open();
    }

    /**
     * Action to be taken when the Error button has been pressed.
     */
    private void errorButtonAction() {
        opened = errorBtn.getSelection();

        TabControlDlg tabControlDlg = TabControlDlg.getInstance(shell);
        if (opened) {
            Item item = tabControlDlg.addTab("      Error Log      ", errorLog);
            item.addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    opened = false;
                    errorBtn.setSelection(false);
                }
            });
            tabControlDlg.open();
        } else {
            tabControlDlg.removeTab(errorLog);
        }
    }

    /**
     * Add a message to the text message log
     *
     * @param statMsg
     */
    public void sendToTextMsgLog(StatusMessage statMsg) {
        errorLog.addMessage(statMsg);
        Source source = configData.lookupSource("AV_ADMIN");

        /*
         * Set error button color based on the highest priority message in the
         * error log
         */
        RGB backgroundRGB = null;
        if ((source == null) || (source.getConfigurationItem() == null)) {
            backgroundRGB = ColorUtil.getColorValue("COLOR_YELLOW");
        } else {
            AlertMetadata am = source.getConfigurationItem()
                    .lookup(errorLog.getHighestPriority());
            backgroundRGB = am.getBackground();
        }

        setErrorLogBtnBackground(backgroundRGB);
    }

    /**
     * Set background color of errorBtn
     *
     * @param background
     *            the background color
     */
    private void setErrorLogBtnBackground(RGB background) {

        if (errorBtnBgColor != null) {
            errorBtnBgColor.dispose();
            errorBtnBgColor = null;
        }

        if (background != null) {
            errorBtnBgColor = new Color(this.display, background);
        }

        if (errorBtn != null && !errorBtn.isDisposed()) {
            errorBtn.setBackground(errorBtnBgColor);
        }
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

                if ((imageFile != null) && (!"null".equals(imageFile)
                        && !imageFile.isEmpty())) {
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
        Map<String, AlertMonitor> map = new ConcurrentSkipListMap<>();
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

    private Rectangle restoreDialogPosition() {
        String hostNamePrefix = SystemUtil.getHostName() + ".";

        // see if host specific preferences are available
        String xPref = hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".x";

        // if not, use the non-host specific preferences
        if (!dialogPrefs.contains(xPref)) {
            hostNamePrefix = "";
        }

        /*
         * The height preference is not used, we just return 0. The calculated
         * dialog height is always used.
         */
        return new Rectangle(
                dialogPrefs.getInt(
                        hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".x"),
                dialogPrefs.getInt(
                        hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".y"),
                dialogPrefs.getInt(
                        hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".width"),
                0);
    }

    private void saveDialogPosition(Rectangle r) {
        /*
         * store preferences with host name prefix so each user has different
         * preferences for each workstation to allow for different monitor
         * configurations
         */
        String hostNamePrefix = SystemUtil.getHostName() + ".";
        dialogPrefs.setValue(hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".x",
                r.x);
        dialogPrefs.setValue(hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".y",
                r.y);

        // don't update width if in monitorOnly mode
        if (!monitorOnly) {
            dialogPrefs.setValue(
                    hostNamePrefix + P_ALERT_MSG_DLG_POSITION + ".width",
                    r.width);
        }

        // if non-host specific preferences don't exist
        // using width preference to ensure it get's set at some point
        if (!dialogPrefs.contains(P_ALERT_MSG_DLG_POSITION + ".width")) {
            dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".x", r.x);
            dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".y", r.y);
            if (!monitorOnly) {
                dialogPrefs.setValue(P_ALERT_MSG_DLG_POSITION + ".width",
                        r.width);
            }
        }

        try {
            dialogPrefs.save();
        } catch (IOException e) {
            statusHandler.handle(UFStatus.Priority.ERROR,
                    e.getLocalizedMessage(), e);
        }
    }
}