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

package com.raytheon.viz.textworkstation;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.TextWorkstationConstants;
import com.raytheon.viz.texteditor.alarmalert.dialogs.CurrentAlarmQueue;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertFunctions;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertNotificationObserver;
import com.raytheon.viz.texteditor.dialogs.TextEditorCfg;
import com.raytheon.viz.texteditor.dialogs.TextEditorDialog;
import com.raytheon.viz.texteditor.msgs.ITextEditorCallback;
import com.raytheon.viz.texteditor.msgs.ITextWorkstationCallback;
import com.raytheon.viz.texteditor.notify.NotifyExpiration;
import com.raytheon.viz.texteditor.scripting.runner.TextWsScriptThreadManager;
import com.raytheon.viz.texteditor.util.RadarTextUtility;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.DialogUtil;

/**
 * TextWorkstationDlg class.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Sep 27, 2007  368      lvenable     Initial creation.
 * Oct 11, 2007  482      grichard     Reformatted file.
 * Nov 28, 2007  520      grichard     Implemented build 11 features.
 * Jan 03, 2008  637      grichard     Implemented build 13 features.
 * Jan 10, 2008  722      grichard     Implemented build 14 features.
 * May 16, 2008  1119     grichard     Added support for IAviationObserver.
 * Jun 03, 2008  937      grichard     Corrected simple date formats.
 * May 08, 2009  2104     grichard     Added support for IRadarObserver.
 * May 08, 2009  2104     grichard     Added support for IScriptRunnerObserver.
 * Jun 07, 2010  5851     cjeanbap     Properly stop alert/alarm observer
 *                                     listener.
 * Aug 23, 2010  2187     cjeanbap     Removed window location to
 *                                     TextEditorDialog.preOpened().
 * Oct 04, 2010  7193     cjeanbap     Added if statement to
 *                                     notificationArrived(), to determine if
 *                                     the message has expired.
 * Nov 08, 2010  7433     cjeanbap     Check TextEditorDialog current mode
 *                                     before tempting to open dialog.
 * Jan 05, 2011  7375     cjeanbap     Fix disposed Widget exception.
 * Feb 01, 2011  7193     cjeanbap     Add boolean condition to check initial
 *                                     start time.
 * Nov 03, 2011  11450    rferrel      Change how old products pruge so it is no
 *                                     longer on times on two machines being in
 *                                     synch.
 * Sep 26, 2012  1196     lvenable     Dialog refactor to not block.
 * Oct 02, 2012  1229     rferrel      Option to allow blocking when top dialog.
 * Dec 13, 2012  1353     rferrel      Fix bug introduced in the Show all
 *                                     dialogs.
 * Jan 30, 2013  14736    D. Friedman  Display local time.
 * Jun 24, 2013  15733    XHuang       Display MAX_BUTTON_CNT (8 button).
 * Jul 25, 2013  15733    Greg Hull    Make dflt and max number of Text Buttons
 *                                     configurable.
 * Oct 28, 2015  5054     randerso     Make TextWorkstationDlg appear in upper
 *                                     left corner of monitor where parent shell
 *                                     is located
 * Dec 14, 2015  4834     njensen      Remove dead menu items
 * Jan 26, 2016  5054     randerso     Changed to use display as parent
 * Feb 15, 2016  4860     njensen      Removed references to IAviationObserver
 * Mar 30, 2016  5513     randerso     Fixed to display on same monitor as
 *                                     parent
 * Feb 14, 2017  6037     randerso     Ensure dialog does not appear over panels
 * Jun 29, 2017  6347     randerso     Use -monitor command line parameter, if
 *                                     present, when opening as top level window
 * Jan 03, 2018  6804     tgurney      Stop all scripts on dispose
 * Jan 24, 2018  7132     tgurney      Set alarm/alert bell to null on dispose
 * May 23, 2018  7313     tgurney      Add scroll bar and allow resize
 * Apr 29, 2021  8137     randerso     Force use of short hostname for
 *                                     text workstation queue
 *
 * </pre>
 *
 * @author lvenable
 */
public class TextWorkstationDlg extends CaveSWTDialog
        implements ITextEditorCallback, INotificationObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private int INIT_BUTTON_CNT = 4;

    private int MAX_BUTTON_CNT = 8;

    private String productToDisplay = null;

    private static final long initDelta = 300L;

    private static final long incDelta = 20L;

    private long delta = TextWorkstationDlg.initDelta;

    private Font font;

    private Font fontAwipsLabel;

    private MenuItem newWindowMenuItem;

    private Label utcTimeLabel;

    private Label localTimeLabel;

    private final SimpleDateFormat sdfLocal = new SimpleDateFormat(
            "EEE dd MMM yyyy HH:mm z");

    private final SimpleDateFormat sdfUTC = new SimpleDateFormat(
            "EEE dd MMM yyyy HH:mm z");

    private Timer timer;

    private Date date;

    private List<Button> textBtnArray;

    private List<TextEditorDialog> textEditorArray;

    private TextEditorDialog wgDlg;

    private NotifyExpiration notify;

    private CurrentAlarmQueue alarmDlg;

    private long initStartTime;

    private Composite textButtonComp;

    private ScrolledComposite scrolledComp;

    private Button alertAlarmBtn;

    /**
     * Create top level Text Workstation Dialog
     *
     * @param display
     *
     */
    public TextWorkstationDlg(Display display) {
        super(display, SWT.DIALOG_TRIM | SWT.MIN | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.INDEPENDENT_SHELL
                        | CAVE.DO_NOT_BLOCK);

        setText("Text Workstation");
        TextDisplayModel.getInstance().setTextRadar(new RadarTextUtility());
        NotificationManagerJob.addQueueObserver(
                TextWorkstationConstants.getLocalTextWorkstationQueueName(),
                this);
        initStartTime = System.currentTimeMillis();
    }

    @Override
    protected void disposed() {
        font.dispose();
        fontAwipsLabel.dispose();
        timer.cancel();
        NotificationManagerJob.removeQueueObserver(
                TextWorkstationConstants.getLocalTextWorkstationQueueName(),
                null, this);
        AlarmAlertFunctions.destroyAlarmAlertBell();
        TextWsScriptThreadManager.getInstance().stopAllScripts();
        for (TextEditorDialog teDlg : textEditorArray) {
            if (teDlg != null) {
                teDlg.disposeDialog();
            }
        }
        if (wgDlg != null) {
            wgDlg.disposeDialog();
        }
        AlarmAlertNotificationObserver.removeNotificationObserver();
    }

    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        notify = new NotifyExpiration(getDisplay());
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        fontAwipsLabel = new Font(shell.getDisplay(), "Helvetica", 24,
                SWT.ITALIC);

        INIT_BUTTON_CNT = TextEditorCfg.getTextEditorCfg()
                .getDefaultNumEditors();
        MAX_BUTTON_CNT = TextEditorCfg.getTextEditorCfg().getMaxNumEditors();

        // Initialize all of the controls and layouts
        sdfUTC.setTimeZone(TimeZone.getTimeZone("UTC"));
        String localTZName = System.getenv("FXA_LOCAL_TZ");
        sdfLocal.setTimeZone(
                localTZName != null ? TimeZone.getTimeZone(localTZName)
                        : TimeZone.getDefault());

        createMenus();
        new Label(shell, SWT.NONE).setText(
                "host: " + TextWorkstationConstants.getShortHostName());
        createAwipsLabel();
        createTimeLabels();
        startTimeTimer();
        createAlertAlarm();
        shell.pack();
        // minimum size gives room for four text editor buttons + spacing
        shell.setMinimumSize(shell.getSize().x,
                shell.getSize().y + alertAlarmBtn.getSize().y * 5);
        createTextButtons();
        createWarngenDisplay();

        shell.addListener(SWT.Close, new Listener() {
            @Override
            public void handleEvent(Event event) {
                event.doit = notify.checkExpirationNotices(getShell());
            }
        });

        // Opens the alarm queue invisibly, to duplicate A1 functionality.
        alarmDlg = CurrentAlarmQueue.getInstance(shell);
        alarmDlg.openInvisible();

        // Create the alarm alert bell
        AlarmAlertFunctions.initAlarmAlertBell(shell);
    }

    @Override
    protected void preOpened() {
        super.preOpened();

        Monitor monitor = null;

        /* If we have a parent shell use the parent's monitor */
        if (getParent() != null) {
            monitor = getParent().getMonitor();
        }

        /* if no parent shell, must be top level window */
        else {
            /* Check for -monitor command line arg */
            ProgramArguments args = ProgramArguments.getInstance();
            Integer monitorIndex = args.getInteger("-monitor");

            Display display = getDisplay();
            if (monitorIndex != null) {
                /* Clip index to valid range of monitors */
                Monitor[] monitors = display.getMonitors();

                if (monitorIndex < 0) {
                    monitorIndex = 0;
                } else if (monitorIndex >= monitors.length) {
                    monitorIndex = monitors.length - 1;
                }
                monitor = monitors[monitorIndex];
            }

            /* Otherwise default to monitor containing cursor */
            else {
                monitor = DialogUtil.getCursorMonitor(display);
            }
        }

        /* Set dialog location to upper left corner of monitor */
        Rectangle clientArea = monitor.getClientArea();
        shell.setLocation(clientArea.x, clientArea.y);
    }

    @Override
    protected void opened() {
        if (productToDisplay != null) {
            wgDlg.showWarngenProduct(productToDisplay, notify);
        }

        // Display the first Text Editor
        showTextEditor(0);
    }

    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createFileMenus(menuBar);
        createWindowsMenus(menuBar);

        shell.setMenuBar(menuBar);
    }

    private void createFileMenus(Menu menuBar) {
        // -------------------------------------
        // Create all the items in the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        // --------------------------------------------------
        // Create Exit menu item
        // --------------------------------------------------
        MenuItem exitMenuItem = new MenuItem(fileMenu, SWT.NONE);
        exitMenuItem.setText("Exit");
        exitMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getShell().close();
            }
        });
    }

    private void createWindowsMenus(Menu menuBar) {
        // ----------------------------------------
        // Create all the items in the Windows menu
        // ----------------------------------------
        MenuItem windowsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        windowsMenuItem.setText("Windows");

        // Create the File menu item with a File "dropdown" menu
        Menu windowsMenu = new Menu(menuBar);
        windowsMenuItem.setMenu(windowsMenu);

        // --------------------------------------------------
        // Create Hide All menu item
        // --------------------------------------------------
        MenuItem hideAllMenuItem = new MenuItem(windowsMenu, SWT.NONE);
        hideAllMenuItem.setText("Hide All");
        hideAllMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                hideAllTextEditors();
            }
        });

        // --------------------------------------------------
        // Create Show All menu item
        // --------------------------------------------------
        MenuItem showAllMenuItem = new MenuItem(windowsMenu, SWT.NONE);
        showAllMenuItem.setText("Show All");
        showAllMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showAllTextEditors();
            }
        });

        // -------------------------------
        // Add a menu separator.
        // -------------------------------
        new MenuItem(windowsMenu, SWT.SEPARATOR);

        // --------------------------------------------------
        // Create New Window menu item
        // --------------------------------------------------
        newWindowMenuItem = new MenuItem(windowsMenu, SWT.NONE);
        newWindowMenuItem.setText("New Window");
        newWindowMenuItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                addNewWindowButton();
            }
        });
    }

    private void createAwipsLabel() {
        GridData gd = new GridData(300, 20);
        Label awipsBlankLabel = new Label(shell, SWT.NONE);
        awipsBlankLabel.setFont(fontAwipsLabel);
        awipsBlankLabel.setText(" ");
        awipsBlankLabel.setLayoutData(gd);
        gd = new GridData(300, 80);
        Label awipsLabel = new Label(shell, SWT.NONE);
        awipsLabel.setFont(fontAwipsLabel);
        awipsLabel.setText("   AWIPS  II");
        awipsLabel.setLayoutData(gd);
    }

    private void createTimeLabels() {
        GridData gd = null;

        gd = new GridData(300, SWT.DEFAULT);
        utcTimeLabel = new Label(shell, SWT.CENTER);
        utcTimeLabel.setLayoutData(gd);

        gd = new GridData(300, SWT.DEFAULT);
        localTimeLabel = new Label(shell, SWT.CENTER);
        localTimeLabel.setLayoutData(gd);

        date = SimulatedTime.getSystemTime().getTime();
        localTimeLabel.setText(sdfLocal.format(date));
        utcTimeLabel.setText(sdfUTC.format(date));
    }

    private void createAlertAlarm() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        alertAlarmBtn = new Button(shell, SWT.PUSH);
        alertAlarmBtn.setText("Alarm/Alert");
        alertAlarmBtn.setLayoutData(gd);
        AlarmAlertNotificationObserver.getInstance();

        alertAlarmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                alarmDlg.show();
            }
        });
    }

    private void createTextButtons() {

        scrolledComp = new ScrolledComposite(shell, SWT.V_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        scrolledComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        scrolledComp.setLayoutData(gd);

        textButtonComp = new Composite(scrolledComp, SWT.NONE);
        textButtonComp.setLayout(new GridLayout(1, false));
        textButtonComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        textBtnArray = new ArrayList<>();
        textEditorArray = new ArrayList<>();

        for (int x = 1; x <= INIT_BUTTON_CNT; ++x) {
            createButtonAndTextEditor(x);
        }
        scrolledComp.setExpandHorizontal(true);
        scrolledComp.setExpandVertical(true);
        scrolledComp.setContent(textButtonComp);
        scrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                scrolledComp.setMinSize(
                        textButtonComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
            }
        });
        scrolledComp.layout();

    }

    private void addNewWindowButton() {
        boolean pack = false;
        // only pack if the scroll bar is not present before adding new button
        int visibleHeight = scrolledComp.getBounds().height;
        int buttonsHeight = textButtonComp.getSize().y;
        if (visibleHeight >= buttonsHeight) {
            pack = true;
        }
        int currentBtnCount = textEditorArray.size();
        if (currentBtnCount < MAX_BUTTON_CNT) {
            ++currentBtnCount;
            createButtonAndTextEditor(currentBtnCount);
        }

        if (currentBtnCount == MAX_BUTTON_CNT) {
            newWindowMenuItem.setEnabled(false);
        }
        scrolledComp.setMinSize(
                textButtonComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        if (pack) {
            shell.pack();
        }
    }

    private void createButtonAndTextEditor(int btnNumber) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button textBtn = new Button(textButtonComp, SWT.PUSH);
        String btnTitle = "Text " + btnNumber;
        textBtn.setText(btnTitle);
        textBtn.setLayoutData(gd);
        textBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showTextEditor(textBtnArray.indexOf(event.getSource()));
            }
        });
        textBtnArray.add(textBtn);

        // Make place holder for the edit dialog and only create if requested.
        textEditorArray.add(null);
    }

    private synchronized void createWarngenDisplay() {
        if (wgDlg == null) {
            wgDlg = new TextEditorDialog(getShell(), "Text Warngen", false, "9",
                    true);
        }
    }

    private void showTextEditor(int editorIndex) {
        TextEditorDialog teDlg = textEditorArray.get(editorIndex);
        if (teDlg == null) {
            // create a new instance
            String btnTitle = "Text " + (editorIndex + 1);
            teDlg = new TextEditorDialog(shell, btnTitle, false, this,
                    ((Integer) (editorIndex + 1)).toString(), true, true,
                    CAVE.PERSPECTIVE_INDEPENDENT);

            textEditorArray.set(editorIndex, teDlg);
        }

        textEditorArray.get(editorIndex).showDialog();
    }

    private void showAllTextEditors() {
        for (int i = 0; i < textEditorArray.size(); i++) {
            showTextEditor(i);
        }
    }

    private void hideAllTextEditors() {
        Shell myShell;
        for (TextEditorDialog teDlg : textEditorArray) {
            if (teDlg != null) {
                teDlg.hideDialog();
            }
        }
        for (int i = 1; i < 9; i++) {
            ITextWorkstationCallback cb = TextDisplayModel.getInstance()
                    .getITextWorkstationCallback(((Integer) i).toString());
            if (cb != null) {
                if (cb.isBrowserActive()) {
                    myShell = cb.getShell();
                    myShell.setVisible(false);
                }
            }
        }
    }

    private void startTimeTimer() {
        timer = new Timer();

        TimerTask updateTimeTask = new TimerTask() {
            @Override
            public void run() {
                getDisplay().syncExec(new Runnable() {
                    @Override
                    public void run() {
                        updateTimeLabels();
                    }
                });
            }
        };

        timer.schedule(updateTimeTask, 200, 20_000);
    }

    private void updateTimeLabels() {
        date = SimulatedTime.getSystemTime().getTime();
        localTimeLabel.setText(sdfLocal.format(date));
        utcTimeLabel.setText(sdfUTC.format(date));
    }

    @Override
    public void restoreText(int teID) {
        textBtnArray.get(teID).setText("Text " + (teID + 1));
    }

    @Override
    public void updateText(int teID, String newText) {
        // Pass in "token-1" as teID and
        // "TextDisplayModel.getInstance().getControlDialogButtonText(((Integer)
        // token).toString())" as newText.
        textBtnArray.get(teID).setText(newText);

    }

    @Override
    public synchronized void notificationArrived(
            NotificationMessage[] messages) {
        // SimpleDateFormat sdf = new
        // SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        // sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        // System.out.println(sdf.format(new Date())
        // + ": warning received by text workstation");

        if (messages.length > 0) {
            try {
                NotificationMessage message = messages[messages.length - 1];
                // Drain old messages in the queue.
                // See DR#7193 and DR#11450
                // Originally DR#7193 fixed its problem by getting a timestamp
                // of when the message was created and seeing if it was less
                // then the startime of this instance of the class. This created
                // problems when the times on the box creating the WarnGen
                // and the the box running textws differ by a large amount
                // causing the problem in DR#11450. You could still get old
                // queue message or even drop messages sent after textws was
                // started.
                //
                // This approach drops messages that come in shortly after
                // starting textws and does not depend of the times of the work
                // stations being in synch. This assumes old messages will be
                // sent shortly after a connection is made to the server.
                //
                // The ideal solution would have the creator of the WarnGen not
                // even queue the message when there is no textws connected to
                // service the queue. This would involve significant changes to
                // how we use 3rd party software and can not be implemented at
                // this time.
                if (System.currentTimeMillis() - initStartTime <= delta) {
                    // Slowly increment the delta in case there are a lot of old
                    // messages.
                    delta += TextWorkstationDlg.incDelta;
                } else if (message.isNotExpired()) {
                    String product = message.getMessagePayload().toString();
                    if (wgDlg == null) {
                        productToDisplay = product;
                    } else {
                        if (!wgDlg.isEditMode()) {
                            wgDlg.showWarngenProduct(product, notify);
                        } else {
                            wgDlg.enqueue(product, notify);
                        }
                    }
                }

                // TODO: Open up a text editor dialog and have it retrieve and
                // parse the warning based on the afosId
            } catch (NotificationException e) {
                statusHandler
                        .warn("Error in received text product notification", e);
            }
        }
    }
}
