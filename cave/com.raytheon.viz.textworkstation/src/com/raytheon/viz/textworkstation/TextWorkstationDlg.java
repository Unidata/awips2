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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
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
import com.raytheon.uf.common.time.SimulatedTime;
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
import com.raytheon.viz.texteditor.util.RadarTextUtility;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TextWorkstationDlg class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/27/2007    368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 11/28/2007   520         grichard    Implemented build 11 features.
 * 1/3/2008     637         grichard    Implemented build 13 features.
 * 1/10/2008    722         grichard    Implemented build 14 features.
 * 5/16/2008    1119        grichard    Added support for IAviationObserver.
 * 6/3/2008     937         grichard    Corrected simple date formats.
 * 5/8/2009     2104        grichard    Added support for IRadarObserver.
 * 5/8/2009     2104        grichard    Added support for IScriptRunnerObserver.
 * 6/07/2010    5851        cjeanbap    Properly stop alert/alarm observer listener.
 * 8/23/2010    2187        cjeanbap    Removed window location to TextEditorDialog.preOpened().
 * 10/04/2010   7193        cjeanbap    Added if statement to notificationArrived(), to 
 *                                      determine if the message has expired.
 * 11/08/2010   7433        cjeanbap    Check TextEditorDialog current mode before tempting
 *                                      to open dialog.
 * 05Jan2011    7375        cjeanbap    Fix disposed Widget exception.
 * 01Feb2011    7193        cjeanbap    Add boolean condition to check initial start time.
 * 03Nov2011    11450       rferrel     Change how old products pruge so it is no longer
 *                                      on times on two machines being in synch.
 * 26Sep2012    1196        lvenable    Dialog refactor to not block.
 * 02Oct2012    1229        rferrel     Option to allow blocking when top dialog.
 * 13Dec2012    1353        rferrel     Fix bug introduced in the Show all dialogs.
 * 30Jan2013    DR 14736    D. Friedman Display local time.
 * 24Jun2013    DR 15733    XHuang      Display MAX_BUTTON_CNT (8 button).
 * 25July2013   DR 15733    Greg Hull   Make dflt and max number of Text Buttons configurable.
 * 28Oct2015    5054        randerso    Make TextWorkstationDlg appear in upper left corner of 
 *                                      monitor where parent shell is located
 * Dec 14, 2015 4834        njensen     Remove dead menu items
 * Jan 26, 2016 5054        randerso    Changed to use display as parent
 * Feb 15, 2016 4860        njensen     Removed references to IAviationObserver
 * Mar 30, 2016 5513        randerso    Fixed to display on same monitor as parent
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class TextWorkstationDlg extends CaveSWTDialog implements
        ITextEditorCallback, INotificationObserver {

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

    SimpleDateFormat sdfLocal = new SimpleDateFormat("EEE dd MMM yyyy HH:mm z");

    SimpleDateFormat sdfUTC = new SimpleDateFormat("EEE dd MMM yyyy HH:mm z");

    private Timer timer;

    private TimerTask updateTimeTask;

    private Date date;

    private List<Button> textBtnArray;

    private List<TextEditorDialog> textEditorArray;

    private TextEditorDialog wgDlg;

    private NotifyExpiration notify;

    private CurrentAlarmQueue alarmDlg;

    private long initStartTime;

    /**
     * Create top level Text Workstation Dialog
     * 
     * @param display
     * 
     */
    public TextWorkstationDlg(Display display) {
        super(display, SWT.DIALOG_TRIM | SWT.MIN, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);

        setText("Text Workstation");
        TextDisplayModel.getInstance().setTextRadar(new RadarTextUtility());
        NotificationManagerJob.addQueueObserver(
                TextWorkstationConstants.getTextWorkstationQueueName(), this);
        initStartTime = System.currentTimeMillis();
    }

    @Override
    protected void disposed() {
        font.dispose();
        fontAwipsLabel.dispose();
        timer.cancel();
        NotificationManagerJob.removeQueueObserver(
                TextWorkstationConstants.getTextWorkstationQueueName(), null,
                this);
        AlarmAlertFunctions.closeAlarmAlertBell();
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
        sdfLocal.setTimeZone(localTZName != null ? TimeZone
                .getTimeZone(localTZName) : TimeZone.getDefault());

        createMenus();
        new Label(shell, SWT.NONE).setText("host: "
                + TextWorkstationConstants.getHostName());
        createAwipsLabel();
        createTimeLabels();
        startTimeTimer();
        createAlertAlarm();
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
        AlarmAlertFunctions.getAlarmAlertBell(shell);
    }

    @Override
    protected void preOpened() {
        super.preOpened();

        Monitor monitor = null;
        if (getParent() != null) {
            monitor = getParent().getShell().getMonitor();

        } else {
            Point cursor = getDisplay().getCursorLocation();
            for (Monitor m : getDisplay().getMonitors()) {
                Rectangle bounds = m.getBounds();
                if (bounds.contains(cursor)) {
                    monitor = m;
                    break;
                }
            }
        }

        Point loc = new Point(0, 0);
        if (monitor != null) {
            Rectangle bounds = monitor.getBounds();
            loc.x = bounds.x;
            loc.y = bounds.y;
        }
        shell.setLocation(loc);
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
        Button alertAlarmBtn = new Button(shell, SWT.PUSH);
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
        textBtnArray = new ArrayList<Button>();
        textEditorArray = new ArrayList<TextEditorDialog>();

        for (int x = 1; x <= INIT_BUTTON_CNT; ++x) {
            createButtonAndTextEditor(x);
        }
    }

    private void addNewWindowButton() {
        int currentBtnCount = textEditorArray.size();
        if (currentBtnCount < MAX_BUTTON_CNT) {
            ++currentBtnCount;
            createButtonAndTextEditor(currentBtnCount);
        }

        if (currentBtnCount == MAX_BUTTON_CNT) {
            newWindowMenuItem.setEnabled(false);
        }

        shell.pack();
    }

    private void createButtonAndTextEditor(int btnNumber) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button textBtn = new Button(shell, SWT.PUSH);
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
            wgDlg = new TextEditorDialog(getShell(), "Text Warngen", false,
                    "9", true);
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

        updateTimeTask = new TimerTask() {
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

        timer.schedule(updateTimeTask, 200, 20000);
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
    public synchronized void notificationArrived(NotificationMessage[] messages) {
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
                e.printStackTrace();
            }
        }
    }
}
