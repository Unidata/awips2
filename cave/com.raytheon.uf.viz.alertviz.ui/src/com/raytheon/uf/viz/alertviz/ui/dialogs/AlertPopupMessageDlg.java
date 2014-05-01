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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;

/**
 * This class displays the pop-up message dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 18 Nov 2010  2235       cjeanbap    Changed button, hideDialogBtn, text
 *                                     and store previous location.
 * 13 Jan 2011  7375       cjeanbap    Commented out shell.setVisible(...) in
 *                                     acknowledgeLastMessage().
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertPopupMessageDlg extends Dialog implements MouseMoveListener,
        MouseListener {
    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Font used for the controls.
     */
    private Font controlFont;

    /**
     * Font used for labels
     */
    private Font labelFont;

    /**
     * Font used for the double click label.
     */
    private Font dblClickLblFont;

    /**
     * Message text field.
     */
    private Text messageTF;

    /**
     * Time label.
     */
    private Label timeLbl;

    /**
     * Hide/Show message log button.
     */
    private Button hideShowLogBtn;

    /**
     * Acknowledge last message button.
     */
    private Button ackSelectedBtn;

    /**
     * Acknowledge Last message button.
     */
    private Button ackLastBtn;

    /**
     * Acknowledge all messages.
     */
    private Button ackAllBtn;

    /**
     * Close button.
     */
    private Button hideDialogBtn;

    /**
     * Log message composite.
     */
    private Composite logComp;

    /**
     * Expanded popup flag.
     */
    private boolean expanded;

    /**
     * List of messages to be acknowledged.
     */
    private List msgLogList;

    /**
     * Details box for messages
     */
    private SimpleDetailsComp detailsComp;

    /**
     * Date format.
     */
    private SimpleDateFormat dateFormat = new SimpleDateFormat(
            "MMM dd yy HH:mm:ss z");

    /**
     * Time format.
     */
    private SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm a");

    /**
     * Array of status messages.
     */
    private ArrayList<StatusMessage> statMsgArray = new ArrayList<StatusMessage>();

    /**
     * Source label.
     */
    private Label sourceLbl;

    /**
     * Priority label.
     */
    private Label priorityLbl;

    /**
     * Category label.
     */
    private Label categoryLbl;

    /**
     * Source string.
     */
    private final String sourceStr = " Source: ";

    /**
     * Category string.
     */
    private final String categoryStr = " Category: ";

    /**
     * Priority string.
     */
    private final String priorityStr = " Priority: ";

    /**
     * Maximum messages to acknowledge.
     */
    private int maxMessages = 100;

    /**
     * Move label.
     */
    private Label moveLabel;

    /**
     * Move dialog flag.
     */
    private boolean moveDialog = false;

    /**
     * Mouse origin.
     */
    private Point origin;

    /**
     * Adjusted dialog location.
     */
    private Point dialogLoc;

    /**
     * Actual dialog X, Y coordinate.
     */
    private Point dialogXY;

    /**
     * Dialog background color.
     */
    private Color bgColor;

    /**
     * Composite for labels
     */
    private Composite topLabelComp;

    /**
     * Composite for messages
     */
    private Composite messageComp;

    /**
     * Composite for actions
     */
    private Composite actionComp;

    /**
     * Label used as filler
     */
    private Label fillerLbl;

    /**
     * Listens for "Hide Dialog" event, implemented by AlertVisualization class
     */
    private Listener hideListener;

    /**
     * Initialized flag indicating if the control have been initialized.
     */
    private boolean initialized = false;

    private AlertVisualization av;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param statMsg
     *            Status message.
     * @param expanded
     *            Expanded flag.
     * @param listener
     *            Hide listener.
     * @param startUpRGB
     *            Color to be displayed at startup.
     */
    public AlertPopupMessageDlg(Shell parent, StatusMessage statMsg,
            boolean expanded, Listener listener, RGB startUpRGB,
            AlertVisualization av) {
        super(parent, 0);
        hideListener = listener;
        statMsgArray.add(statMsg);
        this.expanded = expanded;
        this.av = av;

        initShell(startUpRGB);
    }

    /**
     * Initialize the shell
     */
    private void initShell(RGB startUp) {
        Shell parent = getParent();
        display = parent.getDisplay();

        shell = new Shell(parent, SWT.ON_TOP);
        shell.setText("Alert Visualization Popup Message Dialog");
        Rectangle prvLocation = av.getAlertPopupMsgPrvLocation();
        if (prvLocation != null) {
            shell.getBounds().add(prvLocation);
        }

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        shell.setLayout(mainLayout);

        // initialize data, fonts, and arrays
        initalizeData();

        // Initialize all of the controls and layouts
        initializeComponents();

        setBackgroundColors(startUp);

        // listener event triggers when shell set to not be visible
        shell.addListener(SWT.Hide, hideListener);

        shell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                while (statMsgArray.size() != 0) {
                    if (!display.readAndDispatch()) {
                        display.sleep();
                    }
                }
                bgColor.dispose();
            }
        });
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return True/False/null.
     */
    public Object open() {
        shell.pack();

        setInitialDialogLocation();

        initialized = true;

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        initialized = false;

        controlFont.dispose();
        labelFont.dispose();
        dblClickLblFont.dispose();

        return null;
    }

    /**
     * Initialize font data
     */
    private void initalizeData() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        labelFont = new Font(shell.getDisplay(), "Monospace", 14, SWT.BOLD);
        dblClickLblFont = new Font(shell.getDisplay(), "Monospace", 10,
                SWT.BOLD);
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createTitleBarLabel();

        createTopLabels();

        createMessageControl();

        createActionButtons();

        createMessageLogControl();
    }

    /**
     * Create the title bar of dialog
     */
    private void createTitleBarLabel() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        moveLabel = new Label(shell, SWT.CENTER | SWT.BORDER);
        moveLabel.setText("Alert Visualization Popup Message Dialog");
        moveLabel.setLayoutData(gd);
        moveLabel.setFont(labelFont);
        moveLabel.addMouseListener(this);
        moveLabel.addMouseMoveListener(this);
        moveLabel.setBackground(display
                .getSystemColor(SWT.COLOR_TITLE_BACKGROUND));
        moveLabel.setForeground(display
                .getSystemColor(SWT.COLOR_TITLE_FOREGROUND));
    }

    /**
     * Create the labels at the top of the dialog.
     */
    private void createTopLabels() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        topLabelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 35;
        topLabelComp.setLayout(gl);
        topLabelComp.setLayoutData(gd);
        topLabelComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        sourceLbl = new Label(topLabelComp, SWT.NONE);
        sourceLbl.setText(sourceStr + statMsgArray.get(0).getSourceKey());
        sourceLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        priorityLbl = new Label(topLabelComp, SWT.NONE);
        priorityLbl.setText(priorityStr
                + statMsgArray.get(0).getPriority().ordinal());
        priorityLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        categoryLbl = new Label(topLabelComp, SWT.NONE);
        categoryLbl.setText(categoryStr + statMsgArray.get(0).getCategory());
        categoryLbl.setLayoutData(gd);

        /*
         * If the expanded flag is false then hide the labels that display the
         * Category/Source/Priority information.
         */
        if (expanded == false) {
            // Hide the message labels
            ((GridData) topLabelComp.getLayoutData()).exclude = true;
            topLabelComp.setVisible(false);
            shell.layout();
            shell.pack();
        }
    }

    /**
     * Create the message text control.
     */
    private void createMessageControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        messageComp = new Composite(shell, SWT.NONE);
        messageComp.setLayout(new GridLayout(1, false));
        messageComp.setLayoutData(gd);
        messageComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 920;
        messageTF = new Text(messageComp, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        messageTF.setLayoutData(gd);
        messageTF.setText(statMsgArray.get(0).getMessage());
        messageTF.setEditable(false);
    }

    /**
     * Create the action buttons for the dialog.
     */
    private void createActionButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionComp = new Composite(shell, SWT.NONE);
        actionComp.setLayout(new GridLayout(7, false));
        actionComp.setLayoutData(gd);
        actionComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        gd = new GridData(200, SWT.DEFAULT);
        timeLbl = new Label(actionComp, SWT.NONE);
        timeLbl.setText(dateFormat.format(statMsgArray.get(0).getEventTime()));
        timeLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fillerLbl = new Label(actionComp, SWT.NONE);
        fillerLbl.setText("");
        fillerLbl.setBackground(display.getSystemColor(SWT.COLOR_RED));
        fillerLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        hideShowLogBtn = new Button(actionComp, SWT.PUSH);
        hideShowLogBtn.setLayoutData(gd);

        if (expanded == true) {
            hideShowLogBtn.setText("Hide Log");
        } else {
            hideShowLogBtn.setText("Show Log");
        }

        hideShowLogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                expanded = !expanded;
                showHideLog();
            }
        });

        ackSelectedBtn = new Button(actionComp, SWT.PUSH);
        ackSelectedBtn.setText("Acknowledge Selected");
        ackSelectedBtn.setLayoutData(new GridData());
        ackSelectedBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                acknowledgeSelectedMessage();
            }
        });

        ackLastBtn = new Button(actionComp, SWT.PUSH);
        ackLastBtn.setText("Acknowledge Last");
        ackLastBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                acknowledgeLastMessage();
            }
        });

        ackAllBtn = new Button(actionComp, SWT.PUSH);
        ackAllBtn.setText("Acknowledge All");
        ackAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                acknowledgeAllMessages(true);
            }
        });

        hideDialogBtn = new Button(actionComp, SWT.PUSH);
        hideDialogBtn.setText("Close");
        hideDialogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle prevLocation = Display.getCurrent().getBounds();
                av.setAlertPopupMsgPrvLocation(prevLocation);
                shell.setVisible(false);
            }
        });

        /*
         * If the expanded flag is false then hide the Acknowledge Selected
         * button.
         */
        if (expanded == false) {
            // Hide the Acknowledge Selected button
            ((GridData) ackSelectedBtn.getLayoutData()).exclude = true;
            ackSelectedBtn.setVisible(false);
            actionComp.layout();
            actionComp.pack();
        }
    }

    /**
     * Create the message log list control.
     */
    private void createMessageLogControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        logComp = new Composite(shell, SWT.NONE);
        logComp.setLayout(new GridLayout(1, false));
        logComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label dblClickLBl = new Label(logComp, SWT.BORDER | SWT.CENTER);
        dblClickLBl
                .setText("Double-click message to display more information:");
        dblClickLBl.setFont(dblClickLblFont);
        dblClickLBl.setForeground(display.getSystemColor(SWT.COLOR_DARK_BLUE));
        dblClickLBl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 900;
        gd.heightHint = 250;
        msgLogList = new List(logComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        msgLogList.setFont(controlFont);
        msgLogList.setLayoutData(gd);
        msgLogList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showSelectedListData();
            }
        });

        detailsComp = new SimpleDetailsComp(logComp, SWT.NONE);

        msgLogList.addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick(MouseEvent e) {

                if (detailsComp.isVisible() == false) {
                    int idx = msgLogList.getSelectionIndex();
                    if (idx < 0) {
                        return;
                    }
                    StatusMessage sm = statMsgArray.get(idx);
                    detailsComp.displayDetails(sm);
                    detailsComp.setVisible(true);
                } else {
                    detailsComp.setVisible(false);
                }
            }

            @Override
            public void mouseDown(MouseEvent e) {
            }

            @Override
            public void mouseUp(MouseEvent e) {
            }

        });

        if (expanded == false) {
            ((GridData) logComp.getLayoutData()).exclude = true;
            logComp.setVisible(false);
            shell.layout();
            shell.pack();
        }

        msgLogList.add(getFormattedMessage(statMsgArray.get(0)));
        msgLogList.select(0);

    }

    /**
     * Show/Hide the message log list.
     */
    private void showHideLog() {
        if (expanded == true) {
            // Show the message labels
            ((GridData) topLabelComp.getLayoutData()).exclude = false;
            topLabelComp.setVisible(true);
            shell.layout();
            shell.pack();

            // Show the Acknowledge Selected button
            ((GridData) ackSelectedBtn.getLayoutData()).exclude = false;
            ackSelectedBtn.setVisible(true);
            actionComp.layout();
            actionComp.pack();

            // Show the message log
            ((GridData) logComp.getLayoutData()).exclude = false;
            logComp.setVisible(true);
            shell.layout();
            shell.pack();
            hideShowLogBtn.setText("Hide Log");
        } else {
            // Hide the message labels
            ((GridData) topLabelComp.getLayoutData()).exclude = true;
            topLabelComp.setVisible(false);
            shell.layout();
            shell.pack();

            // Hide the Acknowledge Selected button
            ((GridData) ackSelectedBtn.getLayoutData()).exclude = true;
            ackSelectedBtn.setVisible(false);
            actionComp.layout();
            actionComp.pack();

            // Hide the message log
            detailsComp.setVisible(false);
            ((GridData) logComp.getLayoutData()).exclude = true;
            logComp.setVisible(false);
            shell.layout();
            shell.pack();
            hideShowLogBtn.setText("Show Log");
        }
    }

    /**
     * Show the data associated with the selected message in the message list
     * control.
     */
    private void showSelectedListData() {
        int index = msgLogList.getSelectionIndex();

        if (index < 0) {
            return;
        }

        StatusMessage sm = statMsgArray.get(index);

        categoryLbl.setText(categoryStr + sm.getCategory());
        priorityLbl.setText(priorityStr + sm.getPriority().ordinal());
        sourceLbl.setText(sourceStr + sm.getSourceKey());
        timeLbl.setText(dateFormat.format(sm.getEventTime()));
        messageTF.setText(sm.getMessage());

        detailsComp.displayDetails(sm);

        this.shell.pack();
    }

    /**
     * Bring the dialog to the front.
     */
    public void dialogToFront() {
        // This is the way SWT works to bring a dialog
        // to the front of the display when it is hidden
        // under another dialog.
        shell.setLocation(dialogXY);
        shell.setFocus();
        shell.setVisible(true);
    }

    /**
     * Add a new status message.
     * 
     * @param sm
     *            Status message.
     */
    public void addNewMessage(StatusMessage sm, AlertMetadata amd) {

        if (statMsgArray.size() >= maxMessages) {
            statMsgArray.remove(statMsgArray.size() - 1);
        }

        setBackgroundColors(amd.getBackground());

        statMsgArray.add(0, sm);

        if (msgLogList.isDisposed() == true) {
            return;
        }

        int currentIndex = msgLogList.getSelectionIndex();
        msgLogList.removeAll();

        for (StatusMessage message : statMsgArray) {
            msgLogList.add(getFormattedMessage(message));
        }

        // Check if a message is not selected.
        if (currentIndex < 0 || !expanded) {
            if (msgLogList.getItemCount() > 0) {
                msgLogList.select(0);
                showSelectedListData();
            }
            return;
        }

        ++currentIndex;

        // Check if the current index will go outside the array
        // (it should never do this but check anyway...)
        if (currentIndex > 0 && currentIndex == statMsgArray.size()) {
            msgLogList.select(0);
            showSelectedListData();
            return;
        }

        msgLogList.select(currentIndex);
        if (initialized == true) {
            showDialog(true);
        }
    }

    /**
     * Get the formatted status message.
     * 
     * @param sm
     *            Status message.
     * @return Formatted status message.
     */
    private String getFormattedMessage(StatusMessage sm) {
        StringBuilder strBld = new StringBuilder();
        String localTZ = System.getenv("FXA_LOCAL_TZ");
        if (localTZ == null) {
            localTZ = "GMT";
        }
        timeFormat.setTimeZone(TimeZone.getTimeZone(localTZ));

        strBld.append(timeFormat.format(sm.getEventTime())).append(" ");
        strBld.append("(").append(sm.getPriority().ordinal()).append(") | ");
        strBld.append(sm.getCategory()).append(" | ");
        strBld.append(sm.getSourceKey()).append(": ");
        strBld.append(sm.getMessage());

        return strBld.toString();
    }

    /**
     * Acknowledge the selected message.
     */
    private void acknowledgeSelectedMessage() {
        int index = msgLogList.getSelectionIndex();

        if (index == -1) {
            return;
        }

        StatusMessage sm = statMsgArray.get(index);

        try {
            SystemStatusHandler
                    .acknowledge(sm, System.getProperty("user.name"));
        } catch (Exception ex) {
            Container.logInternal(Priority.ERROR, "Acknowledge failed...", ex);
            return;
        }

        statMsgArray.remove(index);
        msgLogList.remove(index);

        if (statMsgArray.size() == 0) {
            shell.dispose();
            return;
        }

        int newIndex = msgLogList.getItemCount() - 1;

        msgLogList.select(newIndex);
        showSelectedListData();
    }

    /**
     * Acknowledge the last received message and hide the dialog.
     */
    private void acknowledgeLastMessage() {
        StatusMessage sm = statMsgArray.get(0);

        try {
            SystemStatusHandler
                    .acknowledge(sm, System.getProperty("user.name"));
        } catch (Exception ex) {
            Container.logInternal(Priority.ERROR, "Acknowledge last failed...",
                    ex);
            return;
        }

        statMsgArray.remove(0);
        msgLogList.remove(0);

        if (statMsgArray.size() == 0) {
            shell.dispose();
            return;
        }

        msgLogList.select(0);
        showSelectedListData();
    }

    /**
     * Set the background color of the dialog and all components.
     * 
     * @param rgb
     */
    private void setBackgroundColors(RGB rgb) {
        /*
         * Set the shell background color to match the message.
         */
        if (bgColor != null) {
            bgColor.dispose();
        }

        bgColor = new Color(display, rgb);

        shell.setBackground(bgColor);

        topLabelComp.setBackground(bgColor);
        messageComp.setBackground(bgColor);
        actionComp.setBackground(bgColor);
        fillerLbl.setBackground(bgColor);
    }

    /**
     * Acknowledge all of the messages.
     */
    public void acknowledgeAllMessages(boolean confirmPrompt) {
        int index = msgLogList.getSelectionIndex();

        int result = SWT.CANCEL;
        if (index == -1) {
            return;
        }

        boolean expandedForPromp = false;
        if (!expanded) {
            expandedForPromp = expanded = true;
        }

        showHideLog();
        if (confirmPrompt) {
            ConfirmationDlg cd = new ConfirmationDlg(shell,
                    "Are you sure you want to acknowledge all popup messages?",
                    SWT.ICON_QUESTION);

            if (msgLogList != null && !msgLogList.isDisposed()) {
                Rectangle logBounds = msgLogList.getBounds();
                Point logDisplayOrigin = msgLogList.toDisplay(0, 0);
                logBounds.x = logDisplayOrigin.x;
                logBounds.y = logDisplayOrigin.y;
                cd.setAvoidedArea(logBounds);
            }

            result = cd.open();

            if (result != SWT.YES) {
                if (result == SWT.CANCEL && expandedForPromp) {
                    expanded = false;
                    showHideLog();
                }
                return;
            }
        }

        String userName = System.getProperty("user.name");

        for (StatusMessage message : statMsgArray) {
            try {
                SystemStatusHandler.acknowledge(message, userName);
            } catch (Exception ex) {
                Container
                        .logInternal(
                                Priority.ERROR,
                                "AlertPopupMessaeDlg: exception acknowledging message.",
                                ex);
            }
        }

        statMsgArray.clear();
        shell.dispose();
    }

    /**
     * Dispose the dialog.
     */
    public void dispose() {
        shell.dispose();
    }

    /**
     * Sets initial location of dialog.
     */
    private void setInitialDialogLocation() {

        if (dialogXY == null) {

            int screenHeight = display.getBounds().height;
            int screenWidth = display.getPrimaryMonitor().getBounds().width;

            int newX = screenWidth / 2
                    - (shell.getChildren())[0].getBounds().width / 2;
            int newY = (screenHeight / 2 - (shell.getChildren())[0].getBounds().height / 2) - 200;

            shell.setLocation(newX, newY);

            dialogXY = new Point(newX, newY);
        } else {
            shell.setLocation(dialogXY);
        }
    }

    /**
     * Shows or hides the dialog.
     * 
     * @param show
     *            True to show dialog, false to hide.
     */
    public void showDialog(boolean show) {
        shell.setLocation(dialogXY);
        shell.setVisible(show);

        showSelectedListData();
        msgLogList.showSelection();
    }

    /**
     * Get the number of unacknowledged messages.
     * 
     * @return Number of unacknowledged messages.
     */
    public int getNumberOfMessages() {
        return statMsgArray.size();
    }

    /**
     * Returns whether dialog is open or not, checks that she shell is not null,
     * not disposed and visible.
     * 
     * @return True if open, false if not.
     */
    public boolean dialogIsOpen() {
        return (shell != null && !shell.isDisposed() && shell.isVisible());
    }

    /**
     * Mouse double-click event.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDoubleClick(MouseEvent e) {
        // Don't do anything
    }

    /**
     * Mouse button down event.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseDown(MouseEvent e) {
        origin = new Point(e.x, e.y);
        moveDialog = true;
    }

    /**
     * Mouse button up event.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseUp(MouseEvent e) {
        moveDialog = false;
    }

    /**
     * Mouse move event.
     * 
     * @param e
     *            Mouse event.
     */
    @Override
    public void mouseMove(MouseEvent e) {
        if (origin != null && moveDialog == true) {
            // Move the dialog.
            dialogLoc = display.map(shell, null, e.x, e.y);
            dialogXY.x = dialogLoc.x - origin.x;
            dialogXY.y = dialogLoc.y - origin.y;
            shell.setLocation(dialogXY.x, dialogXY.y);
        }
    }

    /**
     * Returns if dialog is disposed.
     * 
     * @return True if disposed, false if not.
     */
    public boolean isDisposed() {
        return shell.isDisposed();
    }

}
