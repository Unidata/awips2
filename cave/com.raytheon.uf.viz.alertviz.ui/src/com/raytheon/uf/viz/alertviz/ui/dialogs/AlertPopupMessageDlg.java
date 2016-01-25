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
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
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
 * 20 Apr 2015  4311       lvenable    Fixed text field to accept really long text strings.
 * 29 Jun 2015  4311       randerso    Reworking AlertViz dialogs to be resizable.
 * 25 Jan 2016  5054       randerso    Converted to stand alone window
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertPopupMessageDlg implements MouseMoveListener, MouseListener,
        DisposeListener {
    private static final int MAX_INITIAL_LINES = 5;

    private static final int WIDTH_IN_CHARS = 135;

    private static final double PERCENT_OF_SCREEN_WIDTH = 0.75;

    private static final int NUM_LIST_ITEMS = 10;

    /*
     * Adjustment for SWT bug where shell.getSize() returns incorrect value when
     * SWT.ON_TOP style is used.
     */
    private static final int SWT_BUG_FACTOR = 6;

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Font used for labels
     */
    private Font labelFont;

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
    private final SimpleDateFormat dateFormat = new SimpleDateFormat(
            "MMM dd yy HH:mm:ss z");

    /**
     * Time format.
     */
    private final SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm a");

    /**
     * Array of status messages.
     */
    private final ArrayList<StatusMessage> statMsgArray = new ArrayList<StatusMessage>();

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
    private final int maxMessages = 100;

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
     * Listens for Hide and Dispose events
     */
    private final Listener eventListener;

    /**
     * Initialized flag indicating if the control have been initialized.
     */
    private boolean initialized = false;

    private boolean first;

    private int controlWidth;

    /**
     * Constructor.
     * 
     * @param display
     * @param statMsg
     *            Status message.
     * @param expanded
     *            Expanded flag.
     * @param listener
     *            Event listener.
     * @param startUpRGB
     *            Color to be displayed at startup.
     */
    public AlertPopupMessageDlg(Display display, StatusMessage statMsg,
            boolean expanded, Listener listener, RGB startUpRGB) {
        this.display = display;
        eventListener = listener;
        statMsgArray.add(statMsg);
        this.expanded = expanded;
        this.first = true;

        initShell(startUpRGB);
    }

    /**
     * Initialize the shell
     */
    private void initShell(RGB startUp) {
        shell = new Shell(display, SWT.ON_TOP | SWT.RESIZE);
        shell.setText("Alert Visualization Popup Message Dialog");

        shell.addDisposeListener(this);

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 0;
        mainLayout.marginHeight = 0;
        mainLayout.verticalSpacing = 0;
        shell.setLayout(mainLayout);

        // initialize data, fonts, and arrays
        initalizeData();

        // Initialize all of the controls and layouts
        initializeComponents();

        setBackgroundColors(startUp);

        // listener event triggers when shell set to not be visible
        shell.addListener(SWT.Hide, eventListener);
        shell.addListener(SWT.Dispose, eventListener);
    }

    @Override
    public void widgetDisposed(DisposeEvent e) {
        while (statMsgArray.size() != 0) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        bgColor.dispose();

        initialized = false;
        labelFont.dispose();
    }

    /**
     * Open method used to display the dialog.
     * 
     */
    private void open() {

        setInitialDialogLocation();

        initialized = true;

        showHideLog();

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    /**
     * Initialize font data
     */
    private void initalizeData() {
        FontData fontData = display.getSystemFont().getFontData()[0];
        labelFont = new Font(shell.getDisplay(), fontData.getName(),
                (int) (fontData.getHeight() * 1.4), SWT.BOLD);

        /*
         * compute preferred height to display entire message.
         */
        int screenWidth = display.getPrimaryMonitor().getBounds().width;

        /*
         * compute width of controls as the lesser of WIDTH_IN_CHARS or
         * PERCENT_OF_SCREEN_WIDTH
         */
        GC gc = new GC(display);
        gc.setFont(display.getSystemFont());
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();

        controlWidth = Math.min(charWidth * WIDTH_IN_CHARS,
                (int) (screenWidth * PERCENT_OF_SCREEN_WIDTH));
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
        moveLabel = new Label(shell, SWT.CENTER);
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
        topLabelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 35;
        topLabelComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        topLabelComp.setLayoutData(gd);
        topLabelComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        sourceLbl = new Label(topLabelComp, SWT.BORDER);
        sourceLbl.setText(sourceStr + statMsgArray.get(0).getSourceKey());
        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        sourceLbl.setLayoutData(gd);

        priorityLbl = new Label(topLabelComp, SWT.BORDER);
        priorityLbl.setText(priorityStr
                + statMsgArray.get(0).getPriority().ordinal());
        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        priorityLbl.setLayoutData(gd);

        categoryLbl = new Label(topLabelComp, SWT.BORDER);
        categoryLbl.setText(categoryStr + statMsgArray.get(0).getCategory());
        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        categoryLbl.setLayoutData(gd);
    }

    /**
     * Create the message text control.
     */
    private void createMessageControl() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        messageComp = new Composite(shell, SWT.NONE);
        messageComp.setLayout(new GridLayout(1, false));
        messageComp.setLayoutData(gd);
        messageComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        messageTF = new Text(messageComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        messageTF.setEditable(false);
        messageTF.setText(statMsgArray.get(0).getMessage());

        int preferredHeight = messageTF.computeSize(controlWidth, SWT.DEFAULT).y;

        /*
         * compute size to display only max initial lines
         */
        int height = messageTF.getLineHeight() * MAX_INITIAL_LINES;
        Rectangle initialSize = messageTF.computeTrim(0, 0, controlWidth,
                height);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = initialSize.width;

        gd.heightHint = Math.min(preferredHeight, initialSize.height);
        gd.minimumHeight = gd.heightHint;
        messageTF.setLayoutData(gd);
    }

    /**
     * Create the action buttons for the dialog.
     */
    private void createActionButtons() {
        actionComp = new Composite(shell, SWT.NONE);
        actionComp.setLayout(new GridLayout(7, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionComp.setLayoutData(gd);
        actionComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        timeLbl = new Label(actionComp, SWT.BORDER);
        timeLbl.setText(dateFormat.format(statMsgArray.get(0).getEventTime()));
        gd = new GridData(200, SWT.DEFAULT);
        timeLbl.setLayoutData(gd);

        fillerLbl = new Label(actionComp, SWT.NONE);
        fillerLbl.setText("");
        fillerLbl.setBackground(display.getSystemColor(SWT.COLOR_RED));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fillerLbl.setLayoutData(gd);

        hideShowLogBtn = new Button(actionComp, SWT.PUSH);
        hideShowLogBtn.setLayoutData(new GridData());
        hideShowLogBtn.setText("Hide Log");
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
                acknowledgeAllMessages();
            }
        });

        hideDialogBtn = new Button(actionComp, SWT.PUSH);
        hideDialogBtn.setText("Close");
        hideDialogBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.setVisible(false);
            }
        });
    }

    /**
     * Create the message log list control.
     */
    private void createMessageLogControl() {
        logComp = new Composite(shell, SWT.NONE);
        logComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        logComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label dblClickLBl = new Label(logComp, SWT.BORDER | SWT.CENTER);
        dblClickLBl
                .setText("Double-click message to display more information:");
        dblClickLBl.setFont(labelFont);
        dblClickLBl.setForeground(display.getSystemColor(SWT.COLOR_DARK_BLUE));
        dblClickLBl.setLayoutData(gd);

        msgLogList = new List(logComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Rectangle size = msgLogList.computeTrim(0, 0, controlWidth,
                msgLogList.getItemHeight() * NUM_LIST_ITEMS);
        gd.widthHint = size.width;
        gd.heightHint = size.height;
        gd.heightHint = (msgLogList.getItemHeight() * 10)
                + (msgLogList.getBorderWidth() * 2);
        msgLogList.setLayoutData(gd);
        msgLogList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                showSelectedListData();
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int delta = 0;
                if (detailsComp.isVisible()) {
                    ((GridData) detailsComp.getLayoutData()).exclude = true;
                    detailsComp.setVisible(false);
                    delta -= detailsComp.getSize().y;
                } else {
                    int idx = msgLogList.getSelectionIndex();
                    if (idx < 0) {
                        return;
                    }
                    StatusMessage sm = statMsgArray.get(idx);
                    detailsComp.displayDetails(sm);
                    ((GridData) detailsComp.getLayoutData()).exclude = false;
                    detailsComp.setVisible(true);
                    delta += detailsComp.getSize().y;
                }

                adjustHeight(delta);
            }
        });

        detailsComp = new SimpleDetailsComp(shell, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        detailsComp.setLayoutData(gd);

        msgLogList.add(getFormattedMessage(statMsgArray.get(0)));
        msgLogList.select(0);

    }

    /**
     * Show/Hide the message log list.
     */
    private void showHideLog() {
        int delta = 0;
        if (expanded == true) {
            // Show the message labels
            ((GridData) topLabelComp.getLayoutData()).exclude = false;
            topLabelComp.setVisible(true);
            delta += topLabelComp.getSize().y;

            // Show the Acknowledge Selected button
            ((GridData) ackSelectedBtn.getLayoutData()).exclude = false;
            ackSelectedBtn.setVisible(true);
            hideShowLogBtn.setText("Hide Log");

            // Show the message log
            ((GridData) logComp.getLayoutData()).exclude = false;
            logComp.setVisible(true);
            delta += logComp.getSize().y;
        } else {
            // Hide the message labels
            ((GridData) topLabelComp.getLayoutData()).exclude = true;
            topLabelComp.setVisible(false);
            delta -= topLabelComp.getSize().y;

            // Hide the Acknowledge Selected button
            ((GridData) ackSelectedBtn.getLayoutData()).exclude = true;
            ackSelectedBtn.setVisible(false);
            hideShowLogBtn.setText("Show Log");

            // Hide the message log
            if (this.first || detailsComp.isVisible()) {
                ((GridData) detailsComp.getLayoutData()).exclude = true;
                detailsComp.setVisible(false);
                delta -= detailsComp.getSize().y;
                this.first = false;
            }

            ((GridData) logComp.getLayoutData()).exclude = true;
            logComp.setVisible(false);
            delta -= logComp.getSize().y;
        }
        actionComp.layout();

        adjustHeight(delta);
    }

    private void adjustHeight(int delta) {
        Point minSize = shell.getMinimumSize();
        minSize.y += delta;
        shell.setMinimumSize(minSize);

        Point size = shell.getSize();
        size.x += SWT_BUG_FACTOR;
        size.y += delta + SWT_BUG_FACTOR;
        shell.setSize(size);
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
        int desiredHeight = Math.min(MAX_INITIAL_LINES,
                messageTF.getLineCount())
                * messageTF.getLineHeight();
        Rectangle clientArea = messageTF.getClientArea();
        if (clientArea.height < desiredHeight) {
            Rectangle trim = messageTF.computeTrim(clientArea.x, clientArea.y,
                    clientArea.width, desiredHeight);
            Point oldSize = messageTF.getSize();
            messageTF.setSize(oldSize.x, trim.height);
            adjustHeight(trim.height - oldSize.y);
        }

        detailsComp.displayDetails(sm);
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
        if ((currentIndex < 0) || !expanded) {
            if (msgLogList.getItemCount() > 0) {
                msgLogList.select(0);
                showSelectedListData();
            }
            return;
        }

        ++currentIndex;

        // Check if the current index will go outside the array
        // (it should never do this but check anyway...)
        if ((currentIndex > 0) && (currentIndex == statMsgArray.size())) {
            msgLogList.select(0);
            showSelectedListData();
            return;
        }

        msgLogList.select(currentIndex);
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
        logComp.setBackground(bgColor);
        detailsComp.setBackground(bgColor);
    }

    /**
     * Acknowledge all of the messages.
     */
    public void acknowledgeAllMessages() {
        int index = msgLogList.getSelectionIndex();

        int result = SWT.CANCEL;
        if (index == -1) {
            return;
        }

        boolean expandedForPrompt = false;
        if (!expanded) {
            expandedForPrompt = expanded = true;
            showHideLog();
        }

        ConfirmationDlg cd = new ConfirmationDlg(shell,
                "Are you sure you want to acknowledge all popup messages?",
                SWT.ICON_QUESTION);

        if ((msgLogList != null) && !msgLogList.isDisposed()) {
            Rectangle logBounds = msgLogList.getBounds();
            Point logDisplayOrigin = msgLogList.toDisplay(0, 0);
            logBounds.x = logDisplayOrigin.x;
            logBounds.y = logDisplayOrigin.y;
            cd.setAvoidedArea(logBounds);
        }

        result = cd.open();

        if (result != SWT.YES) {
            if ((result == SWT.CANCEL) && expandedForPrompt) {
                expanded = false;
                showHideLog();
            }
            return;
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
            Point minSize = shell.computeSize(SWT.DEFAULT, SWT.DEFAULT);
            shell.setMinimumSize(minSize);

            int screenHeight = display.getBounds().height;
            int screenWidth = display.getPrimaryMonitor().getBounds().width;

            Point size = minSize;
            shell.setSize(size);

            dialogXY = new Point((screenWidth - size.x) / 2,
                    (screenHeight - size.y) / 4);
            shell.setLocation(dialogXY);
        } else {
            shell.setLocation(dialogXY);
        }
    }

    /**
     * Shows the dialog.
     * 
     * @param show
     *            True to show dialog, false to hide.
     */
    public void showDialog() {
        if (initialized) {
            shell.setLocation(dialogXY);
            shell.setVisible(true);

            showSelectedListData();
            msgLogList.showSelection();
        } else {
            open();
        }
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
        return ((shell != null) && !shell.isDisposed() && shell.isVisible());
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
        if ((origin != null) && (moveDialog == true)) {
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
