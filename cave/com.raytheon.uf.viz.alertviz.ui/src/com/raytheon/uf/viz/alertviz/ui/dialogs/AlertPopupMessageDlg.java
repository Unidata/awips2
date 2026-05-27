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
import java.text.SimpleDateFormat;
import java.util.LinkedList;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.ui.AlertVisualization;

/**
 * This class displays the pop-up message dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Apr 02, 2009           lvenable  TTR fixes.
 * Nov 18, 2010  2235     cjeanbap  Changed button, hideDialogBtn, text and
 *                                  store previous location.
 * Jan 13, 2011  7375     cjeanbap  Commented out shell.setVisible(...) in
 *                                  acknowledgeLastMessage().
 * Apr 20, 2015  4311     lvenable  Fixed text field to accept really long text
 *                                  strings.
 * Jun 29, 2015  4311     randerso  Reworking AlertViz dialogs to be resizable.
 * Jan 25, 2016  5054     randerso  Converted to stand alone window
 * Feb 08, 2016  5312     randerso  Deleted isDisposed() added isOpen()
 * Feb 14, 2017  6029     randerso  Ensure popup does not appear on top of
 *                                  panels, Made popup appear on monitor with
 *                                  AlertViz bar Ensure popup cannot be dragged
 *                                  on top of panels.
 * Feb 28, 2017  6066     randerso  Move dialog up, if necessary, when expanded
 * Apr 03, 2018  6646     randerso  Fixed order of source/category. Changed time
 *                                  stamp to 24 hour time like the rest of
 *                                  AWIPS. Added sashForm to allow individual
 *                                  resizing of the log and details areas.
 * Sep 11, 2018  7456     randerso  Changed to not update the top pane when
 *                                  message selected in the log pane. Code
 *                                  cleanup.
 * Nov 13, 2018  7512     randerso  Added support for custom images
 * Dec 06, 2019  7597     randerso  Removed fake title bar now that ON_TOP
 *                                  windows have proper tielte bars.
 * Jul 23, 2020  8197     randerso  Removed unnecessary HideListener code
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class AlertPopupMessageDlg implements DisposeListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVisualization.class, "AV_ADMIN", "AV_ADMIN");

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
     * Source string.
     */
    private static final String sourceStr = " Source: ";

    /**
     * Category string.
     */
    private static final String categoryStr = " Category: ";

    /**
     * Priority string.
     */
    private static final String priorityStr = " Priority: ";

    /**
     * Maximum messages to acknowledge.
     */
    private static final int maxMessages = 100;

    private static final String DEFAULT_IMAGE = "AlertViz.png";

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The parent shell
     */
    private Shell parent;

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
    private final SimpleDateFormat dateFormat;

    /**
     * Time format.
     */
    private final SimpleDateFormat timeFormat;

    /**
     * Array of status messages.
     */
    private final java.util.List<Pair<StatusMessage, AlertMetadata>> statMsgArray;

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
     * Initialized flag indicating if the control have been initialized.
     */
    private boolean initialized = false;

    private boolean first;

    private int controlWidth;

    private Point minSize;

    private Point savedLoc = null;

    private SashForm sashForm;

    private Image image;

    private String imageName;

    private Label imageLabel;

    /**
     * Constructor.
     *
     * @param parent
     *            the shell of the AlertMessageDlg
     * @param expanded
     *            Expanded flag.
     */
    public AlertPopupMessageDlg(Shell parent, boolean expanded) {

        this.parent = parent;
        this.display = parent.getDisplay();
        this.expanded = expanded;
        this.first = true;
        statMsgArray = new LinkedList<>();

        String localTZ = System.getenv("FXA_LOCAL_TZ");
        if (localTZ == null) {
            localTZ = "GMT";
        }

        dateFormat = new SimpleDateFormat("MMM dd yy HH:mm:ss z");
        dateFormat.setTimeZone(TimeZone.getTimeZone(localTZ));

        timeFormat = new SimpleDateFormat("HH:mm:ss");
        timeFormat.setTimeZone(TimeZone.getTimeZone(localTZ));

        initShell();
    }

    /**
     * Initialize the shell
     */
    private void initShell() {
        shell = new Shell(display,
                SWT.BORDER | SWT.TITLE | SWT.ON_TOP | SWT.RESIZE);
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
    }

    @Override
    public void widgetDisposed(DisposeEvent e) {
        while (!statMsgArray.isEmpty()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
        bgColor.dispose();

        initialized = false;
        labelFont.dispose();
        image.dispose();
    }

    /**
     * Open method used to display the dialog.
     *
     */
    private void open() {
        minSize = shell.getSize();
        shell.setMinimumSize(minSize);
        shell.setSize(minSize);

        setDialogLocation();

        initialized = true;

        showHideLog();

        updateTopData();
        shell.open();
    }

    /**
     * Initialize data
     */
    private void initalizeData() {
        FontData fontData = display.getSystemFont().getFontData()[0];
        labelFont = new Font(shell.getDisplay(), fontData.getName(),
                (int) (fontData.getHeight() * 1.4), SWT.BOLD);

        /*
         * compute preferred height to display entire message.
         */
        int screenWidth = parent.getMonitor().getClientArea().width;

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

    private void loadImage(String imageName) {
        if (imageName == null) {
            imageName = DEFAULT_IMAGE;
        }

        if (!imageName.equals(this.imageName)) {
            IPathManager pm = PathManagerFactory.getPathManager();
            File file = pm.getStaticFile(
                    LocalizationUtil.join("alertViz", "images", imageName));
            if (file == null || !file.exists()) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to locate AlertViz image: " + imageName);
                imageName = DEFAULT_IMAGE;
            }

            if (!imageName.equals(this.imageName)) {
                if (image != null) {
                    image.dispose();
                    this.imageName = imageName;
                }

                image = new Image(shell.getDisplay(), file.getAbsolutePath());
                imageLabel.setImage(image);
                imageLabel.getParent().layout();
            }
        }
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createTopLabels();

        sashForm = new SashForm(shell, SWT.VERTICAL);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayoutData(gd);

        createMessageControl();

        createActionButtons();

        createMessageLogControl();

        int messageHeight = messageComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        int logHeight = logComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        int detailsHeight = detailsComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        sashForm.setWeights(
                new int[] { messageHeight, logHeight, detailsHeight });

        shell.pack();
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
        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        sourceLbl.setLayoutData(gd);

        priorityLbl = new Label(topLabelComp, SWT.BORDER);
        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        priorityLbl.setLayoutData(gd);

        categoryLbl = new Label(topLabelComp, SWT.BORDER);
        gd = new GridData(200, SWT.DEFAULT);
        gd.verticalIndent = 10;
        categoryLbl.setLayoutData(gd);
    }

    /**
     * Create the message text control.
     */
    private void createMessageControl() {
        Composite comp = new Composite(sashForm, SWT.NONE);
        GridLayout layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.marginLeft = 5;
        comp.setLayout(layout);

        imageLabel = new Label(comp, SWT.NONE);
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        imageLabel.setLayoutData(gd);
        loadImage(DEFAULT_IMAGE);

        messageComp = new Composite(comp, SWT.NONE);
        messageComp.setLayout(new GridLayout(1, false));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        messageComp.setLayoutData(gd);

        messageTF = new Text(messageComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        messageTF.setLayoutData(gd);
    }

    /**
     * Create the action buttons for the dialog.
     */
    private void createActionButtons() {
        actionComp = new Composite(messageComp, SWT.NONE);
        actionComp.setLayout(new GridLayout(7, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionComp.setLayoutData(gd);
        actionComp.setBackground(display.getSystemColor(SWT.COLOR_RED));

        timeLbl = new Label(actionComp, SWT.BORDER);
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
                int index = msgLogList.getSelectionIndex();
                if (index >= 0) {
                    acknowledgeMessage(index);
                }
            }
        });

        Button ackLastBtn = new Button(actionComp, SWT.PUSH);
        ackLastBtn.setText("Acknowledge Last");
        ackLastBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (msgLogList.getItemCount() > 0) {
                    acknowledgeMessage(0);
                }
            }
        });

        Button ackAllBtn = new Button(actionComp, SWT.PUSH);
        ackAllBtn.setText("Acknowledge All");
        ackAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                acknowledgeAllMessages();
            }
        });

        Button hideDialogBtn = new Button(actionComp, SWT.PUSH);
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
        logComp = new Composite(sashForm, SWT.NONE);
        logComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        logComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label dblClickLBl = new Label(logComp, SWT.BORDER | SWT.CENTER);
        dblClickLBl
                .setText("Double-click message to display more information:");
        dblClickLBl.setFont(labelFont);
        dblClickLBl.setForeground(display.getSystemColor(SWT.COLOR_DARK_BLUE));
        dblClickLBl.setLayoutData(gd);

        msgLogList = new List(logComp,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
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
                    detailsComp.setVisible(false);
                    delta -= detailsComp.getSize().y;
                } else {
                    int idx = msgLogList.getSelectionIndex();
                    if (idx < 0) {
                        return;
                    }
                    StatusMessage sm = statMsgArray.get(idx).getFirst();
                    detailsComp.displayDetails(sm);
                    detailsComp.setVisible(true);
                    delta += detailsComp.getSize().y;
                }

                adjustHeight(delta);
            }
        });

        detailsComp = new SimpleDetailsComp(sashForm, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        detailsComp.setLayoutData(gd);
    }

    /**
     * Show/Hide the message log list.
     */
    private void showHideLog() {
        int delta = 0;
        if (expanded) {
            // Show the message labels
            ((GridData) topLabelComp.getLayoutData()).exclude = false;
            topLabelComp.setVisible(true);
            delta += topLabelComp.getSize().y;

            // Show the Acknowledge Selected button
            ((GridData) ackSelectedBtn.getLayoutData()).exclude = false;
            ackSelectedBtn.setVisible(true);
            hideShowLogBtn.setText("Hide Log");

            // Show the message log
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
                detailsComp.setVisible(false);
                delta -= detailsComp.getSize().y;
                this.first = false;
            }

            logComp.setVisible(false);
            delta -= logComp.getSize().y;
        }
        actionComp.layout();

        adjustHeight(delta);
    }

    private void adjustHeight(int delta) {

        int[] weights = new int[3];
        weights[0] = messageComp.getSize().y;
        weights[1] = logComp.getSize().y;
        weights[2] = detailsComp.getSize().y;

        if (!logComp.getVisible()) {
            weights[1] = 0;
        }

        if (!detailsComp.getVisible()) {
            weights[2] = 0;
        }

        Point size = sashForm.getSize();
        size.y += delta;
        sashForm.setSize(size);
        sashForm.setWeights(weights);

        Point minSize = shell.getMinimumSize();
        minSize.y += delta;
        shell.setMinimumSize(minSize);

        size = shell.getSize();
        size.x += SWT_BUG_FACTOR;
        size.y += delta + SWT_BUG_FACTOR;
        shell.setSize(size);

        if (delta < 0 && savedLoc != null) {
            shell.setLocation(savedLoc);
            savedLoc = null;
        }

        Point prevLoc = shell.getLocation();
        Rectangle dialogBounds = shell.getBounds();
        Rectangle clientArea = shell.getMonitor().getClientArea();
        if (dialogBounds.y + dialogBounds.height > clientArea.y
                + clientArea.height) {
            dialogBounds.y = clientArea.y + clientArea.height
                    - dialogBounds.height;
        }

        shell.setLocation(dialogBounds.x, dialogBounds.y);
        if (!prevLoc.equals(shell.getLocation())) {
            savedLoc = prevLoc;
        }
    }

    /**
     * Show the data associated with the selected message in the message list
     * control.
     */
    private void showSelectedListData() {

        StatusMessage sm = null;
        int index = msgLogList.getSelectionIndex();
        if (index >= 0) {
            sm = statMsgArray.get(index).getFirst();
        }

        detailsComp.displayDetails(sm);
    }

    /**
     * Add a new status message.
     *
     * @param sm
     *            Status message.
     * @param bgColor
     *            Background color
     */
    public void addNewMessage(StatusMessage sm, AlertMetadata amd) {

        if (statMsgArray.size() >= maxMessages) {
            statMsgArray.remove(statMsgArray.size() - 1);
        }

        statMsgArray.add(0, new Pair<>(sm, amd));
        updateTopData();

        if (msgLogList.isDisposed()) {
            return;
        }

        int currentIndex = msgLogList.getSelectionIndex();
        msgLogList.removeAll();

        for (Pair<StatusMessage, AlertMetadata> p : statMsgArray) {
            StatusMessage message = p.getFirst();
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

    private void updateTopData() {
        Pair<StatusMessage, AlertMetadata> p = statMsgArray.get(0);

        setBackgroundColors(p.getSecond().getBackground());
        loadImage(p.getSecond().getImage());

        StatusMessage sm = p.getFirst();
        categoryLbl.setText(categoryStr + sm.getCategory());
        priorityLbl.setText(priorityStr + sm.getPriority().ordinal());
        sourceLbl.setText(sourceStr + sm.getSourceKey());
        timeLbl.setText(dateFormat.format(sm.getEventTime()));

        messageTF.setText(sm.getMessage());
        int preferredHeight = messageTF.computeSize(controlWidth,
                SWT.DEFAULT).y;
        int height = Math.min(MAX_INITIAL_LINES * messageTF.getLineHeight(),
                preferredHeight);
        Rectangle clientArea = messageTF.getClientArea();
        if (clientArea.height < height) {
            Rectangle trim = messageTF.computeTrim(clientArea.x, clientArea.y,
                    clientArea.width, height);
            Point oldSize = messageTF.getSize();
            messageTF.setSize(oldSize.x, trim.height);
            adjustHeight(trim.height - oldSize.y);
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

        strBld.append(timeFormat.format(sm.getEventTime())).append(" ");
        strBld.append("(").append(sm.getPriority().ordinal()).append(") | ");
        strBld.append(sm.getSourceKey()).append(" | ");
        strBld.append(sm.getCategory()).append(" : ");
        strBld.append(sm.getMessage());

        return strBld.toString();
    }

    /**
     * Acknowledge the selected message.
     */
    private void acknowledgeMessage(int index) {
        StatusMessage sm = statMsgArray.get(index).getFirst();

        try {
            SystemStatusHandler.acknowledge(sm,
                    System.getProperty("user.name"));
        } catch (Exception ex) {
            Container.logInternal(Priority.ERROR, "Acknowledge failed...", ex);
            return;
        }

        statMsgArray.remove(index);
        msgLogList.remove(index);

        if (statMsgArray.isEmpty()) {
            shell.close();
            return;
        }

        if (index >= msgLogList.getItemCount()) {
            index = msgLogList.getItemCount() - 1;
        }

        msgLogList.select(index);
        showSelectedListData();
        updateTopData();
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
        imageLabel.setBackground(bgColor);
        imageLabel.getParent().setBackground(bgColor);
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

        for (Pair<StatusMessage, AlertMetadata> p : statMsgArray) {
            StatusMessage message = p.getFirst();
            try {
                SystemStatusHandler.acknowledge(message, userName);
            } catch (Exception ex) {
                Container.logInternal(Priority.ERROR,
                        "AlertPopupMessaeDlg: exception acknowledging message.",
                        ex);
            }
        }

        statMsgArray.clear();
        shell.close();
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
    private void setDialogLocation() {

        Rectangle clientArea = parent.getMonitor().getClientArea();

        int x = clientArea.x + Math.max(0, (clientArea.width - minSize.x) / 2);
        int y = clientArea.y + Math.max(0, (clientArea.height - minSize.y) / 4);
        shell.setLocation(x, y);
        savedLoc = null;
    }

    /**
     * Shows the dialog.
     *
     * @param show
     *            True to show dialog, false to hide.
     */
    public void showDialog() {
        if (initialized) {
            if (!shell.isVisible()) {
                shell.setVisible(true);
                setDialogLocation();
            }

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
     * Returns true if dialog is open
     *
     * @return True if open, false if not.
     */
    public boolean isOpen() {
        return (shell != null) && !shell.isDisposed() && shell.isVisible();
    }

    /**
     * Add a listener to be notified when this dialog is disposed
     *
     * @param listener
     */
    public void addDisposeListener(DisposeListener listener) {
        shell.addDisposeListener(listener);
    }
}
