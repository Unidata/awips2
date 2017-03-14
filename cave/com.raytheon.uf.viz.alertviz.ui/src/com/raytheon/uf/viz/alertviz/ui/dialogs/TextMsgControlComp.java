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
import java.util.TimeZone;
import java.util.Vector;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertvizException;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.ui.audio.IAudioAction;
import com.raytheon.uf.viz.alertviz.ui.timer.AlertTimer;
import com.raytheon.uf.viz.alertviz.ui.timer.ITimerAction;

/**
 * This class displays text control that displays an incoming message.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 02 Apr 2009             lvenable    TTR fixes.
 * 17 Nov 2010  5150       cjeanbap    Added MonitorToolTip class.
 * 01 Dec 2010  5632	   cjeanbap	   Added updated Categories list.
 * 01 Dec 2010  6500       cjeanbap    Changed order of Category and Source.
 * 14 Dec 2010  5149       cjeanbap    Removed Audio play functionality.
 * 12 Jan 2011  7375       cjeanbap    Removed unused private member variable.
 * 01 Mar 2011  5632       cjeanbap    Added Category Sort functionality.
 * 09 Mar 2011  8058       rferrel     Added check for audio alert associated
 *                                     with this component.
 * 14 Mar 2011  7679	   cjeanbap	   On the heels conditions.
 * 31 May 2011  8058       cjeanbap    Kill sound based on TextMsgBox id.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TextMsgControlComp extends Composite implements ITimerAction {
    /**
     * Clear button.
     */
    private Button clearBtn;

    /**
     * Image that is used to show dlg up
     */
    private Image dlgUp;

    /**
     * Image that is used to show dlg down
     */
    private Image dlgDown;

    /**
     * Message text control.
     */
    private Text msgTF;

    /**
     * Log button.
     */
    private Button logBtn;

    /**
     * Alert timer.
     */
    private AlertTimer alertTimer;

    /**
     * Foreground color.
     */
    private Color fgColor;

    /**
     * Background color.
     */
    private Color bgColor;

    /**
     * Normal colors flag.
     */
    private boolean normalColors = true;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Shell of parent
     */
    private Shell parentShell;

    /**
     * Log dialog (TabItem) that is used for the TabControlDialog.
     */
    private TextMsgLog textMsgLog;

    /**
     * Audio callback.
     */
    private IAudioAction audioCB;

    /**
     * Time format.
     */
    private SimpleDateFormat timeFormat = new SimpleDateFormat("hh:mm a");

    /**
     * Message vector.
     */
    private Vector<StatusMessage> messageVec = new Vector<StatusMessage>();

    /**
     * Maximum log size.
     */
    private int maxLogSize = 30;

    /**
     * Array of categories.
     */
    private Category[] categories;

    /**
     * Boolean for tabControlDlg is opened or not
     */
    private boolean opened = false;

    /**
     * String that is name of index (H1, Q2, V3, etc)
     */
    private String indexName;

    /**
     * Index into tabControlDlg
     */
    private int index;

    /**
     * Message Time
     */
    private long messageTime;

    /**
     * Dialog that controls the tabs, all instances of TextMsgControlComp use
     * the same controller
     */
    private TabControlDlg tabControlDlg;

    private MonitorToolTip mtt;

    private boolean audioEnabled;

    private RGB prevBackgroundRGB;

    private RGB prevForegroundRGB;

    private int textMsgBoxId;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite.
     * @param audioCB
     *            Audio callback.
     * @param maxLogSize
     *            Maximum log size.
     * @param categories
     *            Array of categories.
     */
    public TextMsgControlComp(Composite parentComp, IAudioAction audioCB,
            int maxLogSize, Category[] categories, int textMsgBoxId) {
        super(parentComp, SWT.NONE);

        this.parentComp = parentComp;

        this.audioCB = audioCB;

        this.maxLogSize = maxLogSize;

        this.categories = categories;

        this.audioEnabled = false;

        this.textMsgBoxId = textMsgBoxId;

        /*
         * This gets us the shell that is the main alert viz bar, we use this so
         * we can correctly position the tabControlDlg
         */
        parentShell = parentComp.getParent().getShell();

        tabControlDlg = TabControlDlg.getInstance(parentShell);

        initComponents();
    }

    /**
     * Initialize the components.
     */
    private void initComponents() {
        alertTimer = new AlertTimer(this.getDisplay(), this, 500);

        fgColor = new Color(this.getDisplay(), 255, 0, 0);
        bgColor = new Color(this.getDisplay(), 0, 0, 0);
        controlFont = new Font(this.getDisplay(), "Monospace", 10, SWT.NORMAL);

        initLayout();

        createControls();

        createButtonImages();

        msgTF.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseHover(MouseEvent e) {
                if (msgTF != null
                        && (msgTF.getText() != null && !msgTF.getText().trim()
                                .equals(""))) {
                    msgTF.setData(MonitorToolTip.tooltipTextKey,
                            msgTF.getText());
                } else {
                    msgTF.setData(MonitorToolTip.tooltipTextKey,
                            "No NEW messages for " + index);
                }
                mtt.open();
            }
        });

        msgTF.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent e) {
                alertTimer.cancelTimer();
                if (isAudioEnabled()) {
                    audioCallbackAction();
                }
            }
        });

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                controlFont.dispose();
                alertTimer.cancelTimer();
                fgColor.dispose();
                bgColor.dispose();

                if (textMsgLog != null) {
                    textMsgLog.disposeDialog();
                }

                if (dlgUp != null) {
                    dlgUp.dispose();
                }

                if (dlgDown != null) {
                    dlgDown.dispose();
                }
            }
        });

        if (categories.length == 0) {
            clearBtn.setEnabled(false);
            msgTF.setEnabled(false);
            logBtn.setEnabled(false);
        } else {
            retrieveLogMessages();
        }

    }

    /**
     * Initialize the composite layout.
     */
    private void initLayout() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 3;
        this.setLayout(gl);
        this.setLayoutData(gd);
    }

    /**
     * Create the message controls.
     */
    private void createControls() {
        clearBtn = new Button(this, SWT.PUSH);
        clearBtn.setText("C");
        clearBtn.setToolTipText("Clear Message");
        clearBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearButtonAction();
            }
        });

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 250;
        msgTF = new Text(this, SWT.BORDER);
        msgTF.setToolTipText(null);
        msgTF.setLayoutData(gd);
        msgTF.setFont(controlFont);
        msgTF.setEditable(false);

        msgTF.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                audioCallbackAction();
                alertTimer.cancelTimer();
            }
        });

        mtt = new MonitorToolTip(msgTF);

        logBtn = new Button(this, SWT.UP);
        drawUp();
        logBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                logButtonAction();
            }

        });
    }

    /**
     * Action taken when the alert timer is fired.
     */
    @Override
    public void timerAction(boolean blinkText) {
        if (msgTF.isDisposed()) {
            return;
        }

        if (blinkText == true) {
            if (normalColors == true) {
                normalColors = false;

                msgTF.setForeground(fgColor);
                msgTF.setBackground(bgColor);
            } else {
                normalColors = true;

                msgTF.setForeground(bgColor);
                msgTF.setBackground(fgColor);
            }
        }
    }

    /**
     * Action taken when the alert timer is finished.
     */
    @Override
    public void timerCompleted() {
        if (msgTF.isDisposed()) {
            return;
        }

        msgTF.setForeground(fgColor);
        msgTF.setBackground(bgColor);
    }

    /**
     * Start the blink timer.
     * 
     * @param duration
     *            Blink duration.
     */
    private void startBlinkTimer(int duration, boolean blinkText) {
        alertTimer.startTimer(duration, blinkText);
    }

    /**
     * Stop the timer.
     */
    public void stopTimer() {
        alertTimer.cancelTimer();
    }

    /**
     * Audio callback action.
     */
    public void audioCallbackAction() {
        if (audioCB != null) {
            audioCB.cancelAudio(textMsgBoxId);
            setAudioEnabled(false);
        }
    }

    /**
     * Set the foreground/background colors.
     * 
     * @param fg
     *            Foreground RGB.
     * @param bg
     *            Background RGB.
     */
    private void setColors(RGB fg, RGB bg) {
        if (fgColor != null) {
            fgColor.dispose();
        }

        if (bgColor != null) {
            bgColor.dispose();
        }

        fgColor = new Color(this.getDisplay(), fg);
        bgColor = new Color(this.getDisplay(), bg);
    }

    /**
     * Log button action method.
     */
    private void logButtonAction() {
        // Clear the on-the-heels color indicator
        this.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        opened = !opened;
        if (tabControlDlg == null || tabControlDlg.isDisposed()) {
            tabControlDlg = TabControlDlg.getInstance(parentShell);
        }
        if (textMsgLog == null) {
            textMsgLog = new TextMsgLog(parentShell, getCategoryNames(), 0,
                    messageVec);
            textMsgLog.setIndex(index);
        }
        if (opened) {
            tabControlDlg.populateClearOptionsCombo(textMsgLog);
            TabItem item = textMsgLog.getTab(tabControlDlg.getTabFolder());
            tabControlDlg.addedTab(textMsgLog);
            item.setText("      " + indexName + "      ");
            item.addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    drawUp();
                    opened = false;
                }
            });
            drawDown();
            if (tabControlDlg.isOpened() == false) {
                tabControlDlg.open();
            }
        } else {
            tabControlDlg.removeTab(textMsgLog);
            drawUp();
        }
    }

    /**
     * Clear button action method.
     */
    private void clearButtonAction() {
        msgTF.setText("");
        msgTF.setToolTipText("");
        alertTimer.cancelTimer();

        if (isAudioEnabled()) {
            audioCallbackAction();
        }

        msgTF.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        msgTF.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        // Clear the on-the-heels color indicator
        this.setBackground(parentComp.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
    }

    /**
     * Handle the incoming messages.
     * 
     * @param statMsg
     *            Status message.
     * @param amd
     *            Alert metadata.
     * @param gConfig
     *            Global configuration.
     */
    public void messageHandler(StatusMessage statMsg, AlertMetadata amd,
            TrayConfiguration gConfig) {
        synchronized (messageVec) {
            if (messageVec.size() >= maxLogSize) {
                while (messageVec.size() >= maxLogSize) {
                    messageVec.remove(0);
                    if (textMsgLog != null) {
                        textMsgLog.removeFirst();
                    }
                }
            }

            if (textMsgLog != null) {
                textMsgLog.addMessage(statMsg);
                textMsgLog.updateClearOptionsCombo(statMsg.getCategory());
            } else {
                messageVec.add(statMsg);
            }
        }

        if (msgTF.isDisposed()) {
            return;
        }

        long timeBetweenMessages = System.currentTimeMillis() - messageTime;
        messageTime = System.currentTimeMillis();
        msgTF.setText(getFormattedMessage(statMsg, gConfig));
        msgTF.setToolTipText(null);
        setColors(amd.getForeground(), amd.getBackground());
        msgTF.setForeground(fgColor);
        msgTF.setBackground(bgColor);

        prevBackgroundRGB = amd.getBackground();
        prevForegroundRGB = amd.getForeground();

        // Check if the blink timer is running.
        if (alertTimer.timerIsRunning() == true || timeBetweenMessages <= 1000) {
            this.setBackground(parentComp.getDisplay().getSystemColor(
                    SWT.COLOR_GREEN));
        } else {
            this.setBackground(null);
        }

        startBlinkTimer(gConfig.getBlinkDuration(), amd.isBlink());
    }

    /**
     * Get the status message in a formatted string.
     * 
     * @param sm
     *            Status message.
     * @param gConfig
     *            Global configuration.
     * @return The status message as a formatted string.
     */
    private String getFormattedMessage(StatusMessage sm,
            TrayConfiguration gConfig) {
        StringBuilder strBld = new StringBuilder();
        String localTZ = System.getenv("FXA_LOCAL_TZ");
        if (localTZ == null) {
            localTZ = "GMT";
        }
        timeFormat.setTimeZone(TimeZone.getTimeZone(localTZ));

        strBld.append(timeFormat.format(sm.getEventTime())).append(" ");

        if (gConfig.isPriorityShown() == true) {
            strBld.append("(").append(sm.getPriority().ordinal())
                    .append(") | ");
        }

        if (gConfig.isSourceKeyShown() == true) {
            strBld.append(sm.getSourceKey()).append("| ");
        }

        if (gConfig.isCategoryShown() == true) {
            strBld.append(sm.getCategory()).append(" : ");
        }

        strBld.append(sm.getMessage());

        return strBld.toString();
    }

    public boolean isAudioEnabled() {
        return audioEnabled;
    }

    public void setAudioEnabled(boolean audioEnabled) {
        this.audioEnabled = audioEnabled;
    }

    /**
     * Get the log messages.
     */
    private void retrieveLogMessages() {
        try {
            StatusMessage[] smArray;
            smArray = SystemStatusHandler.retrieveByCategory(categories,
                    maxLogSize);

            if (smArray == null) {
                return;
            }

            int start = 0;
            int end = smArray.length;
            if (end > maxLogSize) {
                start = end - maxLogSize;
            }
            for (int i = start; i < end; i++) {
                messageVec.add(smArray[i]);
            }

        } catch (AlertvizException e) {
            Container
                    .logInternal(
                            Priority.ERROR,
                            "TextMsgControlComp: exception getting StatusMessages by Category.",
                            e);
        }
    }

    /**
     * Get the category names in a string array.
     * 
     * @return String array of category names.
     */
    private String[] getCategoryNames() {
        String[] catNames = new String[categories.length];

        for (int i = 0; i < catNames.length; i++) {
            catNames[i] = categories[i].getCategoryName();
        }

        return catNames;
    }

    /**
     * Create the buttons for tabControlDlg up (visible) and down (hidden).
     */
    private void createButtonImages() {
        int width = 11, height = 12;
        Display display = this.parentComp.getDisplay();
        dlgUp = new Image(display, width, height);
        GC gc = new GC(dlgUp);

        gc.setAntialias(SWT.ON);

        gc.drawRectangle(0, 0, width, height);

        gc.setForeground(display.getSystemColor(SWT.TITLE));

        gc.drawLine(1, 1, 10, 1);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.drawLine(2, 3, 8, 3);

        gc.drawLine(2, 5, 8, 5);

        gc.drawLine(2, 7, 6, 7);

        logBtn.setImage(dlgUp);

        gc.dispose();

        dlgDown = new Image(display, width, height);
        gc = new GC(dlgDown);

        gc.setAntialias(SWT.ON);

        gc.drawRectangle(0, 0, width, height);

        gc.setForeground(display.getSystemColor(SWT.TITLE));

        gc.drawLine(1, 1, 10, 1);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        gc.drawLine(2, 3, 8, 3);

        gc.drawLine(2, 5, 8, 5);

        gc.drawLine(2, 7, 6, 7);

        gc.setLineWidth(1);

        gc.setForeground(display.getSystemColor(SWT.COLOR_RED));

        gc.drawLine(0, 0, width, height);

        gc.drawLine(0, height, width, 0);

        gc.dispose();
    }

    /**
     * Set logBtn to up.
     */
    private void drawUp() {
        logBtn.setImage(dlgUp);
        logBtn.setToolTipText("Show logs...");
    }

    /**
     * Set logBtn to down.
     */
    private void drawDown() {
        logBtn.setImage(dlgDown);
        logBtn.setToolTipText("Hide logs...");
    }

    /**
     * Set the name of the index (H1, H2, V1, V2, etc...)
     * 
     * @param prefix
     *            (H, V, or Q)
     * @param index
     *            (1-4)
     */
    public void setIndexName(String prefix, int index) {
        this.indexName = prefix + index;
        this.index = index - 1;
    }

    public String getMessageText() {
        return msgTF.getText();
    }

    public void setMessageText(String value) {
        msgTF.setText(value);
        msgTF.setData(MonitorToolTip.tooltipTextKey, value);
    }

    public void setMessageTextBackAndForeground(RGB bg, RGB fg) {
        setColors(fg, bg);
        msgTF.setBackground(bgColor);
        msgTF.setForeground(fgColor);
        prevBackgroundRGB = bg;
        prevForegroundRGB = fg;
    }

    public RGB getBackgroundRGB() {
        return prevBackgroundRGB;
    }

    public RGB getForegroundRGB() {
        return prevForegroundRGB;
    }

    public int getMaxLogSize() {
        return maxLogSize;
    }

    public void setMaxLogSize(int maxLogSize) {
        synchronized (messageVec) {
            if (messageVec.size() > maxLogSize) {
                while (messageVec.size() > maxLogSize) {
                    messageVec.remove(0);
                    if (textMsgLog != null) {
                        textMsgLog.removeFirst();
                    }
                }
            }
            this.maxLogSize = maxLogSize;
        }
    }

}
