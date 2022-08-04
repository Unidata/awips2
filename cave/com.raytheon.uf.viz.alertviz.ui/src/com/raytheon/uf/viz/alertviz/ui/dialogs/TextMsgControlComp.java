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
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertvizException;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;
import com.raytheon.uf.viz.alertviz.ui.audio.AlertAudioMgr;
import com.raytheon.uf.viz.alertviz.ui.audio.IAlertSoundJob;
import com.raytheon.uf.viz.alertviz.ui.audio.IAlertSoundJob.IJobFinshedListener;
import com.raytheon.uf.viz.alertviz.ui.util.MessageFormatter;
import com.raytheon.uf.viz.core.VizApp;

/**
 * This class displays text control that displays an incoming message.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 05, 2008           lvenable  Initial creation.
 * Apr 02, 2009           lvenable  TTR fixes.
 * Nov 17, 2010  5150     cjeanbap  Added MonitorToolTip class.
 * Dec 01, 2010  5632     cjeanbap  Added updated Categories list.
 * Dec 01, 2010  6500     cjeanbap  Changed order of Category and Source.
 * Dec 14, 2010  5149     cjeanbap  Removed Audio play functionality.
 * Jan 12, 2011  7375     cjeanbap  Removed unused private member variable.
 * Mar 01, 2011  5632     cjeanbap  Added Category Sort functionality.
 * Mar 09, 2011  8058     rferrel   Added check for audio alert associated with
 *                                  this component.
 * Mar 14, 2011  7679     cjeanbap  On the heels conditions.
 * May 31, 2011  8058     cjeanbap  Kill sound based on TextMsgBox id.
 * Apr 03, 2018  6646     randerso  Changed time stamp to 24 hour time like the
 *                                  rest of AWIPS.
 * Sep 20, 2018  7457     randerso  Added visual border around text box
 *                                  indicating audio is playing.
 * Sep 27, 2018  7454     randerso  Use a common message format for text display
 *                                  and logs. Code cleanup.
 * Sep 28, 2018  7455     randerso  Bring TabControlDlg to top if already open.
 * Oct 09, 2018  7457     randerso  Fix audio playback when text is not enabled.
 * Oct 09, 2018  7507     randerso  Fixed on-the-heels indication to better
 *                                  match A1
 * Dec 06, 2018  7513     randerso  Changes to properly enforce maxLogSize on a
 *                                  per category basis.
 * Dec 10, 2018  7659     randerso  Sort the categories list
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class TextMsgControlComp extends Canvas implements PaintListener {
    /**
     * Alert timer.
     */
    private static final Duration CONSECUTIVE_MESSAGES_THRESHOLD = Duration
            .ofSeconds(3);

    private static Timer blinkTimer;

    private static boolean blinkPhase;

    private static List<TextMsgControlComp> blinkList = new CopyOnWriteArrayList<>();

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
     * Foreground color.
     */
    private Color fgColor;

    /**
     * Background color.
     */
    private Color bgColor;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Parent composite.
     */
    private Composite parentComp;

    /**
     * Log dialog (TabItem) that is used for the TabControlDialog.
     */
    private TextMsgLog textMsgLog;

    /**
     * Array of categories.
     */
    private Category[] categories;

    private MessageFormatter msgFormat;

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
    private MonitorToolTip mtt;

    private IAlertSoundJob soundJob;

    private RGB prevBackgroundRGB;

    private RGB prevForegroundRGB;

    private long stopBlinkTime;

    /**
     * Constructor.
     *
     * @param parentComp
     *            Parent composite.
     * @param prefix
     *            (H, V, or Q)
     * @param index
     *            (1-4)
     * @param maxLogSize
     *            Maximum log size.
     * @param msgFormat
     *            the message formatter used to format the messages for display
     * @param categories
     *            Array of categories.
     */
    public TextMsgControlComp(Composite parentComp, char prefix, int index,
            int maxLogSize, MessageFormatter msgFormat, Category[] categories) {
        super(parentComp, SWT.NONE);

        this.parentComp = parentComp;

        this.categories = categories;
        this.msgFormat = msgFormat;
        this.indexName = Character.toString(prefix) + index;
        this.index = index;

        initComponents(maxLogSize);

    }

    @Override
    public void dispose() {
        stopBlinking();
        super.dispose();
    }

    /**
     * Initialize the components.
     */
    private void initComponents(int maxLogSize) {
        fgColor = new Color(this.getDisplay(), 255, 0, 0);
        bgColor = new Color(this.getDisplay(), 0, 0, 0);
        controlFont = new Font(this.getDisplay(), "Monospace", 10, SWT.NORMAL);

        initLayout();

        createControls(maxLogSize);

        createButtonImages();

        this.addPaintListener(this);

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                controlFont.dispose();
                fgColor.dispose();
                bgColor.dispose();

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
            retrieveLogMessages(maxLogSize);
        }

    }

    @Override
    public void paintControl(PaintEvent e) {
        if (soundJob != null) {
            Rectangle rect = msgTF.getBounds();
            rect.x -= 2;
            rect.y -= 2;
            rect.width += 3;
            rect.height += 3;

            e.gc.setForeground(msgTF.getForeground());
            e.gc.setLineWidth(3);
            e.gc.drawRectangle(rect);
        }
    }

    /**
     * Initialize the composite layout.
     */
    private void initLayout() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.marginHeight = 3;
        gl.marginWidth = 3;
        this.setLayout(gl);
        this.setLayoutData(gd);
    }

    /**
     * Create the message controls.
     */
    private void createControls(int maxLogSize) {
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

        msgTF.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseHover(MouseEvent e) {
                if (msgTF != null && (msgTF.getText() != null
                        && !msgTF.getText().trim().isEmpty())) {
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
            public void mouseDown(MouseEvent e) {
                if (soundJob != null) {
                    soundJob.kill();
                    soundJob = null;
                }
                stopBlinking();
            }
        });

        mtt = new MonitorToolTip(msgTF);

        logBtn = new Button(this, SWT.TOGGLE);
        drawUp();
        logBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                logButtonAction();
            }

        });

        textMsgLog = new TextMsgLog(index, getCategoryNames(), msgFormat,
                maxLogSize);
    }

    /**
     * Action taken when the blink timer is fired.
     */
    private void blink() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (msgTF.isDisposed()) {
                    return;
                }

                if (System.currentTimeMillis() > stopBlinkTime) {
                    stopBlinking();
                }

                if (isBlinking()) {
                    if (blinkPhase) {
                        msgTF.setForeground(fgColor);
                        msgTF.setBackground(bgColor);
                    } else {
                        msgTF.setForeground(bgColor);
                        msgTF.setBackground(fgColor);
                    }
                    redraw();
                }
            }
        });
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
        this.setBackground(null);

        opened = logBtn.getSelection();
        TabControlDlg tabControlDlg = TabControlDlg.getInstance(getShell());
        if (opened) {
            Item item = tabControlDlg.addTab("      " + indexName + "      ",
                    textMsgLog);
            item.addDisposeListener(new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    logBtn.setSelection(false);
                    drawUp();
                    opened = false;

                    // Clear the on-the-heels color indicator
                    TextMsgControlComp.this.setBackground(null);
                }
            });
            drawDown();
            tabControlDlg.open();
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
        stopBlinking();

        if (soundJob != null) {
            soundJob.kill();
            soundJob = null;
        }

        msgTF.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        msgTF.setBackground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        redraw();
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
        textMsgLog.addMessage(statMsg);

        if (msgTF.isDisposed()) {
            return;
        }

        long timeBetweenMessages = System.currentTimeMillis() - messageTime;
        messageTime = System.currentTimeMillis();
        msgTF.setText(msgFormat.getFormattedMessage(statMsg));
        msgTF.setToolTipText(null);
        setColors(amd.getForeground(), amd.getBackground());
        msgTF.setForeground(fgColor);
        msgTF.setBackground(bgColor);

        prevBackgroundRGB = amd.getBackground();
        prevForegroundRGB = amd.getForeground();

        // Check on-the-heels conditions
        // on-the-heels is true if the previous message is still blinking, audio
        // is still playing, or it's less than CONSECUTIVE_MESSAGES_THRESHOLD
        // since previous message was displayed.
        if (isBlinking() || soundJob != null
                || timeBetweenMessages <= CONSECUTIVE_MESSAGES_THRESHOLD
                        .toMillis()) {

            this.setBackground(
                    parentComp.getDisplay().getSystemColor(SWT.COLOR_GREEN));
        }

        File audioFile = AlertAudioMgr.getAudioFile(amd, statMsg);
        if (audioFile != null || amd.isAudioEnabled()) {
            // stop any current audio
            if (this.soundJob != null) {
                this.soundJob.kill();
            }

            Duration duration = Duration.ofSeconds(gConfig.getAudioDuration());
            this.soundJob = AlertAudioMgr.loopSound(audioFile, duration);
            this.soundJob.addFinishedListener(new IJobFinshedListener() {

                @Override
                public void audioJobFinished(IAlertSoundJob job) {
                    soundJob = null;
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            redraw();
                        }
                    });
                }
            });
        }

        if (amd.isBlink()) {
            startBlinking(Duration.ofSeconds(gConfig.getBlinkDuration()));
        }
        redraw();
    }

    private boolean isBlinking() {
        return blinkList.contains(this);
    }

    /**
     * Start blinking for a specified duration
     *
     * @param duration
     *            in seconds
     */
    private void startBlinking(Duration duration) {
        blinkList.add(this);
        stopBlinkTime = System.currentTimeMillis() + duration.toMillis();
    }

    private void stopBlinking() {
        blinkList.remove(this);

        if (!msgTF.isDisposed()) {
            msgTF.setForeground(fgColor);
            msgTF.setBackground(bgColor);
            redraw();
        }
    }

    /**
     * Get the log messages.
     */
    private void retrieveLogMessages(int maxLogSize) {
        try {
            List<StatusMessage> smList = new ArrayList<>();
            for (Category category : categories) {
                smList.addAll(SystemStatusHandler.retrieveByCategory(maxLogSize,
                        category));
            }

            Collections.sort(smList,
                    Comparator.comparing(StatusMessage::getEventTime));
            for (StatusMessage sm : smList) {
                textMsgLog.addMessage(sm);
            }

        } catch (AlertvizException e) {
            Container.logInternal(Priority.ERROR,
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

        Arrays.sort(catNames);

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
     * @return the message text
     */
    public String getMessageText() {
        return msgTF.getText();
    }

    /**
     * Set the message text
     *
     * @param text
     */
    public void setMessageText(String text) {
        msgTF.setText(text);
        msgTF.setData(MonitorToolTip.tooltipTextKey, text);
    }

    /**
     * Set the foreground and background colors of the message text
     *
     * @param bg
     *            background color
     * @param fg
     *            foreground color
     */
    public void setMessageTextBackAndForeground(RGB bg, RGB fg) {
        setColors(fg, bg);
        msgTF.setBackground(bgColor);
        msgTF.setForeground(fgColor);
        prevBackgroundRGB = bg;
        prevForegroundRGB = fg;
        redraw();
    }

    /**
     * @return the background color of the message text box
     */
    public RGB getBackgroundRGB() {
        return prevBackgroundRGB;
    }

    /**
     * @return the foreground color of the message text box
     */
    public RGB getForegroundRGB() {
        return prevForegroundRGB;
    }

    /**
     * Set the maximum number of messages to be retained in the log
     *
     * @param maxLogSize
     */
    public void setMaxLogSize(int maxLogSize) {
        textMsgLog.setMaxLogSize(maxLogSize);
    }

    /**
     * Start the blink timer
     */
    public static synchronized void startTimer() {
        if (blinkTimer == null) {
            TimerTask blinkTask = new TimerTask() {

                @Override
                public void run() {
                    blinkPhase = !blinkPhase;
                    for (TextMsgControlComp t : blinkList) {
                        t.blink();
                    }
                }
            };
            blinkTimer = new Timer();
            blinkTimer.scheduleAtFixedRate(blinkTask, 0, 500);
        }

    }

    /**
     * Stop the blink timer.
     */
    public static synchronized void stopTimer() {
        blinkTimer.cancel();
        blinkTimer = null;
    }

}
