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
package com.raytheon.viz.avnconfig;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.avncommon.AvnMessageMgr;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;

/**
 * MessageStatusComp class.
 * 
 * This Composite class contains controls that will display a message and its
 * status (green - info, orange - warning, red - error). A flag passed into the
 * constructor indicates whether the message log viewer button should be
 * displayed.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    2/6/2008     817         lvenable    Initial creation.
 *    4/7/2008     934         grichard    Added IStatusSettable implementation.
 *    8/11/2008    1314        grichard    Used PathManager for pathnames.
 *    10/04/2012   1229        rferrel     Added dispose check needed for 
 *                                          non-blocking dialogs.
 *    10/12/2012   1229        rferrel     Changes for non-blocking MessageViewerDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class MessageStatusComp extends Composite implements IStatusSettable {

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Label used to display the status color
     */
    private Label msgSeverityLbl;

    /**
     * Message text control.
     */
    private Text msgTF;

    /**
     * Button to display the past messages dialog.
     */
    private Button msgViewerBtn;

    /**
     * Image for the message viewer button.
     */
    private Image msgLogImg;

    /**
     * The message viewer dialog in the Taf Monitor.
     */
    private MessageViewerDlg msgViewerDlg;

    /**
     * Background RGB color. Only used at construction time to create
     * newBgColor. No need to keep it around in a class variable.
     */
    @Deprecated
    private RGB bgRGB;

    /**
     * New background color for the msgViewerBtn.
     */
    private Color newBgColor;

    /**
     * Status message type.
     */
    private StatusMessageType msgType = null;

    /**
     * Use to control the blinking of the background color.
     */
    private Timer timer;

    /**
     * Task use by timer to do the blinking work.
     */
    private TimerTask timerTask;

    /**
     * Counter used to determine how long to blink.
     */
    private int timerCounter = 0;

    /**
     * The color to use for the blinking.
     */
    private Color currentMsgColor;

    /**
     * When false blinking is active.
     */
    private boolean timerDone = true;

    /**
     * Message background RGB color. Only used at construction time to create
     * messageBgColor. No need to keep it around in a class variable.
     */
    @Deprecated
    private RGB messageBgRGB;

    /**
     * The normal back ground color for the text field.
     */
    private Color messageBgColor;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param bgRGB
     *            Background RGB for dialog display button. (not used therefore
     *            always defaults to white)
     * @param messageBgRGB
     *            Background RGB for the message textfield. When null defaults
     *            to white
     */
    public MessageStatusComp(Composite parent, RGB bgRGB, RGB messageBgRGB) {
        super(parent, SWT.NONE);

        this.parent = parent;

        msgType = null;

        this.messageBgRGB = messageBgRGB;

        init();
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param msgType
     *            Status message type.
     * @param bgRGB
     *            Background RGB for display button. When null defaults to
     *            white.
     * @param messageBgRGB
     *            Background RGB for text field. When null defaults to white.
     */
    public MessageStatusComp(Composite parent, StatusMessageType msgType,
            RGB bgRGB, RGB messageBgRGB) {
        super(parent, SWT.NONE);

        this.parent = parent;

        this.bgRGB = bgRGB;

        this.msgType = msgType;

        this.messageBgRGB = messageBgRGB;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {

        msgLogImg = new Image(parent.getDisplay(), loadMessageGif());

        if (bgRGB == null) {
            bgRGB = this.getDisplay()
                    .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND).getRGB();
        }

        if (messageBgRGB == null) {
            messageBgRGB = this.getDisplay().getSystemColor(SWT.COLOR_WHITE)
                    .getRGB();
        }

        messageBgColor = new Color(this.getDisplay(), messageBgRGB);

        newBgColor = new Color(this.getDisplay(), bgRGB);
        this.setBackground(newBgColor);

        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);

        initializeComponents();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                msgLogImg.dispose();

                if (newBgColor.isDisposed() == false) {
                    newBgColor.dispose();
                }

                if (currentMsgColor != null) {
                    currentMsgColor.dispose();
                }

                messageBgColor.dispose();
            }
        });

        this.pack();
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        createMessageControls();
    }

    /**
     * Create the message controls.
     */
    private void createMessageControls() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.verticalSpacing = 1;
        gl.marginWidth = 2;
        gl.marginHeight = 1;
        Composite msgComp = new Composite(parent, SWT.NONE);
        msgComp.setLayout(gl);
        msgComp.setLayoutData(gd);
        msgComp.setBackground(newBgColor);

        // Label to display the color status.
        gd = new GridData(23, 23);
        msgSeverityLbl = new Label(msgComp, SWT.BORDER);
        msgSeverityLbl.setLayoutData(gd);

        // Message text control.
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        msgTF = new Text(msgComp, SWT.BORDER);
        msgTF.setLayoutData(gd);
        msgTF.setEditable(false);
        msgTF.setBackground(messageBgColor);

        // Check if the message viewer dialog button needs
        // to be displayed.
        if (msgType != null) {
            // Message viewer button.
            gd = new GridData(30, 30);
            msgViewerBtn = new Button(msgComp, SWT.PUSH);
            msgViewerBtn.setLayoutData(gd);
            msgViewerBtn.setImage(msgLogImg);
            msgViewerBtn.setToolTipText("View Past Messages");
            msgViewerBtn.setBackground(newBgColor);
            msgViewerBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    createMessageViewerDialog();
                }
            });
        }
    }

    /**
     * Load the message image.
     * 
     * @return the image
     */
    private String loadMessageGif() {
        IPathManager pm = PathManagerFactory.getPathManager();
        String path = pm.getFile(
                pm.getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE),
                "aviation" + File.separatorChar + "avnwatch"
                        + File.separatorChar + "msgLog2.png").getAbsolutePath();
        return path;
    }

    /**
     * Set the message text in the dialog and start the blinking timer.
     * 
     * @param msg
     *            Message to add to the dialog.
     */
    @Override
    public void setMessageText(String msg, RGB rgbColor) {

        if (currentMsgColor != null) {
            currentMsgColor.dispose();
        }

        if (!parent.isDisposed()) {
            currentMsgColor = new Color(parent.getDisplay(), rgbColor);

            msgTF.setText(String.valueOf(msg));

            blinkAndClear();

            StringBuilder sb = new StringBuilder(calculateIssueTime());
            sb.append(" ").append(msg);

            AvnMessageMgr msgMgr = AvnMessageMgr.getInstance();
            msgMgr.addMessage(msgType, sb.toString());
        }
    }

    /**
     * Blink the status label and then clear the text after the blinking is
     * complete.
     */
    private void blinkAndClear() {
        if (timerDone == false) {
            timerCounter = 0;
            return;
        }

        timer = new Timer();
        timerCounter = 0;
        timerDone = false;

        timerTask = new TimerTask() {
            public void run() {
                if (parent.isDisposed() == true) {
                    return;
                }
                parent.getDisplay().syncExec(new Runnable() {
                    public void run() {
                        if (msgSeverityLbl.isDisposed() == true) {
                            timerDone = true;
                            timer.cancel();
                            return;
                        }

                        if (timer == null) {
                            return;
                        }

                        if ((timerCounter % 2) == 0) {
                            msgSeverityLbl
                                    .setBackground(parent
                                            .getDisplay()
                                            .getSystemColor(
                                                    SWT.COLOR_WIDGET_BACKGROUND));
                        } else {
                            msgSeverityLbl.setBackground(currentMsgColor);
                        }

                        ++timerCounter;

                        if (timerCounter > 7) {
                            timerDone = true;
                            timer.cancel();
                            msgSeverityLbl
                                    .setBackground(parent
                                            .getDisplay()
                                            .getSystemColor(
                                                    SWT.COLOR_WIDGET_BACKGROUND));
                            msgTF.setText("");
                        }
                    }
                });
            }
        };

        timer.schedule(timerTask, 0, 1000);
    }

    /**
     * Create the message viewer dialog.
     */
    private void createMessageViewerDialog() {
        if (msgViewerDlg == null || msgViewerDlg.getShell() == null
                || msgViewerDlg.isDisposed()) {
            msgViewerDlg = new MessageViewerDlg(this, msgType);
            msgViewerDlg.open();
        } else {
            msgViewerDlg.bringToTop();
        }
    }

    /**
     * Method that timestamps the issuance time of an informational message that
     * appears in the message viewer dialog.
     * 
     * @return String representing the issuance time in MM/dd/yy HH:mm:ss:
     *         format
     */
    private String calculateIssueTime() {
        Date now = SimulatedTime.getSystemTime().getTime();
        SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yy HH:mm:ss:");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        Calendar ait = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        ait.setTime(now);
        return (formatter.format(ait.getTime()));
    }
}
