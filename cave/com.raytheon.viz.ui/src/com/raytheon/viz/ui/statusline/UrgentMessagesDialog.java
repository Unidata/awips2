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
package com.raytheon.viz.ui.statusline;

import java.text.SimpleDateFormat;
import java.util.LinkedList;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.statusline.StatusMessage.Importance;

/**
 * This is a popup dialog that displays urgent messages to the user. It is not
 * modal, so other controls can be used while it is visible, but it cannot be
 * dismissed until all the messages added to it have been acknowledged.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	May 19, 2008					Eric Babin Initial Creation
 * 2008-12-09
 *  Apr 10, 2014  15769    ryu         Disposing and reparenting dialog shell.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class UrgentMessagesDialog extends Dialog {
    private static final int ACKNOWLEDGE_ALL_ID = 101;

    private static final int ACKNOWLEDGE_ID = 99;

    private static final int MIN_WIDTH = 500;

    private static final int MIN_HEIGHT = 125;

    private SimpleDateFormat sdf;

    private Color foregroundColor;

    private Color backgroundColor;

    private LinkedList<StatusMessage> urgentBuffer;

    private Text messageText;

    private String title;

    private Composite top;

    private Label pendingLabel;

    /**
     * @param parent
     */
    public UrgentMessagesDialog(Shell parent, String title, RGB foreground,
            RGB background) {
        super(parent);

        setShellStyle(SWT.MODELESS | SWT.RESIZE | SWT.DIALOG_TRIM);

        foregroundColor = new Color(parent.getDisplay(), foreground);
        backgroundColor = new Color(parent.getDisplay(), background);

        urgentBuffer = new LinkedList<StatusMessage>();

        if (title != null) {
            this.title = title;
        }
        sdf = new SimpleDateFormat("yy/MM/dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    public void create() {
        super.create();
        
        getShell().addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                urgentBuffer.clear();
                close();
            }
            
        });
    }
    
    public void reparent(Shell parent) {
        if (getParentShell() != null && !getParentShell().isDisposed())
            return;
        if (parent != null) {
            setParentShell(parent);
        }
    }
    
    @Override
    public boolean close() {
        if (urgentBuffer.size() > 0) {
            return false;
        } else {
            return super.close();
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;

        top.setLayout(mainLayout);

        messageText = new Text(top, SWT.BORDER | SWT.READ_ONLY | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.ON_TOP | SWT.MULTI);
        messageText.setForeground(foregroundColor);
        messageText.setBackground(backgroundColor);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.widthHint = 500;
        messageText.setLayoutData(data);

        return top;
    }

    @Override
    protected Control createButtonBar(Composite parent) {
        Composite composite = (Composite) super.createButtonBar(parent);
        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_END
                | GridData.VERTICAL_ALIGN_CENTER);
        composite.setLayoutData(data);
        return composite;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, ACKNOWLEDGE_ID, "Acknowledge", false);
        super.createButton(parent, ACKNOWLEDGE_ALL_ID, "Acknowledge All", false);

        ((GridLayout) parent.getLayout()).numColumns++;
        pendingLabel = new Label(parent, SWT.RIGHT);
        pendingLabel.setText("0 Pending");
        pendingLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                true));
    }

    private void updateWindow() {
        // Update the Window to show how many messages are pending in
        // addition to the one on the screen.
        // If no messages left, close the window.

        // Find out how many messages are in the buffer
        int messages = urgentBuffer.size();
        if (messages == 0) {
            close();
            return;
        }

        // Display the last message in the buffer
        if (!messageText.isDisposed()) {
            String txt = getText(urgentBuffer.getLast());
            messageText.setText(txt);
            getShell().pack();
        }

        if (!pendingLabel.isDisposed()) {
            pendingLabel.setText(messages - 1 + " Pending");
        }

        getShell().forceActive();
    }

    private String getText(StatusMessage m) {
        return sdf.format(m.getMessageDate()) + " " + m.getMessageText();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setMinimumSize(MIN_WIDTH, MIN_HEIGHT);
        shell.setText(title);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == ACKNOWLEDGE_ID) {
            acknowledge();
        } else if (buttonId == ACKNOWLEDGE_ALL_ID) {
            acknowledgeAll();
        } else {
            System.out.println("Unknown button id: " + buttonId);
        }
    }

    public void addMessage(StatusMessage msg) {
        // Add the incoming message to the urgentBuffer
        urgentBuffer.addLast(msg);

        updateWindow();
    }

    private void acknowledge() {
        // Delete the last message from the urgentBuffer
        urgentBuffer.removeLast();
        // Update the Window
        updateWindow();
    }

    private void acknowledgeAll() {
        urgentBuffer.clear();
        updateWindow();
    }

    public static void main(String[] args) {
        UrgentMessagesDialog umd = new UrgentMessagesDialog(new Shell(),
                "Urgent Messages", new RGB(255, 255, 255), new RGB(255, 0, 0));
        umd.setBlockOnOpen(false);
        umd.open();

        umd.addMessage(new StatusMessage("This is a test.", Importance.URGENT));
        umd.addMessage(new StatusMessage("This is a second test.",
                Importance.URGENT));
        umd.addMessage(new StatusMessage(
                "I am the captain of the Pinafore (and a right good captain, too). I'm very, very good, and be it understood, I command a right good crew. (He's very, very good, and be it understood, he commands a right good crew.) ",
                Importance.URGENT));
        umd.addMessage(new StatusMessage("This is a third test.",
                Importance.URGENT));
        umd.setBlockOnOpen(true);
        umd.open();
    }
}
