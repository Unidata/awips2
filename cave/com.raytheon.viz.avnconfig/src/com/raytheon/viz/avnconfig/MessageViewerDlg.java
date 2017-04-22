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

import java.util.LinkedList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.avncommon.AvnMessageMgr;
import com.raytheon.viz.avncommon.AvnMessageMgr.StatusMessageType;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The MessageViewerDlg class displays the status messages.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 4/8/2008     934        grichard    Added IStatusViewable interface.
 * 9/12/2008    1444       grichard    Accommodate separate message logs.
 * 10/12/2012   1229       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class MessageViewerDlg extends CaveSWTDialog {

    /**
     * Maximum allowable number of messages in the message log.
     */
    private int maxMsgLogSize = 500;

    /**
     * List control containing the status messages.
     */
    private List msgList;

    /**
     * Status message type.
     */
    private StatusMessageType msgType;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param msgType
     *            Status message type.
     */
    public MessageViewerDlg(Composite parent, StatusMessageType msgType) {
        super(parent.getShell(), SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("Message Log");

        this.msgType = msgType;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 5;
        mainLayout.marginWidth = 5;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        AvnMessageMgr msgMgr = AvnMessageMgr.getInstance();
        msgMgr.unregisterMessageDlg(msgType);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createMessageListBoxLabel();
        createMessageListBox();
        createOkButton();

        AvnMessageMgr msgMgr = AvnMessageMgr.getInstance();
        LinkedList<String> messages = msgMgr.registerMessageDlg(msgType, this);

        for (String str : messages) {
            msgList.add(str);
        }

        maxMsgLogSize = msgMgr.getMaxLogMessages();
    }

    /**
     * Create the message label.
     */
    private void createMessageListBoxLabel() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);

        Label msgLbl = new Label(shell, SWT.NONE);
        msgLbl.setText("Past Messages");
        msgLbl.setLayoutData(gd);
    }

    /**
     * Create the message list control.
     */
    private void createMessageListBox() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 300;
        gd.widthHint = 600;
        msgList = new List(shell, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        msgList.setLayoutData(gd);
    }

    /**
     * Create the OK button at the bottom of the dialog.
     */
    private void createOkButton() {
        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayoutData(gd);
        buttonComp.setLayout(new GridLayout(1, true));

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setLayoutData(gd);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Add message to the message list control.
     * 
     * @param msg
     *            Message to display.
     */
    public void addMessageText(String msg) {
        msgList.add(msg, 0);
        if (msgList.getItemCount() >= maxMsgLogSize) {
            msgList.remove(msgList.getItemCount() - 1);
        }
        msgList.setSelection(0);
    }
}
