package com.raytheon.uf.viz.collaboration.ui;

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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CreateSessionDialog extends CaveSWTDialog {

    private Text nameTF;

    private Text subjectTF;

    private Button publicCollaboration;

    private Button inviteUsers;

    private boolean showInvite;

    private StyledText inviteMessageTF;

    private Label inviteLabel;

    public CreateSessionDialog(Shell parentShell, boolean showInvite) {
        super(parentShell);
        this.showInvite = showInvite;
        setText("Create Session");
    }

    private Control createDialogArea(Composite parent) {
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(2, false));
        // body.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        // | GridData.HORIZONTAL_ALIGN_FILL));
        Label label = null;
        label = new Label(body, SWT.NONE);
        label.setText("Name: ");
        nameTF = new Text(body, SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 200;
        nameTF.setLayoutData(gd);
        nameTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                if (" \t\"&'/,<>@".indexOf(e.character) >= 0) {
                    e.doit = false;
                    // Toolkit.getDefaultToolkit().beep();
                }
            }
        });

        label = new Label(body, SWT.NONE);
        label.setText("Subject: ");
        subjectTF = new Text(body, SWT.BORDER);
        subjectTF.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        publicCollaboration = new Button(body, SWT.CHECK);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 2;
        publicCollaboration.setLayoutData(gd);
        publicCollaboration.setSelection(true);
        publicCollaboration.setText("Create Collaboration");

        if (showInvite) {
            inviteUsers = new Button(body, SWT.CHECK);
            inviteUsers.setSelection(true);
            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
            gd.horizontalSpan = 2;
            inviteUsers.setLayoutData(gd);
            inviteUsers.setText("Invite Selected Users");
            // inviteUsers.setSelection(true);
            inviteUsers.setVisible(true);
            // label = new Label(body, SWT.NONE);
            // label.setText("");
            // label.setVisible(showInvite);
            inviteLabel = new Label(body, SWT.NONE);
            inviteLabel.setText("Message: ");
            inviteLabel.setToolTipText("Message to send to invited users");
            inviteMessageTF = new StyledText(body, SWT.BORDER | SWT.MULTI
                    | SWT.WRAP | SWT.V_SCROLL);
            inviteMessageTF.setLayoutData(new GridData(GridData.FILL_BOTH));
            inviteMessageTF.pack();
            inviteMessageTF.setToolTipText("Message to send to invited users");
            Point p = inviteMessageTF.getSize();
            gd = (GridData) inviteMessageTF.getLayoutData();
            gd.heightHint = p.y * 3;
            inviteUsers.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    boolean selected = ((Button) e.widget).getSelection();
                    inviteLabel.setVisible(selected);
                    inviteMessageTF.setVisible(selected);
                }

                @Override
                public void widgetDefaultSelected(SelectionEvent e) {
                    boolean selected = ((Button) e.widget).getSelection();
                    inviteLabel.setVisible(selected);
                    inviteMessageTF.setVisible(selected);

                }
            });
            inviteLabel.setVisible(true);
            inviteMessageTF.setVisible(true);
        }
        return body;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        createDialogArea(shell);
        createButtonBar(shell);
    }

    private void createButtonBar(Composite parent) {
        GridData gd = null;
        Composite bar = new Composite(parent, SWT.NONE);

        // set up to center buttons.
        bar.setLayout(new GridLayout(0, true));
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bar.setLayoutData(gd);
        createButton(bar, IDialogConstants.OK_ID, "Create", true);

        createButton(bar, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void preOpened() {
        super.preOpened();
    }

    /**
     * Creates a new button with the given id.
     * <p>
     * The <code>Dialog</code> implementation of this framework method creates a
     * standard push button, registers it for selection events including button
     * presses, and registers default buttons with its shell. The button id is
     * stored as the button's client data. If the button id is
     * <code>IDialogConstants.CANCEL_ID</code>, the new button will be
     * accessible from <code>getCancelButton()</code>. If the button id is
     * <code>IDialogConstants.OK_ID</code>, the new button will be accesible
     * from <code>getOKButton()</code>. Note that the parent's layout is assumed
     * to be a <code>GridLayout</code> and the number of columns in this layout
     * is incremented. Subclasses may override.
     * </p>
     * 
     * @param parent
     *            the parent composite
     * @param id
     *            the id of the button (see <code>IDialogConstants.*_ID</code>
     *            constants for standard dialog button ids)
     * @param label
     *            the label from the button
     * @param defaultButton
     *            <code>true</code> if the button is to be the default button,
     *            and <code>false</code> otherwise
     * 
     * @return the new button
     * 
     * @see #getCancelButton
     * @see #getOKButton()
     */
    protected Button createButton(Composite parent, int id, String label,
            boolean defaultButton) {
        // increment the number of columns in the button bar
        ((GridLayout) parent.getLayout()).numColumns++;
        Button button = new Button(parent, SWT.PUSH);
        button.setText(label);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 70;
        button.setLayoutData(gd);
        button.setData(new Integer(id));
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                Integer val = (Integer) event.widget.getData();
                if (val != IDialogConstants.OK_ID) {
                    setReturnValue(null);
                    CreateSessionDialog.this.getShell().dispose();
                } else {
                    Text focusField = null;
                    List<String> errorMessages = new ArrayList<String>();
                    String subject = subjectTF.getText().trim();
                    String err = validateVenuName();
                    String name = nameTF.getText();
                    if (err != null) {
                        focusField = nameTF;
                        errorMessages.add(err);
                    }

                    if (focusField == null) {
                        CreateSessionData result = new CreateSessionData();
                        result.setName(name);
                        result.setSubject(subject);
                        result.setCollaborationSessioh(publicCollaboration
                                .getSelection());
                        if (inviteUsers == null) {
                            result.setInviteUsers(false);
                        } else {
                            result.setInviteUsers(inviteUsers.getSelection());
                            result.setInviteMessage(inviteMessageTF.getText());
                        }
                        CollaborationDataManager manager = CollaborationDataManager
                                .getInstance();
                        String sessionId = null;
                        try {
                            if (result.isCollaborationSession()) {
                                sessionId = manager.createCollaborationSession(
                                        result.getName(), result.getSubject());
                            } else {
                                sessionId = manager.createTextOnlySession(
                                        result.getName(), result.getSubject());
                            }
                            result.setSessionId(sessionId);
                            setReturnValue(result);
                            CreateSessionDialog.this.getShell().dispose();
                        } catch (CollaborationException ex) {
                            MessageBox messageBox = new MessageBox(event.widget
                                    .getDisplay().getActiveShell(), SWT.ERROR);
                            messageBox.setText("Session Creation Error");
                            messageBox.setMessage(ex.getMessage());
                            messageBox.open();
                            event.doit = false;
                            setReturnValue(null);
                        }
                    } else {
                        StringBuilder sb = new StringBuilder();
                        String prefix = "";
                        for (String msg : errorMessages) {
                            sb.append(prefix).append(msg);
                            prefix = "\n";
                        }
                        MessageBox messageBox = new MessageBox(event.widget
                                .getDisplay().getActiveShell(), SWT.ERROR);
                        messageBox.setText("Session Creation Error");
                        messageBox.setMessage(sb.toString());
                        messageBox.open();
                        event.doit = false;
                        setReturnValue(null);
                        focusField.setFocus();
                        focusField.selectAll();
                    }
                }
            }
        });
        if (defaultButton) {
            Shell shell = parent.getShell();
            if (shell != null) {
                shell.setDefaultButton(button);
            }
        }
        return button;
    }

    private String validateVenuName() {
        String name = nameTF.getText().trim();
        nameTF.setText(name);
        String err = null;
        if (name.length() <= 0) {
            err = "Must have session name.";
        } else if (false) {
            // TODO Above else make it a test for invalid characters.
            err = "Name contains invalid characters.";
        } else {
            Collection<IVenueInfo> info = CollaborationDataManager
                    .getInstance().getCollaborationConnection(true).getVenueInfo();
            for (IVenueInfo i : info) {
                if (name.equals(i.getVenueName())) {
                    err = "Session already exists. Pick a different name.";
                    break;
                }
            }
        }
        return err;
    }
}
