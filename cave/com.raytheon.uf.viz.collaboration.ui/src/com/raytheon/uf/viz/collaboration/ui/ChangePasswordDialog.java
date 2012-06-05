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
import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.data.CollaborationDataManager;
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
 * Apr 3, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ChangePasswordDialog extends CaveSWTDialog {

    private Label userLabel;

    private Text passwordTF;

    private Text verifyPasswordTF;

    public ChangePasswordDialog(Shell parentShell) {
        super(parentShell);
        setText("Change Password");
    }

    private Control createDialogArea(Composite parent) {
        CollaborationDataManager manager = CollaborationDataManager
                .getInstance();
        UserId user = manager.getCollaborationConnection(true).getUser();
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(2, false));
        // body.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
        // | GridData.HORIZONTAL_ALIGN_FILL));
        Label label = null;
        GridData gd = null;
        userLabel = new Label(body, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        userLabel.setLayoutData(gd);
        userLabel.setText(user.getFQName());

        label = new Label(body, SWT.NONE);
        label.setText("New Password: ");
        passwordTF = new Text(body, SWT.BORDER | SWT.PASSWORD);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        passwordTF.setLayoutData(gd);

        label = new Label(body, SWT.NONE);
        label.setText("Verify Password: ");
        verifyPasswordTF = new Text(body, SWT.BORDER | SWT.PASSWORD);
        verifyPasswordTF.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

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
        createButton(bar, IDialogConstants.OK_ID, "Change", true);

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
                    ChangePasswordDialog.this.getShell().dispose();
                } else {
                    Text focusField = null;
                    List<String> errorMessages = new ArrayList<String>();
                    String password = passwordTF.getText();
                    String verifyPassword = verifyPasswordTF.getText();
                    if (password.length() == 0) {
                        errorMessages.add("Must enter new password.");
                        focusField = passwordTF;
                        verifyPasswordTF.setText("");
                    } else if (verifyPassword.length() == 0) {
                        errorMessages.add("Enter password again to verify.");
                        focusField = verifyPasswordTF;
                    } else if (password.equals(verifyPassword) == false) {
                        errorMessages
                                .add("New and verified password do not match.");
                        errorMessages.add("Enter them again.");
                        focusField = passwordTF;
                        verifyPasswordTF.setText("");
                    }

                    if (focusField == null) {
                        setReturnValue(password);
                        ChangePasswordDialog.this.getShell().dispose();
                    } else {
                        StringBuilder sb = new StringBuilder();
                        String prefix = "";
                        for (String msg : errorMessages) {
                            sb.append(prefix).append(msg);
                            prefix = "\n";
                        }
                        MessageBox messageBox = new MessageBox(event.widget
                                .getDisplay().getActiveShell(), SWT.ERROR);
                        messageBox.setText("Change Password Error");
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
}
