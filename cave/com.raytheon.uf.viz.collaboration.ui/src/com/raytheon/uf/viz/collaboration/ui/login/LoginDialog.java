package com.raytheon.uf.viz.collaboration.ui.login;

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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.data.DataUser;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog for getting user information to establish a connection to the server.
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
public class LoginDialog extends CaveSWTDialog {
    private static DataUser.StatusType[] status = null;

    private Text userTF;

    private Label serverTF;

    private Text passwordTF;

    private Combo statusC;

    private Text messageTF;

    public LoginDialog(Shell parentShell) {
        super(parentShell);
        setText("Collaboration Server Log On");
    }

    private Control createDialogArea(Composite parent) {
        if (status == null) {
            DataUser.StatusType[] types = DataUser.StatusType.values();
            status = new DataUser.StatusType[types.length - 1];
            int index = 0;
            for (DataUser.StatusType type : types) {
                if (type != DataUser.StatusType.NOT_ON_LINE) {
                    status[index] = type;
                    ++index;
                }
            }
        }
        GridData gd = null;
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(3, false));
        Label label = null;
        label = new Label(body, SWT.NONE);
        label.setText("User: ");
        userTF = new Text(body, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        // Set minimum width one time and the fill will handle the other fields.
        gd.minimumWidth = 200;
        userTF.setLayoutData(gd);
        label = new Label(body, SWT.NONE);

        label = new Label(body, SWT.NONE);
        label.setText("Server: ");
        serverTF = new Label(body, SWT.NONE);
        serverTF.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, true,
                false));
        serverTF.setText("awipscm.omaha.us.ray.com");
        Button serverButton = new Button(body, SWT.PUSH);
        serverButton.setText("Server ...");
        serverButton.setToolTipText("Change Server");
        serverButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                System.out.println("Change server here.");
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        label = new Label(body, SWT.NONE);
        label.setText("Password: ");
        passwordTF = new Text(body, SWT.PASSWORD | SWT.BORDER);
        passwordTF.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        passwordTF.setTextLimit(32);
        label = new Label(body, SWT.NONE);
        label = new Label(body, SWT.NONE);
        label.setText("Status: ");
        statusC = new Combo(body, SWT.DEFAULT);

        // TODO get status messages form config file?
        for (DataUser.StatusType type : status) {
            statusC.add(type.value());
        }

        statusC.select(0);
        label = new Label(body, SWT.NONE);

        label = new Label(body, SWT.NONE);
        label.setText("Message: ");

        messageTF = new Text(body, SWT.BORDER);
        // messageTF
        // .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        messageTF.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return body;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        // GridData gd = new GridData();
        // gd.
        // shell.setLayoutData(gd);
        createDialogArea(shell);
        createButtonBar(shell);
    }

    private void createButtonBar(Composite parent) {
        GridData gd = null;
        Composite bar = new Composite(parent, SWT.NONE);
        // bar.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_RED));
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bar.setLayout(new GridLayout(0, true));
        bar.setLayoutData(gd);
        createButton(bar, IDialogConstants.OK_ID, "Log On", true);

        createButton(bar, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void preOpened() {
        super.preOpened();
        userTF.setFocus();
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
                // buttonPressed(((Integer) event.widget.getData()).intValue());
                Integer val = (Integer) event.widget.getData();
                if (val != IDialogConstants.OK_ID) {
                    setReturnValue(null);
                    LoginDialog.this.getShell().dispose();
                } else {
                    Text focusField = null;
                    List<String> errorMessages = new ArrayList<String>();
                    String user = userTF.getText().trim();
                    String server = serverTF.getText().trim();
                    String password = passwordTF.getText();
                    if (user.length() <= 0) {
                        if (focusField == null) {
                            focusField = userTF;
                        }
                        errorMessages.add("Must enter a user.");
                        userTF.setText("");
                    }
                    // if (server.length() <= 0) {
                    // if (focusField == null) {
                    // focusField = serverTF;
                    // }
                    // errorMessages.add("Must have a server.");
                    // serverTF.setText("");
                    // }

                    if (password.length() <= 0) {
                        if (focusField == null) {
                            focusField = passwordTF;
                        }
                        errorMessages.add("Must enter a password.");
                        passwordTF.setText("");
                    }
                    if (focusField == null) {
                        setReturnValue(new LoginData(user, server, password,
                                status[statusC.getSelectionIndex()], messageTF
                                        .getText().trim()));
                        LoginDialog.this.getShell().dispose();
                    } else {
                        StringBuilder sb = new StringBuilder();
                        String prefix = "";
                        for (String msg : errorMessages) {
                            sb.append(prefix).append(msg);
                            prefix = "\n";
                        }
                        MessageBox messageBox = new MessageBox(event.widget
                                .getDisplay().getActiveShell(), SWT.ERROR);
                        messageBox.setText("Login in error");
                        messageBox.setMessage(sb.toString());
                        messageBox.open();
                        event.doit = false;
                        setReturnValue(null);
                        focusField.setFocus();
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
