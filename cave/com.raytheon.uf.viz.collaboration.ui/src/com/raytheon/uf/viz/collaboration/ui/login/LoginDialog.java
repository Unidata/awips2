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
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;

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
public class LoginDialog extends CaveSWTDialogBase {
    private static DataUser.StatusType[] status = null;

    private Text userTF;

    private Text serverTF;

    private Text passwordTF;

    private Combo statusC;

    private Text messageTF;

    // /**
    // * Collection of buttons created by the <code>createButton</code> method.
    // */
    // private Map<Integer, Button> buttons = new HashMap<Integer, Button>();

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
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(2, false));
        body.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        Label label = null;
        label = new Label(body, SWT.NONE);
        label.setText("User: ");
        userTF = new Text(body, SWT.BORDER);
        userTF.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        // TODO Get user name based on linux log in name and the virtual sever
        // from configuration then combine to make this label
        // userTF.setText("rferrel");

        label = new Label(body, SWT.NONE);
        label.setText("Server");
        serverTF = new Text(body, SWT.BORDER);
        serverTF.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        serverTF.setText("awipscm.omaha.us.ray.com");
        serverTF.setEnabled(false);

        label = new Label(body, SWT.NONE);
        label.setText("Password: ");
        passwordTF = new Text(body, SWT.PASSWORD | SWT.BORDER);
        passwordTF.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        passwordTF.setTextLimit(32);
        label = new Label(body, SWT.NONE);
        label.setText("Status: ");
        statusC = new Combo(body, SWT.DEFAULT);

        // TODO get status messages form config file?
        for (DataUser.StatusType type : status) {
            statusC.add(type.value());
        }

        statusC.select(0);

        label = new Label(body, SWT.NONE);
        label.setText("Message: ");

        messageTF = new Text(body, SWT.BORDER);
        messageTF
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        // body.pack();
        return body;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        createDialogArea(shell);
        createButtonBar(shell);
    }

    private void createButtonBar(Composite parent) {
        Composite b = new Composite(parent, SWT.NONE);

        // set up to center buttons.
        b.setLayout(new GridLayout(3, true));
        new Composite(b, SWT.NONE);
        Composite bar = new Composite(b, SWT.NONE);
        new Composite(b, SWT.NONE);
        bar.setLayout(new GridLayout(0, true));
        createButton(bar, IDialogConstants.OK_ID, "Log On", true);

        createButton(bar, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void preOpened() {
        // TODO Auto-generated method stub
        super.preOpened();
        passwordTF.setFocus();
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
        // button.setFont(JFaceResources.getDialogFont());
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
                        errorMessages.add("Must have a user.");
                        userTF.setText("");
                    }
                    if (server.length() <= 0) {
                        if (focusField == null) {
                            focusField = serverTF;
                        }
                        errorMessages.add("Must have a server.");
                        serverTF.setText("");
                    }

                    if (password.length() <= 0) {
                        if (focusField == null) {
                            focusField = passwordTF;
                        }
                        errorMessages.add("Must have a password.");
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
        // buttons.put(new Integer(id), button);
        // setButtonLayoutData(button);
        return button;
    }
}
