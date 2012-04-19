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

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXB;

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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterEventSubscriber;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.DataUser;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
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
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LoginDialog.class);

    private static final String LOGIN_FILE_NAME = "collaboration"
            + File.separator + "config" + File.separator + "gui"
            + File.separator + "LoginData.xml";

    private Text userTF;

    private Text serverTF;

    private Button serverButton;

    private Text passwordTF;

    private Combo statusCombo;

    private Text messageTF;

    private Button logOnButton;

    private String DEFAULT_SERVER = "awipscm.omaha.us.ray.com";

    private Control[] noServerList;

    private Control[] withServerList;

    private LoginData loginData;

    private IRosterEventSubscriber rosterEventSubscriber;

    private CollaborationConnection sessionManager;

    public LoginDialog(Shell parentShell,
            IRosterEventSubscriber rosterEventSubscriber) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Collaboration Server Login");
        this.rosterEventSubscriber = rosterEventSubscriber;
    }

    /**
     * @param parent
     * @return
     */
    private Control createDialogArea(Composite parent) {
        GridData gd = null;
        Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(3, false));
        Label label = null;
        label = new Label(body, SWT.NONE);
        label.setText("User: ");
        userTF = new Text(body, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        // Set minimum width one time and the fill will handle the other fields.
        gd.horizontalSpan = 2;
        userTF.setLayoutData(gd);

        label = new Label(body, SWT.NONE);
        label.setText("Server: ");
        serverTF = new Text(body, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        // Set minimum width one time and the fill will handle the other fields.
        gd.minimumWidth = 200;
        serverTF.setLayoutData(gd);
        serverTF.setText(DEFAULT_SERVER);
        serverTF.setEditable(false);
        serverTF.setBackground(parent.getBackground());
        serverButton = new Button(body, SWT.PUSH);
        serverButton.setText("Edit");
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 45;
        serverButton.setLayoutData(gd);
        serverButton.setToolTipText("Change Server");
        serverButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if ("OK".equals(serverButton.getText())) {
                    serverButton.setText("Edit");
                    serverButton.setToolTipText("Change Server");
                    serverTF.setEditable(false);
                    serverTF.setBackground(serverTF.getParent().getBackground());
                    String server = serverTF.getText().trim();
                    if (server.length() == 0) {
                        serverTF.setText(DEFAULT_SERVER);
                    } else {
                        serverTF.setText(server);
                        DEFAULT_SERVER = server;
                    }
                    serverTF.clearSelection();
                    serverTF.getParent().setTabList(noServerList);
                    logOnButton.setEnabled(true);
                } else {
                    serverButton.setText("OK");
                    serverButton
                            .setToolTipText("Implement Change.\nEmpty field restores previous server.");
                    serverTF.setEditable(true);
                    serverTF.setBackground(null);
                    serverTF.selectAll();
                    serverTF.setFocus();
                    serverTF.getParent().setTabList(withServerList);
                    logOnButton.setEnabled(false);
                }
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        label = new Label(body, SWT.NONE);
        label.setText("Password: ");
        passwordTF = new Text(body, SWT.PASSWORD | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        passwordTF.setLayoutData(gd);
        passwordTF.setTextLimit(32);

        label = new Label(body, SWT.NONE);
        label.setText("Status: ");
        statusCombo = new Combo(body, SWT.DEFAULT);

        // TODO get mode messages from config file?
        for (IPresence.Mode mode : CollaborationUtils.statusModes) {
            statusCombo.add(mode.getMode());
        }
        statusCombo.select(0);
        label = new Label(body, SWT.NONE);

        label = new Label(body, SWT.NONE);
        label.setText("Message: ");

        messageTF = new Text(body, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        messageTF.setLayoutData(gd);

        noServerList = new Control[] { userTF, passwordTF, statusCombo,
                messageTF, serverButton };
        withServerList = new Control[] { userTF, serverTF, serverButton,
                passwordTF, statusCombo, messageTF };
        body.setTabList(noServerList);
        return body;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        shell.setLayout(new GridLayout(1, false));
        createDialogArea(shell);
        createButtonBar(shell);
    }

    /**
     * @param parent
     */
    private void createButtonBar(Composite parent) {
        GridData gd = null;
        Composite bar = new Composite(parent, SWT.NONE);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bar.setLayout(new GridLayout(0, true));
        bar.setLayoutData(gd);
        logOnButton = createButton(bar, IDialogConstants.OK_ID, "Login", true);

        createButton(bar, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        this.loginData = LoginDialog.openUserLoginData();
        userTF.setText(loginData.getUser());
        serverTF.setText(loginData.getServer());

        statusCombo.select(CollaborationUtils.statusModesIndex(loginData
                .getMode()));
        messageTF.setText(loginData.getModeMessage());
        userTF.selectAll();
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
                    IPresence.Mode mode = CollaborationUtils.statusModes[statusCombo
                            .getSelectionIndex()];
                    String modeMessage = messageTF.getText().trim();
                    if (user.length() <= 0) {
                        if (focusField == null) {
                            focusField = userTF;
                        }
                        errorMessages.add("Must enter a user.");
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
                        errorMessages.add("Must enter a password.");
                        passwordTF.setText("");
                    }
                    if (focusField == null) {
                        boolean doSaveLoginData = false;
                        if (!loginData.getUser().equals(user)) {
                            doSaveLoginData = true;
                            loginData.setUser(user);
                        }
                        if (!loginData.getServer().equals(server)) {
                            doSaveLoginData = true;
                            loginData.setServer(server);
                        }
                        loginData.setPassword(password);
                        if (!loginData.getMode().equals(mode)) {
                            doSaveLoginData = true;
                            loginData.setMode(mode);
                        }
                        if (!loginData.getModeMessage().equals(modeMessage)) {
                            doSaveLoginData = true;
                            loginData.setModeMessage(modeMessage);
                        }
                        if (doSaveLoginData) {
                            LoginDialog.saveUserLoginData(loginData);
                        }

                        // loginData = new LoginData(user, server, password,
                        // CollaborationUtils.statusModes[statusCombo
                        // .getSelectionIndex()], messageTF
                        // .getText().trim());
                        try {
                            sessionManager = new CollaborationConnection(loginData
                                    .getAccount(), loginData.getPassword(),
                                    rosterEventSubscriber);
                            DataUser dUser = CollaborationDataManager
                                    .getInstance().getUser(
                                            loginData.getAccount());
                            // // TODO set mode and message here.
                            // dUser.setMode(loginData.getMode());
                            // dUser.type = Type.AVAILABLE;
                            // dUser.statusMessage = loginData.getModeMessage();
                            setReturnValue(loginData);
                            LoginDialog.this.getShell().dispose();
                        } catch (Exception e) {
                            if (focusField == null) {
                                focusField = passwordTF;
                            }
                            errorMessages.add("Inavlid username or password.");
                            passwordTF.setText("");
                            if (sessionManager != null) {
                                sessionManager.closeManager();
                            }
                        }
                    }
                    if (focusField != null) {
                        StringBuilder sb = new StringBuilder();
                        String prefix = "";
                        for (String msg : errorMessages) {
                            sb.append(prefix).append(msg);
                            prefix = "\n";
                        }
                        MessageBox messageBox = new MessageBox(event.widget
                                .getDisplay().getActiveShell(), SWT.ERROR);
                        messageBox.setText("Login Error");
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

    public static LoginData openUserLoginData() {
        LoginData loginData = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        File fname = pm.getStaticFile(LOGIN_FILE_NAME);
        try {
            if (fname != null) {
                loginData = JAXB.unmarshal(fname, LoginData.class);
            }
        } catch (RuntimeException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            if (loginData == null) {
                loginData = new LoginData();
            }
        }
        return loginData;
    }

    public static void saveUserLoginData(LoginData loginData) {
        try {
            LocalizationFile lFile = getFile(LOGIN_FILE_NAME);
            File file = lFile.getFile(false);
            file.getParentFile().mkdirs();
            JAXB.marshal(loginData, file);
            lFile.save();
        } catch (FileNotFoundException ex) {
            statusHandler
                    .handle(Priority.PROBLEM, ex.getLocalizedMessage(), ex);
        } catch (LocalizationException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Obtains a localization file for desired file name.
     * 
     * @param filename
     * @return lFile
     * @throws FileNotFoundException
     */
    private static LocalizationFile getFile(String filename)
            throws FileNotFoundException {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile lFile = pm.getLocalizationFile(context, filename);
        if (lFile == null) {
            String user = LocalizationManager.getInstance().getCurrentUser();
            throw new FileNotFoundException("Unable to find \"" + filename
                    + "\" under the directory for user " + user + ".");

        }
        return lFile;
    }

    /**
     * @return the loginData
     */
    public LoginData getLoginData() {
        return loginData;
    }

    /**
     * @return the sessionManager
     */
    public CollaborationConnection getSessionManager() {
        return sessionManager;
    }
}
