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
package com.raytheon.uf.viz.collaboration.ui.login;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation.HostConfig;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation.SiteConfig;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnectionData;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.collaboration.ui.ConnectionSubscriber;
import com.raytheon.uf.viz.collaboration.ui.SiteConfigurationManager;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;

/**
 * Login dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2012            mschenke     Initial creation
 * Dec 19, 2013 2563       bclement     added option to connect to server not in list
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LoginDialog extends Dialog {

    private static final String OTHER_SERVER_OPTION = "Other server...";

    private IPreferenceStore preferences;

    private CollaborationConnectionData loginData;

    private Text userText;

    private Combo serverText;

    private Text passwordText;

    private Combo statusCombo;

    private Text messageText;

    private Map<String, Combo> attributeCombos;

    private Button loginButton;

    private Button cancelButton;

    private Shell shell;

    /**
     * @param parentShell
     */
    public LoginDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM);
        preferences = Activator.getDefault().getPreferenceStore();
        loginData = new CollaborationConnectionData();
        readLoginData();
    }

    public void open() {
        shell = new Shell(getParent(), getStyle());
        shell.setText("Collaboration Server Login");
        shell.setLayout(new GridLayout(1, false));
        // shell.setLayout(new GridLayout(1, false));
        createMainDialogArea(shell);
        createAttributesArea(shell);
        createButtonsArea(shell);
        shell.setDefaultButton(loginButton);
        shell.pack();
        shell.setVisible(true);
        shell.open();
        // block the UI.
        Display display = getParent().getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    /**
     * @param shell
     */
    private void createMainDialogArea(Composite parent) {
        final Composite body = new Composite(parent, SWT.NONE);
        body.setLayout(new GridLayout(3, false));

        // Add user text
        new Label(body, SWT.NONE).setText("User: ");
        userText = new Text(body, SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        userText.setLayoutData(gd);
        userText.setText(loginData.getUserName());

        // Server setting
        new Label(body, SWT.NONE).setText("Server: ");
        serverText = new Combo(body, SWT.BORDER | SWT.READ_ONLY | SWT.DROP_DOWN);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.minimumWidth = 200;
        gd.horizontalSpan = 2;
        serverText.setLayoutData(gd);
        // retrieve the servers
        SiteConfigInformation information = SiteConfigurationManager
                .getSiteConfigInformation();
        List<HostConfig> servers = information.getServer();
        if (servers == null) {
            servers = new ArrayList<SiteConfigInformation.HostConfig>(0);
        }
        // put configured as true so we don't disable the login button
        serverText.setData("configured", true);
        String[] names = new String[servers.size() + 1];
        names[0] = OTHER_SERVER_OPTION;
        int index = 1;
        for (int i = 1; i < names.length; i++) {
            HostConfig config = servers.get(i - 1);
            names[i] = config.getPrettyName() + " : " + config.getHostname();
            if (loginData.getServer().equals(names[i])) {
                index = i;
            }
        }
        serverText.setItems(names);
        serverText.select(index);
        serverText.addListener(SWT.Selection, new ServerInput(serverText, 0));

        // Password setting
        new Label(body, SWT.NONE).setText("Password: ");
        passwordText = new Text(body, SWT.PASSWORD | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        passwordText.setLayoutData(gd);
        passwordText.setTextLimit(32);

        // Status setting
        new Label(body, SWT.NONE).setText("Status: ");
        statusCombo = new Combo(body, SWT.DEFAULT);

        // TODO get possible status options from config file?
        for (Presence.Mode mode : CollaborationUtils.statusModes) {
            statusCombo.add(CollaborationUtils.formatMode(mode));
        }
        String status = loginData.getStatus();
        if (status != null && status.isEmpty() == false) {
            statusCombo.setText(status);
        } else {
            statusCombo.select(0);
        }
        // Empty filler label so the combo and label stay aligned
        new Label(body, SWT.NONE);

        // Message setting
        new Label(body, SWT.NONE).setText("Message: ");
        messageText = new Text(body, SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        messageText.setLayoutData(gd);
        messageText.setText(loginData.getMessage());

        body.setTabList(new Control[] { userText, passwordText, messageText });
    }

    /**
     * @param shell
     */
    private void createAttributesArea(Composite parent) {
        attributeCombos = new HashMap<String, Combo>();
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(4, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        comp.setLayout(layout);
        comp.setLayoutData(gd);

        // TODO: Default to previous settings
        SiteConfigInformation information = SiteConfigurationManager
                .getSiteConfigInformation();
        List<String> sites = new ArrayList<String>();
        final Map<String, String[]> roles = new HashMap<String, String[]>();
        for (SiteConfig conf : information.getConfig()) {
            sites.add(conf.getSite());
            roles.put(conf.getSite(), conf.getRoles());
        }
        Label infoLabel = new Label(comp, SWT.NONE);
        infoLabel.setText(SiteConfigInformation.SITE_NAME + ":");
        infoLabel
                .setLayoutData(new GridData(SWT.NONE, SWT.CENTER, false, true));

        final Combo siteCombo = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        siteCombo.setData(SiteConfigInformation.SITE_NAME);
        siteCombo.setItems(sites.toArray(new String[0]));
        gd = new GridData(SWT.FILL, SWT.NONE, true, true);
        siteCombo.setLayoutData(gd);
        siteCombo.select(0);
        attributeCombos.put(SiteConfigInformation.SITE_NAME, siteCombo);

        infoLabel = new Label(comp, SWT.NONE);
        infoLabel.setText(SiteConfigInformation.ROLE_NAME + ":");
        infoLabel
                .setLayoutData(new GridData(SWT.NONE, SWT.CENTER, false, true));

        final Combo roleCombo = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        roleCombo.setData(SiteConfigInformation.ROLE_NAME);
        roleCombo.setItems(roles.get(siteCombo.getItem(siteCombo
                .getSelectionIndex())));
        gd = new GridData(SWT.FILL, SWT.NONE, true, true);
        roleCombo.setLayoutData(gd);
        roleCombo.select(0);
        attributeCombos.put(SiteConfigInformation.ROLE_NAME, roleCombo);

        siteCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                roleCombo.setItems(roles.get(siteCombo.getItem(siteCombo
                        .getSelectionIndex())));
                roleCombo.select(0);
            }
        });
    }

    private void createButtonsArea(Composite parent) {
        Composite bar = new Composite(parent, SWT.NONE);
        bar.setLayout(new GridLayout(2, true));
        bar.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));

        GridData buttonData = new GridData(SWT.FILL, SWT.FILL, true, true);
        buttonData.minimumWidth = 70;

        loginButton = new Button(bar, SWT.PUSH);
        loginButton.setText("Login");
        if ((Boolean) serverText.getData("configured") == false) {
            loginButton.setEnabled(false);
        }
        loginButton.setLayoutData(buttonData);
        loginButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                loginData.setUserName(userText.getText().trim());
                loginData.setPassword(passwordText.getText().trim());
                loginData.setServer(serverText.getText().split(":")[1].trim());
                loginData.setStatus(statusCombo.getText());
                loginData.setMessage(messageText.getText().trim());
                Map<String, String> attributes = new HashMap<String, String>();
                for (String attribKey : attributeCombos.keySet()) {
                    Combo cbo = attributeCombos.get(attribKey);
                    attributes.put(attribKey, cbo.getText());
                }
                loginData.setAttributes(attributes);

                // Validate everything is set properly
                List<String> errorMessages = new ArrayList<String>();
                if (loginData.getUserName().isEmpty()) {
                    errorMessages.add("Must enter a user.");
                } else {
                    loginData
                            .setUserName(loginData.getUserName().toLowerCase());
                }
                String server = loginData.getServer();
                if (server.isEmpty()) {
                    errorMessages.add("Must have a server.");
                }
                String error = ServerInput.validate(server);
                if (error != null) {
                    errorMessages.add(error);
                } else {
                    try {
                        loginData.setServer(ServerInput.getFullName(server));
                    } catch (CollaborationException e) {
                        errorMessages.add(e.getLocalizedMessage());
                    }
                }

                if (loginData.getPassword().isEmpty()) {
                    errorMessages.add("Must enter a password.");
                }

                if (errorMessages.size() > 0) {
                    // Item(s) is not set properly
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
                } else {
                    try {
                        CollaborationConnection connection = CollaborationConnection
                                .connect(loginData);
                        ConnectionSubscriber.subscribe(connection);
                        storeLoginData();
                        shell.dispose();
                    } catch (CollaborationException e) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                "Error connecting to collaboration server: "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            }
        });

        cancelButton = new Button(bar, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(buttonData);
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                SiteConfigurationManager.nullifySiteConfigInstance();
                shell.dispose();
            }
        });
    }

    private void storeLoginData() {
        preferences.setValue(CollabPrefConstants.P_USERNAME,
                loginData.getUserName());
        preferences.setValue(CollabPrefConstants.P_SERVER,
                loginData.getServer());
        preferences.setValue(CollabPrefConstants.P_MESSAGE,
                loginData.getMessage());
        preferences.setValue(CollabPrefConstants.P_STATUS,
                loginData.getStatus());
    }

    private void readLoginData() {
        loginData.setUserName(preferences
                .getString(CollabPrefConstants.P_USERNAME));
        loginData
                .setServer(preferences.getString(CollabPrefConstants.P_SERVER));
        loginData
                .setStatus(preferences.getString(CollabPrefConstants.P_STATUS));
        loginData.setMessage(preferences
                .getString(CollabPrefConstants.P_MESSAGE));
    }
}
