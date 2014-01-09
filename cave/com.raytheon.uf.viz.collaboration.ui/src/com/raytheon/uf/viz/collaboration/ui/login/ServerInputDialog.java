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

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.google.common.net.HostAndPort;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Dialog for users to add new servers to options in login dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2014  2563      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class ServerInputDialog extends Dialog {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(ServerInputDialog.class);

    private static final int DEFAULT_XMPP_PORT = 5222;

    private static final int TIMEOUT = 5000; // 5 seconds

    private static final String DEFAULT_DESCRIPTION = "Custom Server";

    private FieldEditorPreferencePage page;

    private String description;

    private String serverAddress;

    private boolean persist;

    private StringFieldEditor descriptionEditor;

    private StringFieldEditor serverAddressEditor;

    private BooleanFieldEditor persistEditor;

    private Text errorMessage;

    /**
     * @param parentShell
     */
    public ServerInputDialog(Shell parentShell) {
        super(parentShell);
    }

    /**
     * @param parentShell
     */
    public ServerInputDialog(IShellProvider parentShell) {
        super(parentShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        page = new FieldEditorPreferencePage(FieldEditorPreferencePage.GRID) {
            @Override
            protected void createFieldEditors() {
                Composite parent = getFieldEditorParent();
                descriptionEditor = new StringFieldEditor("",
                        "Server Description:", parent);
                descriptionEditor.setStringValue(DEFAULT_DESCRIPTION);
                addField(descriptionEditor);
                serverAddressEditor = new StringFieldEditor("",
                        "Server Address:", parent);
                addField(serverAddressEditor);
                persistEditor = new BooleanFieldEditor("",
                        "Persist to user localization", parent);
            }

            @Override
            public void createControl(Composite parent) {
                noDefaultAndApplyButton();
                super.createControl(parent);
            }

        };
        page.createControl(composite);
        Control pageControl = page.getControl();
        GridData layoutData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        layoutData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        pageControl.setLayoutData(layoutData);

        errorMessage = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
        errorMessage.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        Display display = errorMessage.getDisplay();
        errorMessage.setBackground(display
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));
        errorMessage.setForeground(display.getSystemColor(SWT.COLOR_RED));

        serverAddressEditor.setFocus();
        return pageControl;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.OK_ID) {
            this.description = descriptionEditor.getStringValue();
            if (isBlank(this.description)) {
                this.description = DEFAULT_DESCRIPTION;
            }
            this.serverAddress = serverAddressEditor.getStringValue();
            String error = null;
            if (isBlank(this.serverAddress)) {
                error = "Server address cannot be blank";
            } else {
                this.serverAddress = processAddress(this.serverAddress);
                error = validate(this.serverAddress);
            }
            if (error != null) {
                errorMessage.setText(error);
                errorMessage.setEnabled(true);
                errorMessage.setVisible(true);
                errorMessage.getParent().update();
                return;
            }
            this.persist = persistEditor.getBooleanValue();
        }
        super.buttonPressed(buttonId);
    }

    /**
     * First pass at user input processing
     * 
     * @param input
     *            must not be null or blank
     * @return
     */
    private String processAddress(String input) {
        HostAndPort address = HostAndPort.fromString(input.trim());
        String host = getFullName(address);
        if (address.hasPort()) {
            return HostAndPort.fromParts(host, address.getPort()).toString();
        } else {
            return host;
        }
    }

    /**
     * @param input
     * @return true if input is null, empty or only contains whitespace
     */
    public static boolean isBlank(String input) {
        // this could be replaced with apache commons stringutils if we bring in
        // the dependency
        return input == null || input.trim().isEmpty();
    }

    /**
     * Validate server by parsing the string and attempting to ping the server.
     * Returns an error string if server string could not be parsed or server
     * could not be contacted.
     * 
     * @param server
     * @return null if there were no issues
     */
    public static String validate(String server) {
        Socket s = null;
        try {
            HostAndPort hnp = HostAndPort.fromString(server).withDefaultPort(
                    DEFAULT_XMPP_PORT);
            s = new Socket();
            s.setReuseAddress(true);
            SocketAddress sa = new InetSocketAddress(hnp.getHostText(),
                    hnp.getPort());
            s.connect(sa, TIMEOUT);
            return null;
        } catch (Exception e) {
            return "Unable to connect to server: " + e.getLocalizedMessage();
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (IOException e) {
                    // no action
                }
            }
        }
    }

    /**
     * Attempt to get canonical host name for server
     * 
     * @param server
     * @return input host text if unable to get full name
     */
    public static String getFullName(HostAndPort address) {
        String hostText = address.getHostText();
        try {
            return InetAddress.getByName(hostText).getCanonicalHostName();
        } catch (Exception e) {
            // not severe because we will test if the host exists later
            log.info(String.format(
                    "Unable to get full name for host '%s' with reason: %s",
                    hostText, e.getLocalizedMessage()));
            return hostText;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Add Server");
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @return the serverAddress
     */
    public String getServerAddress() {
        return serverAddress;
    }

    /**
     * @return true if new address should be persisted to localization
     */
    public boolean isPersist() {
        return persist;
    }

}
