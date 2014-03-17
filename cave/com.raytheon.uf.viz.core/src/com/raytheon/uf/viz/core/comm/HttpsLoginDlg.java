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
package com.raytheon.uf.viz.core.comm;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

/**
 * CAVE Login Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 06, 2013    1786     mpduff      Initial creation
 * Feb 10, 2014    2704     njensen     Allow message to expand size of dialog
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class HttpsLoginDlg extends Dialog {

    private static final long serialVersionUID = 1L;

    private Shell shell;

    private Display display;

    /** User name text field */
    private Text userText;

    /** Password text field */
    private Text passwdText;

    /** Array of User name and Password */
    private String[] returnValue;

    /** Authorization message */
    private final String message;

    /**
     * Constructor
     * 
     * @param message
     *            Message from server
     */
    public HttpsLoginDlg(String message) {
        super(new Shell(Display.getDefault(), SWT.TITLE));
        this.message = message.replace("\"", "");
    }

    /**
     * Open the dialog.
     */
    public void open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Log in");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        shell.setLayout(mainLayout);

        init();

        shell.pack();

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }

    /**
     * Build the gui
     */
    private void init() {
        Composite comp = new Composite(shell, SWT.NONE);
        comp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(SWT.RIGHT, SWT.None, true, true);
        if (message == null || message.length() < 50) {
            gd.widthHint = 500;
        }
        gd.horizontalSpan = 2;

        Label authMessage = new Label(comp, SWT.CENTER);
        authMessage.setText(this.message);
        authMessage.setLayoutData(gd);

        Label userIdLbl = new Label(comp, SWT.LEFT);
        userIdLbl.setText("User Name: ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        userText = new Text(comp, SWT.BORDER);
        userText.setLayoutData(gd);

        Label passLbl = new Label(comp, SWT.LEFT);
        passLbl.setText("Password: ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        passwdText = new Text(comp, SWT.BORDER);
        passwdText.setEchoChar('*');
        passwdText.setLayoutData(gd);

        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        buttonComp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.NONE);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        shell.setDefaultButton(okBtn);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (validateEntries()) {
                    returnValue = new String[] { userText.getText().trim(),
                            passwdText.getText().trim() };
                }
                shell.dispose();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.NONE);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cancel();
            }
        });
    }

    /**
     * Validate the entries.
     * 
     * @return true if entries are valid
     */
    private boolean validateEntries() {
        boolean valid = true;
        if (this.userText.getText().trim().isEmpty()) {
            valid = false;
        }

        if (this.passwdText.getText().trim().isEmpty()) {
            valid = false;
        }

        return valid;
    }

    /**
     * Get the user's credentials
     * 
     * @return String array of Username and password
     */
    public String[] getCredentials() {
        return returnValue;
    }

    /**
     * Cancel action
     */
    private void cancel() {
        // Check if CAVE is running and exit if it is not
        if (!PlatformUI.isWorkbenchRunning()) {
            System.exit(0);
        }
        shell.dispose();
    }

    /**
     * Main for testing
     * 
     * @param args
     *            args
     */
    public static void main(String[] args) {
        HttpsLoginDlg loginDlg = new HttpsLoginDlg(
                "AWIPS II Thin Client Proxy: Use NOAA email address and password");
        loginDlg.open();
        String[] credentials = loginDlg.getCredentials();
        if (credentials != null) {
            System.out.println(credentials[0] + "  " + credentials[1]);
        } else {
            System.out.println("Nothing entered");
        }
    }
}