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
package com.raytheon.uf.viz.useradmin.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.viz.plugin.nwsauth.FileManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The User Admin new user/role dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class NewDlg extends CaveSWTDialog {
    private String type;

    private Text newTextField;

    private Text description;

    private String application;
    
    /**
     * Constructor.
     * 
     * @param parent
     *            parent shell
     * @param type
     *            type (role/user)
     * @param application
     *            application name
     */
    public NewDlg(Shell parent, String type, String application) {
        super(parent, SWT.DIALOG_TRIM);
        this.type = type;
        this.application = application;
        setText("Add New " + type);
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
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        shell.setLayout(gl);
        shell.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        int textWidth = 225;
        GridData textData = new GridData(textWidth, SWT.DEFAULT);

        Label label = new Label(mainComp, SWT.NONE);
        label.setText("Enter New " + type + ":");

        newTextField = new Text(mainComp, SWT.BORDER);
        newTextField.setLayoutData(textData);

        if (type.equalsIgnoreCase("Role")) {
            Label descLbl = new Label(mainComp, SWT.NONE);
            descLbl.setText("Enter Role Description:");

            textWidth = 250;
            int textHeight = 150;
            GridData descriptionData = new GridData(textWidth, textHeight);

            description = new Text(mainComp, SWT.BORDER | SWT.MULTI | SWT.WRAP);
            description.setLayoutData(descriptionData);

        }

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gl = new GridLayout(2, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOK();
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(btnData);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setReturnValue(false);
                close();
            }
        });
    }

    private void handleOK() {
        if (newTextField.getText() != null && newTextField.getText().length() > 0) {
            FileManager manager = FileManager.getInstance();
            NwsRoleData roleData = manager.getRoleData(application);
            if (type.equalsIgnoreCase("User")) {
                roleData.addUser(newTextField.getText().trim());
            } else if (type.equalsIgnoreCase("Role")) {
                roleData.addRole(newTextField.getText().trim(), description.getText().trim());
            } else if (type.equalsIgnoreCase("Permission")) {
                roleData.addPermission(newTextField.getText().trim());
            }
        }
        Shell parent = shell.getParent().getShell();
        String value = newTextField.getText().trim();
        close();
        
        ManageUserDlg mud = new ManageUserDlg(parent, type, value, application);
        boolean changes = (Boolean) mud.open();
        setReturnValue(changes);
    }
}
