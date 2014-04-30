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
package com.raytheon.uf.viz.collaboration.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog allowing user to create a new group in the roster. At least one user
 * must be added to the group afterwards for it to be persisted on the server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012            bsteffen     Initial creation
 * Jan 24, 2014 2701       bclement     removed local groups
 * Apr 23, 2014 3040       lvenable     Cleaned up dialog code/layout.  Added check for group name.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CreateGroupDialog extends CaveSWTDialog {

    /** Name text field. */
    private Text nameText;

    /** New group name. */
    private String newGroup = null;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public CreateGroupDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Create Group");
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite entryComp = new Composite(shell, SWT.NONE);
        entryComp.setLayout(new GridLayout(2, false));
        entryComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        new Label(entryComp, SWT.NONE).setText("Group Name: ");
        nameText = new Text(entryComp, SWT.BORDER);
        nameText.setLayoutData(new GridData(150, SWT.DEFAULT));
        nameText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                    handleOkAction();
                }
            }
        });

        /*
         * Action buttons
         */
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        Button okButton = new Button(buttonComp, SWT.PUSH);
        okButton.setText("OK");
        okButton.setLayoutData(gd);
        okButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOkAction();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        Button cancelButton = new Button(buttonComp, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(gd);
        cancelButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /**
     * Handle the OK action.
     */
    private void handleOkAction() {
        if (validGroupName() == false) {
            return;
        }
        newGroup = nameText.getText();
        CollaborationConnection.getConnection().getContactsManager()
                .createGroup(newGroup);
        close();
    }

    /**
     * Check if there was something entered in the text field.
     * 
     * @return True if there is text in the group name text field.
     */
    private boolean validGroupName() {
        if (nameText.getText().length() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
            mb.setText("Invalid Name");
            mb.setMessage("You have not entered a group name.  Please enter one.");
            mb.open();
            return false;
        }

        return true;
    }

    /**
     * Get the group name.
     * 
     * @return The group name.
     */
    public String getNewGroup() {
        return newGroup;
    }
}
