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
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
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
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CreateGroupDialog extends CaveSWTDialog {

    private Text nameText;

    private String newGroup = null;

    public CreateGroupDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM);
        setText("Create Group");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite entryComp = new Composite(shell, SWT.NONE);
        RowLayout layout = new RowLayout(SWT.HORIZONTAL);
        layout.center = true;
        entryComp.setLayout(layout);
        new Label(entryComp, SWT.NONE).setText("Group Name: ");
        nameText = new Text(entryComp, SWT.BORDER);
        nameText.setLayoutData(new RowData(100, SWT.DEFAULT));
        nameText.addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
                    finish();
                }
            }

        });
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayoutData(new GridData(SWT.RIGHT, SWT.NONE, false,
                false, 1, 1));
        layout = new RowLayout(SWT.HORIZONTAL);
        layout.pack = false;
        buttonComp.setLayout(layout);
        Button okButton = new Button(buttonComp, SWT.PUSH);
        okButton.setText("OK");
        okButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                finish();
            }

        });

        Button cancelButton = new Button(buttonComp, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }

        });
    }

    private void finish() {
        newGroup = nameText.getText();
        CollaborationConnection.getConnection().getContactsManager()
                .createGroup(newGroup);
        close();
    }

    public String getNewGroup() {
        return newGroup;
    }

}
