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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.jivesoftware.smack.RosterGroup;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog to respond to a chat subscription request. Returns the name of the
 * group if accepted, null for denial of request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 27, 2014 2700       bclement     Initial creation
 * Jan 31, 2014 2700       bclement     don't prompt for group if user is already in one
 * Feb 13, 2014 2755       bclement     roster addition now done in account manager, user input passed back
 * Apr 07, 2014 2785       mpduff       Changed to implement CaveSWTDialog
 *                                      Fix loading of groups
 * Apr 23, 2014 3040       lvenable     Cleaned up dialog code/layout.  Allow the cancellation of the create
 *                                      group dialog without closing this dialog.  Added capability to resize
 *                                      the group combo box if the names get too long.
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SubRequestDialog extends CaveSWTDialog {

    /** User ID. */
    private final String userid;

    /** Combo listing all of the available groups. */
    private Combo groupCbo;

    /** Create group dialog. */
    private CreateGroupDialog createGroupDlg;

    /** Allow button. */
    private Button allowBtn;

    /**
     * Constructor
     * 
     * @param parentShell
     * @param userid
     */
    public SubRequestDialog(Shell parentShell, String userid) {
        super(parentShell, SWT.DIALOG_TRIM);
        this.userid = userid;
        setText("Contact Request");
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
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        /*
         * Top Label
         */
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label msgLbl = new Label(mainComp, SWT.NONE);
        msgLbl.setText(userid + " wants to add you to a contacts list:");
        msgLbl.setLayoutData(gd);

        /*
         * Group composite and controls.
         */
        Composite groupComp = new Composite(mainComp, SWT.NONE);
        gl = new GridLayout(3, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        groupComp.setLayout(gl);
        groupComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label groupLbl = new Label(groupComp, SWT.NONE);
        groupLbl.setText("Group: ");
        groupLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.minimumWidth = 130;
        groupCbo = new Combo(groupComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        groupCbo.setItems(getGroupNames());
        groupCbo.select(0);
        groupCbo.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 5;
        Button newGroup = new Button(groupComp, SWT.PUSH);
        newGroup.setText("New Group...");
        newGroup.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleNewGroupAction();
            }
        });

        addSeparator(mainComp);

        /*
         * Action buttons.
         */
        gl = new GridLayout(2, false);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(mainComp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        int btnWidth = 75;

        gd = new GridData(btnWidth, SWT.DEFAULT);
        allowBtn = new Button(btnComp, SWT.PUSH);
        allowBtn.setText("Allow");
        allowBtn.setLayoutData(gd);
        allowBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleAllowDenyAction(true);
            }
        });

        // Disable the allow button if there are no items in the combo box.
        if (groupCbo.getItemCount() == 0) {
            allowBtn.setEnabled(false);
        }

        gd = new GridData(btnWidth, SWT.DEFAULT);
        Button denyBtn = new Button(btnComp, SWT.PUSH);
        denyBtn.setText("Deny");
        denyBtn.setLayoutData(gd);
        denyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleAllowDenyAction(false);
            }
        });
    }

    /**
     * Get the list of group names.
     * 
     * @return list of existing group names
     */
    private String[] getGroupNames() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return new String[0];
        }
        Collection<RosterGroup> rosterGroups = connection.getContactsManager()
                .getGroups();
        List<String> groupList = new ArrayList<String>(rosterGroups.size());
        for (RosterGroup group : rosterGroups) {
            groupList.add(group.getName());
        }

        Collections.sort(groupList);

        return groupList.toArray(new String[groupList.size()]);
    }

    /**
     * Handle adding a new group.
     */
    private void handleNewGroupAction() {
        if (createGroupDlg == null || createGroupDlg.isDisposed()) {
            createGroupDlg = new CreateGroupDialog(Display.getCurrent()
                    .getActiveShell());
            createGroupDlg.open();
            String groupName = createGroupDlg.getNewGroup();

            // If the group name is not null, add it to the combo and then
            // select it.
            if (groupName != null) {
                allowBtn.setEnabled(true);
                groupCbo.add(groupName, 0);
                groupCbo.select(0);
                shell.pack();
            }
        } else {
            createGroupDlg.bringToTop();
        }
    }

    /**
     * Handle Allow/Deny action.
     * 
     * @param allowRequest
     *            True if request allowed, false if denied
     */
    private void handleAllowDenyAction(boolean allowRequest) {
        if (allowRequest) {
            if (groupCbo.getItemCount() != 0) {
                setReturnValue(groupCbo.getItem(groupCbo.getSelectionIndex()));
            } else {

            }
        } else {
            setReturnValue(null);
        }

        close();
    }

    /**
     * Add a line separator to the given composite.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
}
