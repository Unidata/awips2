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
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SubRequestDialog extends CaveSWTDialog {
    private final String NEW_GROUP = "New Group...";

    private final String userid;

    private Combo groupCbo;

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
    protected void initializeComponents(Shell shell) {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label msgLbl = new Label(mainComp, SWT.NONE);
        msgLbl.setText(userid + " wants to add you to their contacts list.");
        msgLbl.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite groupComp = new Composite(mainComp, SWT.NONE);
        groupComp.setLayout(gl);
        groupComp.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Label groupLbl = new Label(groupComp, SWT.NONE);
        groupLbl.setText("Group: ");
        groupLbl.setLayoutData(gd);
        groupCbo = new Combo(groupComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        groupCbo.setItems(getGroupNames());
        groupCbo.select(0);
        groupCbo.setLayout(gl);
        groupCbo.setLayoutData(gd);

        gl = new GridLayout(2, false);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(mainComp, SWT.NONE);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        int btnWidth = 75;

        gd = new GridData(btnWidth, SWT.DEFAULT);
        Button allowBtn = new Button(btnComp, SWT.PUSH);
        allowBtn.setText("Allow");
        allowBtn.setLayoutData(gd);
        allowBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                action(true);
            }
        });

        gd = new GridData(btnWidth, SWT.DEFAULT);
        Button denyBtn = new Button(btnComp, SWT.PUSH);
        denyBtn.setText("Deny");
        denyBtn.setLayoutData(gd);
        denyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                action(false);
            }
        });
    }

    /**
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
        groupList.add(0, NEW_GROUP);

        return groupList.toArray(new String[groupList.size()]);
    }

    /**
     * Action handler.
     * 
     * @param approved
     *            true if request approved, false if denied
     */
    private void action(boolean approved) {
        if (approved) {
            if (groupCbo.getSelectionIndex() == 0) {
                // new group
                CreateGroupDialog dialog = new CreateGroupDialog(Display
                        .getCurrent().getActiveShell());
                dialog.open();
                String group = dialog.getNewGroup();
                setReturnValue(group);
            } else {
                setReturnValue(groupCbo.getItem(groupCbo.getSelectionIndex()));
            }
        } else {
            setReturnValue(null);
        }

        close();
    }
}
