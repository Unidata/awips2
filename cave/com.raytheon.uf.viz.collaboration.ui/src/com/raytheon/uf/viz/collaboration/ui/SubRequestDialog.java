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

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.jivesoftware.smack.RosterGroup;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Dialog to respond to a chat subscription request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 27, 2014 2700       bclement     Initial creation
 * Jan 31, 2014 2700       bclement     don't prompt for group if user is already in one
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SubRequestDialog extends Dialog {
    
    private final String title;
    
    private final String message;

    private final UserId userid;

    private Combo groupCombo;

    /**
     * @param parentShell
     */
    public SubRequestDialog(Shell parentShell, String title, String message,
            UserId userid) {
        super(parentShell);
        this.title = title;
        this.message = message;
        this.userid = userid;
    }

    /**
     * @param parentShell
     */
    public SubRequestDialog(IShellProvider parentShell, String title,
            String message, UserId userid) {
        super(parentShell);
        this.title = title;
        this.message = message;
        this.userid = userid;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.OK_ID) {
            int count = groupCombo.getItemCount();
            int index = groupCombo.getSelectionIndex();
            String group = null;
            if ( index == count - 1){
                // new group
                CreateGroupDialog dialog = new CreateGroupDialog(Display
                        .getCurrent().getActiveShell());
                dialog.open();
                group = dialog.getNewGroup();
            } else if ( index >= 0){
                group = groupCombo.getItem(index);
            } 
            CollaborationConnection connection = CollaborationConnection.getConnection();
            if ( group != null && connection != null){
               ContactsManager cm = connection.getContactsManager();
                cm.addToGroup(group, userid);
            }
        }
        super.buttonPressed(buttonId);
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite container = (Composite) super.createDialogArea(parent);

        new Label(container, SWT.NONE).setText(message);

        Composite groupComposite = new Composite(container, SWT.NONE);
        groupComposite.setLayout(new GridLayout(2, false));
        groupComposite.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT,
                true, false));
        new Label(groupComposite, SWT.NONE).setText("Group: ");
        groupCombo = new Combo(groupComposite, SWT.BORDER | SWT.READ_ONLY
                | SWT.DROP_DOWN);
        groupCombo.setItems(getGroupNames());
        CollaborationConnection conn = CollaborationConnection.getConnection();
        Collection<RosterGroup> groups = conn.getContactsManager().getGroups(
                userid);
        if (!groups.isEmpty()) {
            // we already have this user in a group in our roster, no need to
            // prompt
            groupComposite.setVisible(false);
        }

        return container;
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
        Collection<RosterGroup> groups = connection.getContactsManager()
                .getGroups();
        String[] rval = new String[groups.size() + 1];
        Iterator<RosterGroup> iter = groups.iterator();
        int i = 0;
        for (; iter.hasNext(); ++i) {
            rval[i] = iter.next().getName();
        }
        rval[i] = "New Group...";
        return rval;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(title);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Allow", true);
        createButton(parent, IDialogConstants.CANCEL_ID, "Deny", false);
    }

}
