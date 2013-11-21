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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CreateGroupDialog;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Create a new local group, if the action is given users they will be added to
 * the new group.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CreateGroupAction extends Action {

    private final UserId[] users;

    public CreateGroupAction() {
        super("Create Group", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "add_group.gif"));
        this.users = new UserId[0];
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    public CreateGroupAction(UserId... users) {
        super("New Group...", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "add_group.gif"));
        this.users = users;
    }

    @Override
    public void run() {
        CreateGroupDialog dialog = new CreateGroupDialog(Display.getCurrent()
                .getActiveShell());
        dialog.open();
        String group = dialog.getNewGroup();
        if (group == null) {
            return;
        }
        for (UserId user : users) {
            CollaborationConnection.getConnection().getContactsManager()
                    .addToLocalGroup(group, user);
        }
    }
}
