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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.jivesoftware.smack.RosterEntry;

import com.raytheon.uf.viz.collaboration.comm.identity.event.RosterChangeType;
import com.raytheon.uf.viz.collaboration.comm.provider.event.RosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CreateGroupDialog;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Add a user or users to a local group.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            bsteffen     Initial creation
 * Dec 20, 2013 2563       bclement    added support for ungrouped roster entries
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class AddToGroupAction extends Action {

    private final String group;

    private final UserId[] users;

    private RosterEntry entry;

    /**
     * This action will create a menu of groups to which the users will be added
     * 
     * @param users
     */
    public AddToGroupAction(UserId... users) {
        super("Add To Group...", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "add_group.gif"));
        this.users = users;
        this.group = null;
        this.setMenuCreator(new MenuCreator());
    }

    public AddToGroupAction(String group, UserId... users) {
        super(group, IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "local_group.gif"));
        this.group = group;
        this.users = users;
    }

    /**
     * Action for roster entry not currently in a group.
     * 
     * @param entry
     */
    public AddToGroupAction(RosterEntry entry) {
        this(IDConverter.convertFrom(entry));
        this.entry = entry;
    }

    @Override
    public void run() {
        String group = this.group;
        if (group == null) {
            CreateGroupDialog dialog = new CreateGroupDialog(Display
                    .getCurrent().getActiveShell());
            dialog.open();
            group = dialog.getNewGroup();
            if (group == null) {
                return;
            }
        }
        CollaborationConnection connection = CollaborationConnection.getConnection();
        for (UserId user : users) {
            connection.getContactsManager()
                    .addToLocalGroup(group, user);
        }
        if (entry != null) {
            // the entry wasn't in a group, so the entire tree needs to be
            // refreshed
            connection.postEvent(new RosterChangeEvent(RosterChangeType.MODIFY,
                    entry));
        }
    }

    private class MenuCreator implements IMenuCreator {

        private Menu menu;

        @Override
        public void dispose() {
            menu.dispose();
        }

        @Override
        public Menu getMenu(Control parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        private void fill() {
            ContactsManager contactsMgr = CollaborationConnection
                    .getConnection().getContactsManager();
            List<LocalGroup> groups = contactsMgr.getLocalGroups();
            List<LocalGroup> usedGroups = new ArrayList<LocalGroup>(groups);
            for (UserId user : users) {
                usedGroups.retainAll(contactsMgr.getLocalGroups(user));
            }
            groups.removeAll(usedGroups);
            for (LocalGroup group : groups) {
                AddToGroupAction action = new AddToGroupAction(group.getName(),
                        users);
                action.setEntry(entry);
                IContributionItem contrib = new ActionContributionItem(action);
                contrib.fill(menu, -1);
            }
            Action action = new CreateGroupAction(users);
            IContributionItem contrib = new ActionContributionItem(action);
            contrib.fill(menu, -1);
        }
    }

    /**
     * @return the entry
     */
    public RosterEntry getEntry() {
        return entry;
    }

    /**
     * @param entry
     *            the entry to set
     */
    public void setEntry(RosterEntry entry) {
        this.entry = entry;
    }

}
