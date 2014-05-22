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
import org.jivesoftware.smack.RosterEntry;

import com.raytheon.uf.viz.collaboration.comm.identity.event.RosterChangeType;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.event.RosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.icon.IconUtil;

/**
 * Action to remove entry from roster on server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2013 2563       bclement     Initial creation
 * Mar 05, 2014 2837       bclement     changed wording from Roster to Contacts, added image
 * Apr 24, 2014 3070       bclement     RosterChangeEvent changes
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class RemoveFromRosterAction extends Action {

    private final RosterEntry entry;

    public RemoveFromRosterAction(RosterEntry entry) {
        super("Remove From Contacts", IconUtil.getImageDescriptor(Activator
                .getDefault().getBundle(), "remove_contact.gif"));
        this.entry = entry;
    }

    @Override
    public void run() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        ContactsManager manager = connection
                .getContactsManager();
        manager.removeFromRoster(entry);
        UserId entryId = IDConverter.convertFrom(entry);
        connection.postEvent(new RosterChangeEvent(RosterChangeType.DELETE,
                entryId));
    }
}
