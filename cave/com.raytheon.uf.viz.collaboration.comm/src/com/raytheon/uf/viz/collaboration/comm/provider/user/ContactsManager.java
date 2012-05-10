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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.core.user.User;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.ecf.presence.roster.IRosterItem;
import org.eclipse.ecf.presence.roster.RosterEntry;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ContactsManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContactsManager.class);

    private Map<UserId, IRosterEntry> usersMap;

    public ContactsManager(CollaborationConnection connection) {
        usersMap = new HashMap<UserId, IRosterEntry>();
        IRoster roster = connection.getRosterManager().getRoster();

        for (Object ob : roster.getItems()) {
            IRosterItem rosterItem = (IRosterItem) ob;
            if (rosterItem instanceof IRosterGroup) {
                IRosterGroup rosterGroup = (IRosterGroup) rosterItem;
                for (Object rOb : rosterGroup.getEntries()) {
                    IRosterEntry rosterEntry = (IRosterEntry) rOb;
                    usersMap.put(
                            IDConverter.convertFrom(rosterEntry.getUser()),
                            rosterEntry);
                }
            }
        }

        // Orphan users not in any group.
        Collection<?> items = roster.getItems();

        for (IRosterItem rosterItem : items.toArray(new IRosterItem[0])) {
            if (rosterItem instanceof IRosterEntry) {
                IRosterEntry rosterEntry = (IRosterEntry) rosterItem;
                usersMap.put(IDConverter.convertFrom(rosterEntry.getUser()),
                        rosterEntry);
            }
        }
        User user = null;
        try {
            user = new User(connection.createID(connection.getUser()));
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        RosterEntry me = new RosterEntry(roster, user, connection.getPresence());
        me.setPresence(connection.getPresence());
        usersMap.put(connection.getUser(), me);
    }

    public Map<UserId, IRosterEntry> getUsersMap() {
        return usersMap;
    }

    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent event) {
        final IRosterChangeEvent rosterChangeEvent = event;
        // TODO update the event's user groups here for the desired type
        IRosterItem rosterItem = rosterChangeEvent.getItem();
        if (rosterItem instanceof IRosterEntry) {
            IRosterEntry rosterEntry = (IRosterEntry) rosterItem;
            IPresence presence = rosterEntry.getPresence();
            switch (rosterChangeEvent.getType()) {
            case ADD:
                UserId id = IDConverter.convertFrom(rosterEntry.getUser());
                if (!usersMap.containsKey(id)) {
                    usersMap.put(id, rosterEntry);
                }
                break;
            case DELETE:
                // Assume user no longer exists and remove.
                usersMap.remove(rosterEntry);
                break;
            case MODIFY:
                // Assume only the presence needs to be updated.
                if (presence == null) {
                    // Nothing to do don't bother doing eventBus post.
                    return;
                }
                for (IUser i : usersMap.keySet()) {
                    id = null;
                    if (i.getID() == null) {
                        id = (UserId) i;
                    } else {
                        id = IDConverter.convertFrom(i);
                    }
                    if (rosterEntry.getUser().equals(id)) {
                        usersMap.put(id, rosterEntry);
                        break;
                    }
                }
                break;
            case PRESENCE:
                break;
            default:
                statusHandler.handle(Priority.PROBLEM, "Unhandled type: "
                        + rosterChangeEvent.getType());
                return;

            }
        }
    }
}
