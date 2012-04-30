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

import java.util.HashMap;
import java.util.Map;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterEntry;
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

        for (IRosterGroup rosterGroup : roster.getGroups()) {
            for (IRosterEntry rosterEntry : rosterGroup.getEntries()) {
                System.out.println("Adding user " + rosterEntry.getUser());
                usersMap.put(rosterEntry.getUser(), rosterEntry);
            }
        }

        // Orphan users not in any group.
        for (IRosterEntry rosterEntry : roster.getEntries()) {
            usersMap.put(rosterEntry.getUser(), rosterEntry);
        }

        RosterEntry me = new RosterEntry(connection.getUser());
        me.setPresence(connection.getPresence());
        usersMap.put(me.getUser(), me);
    }

    public Map<UserId, IRosterEntry> getUsersMap() {
        return usersMap;
    }

    @Subscribe
    public void handleRosterChangeEvent(IRosterChangeEvent event) {
        final IRosterChangeEvent rosterChangeEvent = event;
        // TODO update the event's user groups here for the desired type
        IRosterEntry rosterEntry = rosterChangeEvent.getEntry();
        IPresence presence = rosterChangeEvent.getEntry().getPresence();
        // System.out.println("Roster change event " + rosterEntry.getUser());
        switch (rosterChangeEvent.getType()) {
        case ADD:
            if (!usersMap.containsKey(rosterEntry.getUser())) {
                usersMap.put(rosterEntry.getUser(), rosterEntry);
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
            for (UserId id : usersMap.keySet()) {
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
