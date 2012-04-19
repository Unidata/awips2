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
package com.raytheon.uf.viz.collaboration.comm.provider.roster;

import java.util.Collection;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.roster.IRosterSubscriptionSender;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IRosterListener;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class RosterManager implements IRosterManager {

    private IRoster roster;

    private org.eclipse.ecf.presence.roster.IRoster baseRoster;

    private CollaborationConnection sessionManager;

    /**
     * 
     * @param roster
     */
    public RosterManager(CollaborationConnection manager) {
        sessionManager = manager;
        updateRoster();
    }

    /**
     * 
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager#getRoster()
     */
    @Override
    public IRoster getRoster() {
        return roster;
    }

    private void updateRoster() {
        baseRoster = sessionManager.getPresenceContainerAdapter()
                .getRosterManager().getRoster();
        roster = toLocalRoster(baseRoster);
    }

    /**
     * 
     * @param listener
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager#addRosterListener(com.raytheon.uf.viz.collaboration.comm.identity.listener.IRosterListener)
     */
    @Override
    public IRosterListener addRosterListener(IRosterListener listener) {
        return null;
    }

    /**
     * 
     * @return Collection of Roster Listeners.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager#getRosterListeners()
     */
    @Override
    public Collection<IRosterListener> getRosterListeners() {
        return null;
    }

    /**
     * 
     * @param listener
     *            A listener to remove.
     * @return The listener that was removed.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager#removeRosterListener(com.raytheon.uf.viz.collaboration.comm.identity.listener.IRosterListener)
     */
    @Override
    public IRosterListener removeRosterListener(IRosterListener listener) {
        return null;
    }

    /**
     * 
     * @param account
     * @param nickName
     * @param groups
     */
    @Override
    public void sendRosterAdd(String account, String nickName, String[] groups)
            throws CollaborationException {
        org.eclipse.ecf.presence.roster.IRosterManager manager = baseRoster
                .getPresenceContainerAdapter().getRosterManager();

        IRosterSubscriptionSender sender = manager
                .getRosterSubscriptionSender();

        try {
            sender.sendRosterAdd(account, nickName, groups);
        } catch (ECFException e) {
            throw new CollaborationException();
        }
    }

    /**
     * 
     * @param userId
     */
    @Override
    public void sendRosterRemove(UserId userId) throws CollaborationException {

        IPresenceContainerAdapter adapter = baseRoster
                .getPresenceContainerAdapter();
        org.eclipse.ecf.presence.roster.IRosterManager manager = adapter
                .getRosterManager();

        IRosterSubscriptionSender sender = manager
                .getRosterSubscriptionSender();

        ID id = sessionManager.createID(userId.getFQName());
        try {
            sender.sendRosterRemove(id);
        } catch (ECFException e) {
            e.printStackTrace();
            throw new CollaborationException();
        }

    }

    /**
     * 
     * @param fromId
     * @param presence
     */
    public void updateEntry(UserId fromId, IPresence presence) {
        RosterEntry re = new RosterEntry(fromId);
        re.setPresence(presence);

        IRosterEntry modified = roster.modifyRosterEntry(re);
        if (modified != null) {
            sessionManager.getEventPublisher().post(re);
        }
    }

    /**
     * 
     * @param fromId
     * @param presence
     */
    public void updateEntry(IRosterEntry entry) {
        IRosterEntry modified = roster.modifyRosterEntry(entry);
        if (modified != null) {
            sessionManager.getEventPublisher().post(entry);
        }
    }

    /**
     * 
     * @param roster
     * @return
     */
    public IRoster toLocalRoster(org.eclipse.ecf.presence.roster.IRoster roster) {
        Roster newRoster = null;

        if (roster != null) {
            UserId id = IDConverter.convertFrom(roster.getUser());
            newRoster = new Roster(id, this);

            @SuppressWarnings("rawtypes")
            Collection items = roster.getItems();
            for (Object o : items) {
                if (o instanceof org.eclipse.ecf.presence.roster.IRosterEntry) {
                    org.eclipse.ecf.presence.roster.IRosterEntry entry = (org.eclipse.ecf.presence.roster.IRosterEntry) o;

                    id = IDConverter.convertFrom(entry.getUser());
                    RosterEntry re = new RosterEntry(id);
                    if (!newRoster.getEntries().contains(re)) {
                        IPresence p = Presence.convertPresence(entry
                                .getPresence());
                        re.setPresence(p);
                        newRoster.addRosterEntry(re);
                    }
                } else if (o instanceof org.eclipse.ecf.presence.roster.IRosterGroup) {
                    org.eclipse.ecf.presence.roster.IRosterGroup group = (org.eclipse.ecf.presence.roster.IRosterGroup) o;

                    RosterGroup newGroup = new RosterGroup(group.getName(),
                            null, newRoster);

                    newRoster.populateGroup(newGroup, group.getEntries());
                    newRoster.addGroup(newGroup);
                } else {
                    System.out.println("RosterManager.toLocalRoster "
                            + o.getClass().getName());
                }
            }
        }
        return newRoster;
    }

    /**
     * 
     * @return
     */
    public CollaborationConnection getSessionManager() {
        return sessionManager;
    }

}
