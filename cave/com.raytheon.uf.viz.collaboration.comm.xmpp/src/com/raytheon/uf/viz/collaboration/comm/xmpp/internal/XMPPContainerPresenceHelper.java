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
package com.raytheon.uf.viz.collaboration.comm.xmpp.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.core.user.User;
import org.eclipse.ecf.internal.provider.xmpp.events.IQEvent;
import org.eclipse.ecf.internal.provider.xmpp.events.PresenceEvent;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresenceListener;
import org.eclipse.ecf.presence.IPresenceSender;
import org.eclipse.ecf.presence.roster.IRoster;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.ecf.presence.roster.IRosterItem;
import org.eclipse.ecf.presence.roster.IRosterListener;
import org.eclipse.ecf.presence.roster.IRosterManager;
import org.eclipse.ecf.presence.roster.IRosterSubscriptionListener;
import org.eclipse.ecf.presence.roster.IRosterSubscriptionSender;
import org.eclipse.ecf.provider.xmpp.XMPPContainer;
import org.eclipse.ecf.provider.xmpp.identity.XMPPID;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.RosterPacket;

/**
 * Viz specific implementation of the XMPPContainerPresenceHelper which fixes a
 * bug that causes users to disappear from rosters when they log out when they
 * are in multiple groups. This class no handles all presence events when a user
 * becomes unavaialble, all other events are handled by the ECF version.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class XMPPContainerPresenceHelper extends
        org.eclipse.ecf.internal.provider.xmpp.XMPPContainerPresenceHelper {

    /**
     * Have our own roster manager to track listeners.
     */
    private VizRosterManager rosterManager;

    public XMPPContainerPresenceHelper(XMPPContainer container) {
        super(container);
        rosterManager = new VizRosterManager();
    }

    /**
     * This is overriden because when a roster packet is delivered with a new
     * name then the ecf implementation does not change the name, this will
     * change it before letting the ecf connection handle the event normally.
     */
    @Override
    protected void handleIQEvent(IQEvent evt) {
        final IQ iq = evt.getIQ();
        if (iq instanceof RosterPacket) {
            final RosterPacket rosterPacket = (RosterPacket) iq;
            if (rosterPacket.getType() == IQ.Type.SET) {
                for (RosterPacket.Item item : rosterPacket.getRosterItems()) {
                    final RosterPacket.ItemType itemType = item.getItemType();
                    if (itemType == RosterPacket.ItemType.to
                            || itemType == RosterPacket.ItemType.both) {
                        XMPPID newID = createIDFromName(item.getUser());
                        @SuppressWarnings("unchecked")
                        Collection<Object> items = roster.getItems();
                        synchronized (items) {
                            updateRosterName(items, newID, item.getName());
                        }
                    }
                }
            }
        }
        super.handleIQEvent(evt);
    }

    /**
     * Method to recursively find roster entries that match the user id provided
     * and change the name of the user in the roster entry.
     * 
     * @param rosterItems
     * @param id
     * @param name
     */
    private void updateRosterName(Collection<Object> rosterItems, XMPPID id,
            String name) {
        if (name == null || name.isEmpty()) {
            return;
        }
        for (Object obj : rosterItems) {
            if (obj instanceof IRosterGroup) {
                @SuppressWarnings("unchecked")
                Collection<Object> items = ((IRosterGroup) obj).getEntries();
                synchronized (items) {
                    updateRosterName(items, id, name);
                }
            } else if (obj instanceof IRosterEntry) {
                IRosterEntry entry = (IRosterEntry) obj;
                if (entry.getUser().getID().equals(id)) {
                    ((User) entry.getUser()).setName(name);
                }
            }
        }
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it can call a custom version of updatePresence when the presence
     * type is unavaialble.
     */
    @Override
    protected void handlePresenceEvent(PresenceEvent evt) {
        if (evt.getPresence().getType() == Presence.Type.unavailable) {
            final Presence xmppPresence = evt.getPresence();
            final String from = xmppPresence.getFrom();
            final IPresence newPresence = createIPresence(xmppPresence);
            final XMPPID fromID = createIDFromName(from);
            updatePresence(fromID, newPresence);
            rosterManager.notifyPresenceListeners(fromID, newPresence);
        } else {
            super.handlePresenceEvent(evt);
        }
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it can call a custom version of updatePresenceInGroup and
     * updatePresenceForMatchingEntry, it also only handles cases of removal.
     */
    private void updatePresence(XMPPID fromID, IPresence newPresence) {
        @SuppressWarnings("unchecked")
        final Collection<Object> rosterItems = roster.getItems();
        List<IRosterEntry> newEntrys = new ArrayList<IRosterEntry>();
        synchronized (rosterItems) {
            for (final Iterator<Object> i = rosterItems.iterator(); i.hasNext();) {
                final IRosterItem item = (IRosterItem) i.next();
                if (item instanceof IRosterGroup) {
                    IRosterEntry[] es = updatePresenceInGroup(
                            (IRosterGroup) item, fromID, newPresence);
                    for (int j = 0; j < es.length; j++) {
                        newEntrys.add(es[j]);
                    }
                } else if (item instanceof org.eclipse.ecf.presence.roster.RosterEntry) {
                    IRosterEntry entry = updatePresenceForMatchingEntry(
                            (org.eclipse.ecf.presence.roster.RosterEntry) item,
                            fromID, newPresence);
                    if (entry != null)
                        newEntrys.add(entry);
                }
            }
        }

        IRosterEntry[] entrys = newEntrys.toArray(new IRosterEntry[] {});
        if (entrys.length > 0) {
            for (int i = 0; i < entrys.length; i++) {
                removeItemFromRoster(rosterItems, fromID);
            }
        }
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it can call a custom version of updatePresenceForMatchingEntry
     */
    private IRosterEntry[] updatePresenceInGroup(IRosterGroup group,
            XMPPID fromID, IPresence newPresence) {
        List<IRosterEntry> results = new ArrayList<IRosterEntry>();
        @SuppressWarnings("unchecked")
        final Collection<Object> groupEntries = group.getEntries();
        synchronized (groupEntries) {
            for (final Iterator<Object> i = groupEntries.iterator(); i
                    .hasNext();) {
                IRosterEntry newEntry = updatePresenceForMatchingEntry(
                        (org.eclipse.ecf.presence.roster.RosterEntry) i.next(),
                        fromID, newPresence);
                if (newEntry != null)
                    results.add(newEntry);
            }
        }
        return results.toArray(new IRosterEntry[] {});
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it can call a custom version of removeEntryFromRoster, it has also
     * been simplified to handle only cases of removal when the presence type is
     * unavaialble.
     */
    private IRosterEntry updatePresenceForMatchingEntry(
            org.eclipse.ecf.presence.roster.RosterEntry entry, XMPPID fromID,
            IPresence newPresence) {
        final IUser user = entry.getUser();
        XMPPID oldID = (XMPPID) user.getID();
        // If the username/host part matches that means we either have to update
        // the resource, or create a new client
        if (oldID.equals(fromID)) {
            return removeEntryFromRoster(oldID, entry, newPresence, user);
        } else if (oldID.getUsernameAtHost().equals(fromID.getUsernameAtHost())) {
            return entry;
        }
        return null;
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it can call a custom version of countClientsInRoster, it also uses
     * the results of this method differently
     */
    private IRosterEntry removeEntryFromRoster(XMPPID oldID,
            org.eclipse.ecf.presence.roster.RosterEntry entry,
            IPresence newPresence, IUser user) {
        if (countClientsInRoster(oldID) > 0) {
            // remove this client from roster
            return entry;
        } else {
            // Last one, so we set resource to null and set presence to
            // unavailable
            oldID.setResourceName(null);
            entry.setPresence(newPresence);
            rosterManager.notifyRosterUpdate(entry);
            return null;
        }
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it will return the number of entries that have the same name/hose
     * but a different resource. Returning the number of users with same
     * name/host even if they have the same resource is what causes the ECF
     * version to delete users that are in multiple groups. For a user that is
     * not logged in more than once this will always return 0.
     */
    private int countClientsInRosterGroup(
            org.eclipse.ecf.presence.roster.RosterGroup group, XMPPID oldID) {
        @SuppressWarnings("unchecked")
        Collection<Object> groupItems = group.getEntries();
        int count = 0;
        for (final Iterator<Object> i = groupItems.iterator(); i.hasNext();) {
            final IRosterItem item = (IRosterItem) i.next();
            if (item instanceof org.eclipse.ecf.presence.roster.RosterEntry) {
                org.eclipse.ecf.presence.roster.RosterEntry entry = (org.eclipse.ecf.presence.roster.RosterEntry) item;
                XMPPID entryID = (XMPPID) entry.getUser().getID();
                if (!entryID.equals(oldID)
                        && entryID.getUsernameAtHost().equals(
                                oldID.getUsernameAtHost()))
                    count++;
            }
        }
        return count;
    }

    /**
     * This method has been adapted from the ECF XMPPContainerPresenceHelper so
     * that it will return the number of entries that have the same name/hose
     * but a different resource. Returning the number of users with same
     * name/host even if they have the same resource is what causes the ECF
     * version to delete users that are in multiple groups. For a user that is
     * not logged in more than once this will always return 0.
     */
    private int countClientsInRoster(XMPPID oldID) {
        @SuppressWarnings("unchecked")
        Collection<Object> rosterItems = roster.getItems();
        int count = 0;
        synchronized (rosterItems) {
            for (final Iterator<Object> i = rosterItems.iterator(); i.hasNext();) {
                final IRosterItem item = (IRosterItem) i.next();
                if (item instanceof org.eclipse.ecf.presence.roster.RosterGroup) {
                    final org.eclipse.ecf.presence.roster.RosterGroup group = (org.eclipse.ecf.presence.roster.RosterGroup) item;
                    count += countClientsInRosterGroup(group, oldID);
                } else if (item instanceof org.eclipse.ecf.presence.roster.RosterEntry) {
                    org.eclipse.ecf.presence.roster.RosterEntry entry = (org.eclipse.ecf.presence.roster.RosterEntry) item;
                    XMPPID entryID = (XMPPID) entry.getUser().getID();
                    if (!entryID.equals(oldID)
                            && entryID.getUsernameAtHost().equals(
                                    oldID.getUsernameAtHost())) {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    private void removeItemFromRoster(Collection<Object> rosterItems,
            XMPPID itemIDToRemove) {
        boolean removed = false;
        synchronized (rosterItems) {
            for (final Iterator<Object> i = rosterItems.iterator(); i.hasNext();) {
                final IRosterItem item = (IRosterItem) i.next();
                if (item instanceof org.eclipse.ecf.presence.roster.RosterGroup) {
                    final org.eclipse.ecf.presence.roster.RosterGroup group = (org.eclipse.ecf.presence.roster.RosterGroup) item;
                    removed = removeItemFromRosterGroup(group, itemIDToRemove);
                    // If group is empty, remove it too
                    if (group.getEntries().size() == 0)
                        i.remove();
                } else if (item instanceof org.eclipse.ecf.presence.roster.RosterEntry) {
                    if (((org.eclipse.ecf.presence.roster.RosterEntry) item)
                            .getUser().getID().equals(itemIDToRemove)) {
                        i.remove();
                        removed = true;
                    }
                }
            }
        }
        if (removed)
            rosterManager.notifyRosterUpdate(roster);

    }

    private boolean removeItemFromRosterGroup(
            org.eclipse.ecf.presence.roster.RosterGroup group,
            XMPPID itemIDToRemove) {
        @SuppressWarnings("unchecked")
        final Collection<Object> groupEntries = group.getEntries();
        synchronized (groupEntries) {
            for (final Iterator<Object> i = groupEntries.iterator(); i
                    .hasNext();) {
                final org.eclipse.ecf.presence.roster.RosterEntry entry = (org.eclipse.ecf.presence.roster.RosterEntry) i
                        .next();
                if (entry.getUser().getID().equals(itemIDToRemove)) {
                    i.remove();
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * provide the VizRosterManager access to the super rosterManager
     * 
     * @return
     */
    protected IRosterManager getSuperRosterManager() {
        return super.getRosterManager();
    }

    /**
     * Use the viz roster manager instead of super, most calls are just
     * forwarded through.
     */
    @Override
    public IRosterManager getRosterManager() {
        return rosterManager;
    }

    /**
     * The entire purpose of this class is to intercept the various listeners so
     * that they can be notified when presence changes in the viz
     * XMPPCOntainerPresenceHelper since it does not have access to the super
     * class roster manager listeners.
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jul 26, 2012            bsteffen     Initial creation
     * 
     * </pre>
     * 
     * @author bsteffen
     * @version 1.0
     */
    private class VizRosterManager implements IRosterManager {

        private final List<IRosterListener> rosterUpdateListeners = new ArrayList<IRosterListener>();

        private final List<IPresenceListener> presenceListeners = new ArrayList<IPresenceListener>();

        @Override
        @SuppressWarnings("rawtypes")
        public Object getAdapter(Class adapter) {
            return getSuperRosterManager().getAdapter(adapter);
        }

        public void notifyRosterUpdate(IRosterItem entry) {
            List<IRosterListener> toNotify = null;
            synchronized (rosterUpdateListeners) {
                toNotify = new ArrayList<IRosterListener>(rosterUpdateListeners);
            }
            for (IRosterListener l : toNotify) {
                l.handleRosterUpdate(entry.getRoster(), entry);
            }
        }

        public void notifyPresenceListeners(ID fromID, IPresence presence) {
            List<IPresenceListener> toNotify = null;
            synchronized (presenceListeners) {
                toNotify = new ArrayList<IPresenceListener>(presenceListeners);
            }
            for (IPresenceListener l : toNotify) {
                l.handlePresence(fromID, presence);
            }
        }

        @Override
        public IRoster getRoster() {
            return getSuperRosterManager().getRoster();
        }

        @Override
        public void addRosterListener(IRosterListener listener) {
            getSuperRosterManager().addRosterListener(listener);
            synchronized (rosterUpdateListeners) {
                rosterUpdateListeners.add(listener);
            }
        }

        @Override
        public void removeRosterListener(IRosterListener listener) {
            getSuperRosterManager().removeRosterListener(listener);
            synchronized (rosterUpdateListeners) {
                rosterUpdateListeners.remove(listener);
            }

        }

        @Override
        public void addRosterSubscriptionListener(
                IRosterSubscriptionListener listener) {
            getSuperRosterManager().addRosterSubscriptionListener(listener);
        }

        @Override
        public void removeRosterSubscriptionListener(
                IRosterSubscriptionListener listener) {
            getSuperRosterManager().removeRosterSubscriptionListener(listener);
        }

        @Override
        public IRosterSubscriptionSender getRosterSubscriptionSender() {
            return getSuperRosterManager().getRosterSubscriptionSender();
        }

        @Override
        public IPresenceSender getPresenceSender() {
            return getSuperRosterManager().getPresenceSender();
        }

        @Override
        public void addPresenceListener(IPresenceListener listener) {
            getSuperRosterManager().addPresenceListener(listener);
            synchronized (presenceListeners) {
                presenceListeners.add(listener);
            }
        }

        @Override
        public void removePresenceListener(IPresenceListener listener) {
            getSuperRosterManager().removePresenceListener(listener);
            synchronized (presenceListeners) {
                presenceListeners.remove(listener);
            }
        }

    }

}
