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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterListener;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;

import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.RosterChangeType;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.event.RosterChangeEvent;

/**
 * Keeps track of contacts and resource information. Responsible for sending out
 * roster change events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2014 2822       bclement     Initial creation
 * Apr 24, 2014 3070       bclement     removed roster, 
 *                                      RosterChangedEvent sends UserId not RosterEntry
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ContactsListener implements RosterListener {

    private final ContactsManager manager;

    private final Map<String, List<ResourceInfo>> contactResources = new HashMap<String, List<ResourceInfo>>();

    /**
     * @param manager
     * @param roster
     */
    public ContactsListener(ContactsManager manager) {
        this.manager = manager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smack.RosterListener#presenceChanged(org.jivesoftware
     * .smack.packet.Presence)
     */
    @Override
    public void presenceChanged(Presence presence) {
        String fromId = presence.getFrom();
        UserId u = IDConverter.convertFrom(fromId);
        if (u != null) {
            synchronized (contactResources) {
                /* if they are logging out */
                if (presence.getType() == Type.unavailable) {
                    processLogout(u);
                } else {
                    processUpdate(u, presence);
                }
            }
            RosterEntry entry = manager.getRosterEntry(u);
            if (entry != null) {
                post(entry);
            }
            IRosterChangeEvent event = new RosterChangeEvent(
                    RosterChangeType.PRESENCE, u, presence);
            post(event);
        }
    }

    /**
     * Update contact resources when resource logs out. Must be externally
     * synchronized
     * 
     * @param uid
     */
    private void processLogout(UserId uid) {
        String bareId = uid.getNormalizedId();
        List<ResourceInfo> resources = contactResources.get(bareId);
        if (resources != null) {
            String resource = uid.getResource();
            Iterator<ResourceInfo> iterator = resources.iterator();
            while (iterator.hasNext()) {
                ResourceInfo next = iterator.next();
                if (next.getResourceName().equalsIgnoreCase(resource)) {
                    iterator.remove();
                }
            }
            if (resources.isEmpty()) {
                contactResources.remove(bareId);
            }
        }
    }

    /**
     * Update contact resources when resource changes (non-logout). Must be
     * externally synchronized.
     * 
     * @param uid
     * @param presence
     */
    private void processUpdate(UserId uid, Presence presence) {
        String bareId = uid.getNormalizedId();
        String resource = uid.getResource();
        List<ResourceInfo> resources = contactResources.get(bareId);
        if (resources == null) {
            /* we don't expect a large number of clients per user */
            resources = new ArrayList<ResourceInfo>(2);
            contactResources.put(bareId, resources);
        }
        ResourceInfo oldInfo = null;
        for (ResourceInfo ri : resources) {
            if (ri.getResourceName().equalsIgnoreCase(resource)) {
                oldInfo = ri;
            }
        }
        /* update resource */
        if (oldInfo == null) {
            oldInfo = new ResourceInfo(resource, presence);
            resources.add(oldInfo);
        } else {
            oldInfo.updateInfo(presence);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smack.RosterListener#entriesUpdated(java.util.Collection
     * )
     */
    @Override
    public void entriesUpdated(Collection<String> addresses) {
        send(addresses, RosterChangeType.MODIFY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smack.RosterListener#entriesDeleted(java.util.Collection
     * )
     */
    @Override
    public void entriesDeleted(Collection<String> addresses) {
        send(addresses, RosterChangeType.DELETE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smack.RosterListener#entriesAdded(java.util.Collection)
     */
    @Override
    public void entriesAdded(Collection<String> addresses) {
        send(addresses, RosterChangeType.ADD);
    }

    /**
     * Send event bus notification for roster
     * 
     * @param addresses
     * @param type
     */
    private void send(Collection<String> addresses, RosterChangeType type) {
        for (String addy : addresses) {
            UserId entry = IDConverter.convertFrom(addy);
            /*
             * RosterChangeEvents can't use RosterEntry objects because DELETE
             * events happen after the entry is removed from the server roster
             */
            IRosterChangeEvent event = new RosterChangeEvent(type, entry);
            post(event);
        }
    }

    /**
     * Post event to collaboration event bus
     * 
     * @param event
     */
    private void post(Object event) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        connection.postEvent(event);
    }

    /**
     * Get name of resource that supports shared displays for user
     * 
     * @param user
     * @return null if no resource found for user that supports shared displays
     */
    public String getSharedDisplayEnabledResource(UserId user) {
        String rval = null;
        synchronized (contactResources) {
            List<ResourceInfo> list = contactResources.get(user
                    .getNormalizedId());
            if (list != null) {
                for (ResourceInfo ri : list) {
                    if (ri.supportsSharedDisplays()) {
                        rval = ri.getResourceName();
                        break;
                    }
                }
            }
        }
        return rval;
    }

}
