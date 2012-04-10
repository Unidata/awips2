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
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.user.RosterId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class Roster extends RosterItem implements IRoster {
    // Map of all roster entries in this roster.
    private Map<IQualifiedID, IRosterEntry> internalEntries;

    // Map of roster entries that are not contained in a group.
    private Map<IQualifiedID, IRosterEntry> entries;

    // Map of all roster groups in this roster.
    private Map<String, IRosterGroup> groups = null;

    private final IChatID user;

    private boolean roomRoster = false;

    private IRosterManager rosterManager;

    /**
     * 
     * @param user
     */
    public Roster(IChatID user, IRosterManager manager) {
        rosterManager = manager;
        this.user = user;
        internalEntries = new HashMap<IQualifiedID, IRosterEntry>();
        entries = new HashMap<IQualifiedID, IRosterEntry>();
        groups = new HashMap<String, IRosterGroup>();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#getUser()
     */
    @Override
    public IChatID getUser() {
        return user;
    }
    
    /**
     * Does this roster support nested groups?
     * 
     * @return This roster support nested groups.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#supportsNestedGroups()
     */
    public boolean supportsNestedGroups() {
        return false;
    }

    /**
     * Is this a roster for a chat room?
     * 
     * @return Is this a chat room roster?
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#isRoomRoster()
     */
    @Override
    public boolean isRoomRoster() {
        return roomRoster;
    }

    /**
     * @param entry
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#addEntry(com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry)
     */
    @Override
    public void addRosterEntry(IRosterEntry entry) {
        IRosterEntry re = null;
        
        IQualifiedID id = entry.getUser();
        re = internalEntries.get(id);
        // ensure the entry is not present!
        if(re == null) {
            // put in the internal entries first.
            internalEntries.put(entry.getUser(), entry);
            
            for(IRosterGroup g : entry.getGroups()) {
                RosterGroup rg = (RosterGroup) g;
                rg.setRoster(this);
            }
        }
    }

    /**
     * 
     * @param entry
     * @return
     */
    public IRosterEntry getRosterEntry(IRosterEntry entry) {
        return entries.get(entry);
    }

    /**
     * Get all entries belonging with this roster.
     * 
     * @return A Collection of entries belonging to this Roster.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#getEntries()
     */
    @Override
    public Collection<IRosterEntry> getEntries() {
        return entries.values();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#addGroup(com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup)
     */
    @Override
    public void addGroup(IRosterGroup group) {
        if (!groups.containsKey(group.getName())) {
            groups.put(group.getName(), group);
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#removeGroup(java.lang.String)
     */
    @Override
    public void removeGroup(String groupName) {
        // Need to remove the group from the map
        // visit any entries that reference this group
        // and send updates so that the info is updated
        // at the server.
    }

    /**
     * Get all groups associated with this roster.
     * 
     * @return A Collection of groups belonging to this Roster.
     */
    @Override
    public Collection<IRosterGroup> getGroups() {
        return groups.values();
    }

    /**
     * 
     * @param group
     * @return
     */
    public IRosterGroup getRosterGroup(IRosterGroup group) {
        return groups.get(group);
    }

    /**
     * Add a user entry to this roster. If the groups are null or empty then the
     * entry is considered to be ungrouped.
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#addRosterEntry(com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public void addRosterEntry(IQualifiedID user, String nickName,
            String[] groups) {
        if (user != null) {
            IRosterEntry entry = internalEntries.get(user.getFQName());
            if (entry == null) {
                RosterId id = new RosterId(user.getName(), user.getHost(),
                        null, user.getResource());
                entry = new RosterEntry(id);
            }
            internalEntries.put(entry.getUser(), entry);
            
            listRoster();
        }
    }

    /**
     * 
     * @param A
     *            roster entry to find and modify.
     * @return The modifed roster entry.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#modifyRosterEntry(com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry)
     */
    @Override
    public IRosterEntry modifyRosterEntry(IRosterEntry entry) {
        IRosterEntry re = null;
        // First attempt to find the entry in the internal entry collection
        if (entry != null) {
            IQualifiedID id = entry.getUser();
            re = internalEntries.get(id);
            if (re != null) {
                // We've found the roster entry in the internal entries
                // so update with the presence.
                RosterEntry ret = (RosterEntry) re;
                ret.setPresence(entry.getPresence());
            } else {
            }
        } else {
            // nothing to do. And this shouldn't happen!
        }
        return re;
    }

    /**
     * 
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#
     *      removeFromRoster(com.raytheon.uf.viz.collaboration.comm.identity.user.ID)
     */
    @Override
    public void removeFromRoster(ID user) {

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
        rosterManager.sendRosterAdd(account, nickName, groups);
    }

    /**
     * 
     * @param userId
     */
    @Override
    public void sendRosterRemove(IChatID userId) throws CollaborationException {
        rosterManager.sendRosterRemove(userId);
    }

    /**
     * 
     * @param roster
     * @param group
     * @param entries
     */
    void populateGroup(RosterGroup group,
            @SuppressWarnings("rawtypes") Collection entries) {
        if ((group != null) && (entries != null)) {
            for (Object o : entries) {
                if (o instanceof org.eclipse.ecf.presence.roster.IRosterEntry) {
                    org.eclipse.ecf.presence.roster.IRosterEntry entry = (org.eclipse.ecf.presence.roster.IRosterEntry) o;

                    IChatID id = RosterId.convertFrom(entry.getUser());

                    RosterEntry re = new RosterEntry(id);
                    // Check to see if we already have an entry
                    IRosterEntry reCurrent = getRosterEntry(re);
                    if ((reCurrent != null)
                            && (reCurrent instanceof RosterEntry)) {
                        re = (RosterEntry) reCurrent;
                    }
                    IPresence p = Presence.convertPresence(entry.getPresence());
                    re.setPresence(p);

                    re.addGroup(group);
                    group.addEntry(re);
                    internalEntries.put(re.getUser(), re);
                }
            }
        }
    }

    // *******************************************
    // <pre>
    // internalEntries : A map of all roster entries contained by this roster.
    // Each entry contains a map of all groups it is contained in. If the
    // map is empty then the entry is not contained within a group and will
    // be found in the entries map.
    // </pre>
    // *******************************************

    
    public void listRoster() {
        
        System.out.println("##########################################################################");
        System.out.println("Roster for : " + user.getFQName());
        System.out.println("#####################################");
        System.out.println("# Ungrouped entries");
        System.out.println("-------------------------------------");
        Collection<IRosterEntry> entries = getEntries();
        for(IRosterEntry r : entries) {
            System.out.print("    " + r.getName());
        }
        System.out.println("#####################################");
        System.out.println("# Groups ");
        System.out.println("-------------------------------------");
        Collection<IRosterGroup> groups = getGroups();
        for(IRosterGroup g : groups) {
            System.out.print("    " + g.getName());
            entries = g.getEntries();
            for(IRosterEntry r : entries) {
                System.out.print("        " + r.getName());
            }
            System.out.println("-----------------");
        }
        System.out.println("##########################################################################");
    }
    
    
    
    
}
