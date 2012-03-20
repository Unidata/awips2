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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterItem;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

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

    private Map<IRosterEntry, IRosterEntry> entries;

    private Map<IRosterGroup, IRosterGroup> groups = null;

    private final IChatID user;

    private boolean roomRoster = false;

    // From ecf . . . .
    // IPresenceContainerAdapter presence = (IPresenceContainerAdapter)
    // container
    // .getAdapter(IPresenceContainerAdapter.class);
    // private IPresenceContainerAdapter presenceAdapter = null;
    //
    //
    // presenceAdapter.getRosterManager.getRoster();
    // We will then build our internal roster from the ecf roster.
    //

    /**
     * 
     * @param user
     */
    public Roster(IChatID user) {
        this.user = user;
        entries = new HashMap<IRosterEntry, IRosterEntry>();
        groups = new HashMap<IRosterGroup, IRosterGroup>();
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
        entries.put(entry, entry);
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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#addRosterEntry(com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public void addRosterEntry(IQualifiedID user, String nickName,
            String[] groups) {
        
        
        
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#modifyRosterEntry(com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry)
     */
    @Override
    public void modifyRosterEntry(IRosterEntry entry) {
    }

    /**
     * 
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster#
     * removeFromRoster(com.raytheon.uf.viz.collaboration.comm.identity.user.ID)
     */
    @Override
    public void removeFromRoster(ID user) {

    }

}
