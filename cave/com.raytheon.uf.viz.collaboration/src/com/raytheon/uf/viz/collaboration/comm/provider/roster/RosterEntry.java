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

import org.eclipse.ecf.core.identity.ID;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
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
 * Feb 27, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class RosterEntry extends RosterItem implements IRosterEntry,
        IMutableRosterEntry {

    private UserId userId = null;

    private IPresence presence = null;

    private Map<IRosterGroup, IRosterGroup> groups = null;

    /**
     * 
     * @param id
     */
    public RosterEntry(UserId id) {
        userId = id;
        setName(id.getFQName());
        groups = new HashMap<IRosterGroup, IRosterGroup>();
    }

    /**
     * 
     */
    @Override
    public UserId getUser() {
        return userId;
    }

    /**
     * 
     * @param group
     */
    public void addGroup(IRosterGroup group) {
        if (group != null) {
            if (!groups.containsKey(group)) {
                groups.put(group, group);
            }
        }
    }

    /**
     * 
     */
    @Override
    public Collection<IRosterGroup> getGroups() {
        return groups.values();
    }

    /**
     * 
     * @param presence
     */
    public void setPresence(IPresence presence) {
        this.presence = presence;
    }

    /**
     * 
     * @return
     */
    @Override
    public IPresence getPresence() {
        return presence;
    }

    /**
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((userId == null) ? 0 : userId.hashCode());
        return result;
    }

    /**
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RosterEntry other = (RosterEntry) obj;
        if (userId == null) {
            if (other.userId != null)
                return false;
        } else if (!userId.equals(other.userId))
            return false;
        return true;
    }

    /**
     * 
     * @param entry
     * @return
     */
    public static IRosterEntry convertEntry(
            org.eclipse.ecf.presence.roster.IRosterEntry entry) {
        RosterEntry rosterEntry = null;
        if (entry != null) {
            ID id = entry.getUser().getID();

            String name = Tools.parseName(id.getName());
            String host = Tools.parseHost(id.getName());
            String resource = Tools.parseResource(id.getName());
            UserId rosterId = new UserId(name, host, resource);

            rosterEntry = new RosterEntry(rosterId);
            IPresence p = Presence.convertPresence(entry.getPresence());
            rosterEntry.setPresence(p);

            // Now check the groups
            @SuppressWarnings("unchecked")
            Collection<org.eclipse.ecf.presence.roster.IRosterGroup> inGroups = entry
                    .getGroups();
            for (org.eclipse.ecf.presence.roster.IRosterGroup g : inGroups) {
                RosterGroup group = new RosterGroup(g.getName(), null, null);
                rosterEntry.addGroup(group);
            }
        }
        return rosterEntry;
    }

}
