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

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IRosterListener;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterManager;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.user.RosterId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueUserId;

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

    private String owner;

    private IRoster roster;

    private org.eclipse.ecf.presence.roster.IRoster baseRoster;

    /**
     * 
     * @param roster
     */
    public RosterManager(org.eclipse.ecf.presence.roster.IRoster roster) {
        baseRoster = roster;
        owner = roster.getName();
        this.roster = toLocalRoster(roster);
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
     * @param roster
     * @return
     */
    public IRoster toLocalRoster(org.eclipse.ecf.presence.roster.IRoster roster) {
        Roster newRoster = null;

        if (roster != null) {
            IChatID id = VenueUserId.convertFrom(roster.getUser());
            newRoster = new Roster(id);

            @SuppressWarnings("rawtypes")
            Collection items = roster.getItems();
            for (Object o : items) {
                if (o instanceof org.eclipse.ecf.presence.roster.IRosterEntry) {
                    org.eclipse.ecf.presence.roster.IRosterEntry entry = (org.eclipse.ecf.presence.roster.IRosterEntry) o;

                    id = RosterId.convertFrom(entry.getUser());
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
                }
            }
        }
        return newRoster;
    }

}
