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

import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterItem;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
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

public class RosterGroup extends RosterItem implements IRosterGroup {

    private Map<IQualifiedID, IRosterEntry> entries = null;

    /**
     * 
     */
    public RosterGroup(String name, IRosterItem parent, IRoster roster) {
        super(name, parent, roster);
        entries = new HashMap<IQualifiedID, IRosterEntry>();
    }

    /**
     * 
     * @param entry
     */
    public void addEntry(IRosterEntry entry) {

        IRosterEntry re = entries.get(entry.getUser());
        if (re == null) {
            IQualifiedID user = entry.getUser();
            UserId id = new UserId(user.getName(), user.getHost(),
                    user.getResource());

            re = new RosterEntry(id);
            entries.put(entry.getUser(), entry);
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup#getEntries()
     */
    @Override
    public Collection<IRosterEntry> getEntries() {
        return entries.values();
    }

    /**
     * 
     * @param entry
     * @return
     */
    public IRosterEntry removeEntry(IRosterEntry entry) {
        return entries.remove(entry.getUser());
    }

}
