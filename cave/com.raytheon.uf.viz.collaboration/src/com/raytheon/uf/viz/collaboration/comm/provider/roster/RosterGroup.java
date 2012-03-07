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

import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRoster;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterItem;

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

    private Collection<IRosterEntry> entries = null;
    
    private Collection<IRosterGroup> groups = null;
    
    /**
     * 
     */
    public RosterGroup(String name, IRosterItem parent, IRoster roster) {
        super(name, parent, roster);
        if(roster.supportsNestedGroups()) {
            groups = new ArrayList<IRosterGroup>();
        }
    }
    
    /**
     * 
     */
    private void ensureEntries() {
        if(entries == null) {
            entries = new ArrayList<IRosterEntry>();
        }
    }
    
    /**
     * 
     * @param entry
     */
    public void addEntry(IRosterEntry entry) {
        ensureEntries();
        entries.add(entry);
    }
    
    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup#getEntries()
     */
    @Override
    public Collection<IRosterEntry> getEntries() {
        return entries;
    }

    /**
     * 
     * @param entry
     * @return
     */
    public IRosterEntry removeEntry(IRosterEntry entry) {
        IRosterEntry removed = null;
        if(entries != null) {
            if(entries.remove(entry)) {
                removed = entry;
            }
        }
        return removed;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup#getGroups()
     */
    @Override
    public Collection<IRosterGroup> getGroups() {
        return groups;
    }
}
