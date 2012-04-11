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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import java.util.Collection;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IRosterEventSubscriber;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0 
 */

public class RosterEventHandler implements IRosterEventSubscriber {

    
    @Subscribe
    public void eventHandler(IRosterChangeEvent event) {
        
        StringBuilder sb = new StringBuilder("-------------------------------------------------------\n");
        sb.append("RosterEventHandler.eventHandler(");
        switch(event.getType()) {
        
        case ADD : {
            sb.append("ADD)");
            break;
        }
        case MODIFY : {
            sb.append("MODIFY)");
            break;
        }
        case DELETE : {
            sb.append("DELETE)");
            break;
        }
        case PRESENCE : {
            sb.append("PRESENCE)");
            break;
        }
        }
        System.out.println(sb.toString());
        printRosterEntry(event.getEntry());
    }
    
    private void printRosterEntry(IRosterEntry entry) {
        // System.out.println("handleRosterEntryAdd " + System.currentTimeMillis());
        System.out.println("    user   : " + entry.getUser().getFQName());
        Collection<IRosterGroup> groups = entry.getGroups();
        for(IRosterGroup group : groups) {
            System.out.println("     " + group.getName());
        }
        IPresence presence = entry.getPresence();
        if(presence != null) {
            System.out.println("    pres   : " + presence.getType() + " : " + presence.getMode());
        }
    }
    
}
