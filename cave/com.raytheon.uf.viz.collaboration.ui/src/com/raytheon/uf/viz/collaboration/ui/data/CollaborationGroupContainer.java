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
package com.raytheon.uf.viz.collaboration.ui.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jivesoftware.smack.Roster;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.RosterManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;

/**
 * Container for collaboration information window. Includes current user,
 * sessions and contacts
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2012            mnash     Initial creation
 * Dec 20, 2013 2563       bclement  added items from server roster not in groups
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationGroupContainer {

    private SessionGroupContainer sessionGroup = new SessionGroupContainer();

    public CollaborationGroupContainer() {
    }

    /**
     * Get objects for UI items including current user, sessions and contacts
     * 
     * @return
     */
    public List<Object> getObjects() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return Collections.emptyList();
        }
        List<Object> result = new ArrayList<Object>();
        result.add(connection.getUser());
        result.add(sessionGroup);
        RosterManager rosterManager = connection.getRosterManager();
        Roster roster = rosterManager.getRoster();
        for (RosterGroup obj : roster.getGroups()) {
            result.add(obj);
        }
        Set<String> usersInLocal = new HashSet<String>();
        for (LocalGroup group : connection.getContactsManager()
                .getLocalGroups()) {
            usersInLocal.addAll(group.getUserNames());
            result.add(group);
        }
        for (RosterEntry entry : roster.getUnfiledEntries()) {
            // filter out entries that aren't in a group on the server, but are
            // in a local group so they don't show up twice on the contacts list
            if (!usersInLocal.contains(entry.getUser())) {
                result.add(entry);
            }
        }
        return result;
    }

    /**
     * Get container for session UI objects
     * 
     * @return
     */
    public SessionGroupContainer getSessionGroup() {
        return sessionGroup;
    }

}
