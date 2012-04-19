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
package com.raytheon.uf.viz.collaboration.comm.identity.roster;

import java.util.Collection;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * The Roster provides a structure to maintain user contacts. These contacts may
 * be organized into groups beneath the Roster. A single user contact may be
 * associated with more than a single group. As of this time nesting is not
 * allowed, that is groups may not contain groups.
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

public interface IRoster {

    /**
     * Add an entry directly to the roster.
     * 
     * @param item
     */
    void addRosterEntry(IRosterEntry entry);

    /**
     * Add this user to the roster.
     * 
     * @param user
     * @param nickName
     * @param groups
     */
    void addRosterEntry(IQualifiedID user, String nickName, String[] groups);

    /**
     * Request that the specified entry be modified in the roster.
     * 
     * @param entry
     *            The entry to modify. This entry will contain the modifications
     *            to apply.
     * @return The modified roster entry.
     */
    IRosterEntry modifyRosterEntry(IRosterEntry entry);

    /**
     * Request that the user be removed from the roster.
     * 
     * @param user
     *            The identification of the user to be removed.
     */
    void removeFromRoster(ID user);

    /**
     * Get all entries associated with this roster.
     * 
     * @return A Collection of entries belonging to this Roster.
     */
    Collection<IRosterEntry> getEntries();

    /**
     * Add a group to the roster. This method adds to the roster level groups
     * only.
     * 
     * @param group
     *            A group to add.
     */
    void addGroup(IRosterGroup group);

    /**
     * Removes a group from the roster level groups.
     * 
     * @param groupName
     *            Name of the group to remove.
     */
    void removeGroup(String groupName);

    /**
     * Get all groups associated with this roster.
     * 
     * @return A Collection of groups belonging to this Roster.
     */
    Collection<IRosterGroup> getGroups();

    /**
     * 
     * @return
     */
    UserId getUser();

    /**
     * Does this roster support nested groups?
     * 
     * @return This roster supports nested groups.
     */
    boolean supportsNestedGroups();

    /**
     * Signifies whether this roster associated with a chat room.
     * 
     * @return Is this roster associated with a chat room.
     */
    boolean isRoomRoster();

    /**
     * 
     * @param account
     * @param nickName
     * @param groups
     */
    void sendRosterAdd(String account, String nickName, String[] groups)
            throws CollaborationException;

    /**
     * 
     * @param userId
     */
    void sendRosterRemove(UserId userId) throws CollaborationException;

}
