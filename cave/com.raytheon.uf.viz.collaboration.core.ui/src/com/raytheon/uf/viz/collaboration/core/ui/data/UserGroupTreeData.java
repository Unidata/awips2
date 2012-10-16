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
package com.raytheon.uf.viz.collaboration.core.ui.data;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.ecf.presence.roster.IRosterGroup;

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
 * Jun 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class UserGroupTreeData {

    private String groupName;

    private IRosterGroup group;

    private List<UserTreeData> groupUsers;

    public UserGroupTreeData(String groupName, IRosterGroup group) {
        this.groupName = groupName;
        this.group = group;
        this.groupUsers = new ArrayList<UserTreeData>();
    }

    /**
     * @return the groupName
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * @return the groupUsers
     */
    public List<UserTreeData> getGroupUsers() {
        return new ArrayList<UserTreeData>(groupUsers);
    }

    /**
     * Adds a user to this group
     * 
     * @param user
     */
    public void addUserToGroup(UserId user, IRosterEntry entry) {
        groupUsers.add(new UserTreeData(user, entry));
    }
}
