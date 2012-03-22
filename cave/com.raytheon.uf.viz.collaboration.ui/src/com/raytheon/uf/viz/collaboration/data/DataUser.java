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
package com.raytheon.uf.viz.collaboration.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode;

/**
 * A Data class that contains all the user information needed for the current
 * instance of CAVE.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012             rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class DataUser {
    // public static enum StatusType {
    // AVAILABLE("Available"), AWAY("Away"), DO_NOT_DISTURB("Do Not Disturb"),
    // NOT_ON_LINE(
    // "UnAvailable");
    //
    // private final String value;
    //
    // StatusType(String value) {
    // this.value = value;
    // }
    //
    // public String value() {
    // return value;
    // }
    // }

    private static final Map<String, IPresence.Mode> modeMap = new HashMap<String, IPresence.Mode>();
    static {
        for (Mode mode : Mode.values()) {
            modeMap.put(mode.name(), mode);
        }
    }

    public static enum RoleType {
        LEADER("Session Leader"), DATA_PROVIDER("Data Provider"), PARTICIPANT(
                "Participant"), UNKNOWN("Unknown");
        private final String value;

        RoleType(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    IPresence.Mode mode;

    String statusMessage;

    Map<String, List<RoleType>> roleMap;

    /**
     * Unique id for the usersData.
     */
    private String id;

    /**
     * The groups being tracked that user belongs to.
     */
    private Map<String, DataGroup> groupsMap;

    /**
     * The active sessions the user is in.
     */
    private Map<String, String> sessionsMap;

    /**
     * Only allow classes in the package to use the constructor.
     * 
     * @param id
     */
    DataUser(String id) {
        this.id = id;
        groupsMap = new HashMap<String, DataGroup>();
        sessionsMap = new HashMap<String, String>();
        mode = Mode.EXTENDED_AWAY;
        roleMap = new HashMap<String, List<RoleType>>();
    }

    RoleType[] getSessionRoles(String session) {
        RoleType[] result = null;
        List<RoleType> roleList = roleMap.get(session);
        if (roleList == null) {
            result = new RoleType[0];
        } else {
            result = new RoleType[roleList.size()];
            roleList.toArray(result);
        }
        return result;
    }

    void addSessionRole(final String session, final RoleType role) {
        List<RoleType> roleList = roleMap.get(session);
        if (roleList == null) {
            roleList = new ArrayList<DataUser.RoleType>();
            roleMap.put(session, roleList);
        }

        if (role == DataUser.RoleType.PARTICIPANT
                || role == DataUser.RoleType.UNKNOWN) {
            roleList.clear();
            roleList.add(role);
        } else {
            boolean insertRole = true;
            Iterator<DataUser.RoleType> iter = roleList.iterator();
            while (iter.hasNext()) {
                DataUser.RoleType r = iter.next();
                if (r == role) {
                    insertRole = false;
                }
                if (r == DataUser.RoleType.PARTICIPANT
                        || r == DataUser.RoleType.UNKNOWN) {
                    iter.remove();
                }
            }
            if (insertRole) {
                // Keep order Leader then provider.
                if (role == DataUser.RoleType.LEADER) {
                    roleList.add(0, role);
                } else {
                    roleList.add(role);
                }
            }
        }
    }

    void removeSessionRole(String session, RoleType role) {
        List<RoleType> roleList = roleMap.get(session);
        if (roleList != null) {
            roleList.remove(role);
        }
    }

    void removeSession(String session) {
        roleMap.remove(session);
    }

    DataGroup getGroup(String id) {
        DataGroup group = groupsMap.get(id);
        if (group == null) {
            group = new DataGroup(id);
            groupsMap.put(id, group);
        }
        return group;
    }

    public String getSessString(String key) {
        return sessionsMap.get(key);
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(Mode status) {
        this.mode = status;
    }

    public void setMode(String name) {
        this.mode = modeMap.get(name);
    }

    /**
     * @return the mode
     */
    public Mode getMode() {
        return mode;
    }
}
