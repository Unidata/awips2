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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Mode;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence.Type;

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
    private static final Map<String, IPresence.Mode> modeMap = new HashMap<String, IPresence.Mode>();
    static {
        for (Mode mode : Mode.values()) {
            modeMap.put(mode.name(), mode);
        }
    }

    IPresence.Mode mode;

    IPresence.Type type;

    String statusMessage;

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
        groupsMap = new HashMap<String, DataGroup>();
        sessionsMap = new HashMap<String, String>();
        mode = Mode.EXTENDED_AWAY;
        type = Type.UNKNOWN;
    }

    /**
     * @param id
     * @return
     */
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

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }
}
