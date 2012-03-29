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

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterGroup;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IChatID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;

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

    private IChatID userId = null;

    private IPresence presence = null;

    private Map<IRosterGroup, IRosterGroup> groups = null;

    /**
     * 
     * @param id
     */
    public RosterEntry(IChatID id) {
        userId = id;
        setName(id.getFQName());
        groups = new HashMap<IRosterGroup, IRosterGroup>();
    }

    /**
     * 
     */
    @Override
    public IChatID getUser() {
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

    public static final void main(String[] args) {

        IChatID id = new IChatID() {

            private String name = null;

            private String nickName = null;

            private String host = null;

            private String resource = null;

            @Override
            public void setName(String userName) {
                name = userName;
            }

            @Override
            public String getName() {
                return name;
            }

            @Override
            public void setNickname(String nickname) {
                nickName = nickname;
            }

            @Override
            public String getNickname() {
                return nickName;

            }

            @Override
            public void setHost(String hostName) {
                host = hostName;
            }

            @Override
            public String getHost() {
                return host;
            }

            @Override
            public String getResource() {
                return resource;
            }

            @Override
            public void setResource(String resource) {
                this.resource = resource;
            }

            @Override
            public String getFQName() {
                StringBuilder sb = new StringBuilder(name);
                sb.append("@");
                sb.append(host);
                if (resource != null) {
                    sb.append("/");
                    sb.append(resource);
                }

                return sb.toString();
            }

        };

        id.setName("fred");
        id.setHost("awipscm.omaha.us.ray.com");
        id.setResource("smack");

        IMutableRosterEntry entry = new RosterEntry(id);
        entry.setPresence(new Presence());

        IRosterEntry en = entry;

        System.out.println(id.getFQName());
    }

}
