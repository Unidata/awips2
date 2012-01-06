package com.raytheon.uf.edex.plugin.nwsauth.roles;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.plugin.nwsauth.user.UserId;
import com.raytheon.uf.edex.auth.roles.IRole;

/**
 * Implementation of IRole
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 25, 2010            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class NwsRole implements IRole {
    private static final UserId ALL = new UserId("ALL");

    @XmlAttribute
    private String roleId;

    private NwsRole parentRole;

    private Set<UserId> users = new HashSet<UserId>();

    private Set<String> subRoles = new HashSet<String>();

    @XmlElement(name = "user")
    public void setUserIdCollection(UserId[] userIds) {
        users = new HashSet<UserId>(Arrays.asList(userIds));
    }

    public UserId[] getUserIdCollection() {
        return users.toArray(new UserId[users.size()]);
    }

    @XmlElement(name = "subRole")
    public void setSubRolesCollection(String[] subRoles) {
        this.subRoles = new HashSet<String>(Arrays.asList(subRoles));
    }

    public String[] getSubRolesCollection() {
        return subRoles.toArray(new String[subRoles.size()]);
    }

    public String getRoleId() {
        return roleId;
    }

    public void setRoleId(String roleId) {
        this.roleId = roleId;
    }

    public void setParentRole(NwsRole parent) {
        this.parentRole = parent;
    }

    /**
     * @param userRolesMap
     */
    public void addRolesForUsers(Map<String, NwsRole> userRolesMap) {
        for (String subRoleId : subRoles) {
            NwsRole subRole = userRolesMap.get(subRoleId.toLowerCase());
            if (subRole != null) {
                subRole.setParentRole(this);
                subRole.addRolesForUsers(userRolesMap);
            }
        }
    }

    @Override
    public boolean validForUser(IUser user) {
        if (user != null) {
            if (users.contains(ALL)) {
                return true;
            }
            for (UserId id : users) {
                if (id.equals(user.uniqueId())) {
                    return true;
                }
            }
        }

        if (parentRole != null && parentRole.validForUser(user)) {
            return true;
        }

        return false;
    }

    @Override
    public String toString() {
        return this.roleId;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((roleId == null) ? 0 : roleId.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        NwsRole other = (NwsRole) obj;
        if (roleId == null) {
            if (other.roleId != null)
                return false;
        } else if (!roleId.equals(other.roleId))
            return false;
        return true;
    }

}
