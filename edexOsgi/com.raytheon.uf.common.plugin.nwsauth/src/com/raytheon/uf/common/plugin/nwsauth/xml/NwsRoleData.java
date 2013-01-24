package com.raytheon.uf.common.plugin.nwsauth.xml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@XmlRootElement(name = "nwsRoleData")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NwsRoleData implements ISerializableObject {
    @DynamicSerializeElement
    @XmlElement(name = "application")
    private String application;

    @DynamicSerializeElement
    @XmlElements({ @XmlElement(name = "permission", type = PermissionXML.class) })
    private List<PermissionXML> permissionList = new ArrayList<PermissionXML>();

    @DynamicSerializeElement
    @XmlElements({ @XmlElement(name = "role", type = RoleXML.class) })
    private List<RoleXML> roleList = new ArrayList<RoleXML>();

    @DynamicSerializeElement
    @XmlElements({ @XmlElement(name = "user", type = UserXML.class) })
    private List<UserXML> userList = new ArrayList<UserXML>();

    /** Map of user to all permissions */
    private transient Map<String, Set<String>> permissionMap = new HashMap<String, Set<String>>();

    /**
     * @return the permissionList
     */
    public List<PermissionXML> getPermissionList() {
        return permissionList;
    }

    /**
     * @param permissionList
     *            the permissionList to set
     */
    public void setPermissionList(List<PermissionXML> permissionList) {
        this.permissionList = permissionList;
    }

    /**
     * @return the roleList
     */
    public List<RoleXML> getRoleList() {
        return roleList;
    }

    /**
     * @param roleList
     *            the roleList to set
     */
    public void setRoleList(List<RoleXML> roleList) {
        this.roleList = roleList;
    }

    /**
     * @return the userList
     */
    public List<UserXML> getUserList() {
        return userList;
    }

    /**
     * @param userList
     *            the userList to set
     */
    public void setUserList(List<UserXML> userList) {
        this.userList = userList;
    }

    /**
     * @return the application
     */
    public String getApplication() {
        return application;
    }

    /**
     * @param application
     *            the application to set
     */
    public void setApplication(String application) {
        this.application = application;
    }

    public String[] getPermissions() {
        ArrayList<String> perms = new ArrayList<String>();
        for (PermissionXML p : this.permissionList) {
            perms.add(p.getId());
        }
        Collections.sort(perms);
        
        return perms.toArray(new String[perms.size()]);
    }

    public String[] getRoles() {
        ArrayList<String> roles = new ArrayList<String>();
        for (RoleXML r : this.roleList) {
            roles.add(r.getRoleId());
        }
        Collections.sort(roles);
        
        return roles.toArray(new String[roles.size()]);
    }

    public String[] getUsers() {
        ArrayList<String> users = new ArrayList<String>();
        for (UserXML r : this.userList) {
            users.add(r.getUserId());
        }
        Collections.sort(users);
        
        return users.toArray(new String[users.size()]);
    }

    public void addUser(String user) {
        if (user != null && user.length() > 0) {
            UserXML userXml = new UserXML();
            userXml.setUserId(user);
            this.userList.add(userXml);
        }
    }

    public void addRole(String role, String description) {
        if (role != null && description != null && role.length() > 0 && description.length() > 0) {
            RoleXML roleXml = new RoleXML();
            roleXml.setRoleDescription(description);
            roleXml.setRoleId(role);
            this.roleList.add(roleXml);
        }
    }
    
    /**
     * Add a permission.  This should only be used for 
     * Localization permissions, which are directory access
     * permissions.
     * 
     * @param permission
     */
    public void addPermission(String permission) {
        if (permission != null && permission.length() > 0) {
            PermissionXML pXml = new PermissionXML();
            pXml.setId(permission);
            this.permissionList.add(pXml);            
        }
    }

    /**
     * Get the user's permissions
     * 
     * @param userId id of the user
     * @return String[] of permissions
     */
    public String[] getUserPermissions(String userId) {
        ArrayList<String> userPermissions = new ArrayList<String>();

        for (UserXML userXml : this.userList) {
            if (userXml.getUserId().equals(userId)) {
                for (String permission : userXml.getPermissionList()) {
                    userPermissions.add(permission);
                }
                break;
            }
        }
        Collections.sort(userPermissions);
        
        return userPermissions.toArray(new String[userPermissions.size()]);
    }
    
    /**
     * Get an array of all defined permissions
     * 
     * @return String[] of all defined permissions
     */
    public String[] getAllDefinedPermissions() {
        ArrayList<String> permissions = new ArrayList<String>();
        for (PermissionXML p: this.permissionList) {
            permissions.add(p.getId());
        }
        
        return permissions.toArray(new String[permissions.size()]);
    }
    
    public String[] getRolePermissions(String roleId) {
        ArrayList<String> rolePermissions = new ArrayList<String>();

        for (RoleXML roleXml : this.roleList) {
            if (roleXml.getRoleId().equals(roleId)) {
                for (String permission : roleXml.getPermissionList()) {
                    rolePermissions.add(permission);
                }
                break;
            }
        }
        Collections.sort(rolePermissions);
        
        return rolePermissions.toArray(new String[rolePermissions.size()]);
    }

    public String[] getUserRoles(String userId) {
        ArrayList<String> userRoles = new ArrayList<String>();

        for (UserXML userXml : this.userList) {
            if (userXml.getUserId().equals(userId)) {
                for (String role : userXml.getRoleList()) {
                    userRoles.add(role);
                }
                break;
            }
        }
        Collections.sort(userRoles);
        
        return userRoles.toArray(new String[userRoles.size()]);
    }

    public String[] getRoleIdList() {
        ArrayList<String> roleIdList = new ArrayList<String>();
        for (RoleXML rx : this.roleList) {
            roleIdList.add(rx.getRoleId());
        }
        Collections.sort(roleIdList);
        
        return roleIdList.toArray(new String[roleIdList.size()]);
    }

    /**
     * Get a list of all permissions for this user.
     * 
     * @param user
     *            The user
     * 
     * @return Set of all permissions
     */
    private Set<String> getAuthorizedPermissions(String user) {
        if (!permissionMap.containsKey(user)) {
            Set<String> permSet = new HashSet<String>();

            for (String p : this.getUserPermissions(user)) {
                permSet.add(p);
            }

            String[] roles = this.getUserRoles(user);

            for (RoleXML roleXml : roleList) {
                for (String role : roles) {
                    if (roleXml.getRoleId().equals(role)) {
                        for (String p: roleXml.getPermissionList()) {
                            permSet.add(p);
                        }
                    }
                }
            }

            permissionMap.put(user, permSet);
        }

        return permissionMap.get(user);
    }

    /**
     * If the user has the permission then the user is authorized.
     * 
     * @param permission
     *            the permission id
     * @param user
     *            the user id
     * @return true if the user has the permission
     */
    public boolean isAuthorized(String permission, String user) {
        Set<String> authorizedPermissions = this.getAuthorizedPermissions(user);
        Set<String> allAuthorizedPermissions = this.getAuthorizedPermissions("ALL");
        
        for (String perm: authorizedPermissions) {
            if (perm.equalsIgnoreCase(permission)) {
                return true;
            }
        }
        
        for (String perm: allAuthorizedPermissions) {
            if (perm.equalsIgnoreCase(permission)) {
                return true;
            }
        }
        
        return false;
    }
}
