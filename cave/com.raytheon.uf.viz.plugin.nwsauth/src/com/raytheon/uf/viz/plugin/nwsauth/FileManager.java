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
package com.raytheon.uf.viz.plugin.nwsauth;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.plugin.nwsauth.NwsPermission;
import com.raytheon.uf.common.plugin.nwsauth.NwsRole;
import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.common.plugin.nwsauth.xml.PermissionXML;
import com.raytheon.uf.common.plugin.nwsauth.xml.RoleXML;
import com.raytheon.uf.common.plugin.nwsauth.xml.UserXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Uses localization data to determine role/permissions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2012            mpduff     Initial creation
 * Nov 06, 2012 1302       djohnson   Move to nwsauth plugin.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FileManager {
    /** Status handler */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FileManager.class);

    private static final FileManager instance = new FileManager();

    private final String ROLE_DIR = "roles";

    /** JAXB context */
    private JAXBContext jax;

    /** Marshaller object */
    private Marshaller marshaller;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    private final Map<String, NwsRoleData> roleDataMap = new HashMap<String, NwsRoleData>();

    /**
     * Application name -> LocalizationFile map.
     */
    private final Map<String, LocalizationFile> roleFileMap = new HashMap<String, LocalizationFile>();

    private FileManager() {
        createContext();
        readXML();
    }

    /**
     * Get an instance.
     * 
     * @return an instance
     */
    public static FileManager getInstance() {
        return instance;
    }

    private void createContext() {
        @SuppressWarnings("rawtypes")
        Class[] classes = new Class[] { NwsRoleData.class, PermissionXML.class,
                RoleXML.class, UserXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
            this.marshaller = jax.createMarshaller();

            // format the output xml file
            this.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

        } catch (JAXBException e) {
            e.printStackTrace();
        }
    }

    /**
     * Get the NwsRoleData object.
     * 
     * @param application
     *            The application
     * 
     * @return The NwsRoleData object
     */
    public NwsRoleData getRoleData(String application) {
        return this.roleDataMap.get(application);
    }

    /**
     * Get a list of user ids for the application.
     * 
     * @param application
     *            the application
     * @return String[] of user ids
     */
    public String[] getUserIdList(String application) {
        ArrayList<String> userNameList = new ArrayList<String>();
        for (UserXML ux : this.roleDataMap.get(application).getUserList()) {
            userNameList.add(ux.getUserId());
        }

        return userNameList.toArray(new String[userNameList.size()]);
    }

    /**
     * Get an array of user roles.
     * 
     * @param userId
     *            The user id
     * @param application
     *            The application
     * @return the array of roles
     */
    public String[] getUserRoles(String userId, String application) {
        ArrayList<String> userRoles = new ArrayList<String>();

        for (UserXML userXml : this.roleDataMap.get(application).getUserList()) {
            if (userXml.getUserId().equals(userId)) {
                for (String role : userXml.getRoleList()) {
                    userRoles.add(role);
                }
                break;
            }
        }

        return userRoles.toArray(new String[userRoles.size()]);
    }

    /**
     * Get a list of user permissions.
     * 
     * @param userId
     *            The user id
     * @param application
     *            The application
     * @return The array of user permissions
     */
    public String[] getUserPermissions(String userId, String application) {
        ArrayList<String> userPermissions = new ArrayList<String>();

        for (UserXML userXml : this.roleDataMap.get(application).getUserList()) {
            if (userXml.getUserId().equals(userId)) {
                for (String permission : userXml.getPermissionList()) {
                    userPermissions.add(permission);
                }
                break;
            }
        }

        return userPermissions.toArray(new String[userPermissions.size()]);
    }

    public String[] getRolePermissions(String roleId, String application) {
        ArrayList<String> rolePerms = new ArrayList<String>();

        for (RoleXML roleXml : roleDataMap.get(application).getRoleList()) {
            if (roleXml.getRoleId().equals(roleId)) {
                for (String perm : roleXml.getPermissionList()) {
                    rolePerms.add(perm);
                }
            }
        }

        return rolePerms.toArray(new String[rolePerms.size()]);
    }

    /**
     * @param application
     * @return
     */
    public List<IPermission> getPermissions(String application) {
        List<IPermission> permissions = new ArrayList<IPermission>();
        NwsRoleData roleData = roleDataMap.get(application);

        for (PermissionXML xml : roleData.getPermissionList()) {
            String id = xml.getId();
            String description = xml.getDescription();
            permissions.add(new NwsPermission(id, description));
        }
        return permissions;
    }

    /**
     * @return
     */
    public List<IRole> getRoles(String application) {
        List<IRole> roles = new ArrayList<IRole>();
        for (NwsRoleData roleData : roleDataMap.values()) {
            for (RoleXML xml : roleData.getRoleList()) {
                String id = xml.getRoleId();
                String description = xml.getRoleDescription();
                roles.add(new NwsRole(id,
                        getPermissionsForRole(id, application), description));
            }
        }
        return roles;
    }

    private List<IPermission> getPermissionsForRole(String roleId,
            String application) {
        List<IPermission> rolePerms = new ArrayList<IPermission>();
        List<String> permissionIds = Arrays.asList(getRolePermissions(roleId,
                application));

        for (PermissionXML roleXml : roleDataMap.get(application)
                .getPermissionList()) {
            if (permissionIds.contains(roleXml.getId())) {
                rolePerms.add(new NwsPermission(roleXml.getId(), roleXml
                        .getDescription()));
            }
        }

        return rolePerms;
    }

    /**
     * Return a list of applications that have roles/permissions defined.
     * 
     * @return String[] of application names
     */
    public String[] getApplications() {
        return roleDataMap.keySet().toArray(new String[roleDataMap.size()]);
    }

    public void addUser(String user, String application) {
        if (user != null && user.length() > 0) {
            UserXML userXml = new UserXML();
            userXml.setUserId(user);
            this.roleDataMap.get(application).getUserList().add(userXml);
        }
    }

    public void addRole(String role, String description, String application) {
        if (role != null && description != null && role.length() > 0
                && description.length() > 0) {
            RoleXML roleXml = new RoleXML();
            roleXml.setRoleDescription(description);
            roleXml.setRoleId(role);
            this.roleDataMap.get(application).getRoleList().add(roleXml);
        }
    }

    public void deleteUser(String user, String application) {
        if (user != null && user.length() > 0) {
            int idx = -1;
            for (UserXML u : roleDataMap.get(application).getUserList()) {
                idx++;
                if (u.getUserId().equalsIgnoreCase(user)) {
                    roleDataMap.get(application).getUserList().remove(idx);
                    break;
                }
            }
        }
    }

    public void deleteRole(String role, String application) {
        if (role != null && role.length() > 0) {
            int idx = -1;
            for (RoleXML r : roleDataMap.get(application).getRoleList()) {
                idx++;
                if (r.getRoleId().equalsIgnoreCase(role)) {
                    roleDataMap.get(application).getRoleList().remove(idx);
                    break;
                }
            }
        }
    }

    /**
     * Save the NwsRoleData object.
     * 
     * @param application
     */
    public void save(String application) {
        NwsRoleData roleData = roleDataMap.get(application);
        LocalizationFile lf = roleFileMap.get(application);

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        LocalizationFile locFile = pm
                .getLocalizationFile(context, lf.getName());
        ;

        try {
            marshaller.marshal(roleData, locFile.getFile());
            locFile.save();
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (LocalizationOpFailedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

    }

    private void readXML() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext[] contexts = new LocalizationContext[2];
            contexts[0] = pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.BASE);
            contexts[1] = pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE);
            LocalizationFile[] roleFiles = pm.listFiles(contexts, ROLE_DIR,
                    null, false, true);

            for (LocalizationFile lf : roleFiles) {
                File f = lf.getFile(true);
                if (f != null && f.exists()) {
                    System.out.println(f.getAbsolutePath());
                    NwsRoleData roleData = (NwsRoleData) unmarshaller
                            .unmarshal(f);
                    this.roleDataMap.put(roleData.getApplication(), roleData);
                    this.roleFileMap.put(roleData.getApplication(), lf);
                }
            }
        } catch (JAXBException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Reload theXML files from disk.
     */
    public void reloadXML() {
        readXML();
    }
}
