package com.raytheon.uf.edex.plugin.nwsauth;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.plugin.nwsauth.exception.RoleApplicationNotFoundException;
import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;

/**
 * Implementation of IRoleStorage
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
public class NwsRoleStorage implements IRoleStorage {
    private static NwsRoleStorage instance = null;

    private final Map<String, File> lastUsedFileMap = new HashMap<String, File>();

    private final Map<String, Long> lastModificationTimeMap = new HashMap<String, Long>();

    private final Map<String, NwsRoleData> applicationRoleMap = new HashMap<String, NwsRoleData>();

    /**
     * This is called from the CAVE side for the User Administration dialogs. Do
     * not call this from EDEX, use AuthManager instead
     * 
     * @return Instance of NwsRoleStorage
     */
    public static NwsRoleStorage getInstance() {
        if (instance == null) {
            instance = new NwsRoleStorage();
        }

        return instance;
    }

    private NwsRoleStorage() {
        getRoleDataFiles();
    }

    private synchronized void getRoleDataFiles() {
        IPathManager pm = PathManagerFactory.getPathManager();

        // Check COMMON_STATIC base and site levels
        LocalizationContext[] contexts = new LocalizationContext[2];
        contexts[0] = pm.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        contexts[1] = pm.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);

        String[] extensions = new String[] { ".xml" };
        LocalizationFile[] localizationFiles =
                PathManagerFactory.getPathManager().listFiles(contexts, "roles", extensions, true, true);

        File file = null;
        for (LocalizationFile locFile : localizationFiles) {
            NwsRoleData roleData = new NwsRoleData();
            if (locFile.exists()) {
                file = locFile.getFile();

                if (lastUsedFileMap.get(locFile.getName()) == null
                        || (file != null && (file.equals(lastUsedFileMap.get(locFile.getName())) == false || file
                                .lastModified() > lastModificationTimeMap.get(locFile.getName())))) {
                    // First time we found a role file, or we have a different
                    // file to
                    // use or we were modified since our last check
                    lastUsedFileMap.put(locFile.getName(), file);
                    try {
                        roleData = (NwsRoleData) SerializationUtil.jaxbUnmarshalFromXmlFile(file.getAbsolutePath());
                        applicationRoleMap.put(roleData.getApplication(), roleData);
                    } catch (Exception e) {
                        UFStatus.getHandler().handle(Priority.PROBLEM, "Error loading file: " + file.getName(), e);
                    }

                    lastModificationTimeMap.put(locFile.getName(), file.lastModified());
                }
            }
        }
    }

    /**
     * Get the role/permission data for the application specified.
     * 
     * @param application
     *            The application
     * 
     * @return the NWSRoleData object for that application.
     */
    public NwsRoleData getRoleData(String application)
            throws AuthorizationException {
        getRoleDataFiles();
        NwsRoleData roleData = applicationRoleMap.get(application);
        if (roleData == null) {
            throw new RoleApplicationNotFoundException("Application name, \""
                    + application
                    + "\", was not found in the authorization configuration.");
        }
        return roleData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.plugin.nwsauth.roles.IRoleStorage#isAuthorized
     * (java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public boolean isAuthorized(String permission, String user,
            String application) throws AuthorizationException {
        NwsRoleData roleData = getRoleData(application);
        return roleData.isAuthorized(permission, user);
    }
    
    @Override
    public String[] getAllDefinedPermissions(String application)
            throws AuthorizationException {
        return getRoleData(application).getAllDefinedPermissions();
    }
}
