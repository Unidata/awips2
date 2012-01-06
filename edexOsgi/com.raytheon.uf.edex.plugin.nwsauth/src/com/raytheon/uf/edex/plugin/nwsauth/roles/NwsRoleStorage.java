package com.raytheon.uf.edex.plugin.nwsauth.roles;

import java.io.File;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.auth.roles.IRole;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;
import com.raytheon.uf.edex.core.EdexException;

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
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NwsRoleStorage.class);

    private static final NwsRole DEFAULT_ROLE = new NwsRole();

    private static final LocalizationLevel BASE = LocalizationContext.LocalizationLevel.BASE;

    private static final LocalizationType EDEX_STATIC = LocalizationContext.LocalizationType.EDEX_STATIC;

    private static final String ROLES_FILE_NAME = "roles" + File.separator
            + "userRoles.xml";

    private NwsRoleData roleData = null;

    private File lastUsedFile = null;

    private long lastModificationTime = 0;

    private synchronized NwsRoleData getRoleData() {
        Map<LocalizationLevel, LocalizationFile> fileMap = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(
                        LocalizationType.EDEX_STATIC, ROLES_FILE_NAME);
        LocalizationLevel[] levels = PathManagerFactory.getPathManager()
                .getAvailableLevels();

        File file = null;
        // reverse search for file to get highest level
        for (int i = levels.length - 1; i >= 0; --i) {
            LocalizationFile locFile = fileMap.get(levels[i]);
            if (locFile != null && locFile.exists()) {
                file = locFile.getFile();
                break;
            }
        }

        if (file == null && lastUsedFile == null) {
            // No file found and lastFile wasn't set before, default
            roleData = new NwsRoleData();
        } else if (lastUsedFile == null
                || (file != null && (file.equals(lastUsedFile) == false || file
                        .lastModified() > lastModificationTime))) {
            // First time we found a role file, or we have a different file to
            // use or we were modified since our last check
            lastUsedFile = file;
            try {
                roleData = NwsRoleData.loadRoleData(file);
            } catch (EdexException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error loading NWS Roles file", e);
                roleData = new NwsRoleData();
            }
            lastModificationTime = file.lastModified();
        }

        return roleData;
    }

    @Override
    public boolean isDefaultRole(IRole role) {
        return role == DEFAULT_ROLE;
    }

    @Override
    public IRole lookupRole(String roleId) {
        if (roleId != null && "".equals(roleId) == false) {
            NwsRoleData roleData = getRoleData();
            NwsRole role = roleData.lookupRole(roleId);
            if (role == null) {
                return DEFAULT_ROLE;
            }
            return role;
        }

        return DEFAULT_ROLE;
    }

}
