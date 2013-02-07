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
package com.raytheon.edex.services;

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;

/**
 * Abstract privileged request handler for localization requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2011             mschenke    Initial creation
 * Jul 8, 2012  719        mpduff      Fix order of checks
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 * @param <T>
 */
public abstract class AbstractPrivilegedLocalizationRequestHandler<T extends AbstractPrivilegedRequest>
        extends AbstractPrivilegedRequestHandler<T> {

    private static final String PATH_SEPARATOR = IPathManager.SEPARATOR;

    private static final String SEPARATOR = ".";

    private static final String ROLE_PREFIX = "com.raytheon.localization";

    private static final String APPLICATION = "Localization";

    protected AuthorizationResponse getAuthorizationResponse(IUser user,
            LocalizationContext context, String fileName, String myContextName)
            throws AuthorizationException {
        String contextName = context.getContextName();
        LocalizationLevel level = context.getLocalizationLevel();
        LocalizationType type = context.getLocalizationType();
        boolean contextsMatch = (myContextName != null && myContextName
                .equals(contextName));
        if (level.isSystemLevel()) {
            return new AuthorizationResponse(false,
                    "Modification to system level configuration is prohibited.");
        } else if (level == LocalizationLevel.USER && contextsMatch) {
            // Don't prevent users from modifying own files
            return new AuthorizationResponse(true);
        }

        AuthManager manager = AuthManagerFactory.getInstance().getManager();
        IRoleStorage roleStorage = manager.getRoleStorage();
        String[] permissions = roleStorage
                .getAllDefinedPermissions(APPLICATION);
        Set<String> definedPermissions = new HashSet<String>();
        for (String permission : permissions) {
            definedPermissions.add(permission.toLowerCase());
        }

        String absoluteRoleId = buildRoleId(level, type, contextName, fileName);
        // First round check com.raytheon.localization.level
        // Second round check com.raytheon.localization.level.name
        for (int i = 0; i < 2; ++i) {
            String contextNameToUse = i > 0 ? contextName : null;
            String roleId = buildRoleId(level, type, contextNameToUse, fileName);

            // check most specific to least specific
            // com.raytheon.localization.<level>.(<specificLevel>.)/type/path/name/
            int minLength = roleId.length() - fileName.length() - 1;
            do {
                if (roleStorage.isAuthorized(roleId,
                        user.uniqueId().toString(), APPLICATION)) {
                    return new AuthorizationResponse(true);
                } else if (definedPermissions.contains(roleId.toLowerCase())) {
                    // User not authorized and this roleId is explicitly defined
                    return notAuthorized(user, absoluteRoleId);
                }

                roleId = roleId.substring(0,
                        roleId.lastIndexOf(PATH_SEPARATOR, roleId.length()));
            } while (roleId.length() >= minLength);
        }

        if (level == LocalizationLevel.WORKSTATION && contextsMatch) {
            // If no rule found and user is attempting to modify workstation
            // they are using, default to allow
            return new AuthorizationResponse(true);
        }

        return notAuthorized(user, absoluteRoleId);
    }

    private String buildRoleId(LocalizationLevel level, LocalizationType type,
            String contextName, String fileName) {
        String roleId = ROLE_PREFIX + SEPARATOR + level;
        if (contextName != null) {
            roleId += SEPARATOR + contextName;
        }
        roleId += PATH_SEPARATOR + type;
        roleId += PATH_SEPARATOR + fileName;
        return roleId;
    }

    private AuthorizationResponse notAuthorized(IUser user, String roleId) {
        return new AuthorizationResponse(false, "User, " + user.uniqueId()
                + ", is not authorized to perform request needing role: "
                + roleId);
    }
}
