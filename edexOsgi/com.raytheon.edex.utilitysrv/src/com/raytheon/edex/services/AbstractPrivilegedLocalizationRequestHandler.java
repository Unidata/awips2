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

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRole;
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
 * Jul 6, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 * @param <T>
 */
public abstract class AbstractPrivilegedLocalizationRequestHandler<T extends AbstractPrivilegedRequest>
        extends AbstractPrivilegedRequestHandler<T> {

    protected AuthorizationResponse getAuthorizationResponse(IUser user,
            LocalizationContext context, LocalizationLevel level,
            String fileName, String myContextName) {
        String contextName = context.getContextName();

        if (level.isSystemLevel()) {
            return new AuthorizationResponse(false,
                    "Modification to system level configuration is prohibited.");
        } else if (myContextName != null
                && myContextName.equals(contextName)
                && (context.getLocalizationLevel() == LocalizationLevel.USER || context
                        .getLocalizationLevel() == LocalizationLevel.WORKSTATION)) {
            // If context names match and we are user or workstation file
            // request, that is ok
            return new AuthorizationResponse(true);
        }

        AuthManager manager = AuthManagerFactory.getInstance().getManager();
        IRoleStorage roles = manager.getRoleStorage();

        String roleId = "";
        boolean isValid = true;
        // First round check com.raytheon.localization.level
        // Second round check com.raytheon.localization.level.name
        for (int i = 0; i < 2 && isValid; ++i) {
            roleId = "com.raytheon.localization."
                    + context.getLocalizationLevel().name();
            if (i > 0) {
                if (contextName != null) {
                    roleId += "." + contextName;
                } else {
                    // We already checked this case
                    break;
                }
            }
            // com.raytheon.localization.<level>.(<specificLevel>)
            if (checkRole(roles, roleId, user)) {
                return new AuthorizationResponse(true);
            }

            // com.raytheon.localization.<level>.(<specificLevel>.)/type
            roleId += "/" + context.getLocalizationType().name();
            if (checkRole(roles, roleId, user)) {
                return new AuthorizationResponse(true);
            }

            // check most specific to least specific
            // com.raytheon.localization.<level>.(<specificLevel>.)/type/path/name/
            int minIndex = roleId.length();
            roleId += "/" + fileName;
            int index = roleId.length();
            while (index > minIndex && isValid) {
                roleId = roleId.substring(0, index);
                IRole role = roles.lookupRole(roleId);
                index = roleId.lastIndexOf("/", index - 1);
                if (role.validForUser(user)) {
                    return new AuthorizationResponse(true);
                } else if (!roles.isDefaultRole(role)) {
                    // if not valid for user and is not default role then not
                    // authorized.
                    isValid = false;
                }
            }
        }

        return new AuthorizationResponse(false, "User, " + user.uniqueId()
                + ", is not authorized to perform request needing role: "
                + roleId);
    }

    private boolean checkRole(IRoleStorage roles, String roleId, IUser user) {
        IRole role = roles.lookupRole(roleId);
        return (role != null && role.validForUser(user));
    }
}
