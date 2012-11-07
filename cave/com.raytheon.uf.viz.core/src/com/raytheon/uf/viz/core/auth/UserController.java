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
package com.raytheon.uf.viz.core.auth;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.auth.resp.UserNotAuthenticated;
import com.raytheon.uf.common.auth.resp.UserNotAuthorized;
import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.INotAuthHandler;

/**
 * Class for looking up and managing the current user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2010            mschenke     Initial creation
 * Nov 06, 2012 1302       djohnson     Add ability to retrieve the {@link IUserManager}.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class UserController {
    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(UserController.class, "CAVE");

    private static final String EXTENSION_POINT = "com.raytheon.uf.viz.core.userManager";

    private static IUserManager manager;

    static {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(EXTENSION_POINT);
        if (point != null) {
            IExtension[] extensions = point.getExtensions();

            for (IExtension ext : extensions) {
                for (IConfigurationElement elem : ext
                        .getConfigurationElements()) {
                    if (manager != null) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Not using user authentication manager: "
                                                + elem.getAttribute("class")
                                                + ".\nViz does not currently support multiple authentication methods,"
                                                + " using first one found in extension point.  Remove any authentication"
                                                + " plugins not on edex server");
                    } else {
                        try {
                            manager = (IUserManager) elem
                                    .createExecutableExtension("class");
                        } catch (CoreException e) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Error creating IUserManager from extension point",
                                            e);
                        }
                    }
                }
            }
        }

        if (manager == null) {
            manager = new IUserManager() {
                @Override
                public IUser getUserObject() {
                    return null;
                }

                @Override
                public void updateUserObject(IUser user,
                        IAuthenticationData authData) {

                }

                @Override
                public INotAuthHandler getNotAuthHandler() {
                    return new INotAuthHandler() {
                        @Override
                        public Object notAuthenticated(
                                UserNotAuthenticated response)
                                throws VizException {
                            throw new VizException(
                                    "Could not perform request, user is not authenticated with server.");
                        }

                        @Override
                        public Object notAuthorized(UserNotAuthorized response)
                                throws VizException {
                            throw new VizException(response.getMessage());
                        }
                    };
                }

                /**
                 * {@inheritDoc}
                 */
                @Override
                public List<IPermission> getPermissions(String application) {
                    return Collections.emptyList();
                }

                @Override
                public List<IRole> getRoles(String application) {
                    return Collections.emptyList();
                }

            };
        }
    }


    public static IUser getUserObject() {
        return manager.getUserObject();
    }

    public static void updateUserData(IAuthenticationData data) {
        manager.updateUserObject(manager.getUserObject(), data);
    }

    public static INotAuthHandler getNotAuthHandler() {
        return manager.getNotAuthHandler();
    }

    /**
     * @return the manager
     */
    public static IUserManager getManager() {
        return manager;
    }
}
