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

import java.util.List;

import com.raytheon.uf.common.auth.user.IAuthenticationData;
import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.viz.core.requests.INotAuthHandler;

/**
 * Interface for interacting with client user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2010            mschenke     Initial creation
 * Nov 06, 2012 1302       djohnson     Add ability to get roles/permissions for an application.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IUserManager {

    /**
     * Get the current user's object
     * 
     * @return
     */
    IUser getUserObject();

    /**
     * Update the user object with the authentication data update
     * 
     * @param user
     * @param authData
     */
    void updateUserObject(IUser user, IAuthenticationData authData);

    /**
     * Get the handler for UserNotAuthenticated and UserNotAuthorized response
     * messages
     * 
     * @return
     */
    INotAuthHandler getNotAuthHandler();

    /**
     * Get the list of permissions.
     * 
     * @param application
     *            the application
     * 
     * @return the permissions
     */
    List<IPermission> getPermissions(String application);

    /**
     * Get the list of roles.
     * 
     * @return the list of roles
     */
    List<IRole> getRoles(String application);
}
