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
package com.raytheon.uf.common.auth.req;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.user.IUser;

/**
 * Interface that defines the service to work with permissions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013 1441       djohnson     Initial creation
 * Jul 26, 2013 2232       mpduff       Moved to common.auth.req.
 * Aug 21, 2013 1848       mpduff       Added to javadoc.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IPermissionsService {

    /**
     * Interface for an authorized permission response.
     */
    public static interface IAuthorizedPermissionResponse {
        /**
         * True if the user had one of the specified permissions.
         * 
         * @return true if the user had one of the specified permissions
         */
        boolean isAuthorized();

        /**
         * Returns true if the user had the specific permission.
         * 
         * @param permission
         *            the permission
         * @return true if the user had the specific permission
         */
        boolean hasPermission(String permission);
    }

    /**
     * Check whether a user has the specified permissions. If not authorized an
     * AlertViz message is displayed to the user.
     * 
     * @param user
     *            the user to check permissions for
     * @param notAuthorizedMessage
     *            the not authorized message that should be displayed if they
     *            are not authorized
     * @param permission
     *            the permission to check
     * @return IAuthorizedPermissionResponse the response
     * @throws AuthException
     *             on error checking permissions
     */
    public IAuthorizedPermissionResponse checkPermission(IUser user,
            String notAuthorizedMessage, String permission)
            throws AuthException;

    /**
     * Check whether a user has one of the specified permissions. If no
     * permissions are authorized then an AlertViz message is displayed to the
     * user.
     * 
     * @param user
     *            the user to check permissions for
     * @param notAuthorizedMessage
     *            the not authorized message that should be displayed if they
     *            are not authorized
     * @param permissions
     *            the permissions to check
     * @return IAuthorizedPermissionResponse the response
     * @throws AuthException
     *             on error checking permissions
     */
    public IAuthorizedPermissionResponse checkPermissions(IUser user,
            String notAuthorizedMessage, String... permissions)
            throws AuthException;
}
