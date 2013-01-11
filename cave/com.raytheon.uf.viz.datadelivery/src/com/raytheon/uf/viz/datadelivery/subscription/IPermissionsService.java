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
package com.raytheon.uf.viz.datadelivery.subscription;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.viz.core.exception.VizException;

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
        boolean hasPermission(DataDeliveryPermission permission);
    }

    /**
     * Check whether a user has the permissions to change a subscription.
     * 
     * @param user
     *            the user requesting to change the subscription
     * @param notAuthorizedMessage
     *            the message that should be displayed if they are not
     *            authorized
     * @param subscription
     *            the subscription they are attempting to change
     * @return the response
     * @throws VizException
     */
    public IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final Subscription subscription) throws VizException;

    /**
     * Check whether a user has the specified permissions.
     * 
     * @param user
     *            the user to check permissions for
     * @param notAuthorizedMessage
     *            the not authorized message that should be displayed if they
     *            are not authorized
     * @param permission
     *            the permission to check
     * @return IAuthorizedPermissionResponse the response
     * @throws VizException
     *             on error checking permissions
     */
    public IAuthorizedPermissionResponse checkPermission(IUser user,
            String notAuthorizedMessage, DataDeliveryPermission permission)
            throws VizException;

    /**
     * Check whether a user has one of the specified permissions.
     * 
     * @param user
     *            the user to check permissions for
     * @param notAuthorizedMessage
     *            the not authorized message that should be displayed if they
     *            are not authorized
     * @param permissions
     *            the permissions to check
     * @return IAuthorizedPermissionResponse the response
     * @throws VizException
     *             on error checking permissions
     */
    public IAuthorizedPermissionResponse checkPermissions(IUser user,
            String notAuthorizedMessage, DataDeliveryPermission... permissions)
            throws VizException;
}
