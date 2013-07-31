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

import java.rmi.RemoteException;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.req.IPermissionsService;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.SharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.datadelivery.service.BasePrivilegedDataDeliveryService;
import com.raytheon.uf.common.plugin.nwsauth.NwsAuthRequest;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * {@link IPermissionsService} implementation that requests permissions from the
 * server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 04, 2013 1441       djohnson     Initial creation
 * Jan 21, 2013 1441       djohnson     Use RequestRouter.
 * Feb 26, 2013 1643       djohnson     Extends base class.
 * Mar 29, 2013 1841       djohnson     Subscription is now UserSubscription.
 * May 21, 2013 2020       mpduff       Rename UserSubscription to SiteSubscription.
 * Jul 26, 2031   2232     mpduff       Refactored Data Delivery permissions, removed DataDeliveryAuthRequest.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RequestFromServerPermissionsService extends
        BasePrivilegedDataDeliveryService<NwsAuthRequest> implements
        IPermissionsService {

    /**
     * Adapts the {@link NwsAuthRequestAdapter} to match the
     * {@link IAuthorizedPermissionResponse} interface.
     */
    private class NwsAuthRequestAdapter implements
            IAuthorizedPermissionResponse {

        private final NwsAuthRequest response;

        /**
         * The response to adapt.
         * 
         * @param response
         */
        private NwsAuthRequestAdapter(NwsAuthRequest response) {
            this.response = response;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isAuthorized() {
            return response != null && response.isAuthorized();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean hasPermission(String permission) {
            return (isAuthorized()) ? response.isAuthorized(permission) : false;
        }
    }

    /**
     * 
     * @param user
     * @param notAuthorizedMessage
     * @param subscription
     * @return
     * @throws VizException
     */
    public IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final Subscription subscription) throws AuthException {

        // TODO: Can this be done better?
        if (subscription instanceof SiteSubscription) {
            return checkPermissionToChangeSubscription(user,
                    notAuthorizedMessage, (SiteSubscription) subscription);
        } else {
            return checkPermissionToChangeSubscription(user,
                    notAuthorizedMessage, (SharedSubscription) subscription);
        }

    }

    private IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final SiteSubscription subscription) throws AuthException {

        String approveSitePermission = DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE
                .toString();
        String approveUserPermission = DataDeliveryPermission.SUBSCRIPTION_APPROVE_USER
                .toString();
        final IAuthorizedPermissionResponse r = checkPermissions(user,
                notAuthorizedMessage, approveSitePermission,
                approveUserPermission);

        // If they have site permissions, then yes they can approve the
        // subscription
        if (r.hasPermission(approveSitePermission)) {
            return r;
        } else {
            // Otherwise they must have user approval permission and be the
            // ownerBaseServerService
            return new IAuthorizedPermissionResponse() {
                @Override
                public boolean isAuthorized() {
                    return r.isAuthorized()
                            && user.uniqueId().toString()
                                    .equals(subscription.getOwner());
                }

                @Override
                public boolean hasPermission(String permission) {
                    return r.hasPermission(permission);
                }
            };
        }
    }

    private IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final SharedSubscription subscription) throws AuthException {

        // TODO: New permission to approve/change shared subscriptions?
        final IAuthorizedPermissionResponse r = checkPermissions(user,
                notAuthorizedMessage,
                DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE.toString());

        return r;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IAuthorizedPermissionResponse checkPermission(IUser user,
            String notAuthorizedMessage, String permission)
            throws AuthException {
        return checkPermissions(user, notAuthorizedMessage,
                new String[] { permission });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IAuthorizedPermissionResponse checkPermissions(IUser user,
            String notAuthorizedMessage, String... permissions)
            throws AuthException {

        NwsAuthRequest request = new NwsAuthRequest();
        request.setUser(user);
        request.addRequestedPermissions(permissions);
        request.setNotAuthorizedMessage(notAuthorizedMessage);

        try {
            NwsAuthRequest r = sendRequest(request, NwsAuthRequest.class);
            return new NwsAuthRequestAdapter(r);
        } catch (RemoteException e) {
            throw new AuthException(e);
        }
    }
}
