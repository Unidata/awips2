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

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.SharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryAuthRequest;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.datadelivery.service.BasePrivilegedDataDeliveryService;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RequestFromServerPermissionsService extends
        BasePrivilegedDataDeliveryService<DataDeliveryAuthRequest> implements
        IPermissionsService {

    /**
     * Adapts the {@link DataDeliveryAuthRequestAdapter} to match the
     * {@link IAuthorizedPermissionResponse} interface.
     */
    private class DataDeliveryAuthRequestAdapter implements
            IAuthorizedPermissionResponse {

        private final DataDeliveryAuthRequest response;

        /**
         * The response to adapt.
         * 
         * @param response
         */
        private DataDeliveryAuthRequestAdapter(DataDeliveryAuthRequest response) {
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
        public boolean hasPermission(DataDeliveryPermission permission) {
            return (isAuthorized()) ? response.isAuthorized(permission) : false;
        }
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final Subscription subscription) throws VizException {

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
            final SiteSubscription subscription) throws VizException {

        final IAuthorizedPermissionResponse r = checkPermissions(user,
                notAuthorizedMessage,
                DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE,
                DataDeliveryPermission.SUBSCRIPTION_APPROVE_USER);

        // If they have site permissions, then yes they can approve the
        // subscription
        if (r.hasPermission(DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE)) {
            return r;
        } else {
            // Otherwise they must have user approval permission and be the
            // owner
            return new IAuthorizedPermissionResponse() {
                @Override
                public boolean isAuthorized() {
                    return r.isAuthorized()
                            && user.uniqueId().toString()
                                    .equals(subscription.getOwner());
                }

                @Override
                public boolean hasPermission(DataDeliveryPermission permission) {
                    return r.hasPermission(permission);
                }
            };
        }
    }

    private IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final SharedSubscription subscription) throws VizException {

        // TODO: New permission to approve/change shared subscriptions?
        final IAuthorizedPermissionResponse r = checkPermissions(user,
                notAuthorizedMessage,
                DataDeliveryPermission.SUBSCRIPTION_APPROVE_SITE);

        return r;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IAuthorizedPermissionResponse checkPermission(IUser user,
            String notAuthorizedMessage, DataDeliveryPermission permission)
            throws VizException {
        return checkPermissions(user, notAuthorizedMessage,
                new DataDeliveryPermission[] { permission });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IAuthorizedPermissionResponse checkPermissions(IUser user,
            String notAuthorizedMessage, DataDeliveryPermission... permissions)
            throws VizException {

        DataDeliveryAuthRequest request = new DataDeliveryAuthRequest();
        request.setUser(user);
        request.addRequestedPermissions(permissions);
        request.setNotAuthorizedMessage(notAuthorizedMessage);

        try {
            DataDeliveryAuthRequest r = sendRequest(request,
                    DataDeliveryAuthRequest.class);
            return new DataDeliveryAuthRequestAdapter(r);
        } catch (RemoteException e) {
            throw new VizException(e);
        }
    }
}
