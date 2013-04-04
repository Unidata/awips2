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

import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryAuthRequest;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RequestFromServerPermissionsService implements IPermissionsService {

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
     * Send an authorization request. Private because the method of constructing
     * an authorization request and processing the response should remain
     * isolated to this utility class.
     * 
     * @param request
     *            The request object
     * @return DataDeliveryAuthReqeust object
     * @throws VizException
     */
    private DataDeliveryAuthRequest sendAuthorizationRequest(
            DataDeliveryAuthRequest request) throws VizException {
        try {
            return (DataDeliveryAuthRequest) ((SuccessfulExecution) RequestRouter
                    .route(request, DataDeliveryConstants.DATA_DELIVERY_SERVER))
                    .getResponse();
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IAuthorizedPermissionResponse checkPermissionToChangeSubscription(
            final IUser user, String notAuthorizedMessage,
            final Subscription subscription) throws VizException {

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

        DataDeliveryAuthRequest r = sendAuthorizationRequest(request);

        return new DataDeliveryAuthRequestAdapter(r);
    }
}
