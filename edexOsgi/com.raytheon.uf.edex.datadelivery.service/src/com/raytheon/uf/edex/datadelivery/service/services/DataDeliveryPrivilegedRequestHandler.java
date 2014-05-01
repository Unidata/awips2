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
package com.raytheon.uf.edex.datadelivery.service.services;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermission;
import com.raytheon.uf.common.plugin.nwsauth.NwsAuthRequest;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;

/**
 * Handler for Data Delivery Privileged Requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012  224       mpduff      Initial creation
 * Oct 03, 2012  1241      djohnson    Use {@link DataDeliveryPermission}.
 * Jul 26, 2031  2232      mpduff      Refactored Data Delivery permissions.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataDeliveryPrivilegedRequestHandler<T extends AbstractPrivilegedRequest>
        extends AbstractPrivilegedRequestHandler<T> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler#authorized
     * (com.raytheon.uf.common.auth.user.IUser,
     * com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest)
     */
    @Override
    public AuthorizationResponse authorized(IUser user, T request)
            throws AuthorizationException {
        if (request instanceof NwsAuthRequest) {
            NwsAuthRequest authRequest = (NwsAuthRequest) request;

            AuthManager manager = AuthManagerFactory.getInstance().getManager();
            IRoleStorage roles = manager.getRoleStorage();

            boolean addedAuthorization = false;

            for (String permission : authRequest.getRequestedPermissions()) {
                boolean authorized = roles.isAuthorized(permission.toString(),
                        user.uniqueId().toString(), "Data Delivery");
                addedAuthorization |= authorized;

                if (authorized) {
                    authRequest.addAuthorized(permission);
                }
            }

            if (addedAuthorization) {
                return new AuthorizationResponse(true);
            }

            return new AuthorizationResponse(
                    authRequest.getNotAuthorizedMessage());
        }

        return new AuthorizationResponse(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(T request) throws Exception {
        /*
         * We are only looking for authorized or not and don't have any work to
         * do here. If we get here then we are authorized, set true in request
         * object and return it
         */
        if (request instanceof NwsAuthRequest) {
            NwsAuthRequest r = (NwsAuthRequest) request;
            r.setAuthorized(true);
        }

        return request;
    }
}
