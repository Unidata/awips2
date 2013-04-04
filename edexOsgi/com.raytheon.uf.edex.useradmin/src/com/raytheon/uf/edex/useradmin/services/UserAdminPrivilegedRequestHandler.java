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
package com.raytheon.uf.edex.useradmin.services;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.useradmin.request.UserAdminAuthRequest;
import com.raytheon.uf.edex.auth.AuthManager;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.roles.IRoleStorage;

/**
 * Handler for User Admin Privileged Requests.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2012            mpduff      Initial creation.
 * Sep 24, 2012   1157     mpduff      Use the application member variable.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class UserAdminPrivilegedRequestHandler extends
        AbstractPrivilegedRequestHandler<UserAdminAuthRequest> {

    /**
     * Application name. This must match the application tag in the user role
     * file.
     */
    private static final String APPLICATION = "User Administration";

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public UserAdminAuthRequest handleRequest(UserAdminAuthRequest request)
            throws Exception {
        // If it reaches this point in the code, then the user is authorized, so
        // just return the request object with authorized set to true
        request.setAuthorized(true);
        return request;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler#authorized
     * (com.raytheon.uf.common.auth.user.IUser,
     * com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest)
     */
    @Override
    public AuthorizationResponse authorized(IUser user,
            UserAdminAuthRequest request) throws AuthorizationException {

        AuthManager manager = AuthManagerFactory.getInstance().getManager();
        IRoleStorage roleStorage = manager.getRoleStorage();

        boolean authorized = roleStorage.isAuthorized((request).getRoleId(),
                user.uniqueId().toString(), APPLICATION);

        if (authorized) {
            return new AuthorizationResponse(authorized);
        } else {
            return new AuthorizationResponse(
                    (request).getNotAuthorizedMessage());
        }
    }
}
