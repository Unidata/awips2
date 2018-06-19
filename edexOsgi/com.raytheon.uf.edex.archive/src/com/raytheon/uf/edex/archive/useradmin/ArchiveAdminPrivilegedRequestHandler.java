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
package com.raytheon.uf.edex.archive.useradmin;

import com.raytheon.uf.common.archive.request.ArchiveAdminAuthRequest;
import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.edex.auth.AuthManagerFactory;
import com.raytheon.uf.edex.auth.IPermissionsManager;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;

/**
 * Handler for Archive Admin Privileged Requests.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 02, 2013  2326     rferrel   Initial creation.
 * May 28, 2014  3211     njensen   Use IAuthorizer instead of IRoleStorage
 * Jul 17, 2017  6288     randerso  Changed to use new Roles/Permissions
 *                                  framework
 *
 * </pre>
 *
 * @author rferrel
 */

public class ArchiveAdminPrivilegedRequestHandler
        extends AbstractPrivilegedRequestHandler<ArchiveAdminAuthRequest> {

    @Override
    public ArchiveAdminAuthRequest handleRequest(
            ArchiveAdminAuthRequest request) throws Exception {
        /*
         * If it reaches this point in the code, then the user is authorized, so
         * just return the request object with authorized set to true.
         */
        request.setAuthorized(true);
        return request;
    }

    @Override
    public AuthorizationResponse authorized(ArchiveAdminAuthRequest request)
            throws AuthorizationException {

        IPermissionsManager manager = AuthManagerFactory.getInstance()
                .getPermissionsManager();

        boolean authorized = manager.isPermitted(request.getRoleId());

        if (authorized) {
            return new AuthorizationResponse(authorized);
        } else {
            return new AuthorizationResponse(request.getNotAuthorizedMessage());
        }
    }
}
