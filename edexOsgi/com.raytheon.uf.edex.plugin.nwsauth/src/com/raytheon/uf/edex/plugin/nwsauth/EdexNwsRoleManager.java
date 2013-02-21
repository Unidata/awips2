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
package com.raytheon.uf.edex.plugin.nwsauth;

import java.util.Map;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.plugin.nwsauth.NwsRoleDataRequest;
import com.raytheon.uf.common.plugin.nwsauth.NwsRoleDataRequest.NwsRoleDataRequestType;
import com.raytheon.uf.common.plugin.nwsauth.xml.NwsRoleData;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.useradmin.request.UserAuthenticationDataChanged;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;

/**
 * Receives requests to retrieve or submit NWS role data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 09, 2013 1412       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EdexNwsRoleManager extends
        AbstractPrivilegedRequestHandler<NwsRoleDataRequest> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(NwsRoleDataRequest request) throws Exception {
        final FileManager fileManager = FileManager.getInstance();
        final NwsRoleDataRequestType type = request.getType();
        switch (type) {
        case REQUEST:
            request.setRoleDataMap(fileManager.getRoleDataMap());
            break;
        case SUBMIT:
            final Map<String, NwsRoleData> roleDataMap = request
                    .getRoleDataMap();
            fileManager.writeApplicationRoleData(roleDataMap);

            RequestRouter.route(new UserAuthenticationDataChanged());
            break;
        }

        return request;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AuthorizationResponse authorized(IUser user,
            NwsRoleDataRequest request) throws AuthorizationException {
        return new AuthorizationResponse(true);
    }
}
