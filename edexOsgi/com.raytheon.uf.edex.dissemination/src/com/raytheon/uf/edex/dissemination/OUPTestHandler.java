package com.raytheon.uf.edex.dissemination;

import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.dissemination.OUPTestRequest;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;

/**
 * Check if an OUPRequest will work
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Nov 20, 2013  16777    D. Friedman  Initial creation
 * Jul 18, 2017  6288     randerso     Changed to use new Roles/Permissions
 *                                     framework
 *
 * </pre>
 *
 */
public class OUPTestHandler
        extends AbstractPrivilegedRequestHandler<OUPTestRequest> {

    private OUPHandler oupHandler;

    @Override
    public Object handleRequest(OUPTestRequest request) throws Exception {
        return oupHandler.handleOUPRequest(request.getOupRequest(), true);
    }

    @Override
    public AuthorizationResponse authorized(OUPTestRequest request)
            throws AuthorizationException {
        return oupHandler.authorized(request.getOupRequest());
    }

    public OUPHandler getOupHandler() {
        return oupHandler;
    }

    public void setOupHandler(OUPHandler oupHandler) {
        this.oupHandler = oupHandler;
    }

}
