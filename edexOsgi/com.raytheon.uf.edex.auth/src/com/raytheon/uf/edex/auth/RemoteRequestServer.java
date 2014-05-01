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
package com.raytheon.uf.edex.auth;

import com.raytheon.uf.common.auth.AuthException;
import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthenticationResponse;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.auth.resp.ResponseFactory;

/**
 * The server used for executing requests through handlers. Request canonical
 * name should be used to map requests to handlers (register in spring.xml
 * files)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009            mschenke     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RemoteRequestServer {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteRequestServer.class);

    private static final RemoteRequestServer instance = new RemoteRequestServer();

    private HandlerRegistry registry;

    private Boolean ableToValidatePrivilegedRequests = null;

    public static RemoteRequestServer getInstance() {
        return instance;
    }

    private RemoteRequestServer() {

    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public Object handleThriftRequest(IServerRequest request) throws Exception {
        String id = request.getClass().getCanonicalName();
        IRequestHandler handler = registry.getRequestHandler(id);

        validateObjects();

        if (request instanceof AbstractPrivilegedRequest
                && ableToValidatePrivilegedRequests) {
            AuthManager manager = AuthManagerFactory.getInstance().getManager();

            // Not the default role, attempt to cast handler and request
            try {
                AbstractPrivilegedRequest privReq = (AbstractPrivilegedRequest) request;
                AbstractPrivilegedRequestHandler privHandler = (AbstractPrivilegedRequestHandler) handler;

                IUser user = privReq.getUser();

                // Do not process request if user passed in is null
                if (user == null || user.uniqueId() == null) {
                    return ResponseFactory
                            .constructNotAuthorized(privReq,
                                    "Unable to process privileged request for null user");
                }

                // check handler if user has authorization
                AuthorizationResponse authResp = privHandler.authorized(user,
                        privReq);
                if (authResp != null && !authResp.isAuthorized() && authResp.getResponseMessage() != null) {
                    return ResponseFactory.constructNotAuthorized(privReq,
                            authResp.getResponseMessage());
                }

                // user has role, check if authenticated
                AuthenticationResponse resp = manager.getAuthenticator()
                        .authenticate(user);
                if (!resp.isAuthenticated()) {
                    return ResponseFactory.constructNotAuthenticated(privReq,
                            resp.getUpdatedData());
                }

                try {
                    return ResponseFactory.constructSuccessfulExecution(
                            privHandler.handleRequest(privReq),
                            resp.getUpdatedData());
                } catch (Throwable t) {
                    throw new AuthException(resp.getUpdatedData(), t);
                }
            } catch (ClassCastException e) {
                throw new AuthException(
                        "Roles can only be defined for requests/handlers of AbstractPrivilegedRequest/Handler",
                        e);

            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occured while performing privileged request", t);
                throw new AuthException(
                        "Error occured while performing privileged request", t);
            }
        }

        return handler.handleRequest(request);
    }

    public void setRegistry(HandlerRegistry registry) {
        this.registry = registry;
    }

    private void validateObjects() {
        // If first request, validate role and authentication objects
        if (ableToValidatePrivilegedRequests == null) {
            AuthManager manager = AuthManagerFactory.getInstance().getManager();
            ableToValidatePrivilegedRequests = manager != null
                    && manager.getAuthenticationStorage() != null
                    && manager.getAuthenticator() != null
                    && manager.getRoleStorage() != null;
            if (!ableToValidatePrivilegedRequests) {
                IllegalStateException throwable = new IllegalStateException(
                        "Unable to perform priviledged request validation, required objects not set (IAuthenticator, IRoleStorage, IAuthenticationStorage).  ALL REQUESTS WILL BE EXECUTED!");
                statusHandler
                        .handle(Priority.PROBLEM,
                        throwable.getLocalizedMessage(), throwable);
            }
        }
    }
}
