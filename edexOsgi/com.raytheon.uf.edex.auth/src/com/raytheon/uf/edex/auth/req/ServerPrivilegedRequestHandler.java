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
package com.raytheon.uf.edex.auth.req;

import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.edex.auth.HandlerRegistry;

/**
 * Allows a server and server->server request to bypass the normal privileged
 * request checks. This class MUST remain in an EDEX specific plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2012 1322       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ServerPrivilegedRequestHandler
        implements
        IRequestHandler<ServerPrivilegedRequestHandler.ServerPrivilegedRequest> {

    /**
     * Wraps a server request in a special request that allows server->server
     * communication to bypass privileged request verification.
     */
    @DynamicSerialize
    public static class ServerPrivilegedRequest implements IServerRequest {

        @DynamicSerializeElement
        private IServerRequest wrappedRequest;

        /**
         * Added only to comply with dynamic serialization.
         * 
         * @deprecated added only to comply with dynamic serialization
         */
        @Deprecated
        public ServerPrivilegedRequest() {
        }

        /**
         * Constructor.
         * 
         * @param wrappedRequest
         *            the request to wrap
         */
        public ServerPrivilegedRequest(IServerRequest wrappedRequest) {
            this.wrappedRequest = wrappedRequest;
        }

        /**
         * @return the wrappedRequest
         */
        public IServerRequest getWrappedRequest() {
            return wrappedRequest;
        }

        /**
         * @param wrappedRequest
         *            the wrappedRequest to set
         */
        public void setWrappedRequest(IServerRequest wrappedRequest) {
            this.wrappedRequest = wrappedRequest;
        }
    }

    private final HandlerRegistry registry;

    /**
     * Constructor.
     * 
     * @param handlerRegistry
     *            the handler registry
     */
    public ServerPrivilegedRequestHandler(HandlerRegistry handlerRegistry) {
        this.registry = handlerRegistry;
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Object handleRequest(ServerPrivilegedRequest request) throws Exception {
        // Delegate to the handler for the wrapped request
        String id = request.wrappedRequest.getClass().getCanonicalName();
        IRequestHandler handler = registry.getRequestHandler(id);

        // Send back a SuccessfulExecution since that's expected of privileged
        // requests
        SuccessfulExecution response = new SuccessfulExecution();
        response.setResponse(handler.handleRequest(request.wrappedRequest));

        return response;
    }
}
