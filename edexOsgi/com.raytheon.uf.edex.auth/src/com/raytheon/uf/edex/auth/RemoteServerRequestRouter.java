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

import java.rmi.RemoteException;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IRequestRouter;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.edex.auth.req.ServerPrivilegedRequestHandler;

/**
 * {@link IRequestRouter} implementation that transfers requests to another
 * server for processing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2012 1322       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RemoteServerRequestRouter implements IRequestRouter {

    private final String httpAddress;

    /**
     * Constructor.
     * 
     * @param httpAddress
     *            the http address to be used to connect to the remote server
     */
    public RemoteServerRequestRouter(String httpAddress) {
        this.httpAddress = httpAddress;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object route(IServerRequest request) throws Exception {
        // Wrap privileged requests so they are not checked for privileges
        // internally to the server
        if (request instanceof AbstractPrivilegedRequest) {
            request = new ServerPrivilegedRequestHandler.ServerPrivilegedRequest(request);
        }

        byte[] message = SerializationUtil.transformToThrift(request);
        byte[] response = null;
        try {
            response = sendSerializedRequest(message);
        } catch (Exception e) {
            throw new RemoteException("Error communicating with the server.", e);
        }
        Object rval = null;
        if (response != null) {
            rval = SerializationUtil
                    .transformFromThrift(Object.class, response);
        }
        return rval;
    }

    /**
     * Send the serialized request, and return the response. Package-access so
     * the test can override for verification.
     * 
     * @param message
     *            the binary data
     * @return the response
     * @throws CommunicationException
     * @throws Exception
     */
    byte[] sendSerializedRequest(byte[] message)
            throws CommunicationException, Exception {
        return HttpClient.getInstance()
                .postBinary(httpAddress, message);
    }

}
