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
package com.raytheon.uf.common.comm;

import java.rmi.RemoteException;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;

/**
 * Performs a thrift communication with the server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Moved from ThriftRegistryHandler.
 * Nov 09, 2012 1286       djohnson     Move out of registry code.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ThriftCommunicator<REQUEST extends AbstractPrivilegedRequest, RESPONSE> {

    /**
     * Provides the configuration required to communicate via thrift.
     */
    public static interface IThriftCommunicatorConfiguration {
        IUser getUser();

        String getServer();
    }

    private final IThriftCommunicatorConfiguration config;

    private ThriftCommunicator(IThriftCommunicatorConfiguration configuration) {
        this.config = configuration;
    }

    /**
     * Sends a request via thrift.
     * 
     * @param request
     * @throws RemoteException
     *             on error communicating with the server
     * @throws SerializationException
     *             on error serializing the request
     * @throws ClassCastException
     *             on receiving a response object that is neither a
     *             {@link ServerErrorResponse} nor of type RESPONSE
     */
    @SuppressWarnings("unchecked")
    public RESPONSE sendRequestViaThrift(REQUEST request)
            throws RemoteException,
            SerializationException {
        String httpAddress = config.getServer();
        if (httpAddress == null) {
            throw new IllegalStateException(
                    "The thrift url is null, please verify proper configuration!");
        }

        request.setUser(config.getUser());
        byte[] message = SerializationUtil.transformToThrift(request);
        byte[] response = null;
        try {
            response = HttpClient.getInstance()
                    .postBinary(httpAddress, message);
        } catch (Exception e) {
            throw new RemoteException("Error communicating with the server.", e);
        }
        Object rval = null;
        if (response != null) {
            rval = SerializationUtil
                    .transformFromThrift(Object.class, response);
        }
        if (rval instanceof ServerErrorResponse) {
            ServerErrorResponse resp = (ServerErrorResponse) rval;
            Throwable serverException = ExceptionWrapper.unwrapThrowable(resp
                    .getException());
            throw new RemoteException(
                    "Trapped throwable communicating with server.",
                    serverException);
        }

        return (RESPONSE) rval;
    }

    /**
     * Static method to retrieve an instance. Used to simplify generics, and
     * allow for implementation class changes later.
     * 
     * @param config
     *            the configuration object
     * @return the thrift communicator
     */
    public static <REQUEST extends AbstractPrivilegedRequest, RESPONSE> ThriftCommunicator<REQUEST, RESPONSE> newInstance(
            IThriftCommunicatorConfiguration config) {
        return new ThriftCommunicator<REQUEST, RESPONSE>(config);
    }
}
