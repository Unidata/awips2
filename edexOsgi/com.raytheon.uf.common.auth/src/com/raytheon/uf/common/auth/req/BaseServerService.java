package com.raytheon.uf.common.auth.req;

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

import java.rmi.RemoteException;

import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

/**
 * Base class for services that send requests to a server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2013 1643       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class BaseServerService<T extends IServerRequest> {

    protected final String serviceKey;

    /**
     * Constructor.
     * 
     * @param serviceKey
     *            the service key
     */
    protected BaseServerService(String serviceKey) {
        this.serviceKey = serviceKey;
    }

    /**
     * Send a request to the server.
     * 
     * @param request
     * @return
     * @throws Exception
     */
    protected final <U> U sendRequest(T request, Class<U> responseType)
            throws RemoteException {
        Object object;
        try {
            object = getResponseFromServer(request);
        } catch (Exception e) {
            throw new RemoteException(
                    "An exception occurred while communicating with the server",
                    e);
        }
        return unwrapResponse(responseType, object);
    }

    /**
     * Send a request to the server.
     * 
     * @param request
     *            the request
     * @throws Exception
     */
    protected final void sendRequest(T request) throws RemoteException {
        sendRequest(request, Void.class);
    }

    protected Object getResponseFromServer(T request) throws Exception {
        return RequestRouter.route(request, serviceKey);
    }

    protected <U> U unwrapResponse(Class<U> responseType, Object object)
            throws RemoteException {
        if (object != null) {
            final Class<? extends Object> objectClass = object.getClass();
            if (!responseType.isAssignableFrom(objectClass)) {
                throw new RemoteException(
                        "Received an unexpected object type of "
                                + objectClass.getName() + " from the server!");
            }
        }
        return responseType.cast(object);
    }
}
