package com.raytheon.uf.common.registry.ebxml;

import java.rmi.RemoteException;
import java.util.Arrays;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import org.apache.http.conn.HttpHostConnectException;

import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.registry.IRegistryRequest;
import com.raytheon.uf.common.registry.IRegistryRequest.Action;
import com.raytheon.uf.common.registry.RegistryConstants;
import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.RegistryHandler;
import com.raytheon.uf.common.registry.RegistryQuery;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;

/**
 * 
 * A Thrift client implementation for use with the RegistryManager Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012 356        jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Performance improvements.
 * 8/3/2012     724        bphillip    Added user to request
 * Aug 20, 2012 0743       djohnson    Finish making registry type-safe.
 * Aug 30, 2012 1123       djohnson    Use {@link EdexRegistryResponse}.
 * Sep 12, 2012 1167       djohnson    Use localization, move to common plugin so it is reusable outside of CAVE (e.g. dataprovideragent).
 * Sep 14, 2012 1169       djohnson    Add use of create only mode, remove EdexRegistryResponse.
 * Sep 28, 2012 1187       djohnson    Break out reusable thrift communication code to ThriftCommunicator.
 * Nov 15, 2012 1322       djohnson    Remove ThriftCommunicator in lieu of server-keyed routing.
 * Dec 03, 2012 1379       djohnson    Use registry service keys.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public class ThriftRegistryHandler implements RegistryHandler {

    /**
     * Handle the packaging and unpackaging of the Thrift communication with the
     * registry.
     * 
     * @param request
     *            The IRegistryRequest that contains the query to make against
     *            the registry.
     * 
     * @return The response from the registry.
     * 
     * @throws MsgRegistryException
     *             If there is a problem executing the query.
     */
    private <T> RegistryResponse<T> sendRequest(IRegistryRequest<T> request) {
        final RegistryQueryResponse<T> response = new RegistryQueryResponse<T>(
                request.getQuery());
        try {
            return sendRequestViaThrift(request);
        } catch (MsgRegistryException e) {
            return RegistryUtil.getFailedResponse(response, e);
        } catch (SerializationException e) {
            return RegistryUtil.getFailedResponse(response, e);
        } catch (RemoteException re) {
            Throwable cause = re.getCause();

            if (cause instanceof HttpHostConnectException) {
                return RegistryUtil
                        .getFailedResponse(response, new RegistryException(
                                RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, re));
            } else if (cause instanceof CommunicationException) {
                return RegistryUtil.getFailedResponse(response,
                        (CommunicationException) cause);
            } else {
                return RegistryUtil.getFailedResponse(response, re);
            }
        }
    }

    /**
     * Send the actual request via Thrift. Broken out into a package-private
     * method so it can be overridden for tests.
     * 
     * @param <T>
     *            the type the request/response is for
     * @param request
     *            the request object
     * @return the response
     * @throws SerializationException
     *             on error serializing the request/response
     * @throws RemoteException
     *             on error communicating with the server
     * @throws MsgRegistryException
     */
    @SuppressWarnings("unchecked")
    <T> RegistryResponse<T> sendRequestViaThrift(IRegistryRequest<T> request)
            throws RemoteException, SerializationException,
            MsgRegistryException {
        Object object;
        try {
            object = RequestRouter.route(request,
                    RegistryConstants.EBXML_REGISTRY_SERVICE);

            if (object instanceof SuccessfulExecution) {
                SuccessfulExecution response = (SuccessfulExecution) object;
                return (RegistryResponse<T>) response.getResponse();
            } else if (object instanceof ServerErrorResponse) {
                throw ExceptionWrapper
                        .unwrapThrowable(((ServerErrorResponse) object)
                                .getException());
            } else {
                throw new IllegalStateException(
                        "Received unexpected response type from the server.  Object was ["
                                + object.getClass().getName() + "]");
            }
        } catch (Throwable e) {
            throw new RemoteException(
                    "Error communicating with the registry service!", e);
        }
    }

    /**
     * Retrieve registry objects that satisfy the RegistryQuery.
     * 
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects.
     * 
     * @return A RegistryQueryResponse containing the status of the request, any
     *         registry objects that satisfied the RegistryQuery and any
     *         Exceptions generated from processing the RegistryQuery.
     * 
     * @see AdhocRegistryQuery
     * @see IdQuery
     */
    @Override
    public <T> RegistryQueryResponse<T> getObjects(
            RegistryQuery<T> registryQuery) {
        IRegistryRequest<T> request = new IRegistryRequest<T>();
        request.setQuery(registryQuery);
        request.setAction(Action.QUERY);

        return (RegistryQueryResponse<T>) sendRequest(request);
    }

    @Override
    public <T> RegistryResponse<T> removeObjects(String username,
            RegistryQuery<T> registryQuery) {
        IRegistryRequest<T> request = new IRegistryRequest<T>();
        request.setUsername(username);
        request.setQuery(registryQuery);
        request.setAction(Action.REMOVE);

        return sendRequest(request);
    }

    @Override
    public <T> RegistryResponse<T> removeObjects(RegistryQuery<T> registryQuery) {
        IRegistryRequest<T> request = new IRegistryRequest<T>();
        request.setQuery(registryQuery);
        request.setAction(Action.REMOVE);

        return sendRequest(request);
    }

    @Override
    public <T> RegistryResponse<T> removeObjects(String username,
            List<T> registryObjects) {
        IRegistryRequest<T> request = new IRegistryRequest<T>();
        request.setUsername(username);
        request.setObjects(registryObjects);
        request.setAction(Action.REMOVE);

        return sendRequest(request);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> RegistryResponse<T> storeObject(T object) {
        IRegistryRequest<T> request = new IRegistryRequest<T>();
        request.setObjects(Arrays.<T> asList(object));
        request.setAction(Action.STORE);

        return sendRequest(request);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> RegistryResponse<T> storeOrReplaceObject(T object) {
        IRegistryRequest<T> request = new IRegistryRequest<T>();
        request.setObjects(Arrays.<T> asList(object));
        request.setAction(Action.STORE_OR_REPLACE);

        return sendRequest(request);
    }
}