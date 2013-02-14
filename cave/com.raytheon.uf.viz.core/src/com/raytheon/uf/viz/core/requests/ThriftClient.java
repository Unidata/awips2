package com.raytheon.uf.viz.core.requests;

import java.io.IOException;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.UUID;

import javax.jws.WebService;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.auth.resp.SuccessfulExecution;
import com.raytheon.uf.common.auth.resp.UserNotAuthenticated;
import com.raytheon.uf.common.auth.resp.UserNotAuthorized;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.NetworkStatistics;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RemoteServiceRequest;
import com.raytheon.uf.common.serialization.comm.RequestWrapper;
import com.raytheon.uf.common.serialization.comm.ServiceException;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

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

/**
 * The thrift client. used to send requests to the RemoteRequestServer. Make
 * sure request type has registered a handler to handle the request on the
 * server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009             mschenke    Initial creation
 * Jul 24, 2012            njensen     Enhanced logging
 * Nov 15, 2012 1322       djohnson    Publicize ability to specify specific httpAddress.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ThriftClient {

    private static abstract class ThriftServiceHandler implements
            InvocationHandler {

        @Override
        public Object invoke(Object proxy, Method method, Object[] args)
                throws Throwable {
            Class<?> clazz = null;
            for (Class<?> interfaze : proxy.getClass().getInterfaces()) {
                if (interfaze.getAnnotation(WebService.class) != null) {
                    clazz = interfaze;
                }
            }

            RemoteServiceRequest request = new RemoteServiceRequest();
            request.setInterfaceToUse(clazz.getCanonicalName());
            request.setMethod(method.getName());
            request.setArguments(args);
            try {
                return handleRequest(request);
            } catch (VizException e) {
                throw new ServiceException(e);
            }
        }

        protected abstract Object handleRequest(RemoteServiceRequest request)
                throws VizException;
    }

    private static InvocationHandler httpHandler = new ThriftServiceHandler() {

        @Override
        protected Object handleRequest(RemoteServiceRequest request)
                throws VizException {
            return ThriftClient.sendRequest(request);
        }
    };

    private static InvocationHandler localizationHandler = new ThriftServiceHandler() {

        @Override
        protected Object handleRequest(RemoteServiceRequest request)
                throws VizException {
            return ThriftClient.sendLocalizationRequest(request);
        }
    };

    private static final long SIMPLE_LOG_TIME = 1000;

    private static final long BAD_LOG_TIME = 5000;

    private static INotAuthHandler defaultHandler = UserController
            .getNotAuthHandler();

    private static NetworkStatistics stats = HttpClient.getInstance()
            .getStats();

    /**
     * Construct a thrift web service object that sends method calls to the http
     * server to be executed. EVERY FUNCTION CALL MADE TO INTERFACE MAY THROW A
     * VizException, be safe and catch them!
     * 
     * @param interfaze
     * @return Implementation of interface passed in
     */
    public static <T> T constructServiceObject(Class<T> interfaze)
            throws VizException {
        return constructServiceObject(interfaze, httpHandler);
    }

    /**
     * Construct a thrift web service object that sends method calls to the
     * localization server to be executed. EVERY FUNCTION CALL MADE TO INTERFACE
     * MAY THROW A VizException, be safe and catch them!
     * 
     * @param interfaze
     * @return Implementation of interface passed in
     */
    public static <T> T constructLocalizationServiceObject(Class<T> interfaze)
            throws VizException {
        return constructServiceObject(interfaze, localizationHandler);
    }

    /**
     * Send a request to the localization server
     * 
     * @param request
     * @return
     * @throws VizException
     */
    public static Object sendLocalizationRequest(IServerRequest request)
            throws VizException {
        if (request instanceof AbstractPrivilegedRequest) {
            return sendPrivilegedLocalizationRequest((AbstractPrivilegedRequest) request);
        }
        return sendRequest(request, LocalizationManager.getInstance()
                .getLocalizationServer());
    }

    /**
     * Send a request to the http server
     * 
     * @param request
     * @return
     * @throws VizException
     */
    public static Object sendRequest(IServerRequest request)
            throws VizException {
        if (request instanceof AbstractPrivilegedRequest) {
            return sendPrivilegedRequest((AbstractPrivilegedRequest) request);
        }
        return sendRequest(request, VizApp.getHttpServer());
    }

    /**
     * Send the privileged request to the http server using the controller's
     * handler
     * 
     * @param request
     * @return
     * @throws VizException
     */
    public static Object sendPrivilegedRequest(AbstractPrivilegedRequest request)
            throws VizException {
        return sendPrivilegedRequest(request, defaultHandler);
    }

    /**
     * Send the privileged request to the localization server using the
     * controller's handler
     * 
     * @param request
     * @return
     * @throws VizException
     */
    public static Object sendPrivilegedLocalizationRequest(
            AbstractPrivilegedRequest request) throws VizException {
        return sendPrivilegedLocalizationRequest(request, defaultHandler);
    }

    /**
     * Send the privileged request to the localization server using a custom
     * INotAuthHandler
     * 
     * @param request
     * @param handler
     * @return
     * @throws VizException
     */
    public static Object sendPrivilegedLocalizationRequest(
            AbstractPrivilegedRequest request, INotAuthHandler handler)
            throws VizException {
        return sendPrivilegedRequest(request, handler, LocalizationManager
                .getInstance().getLocalizationServer());
    }

    /**
     * Send the privileged request to the http server using a custom
     * INotAuthHandler
     * 
     * @param request
     * @param handler
     * @return
     * @throws VizException
     */
    public static Object sendPrivilegedRequest(
            AbstractPrivilegedRequest request, INotAuthHandler handler)
            throws VizException {
        return sendPrivilegedRequest(request, handler, VizApp.getHttpServer());
    }

    /**
     * Send a privileged request to the given server, wraps expected privileged
     * response types
     * 
     * @param request
     * @param handler
     * @param server
     * @return
     * @throws VizException
     */
    private static Object sendPrivilegedRequest(
            AbstractPrivilegedRequest request, INotAuthHandler handler,
            String server) throws VizException {
        Object rval = ThriftClient.sendRequest(request, server);
        if (rval instanceof UserNotAuthorized) {
            rval = handler.notAuthorized((UserNotAuthorized) rval);
        } else if (rval instanceof UserNotAuthenticated) {
            rval = handler.notAuthenticated((UserNotAuthenticated) rval);
        } else if (rval instanceof SuccessfulExecution) {
            SuccessfulExecution response = (SuccessfulExecution) rval;
            UserController.updateUserData(response.getUpdatedData());
            rval = response.getResponse();
        }
        return rval;
    }

    public static Object sendRequest(IServerRequest request, String httpAddress)
            throws VizException {
        return sendRequest(request, httpAddress, "/thrift");
    }

    private static Object sendRequest(IServerRequest request,
            String httpAddress, String uri) throws VizException {
        httpAddress += uri;
        String uniqueId = UUID.randomUUID().toString();
        RequestWrapper wrapper = new RequestWrapper(request, VizApp.getWsId(),
                uniqueId);
        byte[] message;
        try {
            message = SerializationUtil.transformToThrift(wrapper);
        } catch (SerializationException e) {
            throw new VizException("unable to serialize request object", e);
        }

        byte[] response = null;
        try {
            long t0 = System.currentTimeMillis();
            response = HttpClient.getInstance()
                    .postBinary(httpAddress, message);
            long time = System.currentTimeMillis() - t0;
            if (time >= SIMPLE_LOG_TIME) {
                System.out.println("Took " + time + "ms to run request id["
                        + uniqueId + "] " + request.toString());
            }
            if (time >= BAD_LOG_TIME) {
                new Exception() {
                    private static final long serialVersionUID = 1L;

                    /*
                     * (non-Javadoc)
                     * 
                     * @see java.lang.Throwable#toString()
                     */
                    @Override
                    public String toString() {
                        return "(NOT AN ERROR) ThriftClient Diagnostic Stack For Long Requests:";
                    }

                }.printStackTrace(System.out);
            }

            long responseLen = 0;
            if (response != null) {
                responseLen = response.length;
            }
            // Log request stats
            stats.log(request.getClass().getSimpleName(), message.length,
                    responseLen);
        } catch (IOException e) {
            throw new VizCommunicationException(
                    "unable to post request to server", e);
        } catch (CommunicationException e) {
            throw new VizCommunicationException(
                    "unable to post request to server", e);
        } catch (Exception e) {
            throw new VizException("unable to post request to server", e);
        }
        Object rval = null;
        if (response != null) {
            try {
                rval = SerializationUtil.transformFromThrift(response);
            } catch (SerializationException e) {
                throw new VizException(
                        "unable to transform response to object", e);
            }
        }
        if (rval instanceof ServerErrorResponse) {
            ServerErrorResponse resp = (ServerErrorResponse) rval;
            Throwable serverException = ExceptionWrapper.unwrapThrowable(resp
                    .getException());
            throw new ServerRequestException(serverException.getMessage(),
                    serverException);
        }
        return rval;
    }

    @SuppressWarnings("unchecked")
    private static <T> T constructServiceObject(Class<T> interfaze,
            InvocationHandler handler) throws VizException {
        try {
            Validate.notNull(interfaze, "Interface must not be null");
            Validate.isTrue(interfaze.isInterface(),
                    "Class must be an interface");
            Validate.notNull(interfaze.getAnnotation(WebService.class),
                    "Interface must have " + WebService.class + " annotation");

            return (T) Proxy.newProxyInstance(interfaze.getClassLoader(),
                    new Class[] { interfaze }, handler);
        } catch (Throwable t) {
            throw new VizException("Error constructing service object: "
                    + t.getLocalizedMessage(), t);
        }
    }
}
