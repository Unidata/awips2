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

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.GZIPOutputStream;

import org.apache.http.Header;
import org.apache.http.HeaderElement;
import org.apache.http.HttpEntity;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponse;
import org.apache.http.HttpResponseInterceptor;
import org.apache.http.client.entity.GzipDecompressingEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ConnectionPoolTimeoutException;
import org.apache.http.entity.AbstractHttpEntity;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.AbstractHttpClient;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.util.EntityUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool;
import com.raytheon.uf.common.util.ByteArrayOutputStreamPool.ByteArrayOutputStream;

/**
 * 
 * Provides connectivity to the ESB server
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    7/1/06        #1088       chammack    Initial Creation.
 *    5/17/10      #5901       njensen        Moved to common
 *    03/02/11      #8045       rferrel     Add connect reestablished message.
 *    07/17/12    #911         njensen    Refactored significantly
 *    08/09/12     15307        snaples   Added putEntitiy in postStreamingEntity.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class HttpClient {

    private static final int SUCCESS_CODE = 200;

    private final org.apache.http.client.HttpClient client;

    private boolean previousConnectionFailed;

    private static HttpClient instance;

    /**
     * Number of times to retry in the event of a connection exception. Default
     * is 1.
     */
    private int retryCount = 1;

    private static IUFStatusHandler statusHandler = UFStatus.getHandler(
            HttpClient.class, "DEFAULT");

    private ThreadSafeClientConnManager connManager = null;

    private NetworkStatistics stats = NetworkStatistics.getInstance();

    private boolean gzipRequests = false;

    /** number of requests currently in process by the application per host */
    private Map<String, AtomicInteger> currentRequestsCount = new ConcurrentHashMap<String, AtomicInteger>();

    private HttpClient() {
        connManager = new ThreadSafeClientConnManager();
        DefaultHttpClient client = new DefaultHttpClient(connManager);
        client.addRequestInterceptor(new HttpRequestInterceptor() {

            public void process(final HttpRequest request,
                    final HttpContext context) throws HttpException,
                    IOException {
                try {
                    stats.log(
                            Long.valueOf(request.getFirstHeader(
                                    "Content-Length").getValue()), 0);
                } catch (Throwable t) {
                    // Ignore any errors when logging
                }
            }

        });

        client.addResponseInterceptor(new HttpResponseInterceptor() {
            public void process(final HttpResponse response,
                    final HttpContext context) throws HttpException,
                    IOException {
                try {
                    stats.log(0, response.getEntity().getContentLength());
                } catch (Throwable t) {
                    // Ignore any errors when logging
                }
            }
        });
        this.client = client;
        previousConnectionFailed = false;
    }

    /**
     * Enables gzip capabilities by advertising gzip is an accepted response
     * encoding and decompressing responses if they arrived gzipped. This should
     * only ever be called once per runtime.
     */
    public void enableGzipResponseHandling() {
        // Add gzip compression handlers

        // advertise we accept gzip
        ((AbstractHttpClient) client)
                .addRequestInterceptor(new HttpRequestInterceptor() {

                    public void process(final HttpRequest request,
                            final HttpContext context) throws HttpException,
                            IOException {
                        if (!request.containsHeader("Accept-Encoding")) {
                            request.addHeader("Accept-Encoding", "gzip");
                        }
                    }

                });

        // handle gzip contents
        ((AbstractHttpClient) client)
                .addResponseInterceptor(new HttpResponseInterceptor() {

                    public void process(final HttpResponse response,
                            final HttpContext context) throws HttpException,
                            IOException {
                        HttpEntity entity = response.getEntity();
                        Header ceheader = entity.getContentEncoding();
                        if (ceheader != null) {
                            HeaderElement[] codecs = ceheader.getElements();
                            for (int i = 0; i < codecs.length; i++) {
                                if (codecs[i].getName()
                                        .equalsIgnoreCase("gzip")) {
                                    response.setEntity(new GzipDecompressingEntity(
                                            response.getEntity()));
                                    return;
                                }
                            }
                        }
                    }

                });
    }

    public void enableRequestCompression() {
        gzipRequests = true;
    }

    public static synchronized HttpClient getInstance() {
        if (instance == null) {
            instance = new HttpClient();
        }

        return instance;
    }

    /**
     * Post a message to an http address, and return the result as a string.
     * 
     * 
     * @param address
     * @param message
     * @return
     * @throws Exception
     */
    public String post(String address, String message) throws Exception {

        String returnValue = new String(postByteResult(address, message));

        return returnValue;

    }

    /**
     * Post a message to an http address, and return the result as a byte array.
     * 
     * 
     * @param address
     * @param message
     * @return
     * @throws Exception
     */
    public byte[] postByteResult(String address, String message)
            throws Exception {
        HttpPost put = new HttpPost(address);
        put.setEntity(new StringEntity(message, "text/xml", "ISO-8859-1"));

        return executePostMethod(put);
    }

    /**
     * Sends the request to the server, checks the status code (in case of 404,
     * 403, etc), and returns the response if there was no error code.
     * 
     * @param put
     *            the request to send
     * @return the response from the server
     * @throws IOException
     * @throws CommunicationException
     */
    private HttpResponse postRequest(HttpPost put) throws IOException,
            CommunicationException {
        HttpResponse resp = client.execute(put);
        int code = resp.getStatusLine().getStatusCode();
        if (code != SUCCESS_CODE) {
            throw new CommunicationException(
                    "Error reading server response.  Got error message: "
                            + EntityUtils.toString(resp.getEntity()));
        } else if (previousConnectionFailed) {
            previousConnectionFailed = false;
            statusHandler.handle(Priority.INFO,
                    "Connection with server reestablished.");
        }
        return resp;
    }

    /**
     * Posts the request to the server and passes the response stream to the
     * handler callback. Will also retry the request if it fails due to a
     * timeout or IO problem.
     * 
     * @param put
     *            the request to post
     * @param handlerCallback
     *            the handler to handle the response stream
     * @throws CommunicationException
     */
    private void process(HttpPost put, IStreamHandler handlerCallback)
            throws CommunicationException {
        int tries = 0;
        boolean retry = true;
        HttpResponse resp = null;
        AtomicInteger ongoing = null;

        try {
            String host = put.getURI().getHost();
            ongoing = currentRequestsCount.get(host);
            if (ongoing == null) {
                ongoing = new AtomicInteger();
                currentRequestsCount.put(host, ongoing);
            }
            int currentCount = ongoing.incrementAndGet();
            if (currentCount > getMaxConnectionsPerHost()) {
                String msg = currentCount + " ongoing http requests to " + host
                        + ".  Likely waiting for free connection from pool.";
                statusHandler.debug(msg);
            }
            while (retry) {
                retry = false;
                tries++;

                String errorMsg = null;
                Exception exc = null;
                try {
                    resp = postRequest(put);
                } catch (ConnectionPoolTimeoutException e) {
                    errorMsg = "Timed out waiting for http connection from pool: "
                            + e.getMessage();
                    errorMsg += ".  Currently " + ongoing.get()
                            + " requests ongoing";
                    exc = e;
                } catch (IOException e) {
                    errorMsg = "Error occurred communicating with server: "
                            + e.getMessage();
                    exc = e;
                }

                if (errorMsg != null && exc != null) {
                    if (tries > retryCount) {
                        previousConnectionFailed = true;
                        // close/abort connection
                        if (put != null) {
                            put.abort();
                        }
                        errorMsg += ".  Hit retry limit, aborting connection.";
                        throw new CommunicationException(errorMsg, exc);
                    } else {
                        errorMsg += ".  Retrying...";
                        statusHandler.handle(Priority.INFO, errorMsg);
                        retry = true;
                    }
                }
            }

            // should only be able to get here if we didn't encounter the
            // exceptions above on the most recent try
            processResponse(resp, handlerCallback);
        } finally {
            if (ongoing != null) {
                ongoing.decrementAndGet();
            }
        }
    }

    /**
     * Streams the response content to the handler callback and closes the http
     * connection once finished.
     * 
     * @param resp
     *            the http response to stream
     * @param handlerCallback
     *            the handler that should process the response stream
     * @throws CommunicationException
     */
    private void processResponse(HttpResponse resp,
            IStreamHandler handlerCallback) throws CommunicationException {
        InputStream is = null;
        if (resp != null && resp.getEntity() != null) {
            try {
                is = resp.getEntity().getContent();
                handlerCallback.handleStream(is);
            } catch (IOException e) {
                throw new CommunicationException(
                        "IO error processing http response", e);
            } finally {
                if (is != null) {
                    try {
                        is.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }

                // Closes the stream if it's still open
                try {
                    EntityUtils.consume(resp.getEntity());
                } catch (IOException e) {
                    // if there was an error reading the input stream,
                    // notify but continue
                    statusHandler.handle(Priority.EVENTB,
                            "Error reading InputStream, assuming closed", e);
                }
            }
        } else {
            // this should be impossible to reach
            throw new CommunicationException(
                    "Error ocurred while contacting server, did not get a reponse or an exception");
        }
    }

    /**
     * Posts the request and uses a DefaultInternalStreamHandler to
     * automatically stream the response into a byte[].
     * 
     * @param put
     *            the post to send to the server
     * @return the byte[] of the response
     * @throws CommunicationException
     */
    private byte[] executePostMethod(HttpPost put)
            throws CommunicationException {
        DefaultInternalStreamHandler handlerCallback = new DefaultInternalStreamHandler();
        this.process(put, handlerCallback);
        return handlerCallback.byteResult;
    }

    /**
     * Post a message to an http address, and return the result as a byte array.
     * 
     * 
     * @param address
     * @param message
     * @return
     * @throws Exception
     */
    public byte[] postBinary(String address, byte[] message)
            throws CommunicationException, Exception {

        HttpPost put = new HttpPost(address);
        if (gzipRequests) {
            ByteArrayOutputStream byteStream = ByteArrayOutputStreamPool
                    .getInstance().getStream(message.length);
            GZIPOutputStream gzipStream = new GZIPOutputStream(byteStream);
            gzipStream.write(message);
            gzipStream.finish();
            gzipStream.flush();
            byte[] gzipMessage = byteStream.toByteArray();
            gzipStream.close();
            if (message.length > gzipMessage.length) {
                message = gzipMessage;
                put.setHeader("Content-Encoding", "gzip");
            }
        }

        put.setEntity(new ByteArrayEntity(message));

        return executePostMethod(put);
    }

    /**
     * Post a string to an endpoint and stream the result back.
     * 
     * The result should be handled inside of the handlerCallback
     * 
     * @param address
     *            the http address
     * @param message
     *            the message to send
     * @param handlerCallback
     *            the handler callback
     * @throws CommunicationException
     *             if an error occurred during transmission
     */
    public void postStreamingByteArray(String address, byte[] message,
            IStreamHandler handlerCallback) throws CommunicationException {
        postStreamingEntity(address, new ByteArrayEntity(message),
                handlerCallback);
    }

    /**
     * Post a string to an endpoint and stream the result back.
     * 
     * The result should be handled inside of the handlerCallback
     * 
     * @param address
     *            the http address
     * @param message
     *            the message to send
     * @param handlerCallback
     *            the handler callback
     * @throws UnsupportedEncodingException
     * @throws CommunicationException
     */
    @Deprecated
    public void postStreamingString(String address, String message,
            IStreamHandler handlerCallback) throws CommunicationException,
            UnsupportedEncodingException {
        postStreamingEntity(address, new StringEntity(message), handlerCallback);
    }

    /**
     * Posts an entity to the address and stream the result back.
     * 
     * @param address
     *            the http address to post to
     * @param entity
     *            an entity containing the message to send
     * @param handlerCallback
     *            the handler callback
     * @throws CommunicationException
     */
    private void postStreamingEntity(String address, AbstractHttpEntity entity,
            IStreamHandler handlerCallback) throws CommunicationException {
        HttpPost put = new HttpPost(address);
        put.setEntity(entity);
        process(put, handlerCallback);
    }

    public void setMaxConnectionsPerHost(int maxConnections) {
        connManager.setDefaultMaxPerRoute(maxConnections);
    }

    public int getMaxConnectionsPerHost() {
        return connManager.getDefaultMaxPerRoute();
    }

    public void setSocketTimeout(int socketTimeout) {
        HttpConnectionParams.setSoTimeout(client.getParams(), socketTimeout);
        // client.getParams().setIntParameter(CoreConnectionPNames.SO_TIMEOUT,
        // socketTimeout);
    }

    public void setConnectionTimeout(int connectionTimeout) {
        HttpConnectionParams.setConnectionTimeout(client.getParams(),
                connectionTimeout);
    }

    /**
     * Number of times to retry in the event of a socket exception. Default is
     * 1.
     */
    public int getRetryCount() {
        return retryCount;
    }

    /**
     * Number of times to retry in the event of a socket exception. Default is
     * 1.
     * 
     * @param retryCount
     */
    public void setRetryCount(int retryCount) {
        this.retryCount = retryCount;
    }

    /**
     * Provides a safe interface callback for implementing stream behavior with
     * http
     * 
     * The lifetime of the stream is only guaranteed inside the scope of the
     * handleScope method. A user should not close the stream, it will be closed
     * for them after the method completes.
     * 
     * @author chammack
     * @version 1.0
     */
    public static interface IStreamHandler {

        /**
         * Implementation method for stream callbacks
         * 
         * A user should NOT close the stream, it will be done for them after
         * the method terminates. A user should NOT store off copies of the
         * input stream for later use.
         * 
         * @param is
         * @throws VizException
         */
        public abstract void handleStream(InputStream is)
                throws CommunicationException;
    }

    /**
     * Automatically reads a stream into a byte array and stores the byte array
     * in byteResult. Should only be used internally in HttpClient with
     * convenience methods that do not take an IStreamHandler as an argument.
     * 
     */
    private static class DefaultInternalStreamHandler implements IStreamHandler {

        private byte[] byteResult;

        @Override
        public void handleStream(InputStream is) throws CommunicationException {
            ByteArrayOutputStream baos = ByteArrayOutputStreamPool
                    .getInstance().getStream();
            try {
                byte[] underlyingArray = baos.getUnderlyingArray();
                int read = 0;
                int index = 0;
                do {
                    try {
                        read = is.read(underlyingArray, index,
                                underlyingArray.length - index);
                    } catch (IOException e) {
                        throw new CommunicationException(
                                "Error reading byte response", e);
                    }

                    if (read > 0) {
                        index += read;
                        if (index == underlyingArray.length) {
                            baos.setCapacity(underlyingArray.length << 1);
                            underlyingArray = baos.getUnderlyingArray();
                        }
                    }
                } while (read > 0);

                baos.setCount(index);
                byteResult = new byte[index];
                System.arraycopy(underlyingArray, 0, byteResult, 0, index);
            } finally {
                if (baos != null) {
                    try {
                        baos.close();
                    } catch (IOException e) {
                        // ignore
                    }
                }
            }
        }

    }

}
