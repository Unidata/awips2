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
import java.io.OutputStream;
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
import org.apache.http.client.methods.HttpUriRequest;
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
 *    01/07/13     DR 15294     D. Friedman  Added streaming requests.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class HttpClient {

    public static class HttpClientResponse {
        public final int code;

        public final byte[] data;

        private HttpClientResponse(int code, byte[] data) {
            this.code = code;
            this.data = data != null ? data : new byte[0];
        }
    }

    private static final int SUCCESS_CODE = 200;

    private final org.apache.http.client.HttpClient client;

    private boolean previousConnectionFailed;

    private static HttpClient instance;

    /**
     * Number of times to retry in the event of a connection exception. Default
     * is 1.
     */
    private int retryCount = 1;

    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(
            HttpClient.class, "DEFAULT");

    private ThreadSafeClientConnManager connManager = null;

    private final NetworkStatistics stats = new NetworkStatistics();

    private boolean gzipRequests = false;

    private boolean handlingGzipResponses = false;

    /** number of requests currently in process by the application per host */
    private final Map<String, AtomicInteger> currentRequestsCount = new ConcurrentHashMap<String, AtomicInteger>();

    private HttpClient() {
        connManager = new ThreadSafeClientConnManager();
        DefaultHttpClient client = new DefaultHttpClient(connManager);

        client.addRequestInterceptor(new HttpRequestInterceptor() {

            @Override
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
            @Override
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
        HttpConnectionParams.setTcpNoDelay(client.getParams(), true);

        this.client = client;
        previousConnectionFailed = false;
    }

    /**
     * Enables gzip capabilities by advertising gzip is an accepted response
     * encoding and decompressing responses if they arrived gzipped. This should
     * only ever be called once per runtime.
     * 
     * @param acceptGzip
     *            whether or not to handle gzip responses
     */
    public void setGzipResponseHandling(boolean acceptGzip) {
        if (acceptGzip && !handlingGzipResponses) {
            // Add gzip compression handlers

            // advertise we accept gzip
            ((AbstractHttpClient) client)
                    .addRequestInterceptor(new GzipRequestInterceptor());
            // handle gzip contents
            ((AbstractHttpClient) client)
                    .addResponseInterceptor(new GzipResponseInterceptor());
        } else if (!acceptGzip && handlingGzipResponses) {
            ((AbstractHttpClient) client)
                    .removeRequestInterceptorByClass(GzipRequestInterceptor.class);
            ((AbstractHttpClient) client)
                    .removeResponseInterceptorByClass(GzipResponseInterceptor.class);
        }
        handlingGzipResponses = acceptGzip;
    }

    /**
     * Sets whether or not to compress the outgoing requests to reduce bandwidth
     * sent by the client.
     * 
     * @param compress
     */
    public void setCompressRequests(boolean compress) {
        gzipRequests = compress;
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
    private HttpResponse postRequest(HttpUriRequest put) throws IOException,
            CommunicationException {
        HttpResponse resp = client.execute(put);
        if (previousConnectionFailed) {
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
     * @return the http status code
     * @throws CommunicationException
     */
    private HttpClientResponse process(HttpUriRequest put,
            IStreamHandler handlerCallback) throws CommunicationException {
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

                if ((errorMsg != null) && (exc != null)) {
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
            byte[] byteResult = null;
            if (handlerCallback instanceof DefaultInternalStreamHandler) {
                byteResult = ((DefaultInternalStreamHandler) handlerCallback).byteResult;
            }
            return new HttpClientResponse(resp.getStatusLine().getStatusCode(),
                    byteResult);
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
        if ((resp != null) && (resp.getEntity() != null)) {
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
        HttpClientResponse resp = this.process(put, handlerCallback);
        checkStatusCode(resp);
        return resp.data;
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
     * Post a message to an http address, and return the result as a byte array.
     * <p>
     * Implementation note: The given stream handler will be used at least
     * twice:  Once to determine the length, another to actually send the
     * content.  This is done because pypies does not accept chunked requests
     * bodies.
     *
     * @param address
     * @param handler the handler responsible for generating the message to be posted
     * @return
     * @throws CommunicationException
     */
    public byte[] postBinary(String address, OStreamHandler handler) throws CommunicationException {
        class OStreamEntity extends AbstractHttpEntity {
            OStreamHandler handler;
            long contentLength = -1;

            public OStreamEntity(OStreamHandler handler) {
                this.handler = handler;
            }

            @Override
            public InputStream getContent() throws IOException,
                    IllegalStateException {
                throw new IllegalStateException("OStreamEntity does not support getContent().");
            }

            @Override
            public long getContentLength() {
                if (contentLength < 0) {
                    class CountingStream extends OutputStream {
                        long count;

                        @Override
                        public void write(int b) throws IOException {
                            ++count;
                        }

                        @Override
                        public void write(byte[] b) throws IOException {
                            count += b.length;
                        }

                        @Override
                        public void write(byte[] b, int off, int len)
                                throws IOException {
                            count += len;
                        }
                    }

                    CountingStream cs = new CountingStream();
                    try {
                        handler.writeToStream(cs);
                        contentLength = cs.count;
                    } catch (CommunicationException e) {
                        // ignore
                    }
                }
                return contentLength;
            }

            @Override
            public boolean isRepeatable() {
                return true;
            }

            @Override
            public boolean isStreaming() {
                return false;
            }

            @Override
            public void writeTo(OutputStream stream) throws IOException {
                try {
                    handler.writeToStream(stream);
                } catch (CommunicationException e) {
                    throw new IOException(e.getMessage(), e.getCause());
                }
            }
        }

        OStreamEntity entity = new OStreamEntity(handler);
        HttpPost put = new HttpPost(address);
        put.setEntity(entity);

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
     * Executes an HttpUriRequest and returns a response with the byte[] and
     * http status code.
     * 
     * @param request
     *            the request to execute
     * @return the result and status code
     * @throws CommunicationException
     */
    public HttpClientResponse executeRequest(HttpUriRequest request)
            throws CommunicationException {
        DefaultInternalStreamHandler streamHandler = new DefaultInternalStreamHandler();
        return process(request, streamHandler);
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

    private void checkStatusCode(HttpClientResponse response)
            throws CommunicationException {
        if (response.code != SUCCESS_CODE) {
            throw new CommunicationException(
                    "Error reading server response.  Got error message: "
                            + response.data != null ? new String(response.data)
                            : null);
        }
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
        HttpClientResponse resp = process(put, handlerCallback);
        checkStatusCode(resp);
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
     * Gets the network statistics for http traffic.
     * 
     * @return
     */
    public NetworkStatistics getStats() {
        return stats;
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
     * Responsible for writing HTTP content to a stream.  May be called
     * more than once for a given entity.  See postBinary(String, OStreamHandler)
     * for details.
     */
    public static interface OStreamHandler {
        public void writeToStream(OutputStream os) throws CommunicationException;
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

    /**
     * Adds Accept-Encoding: gzip to every outgoing request
     */
    private static class GzipRequestInterceptor implements
            HttpRequestInterceptor {

        @Override
        public void process(HttpRequest request, HttpContext context)
                throws HttpException, IOException {
            if (!request.containsHeader("Accept-Encoding")) {
                request.addHeader("Accept-Encoding", "gzip");
            }
        }

    }

    /**
     * Decompresses any responses that arrive with Content-Encoding: gzip
     */
    private static class GzipResponseInterceptor implements
            HttpResponseInterceptor {

        @Override
        public void process(HttpResponse response, HttpContext context)
                throws HttpException, IOException {
            HttpEntity entity = response.getEntity();
            Header ceheader = entity.getContentEncoding();
            if (ceheader != null) {
                HeaderElement[] codecs = ceheader.getElements();
                for (HeaderElement codec : codecs) {
                    if (codec.getName().equalsIgnoreCase("gzip")) {
                        response.setEntity(new GzipDecompressingEntity(response
                                .getEntity()));
                        return;
                    }
                }
            }
        }

    }

}
