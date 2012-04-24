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
     * Number of times to retry in the event of a socket exception. Default is
     * 1.
     */
    private int retryCount = 1;

    private static IUFStatusHandler statusHandler = UFStatus.getHandler(
            HttpClient.class, "DEFAULT");

    private ThreadSafeClientConnManager connManager = null;

    private NetworkStatistics stats = new NetworkStatistics();

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
    public void enableGzipHandling() {
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

    private byte[] executePostMethod(HttpPost put) throws IOException,
            HttpException, CommunicationException {
        HttpClientResponse response = executeRequest(put);
        if (response.code != SUCCESS_CODE) {
            throw new CommunicationException(
                    "Error reading server response.  Got error message: "
                            + response.data != null ? new String(response.data)
                            : null);
        }
        return response.data;
    }

    public HttpClientResponse executeRequest(HttpUriRequest request)
            throws IOException, HttpException, CommunicationException {
        int tries = 0;
        boolean retry = true;
        // long ts = System.currentTimeMillis();

        while (retry) {
            retry = false;
            tries++;

            try {
                HttpResponse resp = client.execute(request);
                int code = resp.getStatusLine().getStatusCode();

                if (previousConnectionFailed) {
                    previousConnectionFailed = false;
                    statusHandler.handle(Priority.INFO,
                            "Connection with server reestablished.");
                }

                ByteArrayOutputStream baos = null;
                InputStream is = null;

                try {
                    // long t0 = System.currentTimeMillis();
                    HttpEntity entity = resp.getEntity();
                    byte[] rval = null;

                    if (entity != null) {
                        // TODO: print error if entity larger than int, won't be
                        // able to deserialize
                        int size = (int) entity.getContentLength();
                        is = entity.getContent();

                        if (size > 0) {
                            rval = new byte[size];
                            int read = 0;
                            int index = 0;
                            // int count = 0;
                            do {
                                read = is
                                        .read(rval, index, rval.length - index);

                                if (read > 0) {
                                    index += read;
                                    // count++;
                                }
                            } while (read > 0 && index != rval.length);
                            // long t2 = System.currentTimeMillis();
                            // System.out.println("ContentLength: Read " +
                            // rval.length
                            // + " bytes in " + count + " reads, took"
                            // + (t2 - t0) + "ms, total round trip "
                            // + (t2 - ts));
                        } else {
                            // grabbing an instance of the pool to use the
                            // underlying array so as to not create a tmp buffer
                            // all
                            // the time
                            // TODO: Update edex/jetty to set chunked=false so
                            // that
                            // it sends content length, currently broken as
                            // jetty is
                            // scrambling -128 to 63...
                            baos = ByteArrayOutputStreamPool.getInstance()
                                    .getStream();
                            byte[] underlyingArray = baos.getUnderlyingArray();
                            int read = 0;
                            int index = 0;
                            // int count = 0;
                            do {
                                read = is.read(underlyingArray, index,
                                        underlyingArray.length - index);

                                if (read > 0) {
                                    index += read;
                                    // count++;
                                    if (index == underlyingArray.length) {
                                        baos.setCapacity(underlyingArray.length << 1);
                                        underlyingArray = baos
                                                .getUnderlyingArray();
                                    }
                                }
                            } while (read > 0);

                            baos.setCount(index);
                            rval = new byte[index];
                            System.arraycopy(underlyingArray, 0, rval, 0, index);
                            // long t2 = System.currentTimeMillis();
                            // System.out.println("Read " + rval.length +
                            // " bytes in "
                            // + count + " reads, took" + (t2 - t0)
                            // + "ms, total round trip " + (t2 - ts));
                        }
                    }

                    return new HttpClientResponse(code, rval);
                } finally {
                    if (baos != null) {
                        try {
                            baos.close();
                        } catch (IOException e) {
                            // ignore
                        }
                    }

                    // It seems we do not need to do this with 4.1 closing the
                    // input stream from the entity ( 'is' at the time of
                    // writing ) should allow the connection to be released

                    // if (put != null) {
                    // put.releaseConnection();
                    // }

                    if (is != null) {
                        try {
                            is.close();
                        } catch (IOException e) {
                            // ignore
                        }
                    }

                    if (resp != null && resp.getEntity() != null) {
                        try {
                            EntityUtils.consume(resp.getEntity());
                        } catch (IOException e) {
                            // if there was an error reading the input stream,
                            // notify but continue
                            statusHandler
                                    .handle(Priority.EVENTB,
                                            "Error reading InputStream, assuming closed",
                                            e);
                        }
                    }
                }
            } catch (IOException e) {
                if (tries <= retryCount) {
                    statusHandler.handle(
                            Priority.INFO,
                            "Error occurred communicating with server: "
                                    + e.getMessage() + ".  Retrying...");
                    retry = true;
                    continue;
                }

                previousConnectionFailed = true;
                // close/abort connection
                if (request != null) {
                    request.abort();
                }
                statusHandler.handle(Priority.EVENTA,
                        "IO error in HttpClient, aborting connection.", e);
                throw e;
            }
        }

        // This point should never be reached
        CommunicationException e = new CommunicationException(
                "Error ocurred while contacting host, did not get a reponse or an exception",
                new Exception(
                        "Error ocurred while contacting host, did not get a reponse or an exception"));
        statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        throw e;
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
     * @throws VizCommunicationException
     *             if an error occurred during transmission
     * @throws VizException
     *             if an error occurred inside the callback
     */
    public void postStreamingByteArray(String address, byte[] message,
            IStreamHandler handlerCallback) throws CommunicationException {
        HttpPost put = new HttpPost(address);

        put.setEntity(new ByteArrayEntity(message));
        int tries = 0;
        boolean retry = true;
        while (retry) {
            retry = false;
            tries++;
            try {
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
                InputStream is = null;
                try {
                    is = resp.getEntity().getContent();
                    handlerCallback.handleStream(is);
                } finally {
                    if (is != null) {
                        try {
                            is.close();
                        } catch (IOException e) {
                            // ignore
                        }
                    }

                    // It seems we do not need to do this with 4.1 closing the
                    // input stream from the entity ( 'is' at the time of
                    // writing ) should allow the connection te be released

                    // if (put != null) {
                    // put.releaseConnection();
                    // }

                    try {
                        // Do not consume if content length unknown: breaks
                        // compression
                        if (resp != null && resp.getEntity() != null) {
                            EntityUtils.consume(resp.getEntity());
                        }
                    } catch (IOException e) {
                        // if there was an error reading the input stream,
                        // notify but continue
                        statusHandler
                                .handle(Priority.EVENTB,
                                        "Error reading InputStream, assuming closed",
                                        e);
                    }
                }
            } catch (IOException e) {
                if (tries <= retryCount) {
                    statusHandler.handle(
                            Priority.INFO,
                            "Error occurred communicating with server: "
                                    + e.getMessage() + ".  Retrying...");
                    retry = true;
                    continue;
                }

                previousConnectionFailed = true;
                // close/abort connection
                if (put != null) {
                    put.abort();
                }
                statusHandler.handle(Priority.EVENTA,
                        "IO error in HttpClient, aborting connection.", e);
                throw new CommunicationException(
                        "Error ocurred while contacting host", e);
            }
        }
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
     * @throws VizCommunicationException
     *             if an error occurred during transmission
     * @throws VizException
     *             if an error occurred inside the callback
     */
    @Deprecated
    public void postStreamingString(String address, String message,
            IStreamHandler handlerCallback) throws CommunicationException,
            UnsupportedEncodingException {
        HttpPost put = new HttpPost(address);

        put.setEntity(new StringEntity(message));
        int tries = 0;
        boolean retry = true;
        while (retry) {
            retry = false;
            tries++;
            try {
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

                InputStream is = null;
                try {
                    is = resp.getEntity().getContent();
                    handlerCallback.handleStream(is);
                } finally {
                    try {
                        if (is != null) {
                            is.close();
                        }
                    } catch (IOException e) {
                        // ignore
                    }

                    // It seems we do not need to do this with 4.1 closing the
                    // input stream from the entity ( 'is' at the time of
                    // writing ) should allow the connection te be released

                    // if (put != null) {
                    // put.releaseConnection();
                    // }

                    try {
                        // Do not consume if content length unknown: breaks
                        // compression
                        if (resp != null && resp.getEntity() != null) {
                            EntityUtils.consume(resp.getEntity());
                        }
                    } catch (IOException e) {
                        // if there was an error reading the input stream,
                        // notify but continue
                        statusHandler
                                .handle(Priority.EVENTB,
                                        "Error reading InputStream, assuming closed",
                                        e);
                    }
                }
            } catch (IOException e) {
                if (tries <= retryCount) {
                    statusHandler.handle(
                            Priority.INFO,
                            "Error occurred communicating with server: "
                                    + e.getMessage() + ".  Retrying...");
                    retry = true;
                    continue;
                }

                previousConnectionFailed = true;
                // close/abort connection
                if (put != null) {
                    put.abort();
                }
                statusHandler.handle(Priority.EVENTA,
                        "IO error in HttpClient, aborting connection.", e);
                throw new CommunicationException(
                        "Error ocurred while contacting host", e);
            }
        }

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

}
