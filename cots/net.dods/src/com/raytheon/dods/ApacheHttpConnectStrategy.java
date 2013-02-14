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
package com.raytheon.dods;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.params.ConnRoutePNames;
import org.apache.http.conn.scheme.PlainSocketFactory;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.apache.http.util.EntityUtils;

import dods.dap.DConnect;
import dods.dap.DODSException;
import dods.dap.ServerVersion;

/**
 * An {@link HttpConnectStrategy} implementation using the Apache HTTP Client.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 06, 2012 634        djohnson    Extracted from DConnect, 
 *                                      added {@link EntityInputStream}.
 * Jul 09, 2012 634        djohnson    Fully consume the entity prior to throwing exception.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ApacheHttpConnectStrategy extends BaseHttpConnectStrategy {

    private static HttpClient httpClient;

    private static ThreadSafeClientConnManager connectionManager;
    static {
        SchemeRegistry schemeRegistry = new SchemeRegistry();
        schemeRegistry.register(new Scheme("http", 80, PlainSocketFactory
                .getSocketFactory()));

        connectionManager = new ThreadSafeClientConnManager(schemeRegistry);

        connectionManager.setDefaultMaxPerRoute(10);
        connectionManager.setMaxTotal(100);

        httpClient = new DefaultHttpClient(connectionManager);
    }

    @Override
    public InputStream getInputStream(URL url) throws IOException,
            DODSException {
        HttpResponse response = openHttpConnection(url);

        // If the response was null, return null to the caller
        // who will handle any re-requests
        if (response == null) {
            return null;
        }

        HttpEntity entity = response.getEntity();
        return new EntityInputStream(getInputStream(response, entity), entity);
    }

    /**
     * Open a connection to the DODS server.
     * 
     * @param url
     *            the URL to open.
     * @return the opened <code>InputStream</code>.
     * @exception IOException
     *                if an IO exception occurred.
     */
    private HttpResponse openHttpConnection(URL url) throws IOException {

        HttpContext context = new BasicHttpContext();
        HttpGet httpget = null;

        try {
            httpget = new HttpGet(url.toExternalForm());
            HttpResponse r = httpClient.execute(httpget, context);

            if (r.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
                return r;
            }

            // Status is not OK, abort current get and start again...
            httpget.abort();
        } catch (Exception ex) {
            if (!httpget.isAborted())
                httpget.abort();
            throw new IOException("Failed to open connection to URL: "
                    + url.toExternalForm(), ex);
        }

        return null;
    }

    private InputStream getInputStream(HttpResponse response, HttpEntity entity)
            throws IOException, DODSException {

        StatusLine status = response.getStatusLine();

        if (status.getStatusCode() == HttpStatus.SC_OK) {
            Header descriptionHeader = response
                    .getFirstHeader(CONTENT_DESCRIPTION);
            if (descriptionHeader != null
                    && DODS_ERROR.equals(descriptionHeader.getValue())) {
                DODSException ds = new DODSException();
                // parse the Error object from stream and throw it
                // The entity must be fully consumed in the finally clause
                // since we are throwing an exception.
                try {
                    ds.parse(entity.getContent());
                    throw ds;
                } finally {
                    EntityUtils.consume(entity);
                }
            }

            ver = new ServerVersion(response.getFirstHeader(XDODS_SERVER)
                    .getValue());
            String encoding = null;
            Header e = entity.getContentEncoding();
            if (e != null) {
                encoding = e.getValue();
            }

            return DConnect
                    .handleContentEncoding(entity.getContent(), encoding);
        }

        return null;
    }

    @Override
    public void setProxy(String host, int port) {
        HttpHost proxy = new HttpHost(host, port);
        httpClient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY,
                proxy);
    }

    @Override
    public void setDeflate(boolean deflate) {
        if (deflate) {
            httpClient.getParams()
                    .setParameter(ACCEPT_ENCODING_HEADER, DEFLATE);
        } else {
            httpClient.getParams().removeParameter(ACCEPT_ENCODING_HEADER);
        }
    }


    @Override
    public void setConnectionTimeout(int connectionTimeout) {
        HttpConnectionParams.setConnectionTimeout(httpClient.getParams(),
                connectionTimeout);
    }

    @Override
    public void setSocketTimeout(int socketTimeout) {
        HttpConnectionParams
                .setSoTimeout(httpClient.getParams(), socketTimeout);
    }

    /**
     * Decorates an input stream such that when any operation other than close()
     * is called it is passed through to the decorated object. If close is
     * called, it closes the stream AND calls EntityUtils.consume().
     */
    private static class EntityInputStream extends InputStream {

        private final InputStream decorated;

        private final HttpEntity entity;

        private EntityInputStream(InputStream decorated, HttpEntity entity) {
            this.decorated = decorated;
            this.entity = entity;
        }

        @Override
        public int read() throws IOException {
            return decorated.read();
        }

        @Override
        public int read(byte[] b) throws IOException {
            return decorated.read(b);
        }

        @Override
        public int read(byte[] b, int off, int len) throws IOException {
            return decorated.read(b, off, len);
        }

        @Override
        public long skip(long n) throws IOException {
            return decorated.skip(n);
        }

        @Override
        public int available() throws IOException {
            return decorated.available();
        }

        /**
         * Overridden to consume the {@link HttpEntity} before closing the
         * underlying stream.
         */
        @Override
        public void close() throws IOException {
            try {
                EntityUtils.consume(entity);
            } finally {
                decorated.close();
            }
        }

        @Override
        public synchronized void mark(int readlimit) {
            decorated.mark(readlimit);
        }

        @Override
        public synchronized void reset() throws IOException {
            decorated.reset();
        }

        @Override
        public boolean markSupported() {
            return decorated.markSupported();
        }

        @Override
        public int hashCode() {
            return decorated.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return decorated.equals(obj);
        }

        @Override
        public String toString() {
            return decorated.toString();
        }
    }
}
