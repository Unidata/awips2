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

import static org.junit.Assert.assertEquals;

import java.net.URI;

import org.apache.http.client.methods.HttpGet;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.util.JettyServer;

/**
 * HttpClient test class. This case tests the https connection with valid
 * credentials already provided.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2013   1786     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HttpClientValidCredentialsTest {
    private static JettyServer server;

    @Before
    public void startHttpsServer() {
        server = new JettyServer(HttpTestConstants.HTTPS_PORT);
        server.addServlet(new TestServletInstance(), "/test");
        try {
            server.startSSLServer(true);
        } catch (Exception e) {
            throw new RuntimeException("Error starting Jetty Server", e);
        }
    }

    @After
    public void stopServer() {
        try {
            server.stopServer();
        } catch (Exception e) {
            throw new RuntimeException("Error stopping Jetty Server", e);
        }
    }

    @Test
    public void testHttpsConnectionWithValidCredentials() {
        int expectedCode = 200;
        int actualCode = 200;

        HttpClient client = HttpClient.getInstance();
        client.setHandler(new TestHttpsCredentialsHandler());
        client.setHttpsConfiguration(new TestHttpsConfiguration());

        try {
            client.setCredentials(HttpTestConstants.HOST,
                    HttpTestConstants.HTTPS_PORT, HttpTestConstants.REALM,
                    HttpTestConstants.USERNAME, HttpTestConstants.PASSWD);

            HttpGet request = new HttpGet();
            request.setURI(new URI(HttpTestConstants.HTTPS_URI));
            HttpClientResponse response = client.executeRequest(request);
            actualCode = response.code;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        assertEquals(expectedCode, actualCode);
    }
}
