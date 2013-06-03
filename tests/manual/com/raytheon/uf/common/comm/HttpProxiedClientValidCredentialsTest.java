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
import java.util.ArrayList;
import java.util.List;

import org.apache.http.client.methods.HttpGet;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.servlets.ProxyServlet;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Connection.Encryption;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Projection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.util.ProxiedJettyServer;

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
 * Jun 11, 2013   1763     dhladky      Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HttpProxiedClientValidCredentialsTest {

    private static ProxiedJettyServer server;

    @Before
    public void startHttpsServer() {
        server = new ProxiedJettyServer(HttpProxyTestConstants.HTTP_URI,
                HttpProxyTestConstants.HTTP_PORT,
                HttpProxyTestConstants.HTTPS_PORT,
                HttpProxyTestConstants.CONTEXT, HttpProxyTestConstants.REALM,
                HttpProxyTestConstants.USERNAME, HttpProxyTestConstants.PASSWD);
        
        // sample setup
        ProxyServlet.Transparent servlet = new ProxyServlet.Transparent();
        ServletHolder servletHolder = new ServletHolder(servlet);
        servletHolder.setInitParameter("ProxyTo",
                HttpProxyTestConstants.HTTP_URI);
        servletHolder.setInitParameter("Prefix", "/"
                + HttpProxyTestConstants.CONTEXT);
        server.addServlet(servletHolder, "/" + HttpProxyTestConstants.CONTEXT
                + "/*");

        try {
            server.startSSLServer();
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
        String xmlResponse;
        HttpClient http = null;

        try {
            // Sets up any proxy info that might be necessary
            // TODO: consider using HTTP POST instead of GET
            http = HttpClient.getInstance();
            http.setHandler(new TestHttpsCredentialsHandler());
            http.setHttpsConfiguration(new TestHttpsConfiguration());
            // connection object
            Connection conn = new Connection();
            conn.setUserName(HttpProxyTestConstants.USERNAME);
            conn.setPassword(HttpProxyTestConstants.PASSWD);
            conn.setEncryption(Encryption.CLEAR);
            conn.setUrl(HttpProxyTestConstants.HTTPS_URI);
            // projection object
            Projection proj = new Projection();
            proj.setName("MadisLatLon");
            proj.setDescription("Madis test LatLon");
            // provider
            Provider provider = new Provider();
            provider.setName(HttpProxyTestConstants.REALM);
            provider.setServiceType(ServiceType.WFS);
            provider.setConnection(conn);
            // provider type
            ProviderType pt = new ProviderType();
            pt.setAvailabilityDelay(0);
            pt.setDataType(DataType.POINT);
            pt.setPlugin("madis");
            List<ProviderType> types = new ArrayList<ProviderType>();
            types.add(pt);
            // addd the provider back
            provider.setProviderType(types);

            String url = HttpProxyTestConstants.HTTPS_URI
                    + "?request=getfeature&typename=http://madis.edex.uf.raytheon.com:madis&srsName=crs:84&bbox=-96.0,41.0,-94.0,43.0&maxFeatures=1000";
            Connection conn1 = provider.getConnection();
            HttpGet get = new HttpGet();
            URI uri = new URI(url);

            // check for the need to do a username password auth check
            if (conn1 != null && conn1.getUserName() != null
                    && conn1.getPassword() != null) {

                http.setCredentials(uri.getHost(), uri.getPort(),
                        provider.getName(), conn1.getUserName(),
                        conn1.getPassword());

                System.out.println("Credentials set! " + conn1.getUserName()
                        + " " + conn1.getPassword());
            }

            get.setURI(uri);
            System.out.println("Get: " + get.getURI().toString());
            HttpClientResponse response = http.executeRequest(get);
            xmlResponse = new String(response.data);
            System.out.println(xmlResponse);

        } catch (Exception e) {
            e.printStackTrace();
        }

        assertEquals(expectedCode, actualCode);
    }
}
