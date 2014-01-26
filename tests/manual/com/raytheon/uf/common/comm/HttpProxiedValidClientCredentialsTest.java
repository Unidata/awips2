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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.servlets.ProxyServlet;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Encryption;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Encryption.Algorithim;
import com.raytheon.uf.common.datadelivery.registry.Encryption.Padding;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.datadelivery.registry.PointTime;
import com.raytheon.uf.common.datadelivery.registry.Projection;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.util.ProxiedJettyServer;
import com.vividsolutions.jts.geom.Coordinate;

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
 * Jun 17, 2013   2106     djohnson     Use unencrypted password getter.
 * July15, 2103   2180     dhladky      Updated encryption
 * Aug 08, 2013   2097     dhladky      updated for WFS 2.0 and HTTP POST
 * Sept 30, 2013  1797     dhladky      separated gridded time from time
 * Oct 10, 2013 1797       bgonzale     Refactored registry Time objects.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HttpProxiedValidClientCredentialsTest {

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
        String xmlResponse;
        HttpClient http = null;

        try {
            // Sets up any proxy info that might be necessary
            http = HttpClient.getInstance();
            
            Provider provider = getProvider();
            Connection conn = provider.getConnection();
           
            MockWfsRequestBuilder wrb = new MockWfsRequestBuilder(getRetrievalAttribute());
            String request = wrb.getRequest();
            //String request = getRequest("/home/dhladky/Desktop/good.xml");
            System.out.println(request);
            URI uri = new URI(conn.getUrl());
            //URI uri = new URI("http://stormy.oma.us.ray.com:8085/wfs");
            HttpPost post = new HttpPost(uri);
            System.out.println("HTTPS URL: "+conn.getUrl());
                     
            // check for the need to do a username password auth check
            if (conn != null && conn.getUserName() != null
                    && conn.getPassword() != null) {

                conn.setProviderKey(HttpProxyTestConstants.PROVIDER_KEY);
                final String unencryptedPassword = conn.getUnencryptedPassword();
                final String unencryptedUsername = conn.getUnencryptedUsername();
                http.setHandler(new TestHttpsCredentialsHandler());
                http.setHttpsConfiguration(new TestHttpsConfiguration());
                http.setCredentials(uri.getHost(), uri.getPort(),
                        provider.getName(), unencryptedUsername,
                        unencryptedPassword);

                System.out.println("Credentials set! " + unencryptedUsername
                        + " " + unencryptedPassword);
            }

            post.setEntity(new StringEntity(request, "text/xml", "ISO-8859-1"));
            HttpClientResponse response = http.executeRequest(post);
            xmlResponse = new String(response.data);
            System.out.println(xmlResponse);
            assertEquals(expectedCode, response.code);
            
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }
    
    private Connection getConnection() {
        
        Connection conn = new Connection();
        conn.setUserName(HttpProxyTestConstants.USERNAME);
        conn.setPassword(HttpProxyTestConstants.PASSWD);
        conn.setProviderKey(HttpProxyTestConstants.PROVIDER_KEY);
        Encryption encryption = new Encryption();
        encryption.setAlgorithim(Algorithim.AES);
        encryption.setPadding(Padding.AES);
        conn.setEncryption(encryption);
        // encrypt credentials
        conn.encryptUserName();
        conn.encryptPassword();
        conn.setUrl(HttpProxyTestConstants.HTTPS_URI);
        
        return conn;
    }
    
    private Projection getProjection() {
        // projection object
        Projection proj = new Projection();
        proj.setName("MadisLatLon");
        proj.setDescription("Madis test LatLon");
        return proj;
    }
    
    private Provider getProvider() {
        Provider provider = new Provider();
        provider.setName(HttpProxyTestConstants.REALM);
        provider.setServiceType(ServiceType.WFS);
        provider.setConnection(getConnection());
        // provider type
        ProviderType pt = new ProviderType();
        pt.setAvailabilityDelay(0);
        pt.setDataType(DataType.POINT);
        pt.setPlugin("madis");
        List<ProviderType> types = new ArrayList<ProviderType>();
        types.add(pt);
        // addd the provider back
        provider.setProviderType(types);
        List<Projection> projs = new ArrayList<Projection>();
        projs.add(getProjection());
        provider.setProjection(projs);
               
        return provider;
    }
    
    private RetrievalAttribute getRetrievalAttribute() throws ParseException {
        RetrievalAttribute ra = new RetrievalAttribute();
        ra.setPlugin("madis");
        ra.setCoverage(getCoverage());
        ra.setProvider(getProvider().getName());
        ra.setTime(getTime());
  
        return ra;
    }
    
    private Coverage getCoverage() {
        
        double lowerLon = -100.0;
        double lowerLat = 40.0;
        double upperLon = -98.0;
        double upperLat = 42.0;

        Coordinate lowerRight = new Coordinate(upperLon, lowerLat);
        Coordinate upperLeft = new Coordinate(lowerLon, upperLat);
        ReferencedEnvelope re = EnvelopeUtils.createLatLonEnvelope(lowerRight,
                upperLeft);
        Coverage coverage = new Coverage();
        coverage.setEnvelope(re);
        coverage.setRequestEnvelope(re);

        return coverage;
    }
    
    private Time getTime() throws ParseException {
        
        String startDateString = "2013-11-12T18:00:00.000";
        String endDateString = "2013-11-12T19:00:00.000";
                
        PointTime time = new PointTime();
        time.setFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
        time.setRequestStartDate(startDateString);
        time.setStartDate(startDateString);
        time.setRequestEndDate(endDateString);
        time.setEndDate(endDateString);
       
        return time;
    }

    /**
     * Used in cases where you have XML you want to read in and test     
     * @param path
     * @return
     */
    private String getRequest(String path)  {

        StringBuffer fileData = new StringBuffer(1000);
        BufferedReader reader = null;
        try {

            reader = new BufferedReader(new FileReader(path));
            char[] buf = new char[1024];
            int numRead = 0;
            while ((numRead = reader.read(buf)) != -1) {
                String readData = String.valueOf(buf, 0, numRead);
                fileData.append(readData);
                buf = new char[1024];
            }
            reader.close();
        } catch (Exception e) {
            System.out.println("Didn't read file correctly!" + e);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        return fileData.toString();
    }
}