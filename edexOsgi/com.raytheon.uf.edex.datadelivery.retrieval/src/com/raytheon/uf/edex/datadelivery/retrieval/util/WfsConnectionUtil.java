package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.net.URI;
import java.net.URISyntaxException;

import org.apache.http.client.methods.HttpGet;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.comm.IHttpsConfiguration;
import com.raytheon.uf.common.comm.IHttpsCredentialsHandler;
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * WFS Connection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013 753        dhladky     created.
 * May 31, 2013 1763       dhladky     refined.
 * Jun 17, 2013 2106       djohnson    Use getUnencryptedPassword().
 * Jun 18, 2013 2120       dhladky     Times fixes and SSL changes
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class WfsConnectionUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WfsConnectionUtil.class);
       
    public static String wfsConnect(String url, Connection conn,
            String providerName) {
        String xmlResponse = null;
        HttpClient http = null;

        try {
            // TODO: consider using HTTP POST instead of GET
            http = HttpClient.getInstance();
            HttpGet get = new HttpGet();
            URI uri = new URI(url);
            // check for the need to do a username password auth check
            if (conn != null && conn.getUserName() != null
                    && conn.getPassword() != null) {
                statusHandler.handle(Priority.INFO,
                        "Attempting credential request: " + providerName);
                http.setHandler(new WfsCredentialsHandler(conn.getUserName(),
                        conn.getUnencryptedPassword()));
                http.setHttpsConfiguration(new WfsHttpsConfiguration(uri));
                http.setCredentials(uri.getHost(), uri.getPort(), providerName,
                        conn.getUserName(), conn.getUnencryptedPassword());
            }

            get.setURI(uri);
            HttpClientResponse response = http.executeRequest(get);
            xmlResponse = new String(response.data);
            //System.out.println("Response: "+xmlResponse);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't connect to WFS server: " + url, e);
        }

        return xmlResponse;
    }
    
    /**
     * 
     * Credentials Holder
     * 
     * <pre>
     *
     * SOFTWARE HISTORY
     *
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jun 19, 2013  2120       dhladky     Initial creation
     *
     * </pre>
     *
     * @author dhladky
     * @version 1.0
     */
    private static class WfsCredentialsHandler implements IHttpsCredentialsHandler {

        private String username;
        
        private String password;
        
        @Override
        public String[] getCredentials(String message) {
            return new String[] { username,
                    password };
        }
        
        public WfsCredentialsHandler(String username, String password) {
            this.password = password;
            this.username = username;
        }
    }
    
    /**
     * 
     * HTTPS Configuration
     * 
     * <pre>
     *
     * SOFTWARE HISTORY
     *
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jun 19, 2013  2120       dhladky     Initial creation
     *
     * </pre>
     *
     * @author dhladky
     * @version 1.0
     */
    private static class WfsHttpsConfiguration implements IHttpsConfiguration {
        
        private int httpsPort = 443;
        
        private int httpPort = 80;

        public WfsHttpsConfiguration(URI uri) {

            try {
                if (uri.getScheme().equals("http")) {
                    httpPort = uri.getPort();
                } else if (uri.getScheme().equals("https")) {
                    httpsPort = uri.getPort();
                    if (httpsPort == -1) {
                        httpsPort = 443; // The default https port
                    }
                } else {
                    throw new URISyntaxException(uri.toString(),
                            "Invalid server");
                }
            } catch (URISyntaxException e) {
                statusHandler.handle(Priority.PROBLEM, "Syntax or URI is bad!", e);
            }
        }

        @Override
        public int getHttpsPort() {
            return httpsPort;
        }

        @Override
        public int getHttpPort() {
            return httpPort;
        }
    }
  
}
