package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.regex.Pattern;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.comm.IHttpsConfiguration;
import com.raytheon.uf.common.comm.IHttpsCredentialsHandler;
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.ProviderCredentials;
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
 * Jul 10, 2013 2180       dhladky     Updated credential requests
 * Aug 23, 2013 2180       mpduff      Implement changes to ProviderCredentialsUtil
 * Aug 06, 2013 2097       dhladky     WFS 2.0 compliance upgrade and switched to POST
 * Nov 20, 2013 2554       dhladky     Added GZIP capability to WFS requests.
 * Jan 13, 2014 2697       dhladky     Added util to strip unique Id field from URL.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class WfsConnectionUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WfsConnectionUtil.class);
    
    private static final Pattern COMMA_PATTERN = Pattern.compile(",");

    /**
     * Connect to the provided URL and return the xml response.
     * 
     * @param url
     *            The URL
     * @param providerConn
     *            The Connection object
     * @param providerName
     *            The data provider's name
     * @return xml response
     */
    public static String wfsConnect(String request, Connection providerConn,
            String providerName) {

        String xmlResponse = null;
        HttpClient http = null;
        String rootUrl = null;

        try {

            rootUrl = getCleanUrl(providerConn.getUrl());
            http = HttpClient.getInstance();
            // accept gzipped data for WFS
            http.setGzipResponseHandling(true);
            URI uri = new URI(rootUrl);
            HttpPost post = new HttpPost(uri);
            // check for the need to do a username password auth check
            ProviderCredentials creds = ProviderCredentialsUtil
                    .retrieveCredentials(providerName);
            Connection localConnection = creds.getConnection();

            if (localConnection != null
                    && localConnection.getProviderKey() != null) {
                statusHandler.handle(Priority.INFO,
                        "Attempting credentialed request: " + providerName);
                // Local Connection object contains the username, password and
                // encryption method for password storage and decrypt.
                String userName = localConnection.getUnencryptedUsername();
                String password = localConnection.getUnencryptedPassword();

                http.setHandler(new WfsCredentialsHandler(userName, password));
                http.setHttpsConfiguration(new WfsHttpsConfiguration(uri));
                http.setCredentials(uri.getHost(), uri.getPort(), providerName,
                        userName, password);
            }

            post.setEntity(new StringEntity(request, "text/xml", "ISO-8859-1"));
            HttpClientResponse response = http.executeRequest(post);
            xmlResponse = new String(response.data);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't connect to WFS server: " + rootUrl, e);
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
    private static class WfsCredentialsHandler implements
            IHttpsCredentialsHandler {

        private final String username;

        private final String password;

        @Override
        public String[] getCredentials(String message) {
            return new String[] { username, password };
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

        public WfsHttpsConfiguration(URI uri) throws URISyntaxException {

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
                throw new URISyntaxException(uri.toString(),
                        "Syntax or URI is bad!");
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
    
    /**
     * Removes un-needed unique Identifier from PointDataSetMetaData derived URL's
     * @param rootUrl
     * @return
     */
    private static String getCleanUrl(String providerUrl) {
        return COMMA_PATTERN.split(providerUrl)[0];
    }
}
