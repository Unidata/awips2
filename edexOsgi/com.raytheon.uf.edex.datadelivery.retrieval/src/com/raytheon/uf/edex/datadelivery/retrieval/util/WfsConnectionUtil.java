package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.net.URI;

import org.apache.http.client.methods.HttpGet;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Provider;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class WfsConnectionUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WfsConnectionUtil.class);

    public static String wfsConnect(String url, Provider provider) {
        String xmlResponse = null;
        HttpClient http = null;

        try {
            // Sets up any proxy info that might be necessary
            // TODO: consider using HTTP POST instead of GET
            ConnectionUtil.getProxyParameters();
            http = HttpClient.getInstance();
            HttpGet get = new HttpGet();
            URI uri = new URI(url);
            Connection conn = provider.getConnection();
            // check for the need to do a username password auth check
            if (conn != null && conn.getUserName() != null
                    && conn.getPassword() != null) {

                http.setCredentials(uri.getHost(), uri.getPort(),
                        provider.getName(), conn.getUserName(),
                        conn.getPassword());
            }

            get.setURI(uri);
            HttpClientResponse response = http.executeRequest(get);
            xmlResponse = new String(response.data);

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Couldn't connect to WFS server: " + url, e);
        }

        return xmlResponse;
    }

}
