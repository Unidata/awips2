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
package com.raytheon.uf.common.registry.services;

import org.apache.cxf.transports.http.configuration.ConnectionType;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;

import com.raytheon.uf.common.comm.ProxyConfiguration;

/**
 * 
 * Class containing configuration items for registry soap and rest services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/05/2014     1712        bphillip    Initial coding
 * 06/18/2014     1712        bphillip    Updated Proxy configuration
 * 07/01/2020     8168        skabasele   Updated the connection type to keep alive. 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryServiceConfiguration {
    
    /** The name of the receive timeout property */
    private static final String RECEIVE_TIMEOUT_PROPERTY = "ebxml-http-receive-timeout";
    
    /** The name of the connect timeout property */
    private static final String CONNECT_TIMEOUT_PROPERTY = "ebxml-http-connection-timeout";

    /** Default timeout for receiving HTTP data */
    private static final String DEFAULT_RECEIVE_TIMEOUT = "60000";

    /** Default value for establishing an HTTP connection */
    private static final String DEFAULT_CONNECT_TIMEOUT = "10000";

    /**
     * Gets the HTTP communication policy.
     * 
     * @return The HTTP communication policy
     */
    public HTTPClientPolicy getHttpClientPolicy() {
        HTTPClientPolicy httpClientPolicy = new HTTPClientPolicy();
        httpClientPolicy.setReceiveTimeout(Long.parseLong(System.getProperty(
                RECEIVE_TIMEOUT_PROPERTY, DEFAULT_RECEIVE_TIMEOUT)));
        httpClientPolicy.setConnectionTimeout(Long.parseLong(System
                .getProperty(CONNECT_TIMEOUT_PROPERTY,
                        DEFAULT_CONNECT_TIMEOUT)));

        httpClientPolicy.setConnection(ConnectionType.KEEP_ALIVE);
        httpClientPolicy.setMaxRetransmits(5);
        if (ProxyConfiguration.HTTPS_PROXY_DEFINED) {
            httpClientPolicy.setProxyServer(ProxyConfiguration.getHttpsProxyHost());
            httpClientPolicy.setProxyServerPort(ProxyConfiguration.getHttpsProxyPort());
            httpClientPolicy.setNonProxyHosts(ProxyConfiguration.getHttpsNonProxyHosts());
        }
        return httpClientPolicy;
    }
}
