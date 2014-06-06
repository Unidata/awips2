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

import java.io.File;
import java.io.IOException;

import org.apache.cxf.transports.http.configuration.ConnectionType;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;

import com.raytheon.uf.common.comm.ProxyConfiguration;
import com.raytheon.uf.common.comm.ProxyUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

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
 * 6/5/2014     1712        bphillip    Initial coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryServiceConfiguration {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryServiceConfiguration.class);

    /** Default timeout for receiving HTTP data */
    private static final String DEFAULT_RECEIVE_TIMEOUT = "60000";

    /** Default value for establishing an HTTP connection */
    private static final String DEFAULT_CONNECT_TIMEOUT = "10000";

    /** The HTTP Communication policy configuration */
    private HTTPClientPolicy httpClientPolicy;

    /** The proxy configuration */
    private ProxyConfiguration proxyConfig;

    /**
     * Gets the HTTP communication policy.
     * 
     * @return The HTTP communication policy
     */
    public HTTPClientPolicy getHttpClientPolicy() {
        if (httpClientPolicy == null) {
            initHttpClientPolicy();
        }
        return httpClientPolicy;
    }

    /**
     * Initializes the HTTP communication policy
     */
    private void initHttpClientPolicy() {
        initProxyConfiguration();
        httpClientPolicy = new HTTPClientPolicy();
        httpClientPolicy.setReceiveTimeout(Long.parseLong(System.getProperty(
                "ebxml-http-receive-timeout", DEFAULT_RECEIVE_TIMEOUT)));
        httpClientPolicy.setConnectionTimeout(Long.parseLong(System
                .getProperty("ebxml-http-connection-timeout",
                        DEFAULT_CONNECT_TIMEOUT)));

        httpClientPolicy.setConnection(ConnectionType.CLOSE);
        httpClientPolicy.setMaxRetransmits(5);
        if (proxyConfig != null) {
            httpClientPolicy.setProxyServer(proxyConfig.getHost());
            httpClientPolicy.setProxyServerPort(proxyConfig.getPort());
            httpClientPolicy.setNonProxyHosts(proxyConfig.getNonProxyHosts());
        }
    }

    /**
     * Gets the proxy configuration
     * 
     * @return The proxy configuration
     */
    private void initProxyConfiguration() {
        if (proxyConfig == null) {
            File proxyFile = PathManagerFactory.getPathManager().getStaticFile(
                    "datadelivery" + File.separator + "proxy.properties");
            if (proxyFile != null) {
                try {
                    proxyConfig = ProxyUtil.getProxySettings(proxyFile);
                } catch (IOException e) {
                    throw new RegistryServiceException(
                            "Error reading proxy properties", e);
                }
            }
        }
    }
}
