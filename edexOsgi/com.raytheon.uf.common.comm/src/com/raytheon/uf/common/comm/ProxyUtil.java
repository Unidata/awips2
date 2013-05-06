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

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import com.raytheon.uf.common.util.PropertiesUtil;

/**
 * Proxy Utility Class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2013    1786     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ProxyUtil {
    /** Proxy host environment variable name */
    public static final String HTTP_PROXY_HOST = "http.proxyHost";

    /** Proxy port environment variable name */
    public static final String HTTP_PROXY_PORT = "http.proxyPort";

    /**
     * Get the proxy server settings if any are present.
     * 
     * @return ProxyConfiguration object or null if no settings
     */
    public static ProxyConfiguration getProxySettings() {
        String host = System.getProperty(HTTP_PROXY_HOST);
        String port = System.getProperty(HTTP_PROXY_PORT);

        if (host != null && port != null) {
            return new ProxyConfiguration(host, port);
        }

        return null;
    }

    /**
     * Get the proxy settings from the provided properties file.
     * 
     * @param proxyFile
     *            The properties file
     * @return ProxyConfiguration object, or null if no settings
     * @throws IOException
     *             If error reading properties file
     */
    public static ProxyConfiguration getProxySettings(File proxyFile)
            throws IOException {
        ProxyConfiguration proxySettings = null;
        Properties properties = PropertiesUtil.read(proxyFile);
        String host = properties.getProperty(HTTP_PROXY_HOST);
        String port = properties.getProperty(HTTP_PROXY_PORT);

        if (host != null && port != null) {
            proxySettings = new ProxyConfiguration(host, port);
        }

        return proxySettings;
    }
}
