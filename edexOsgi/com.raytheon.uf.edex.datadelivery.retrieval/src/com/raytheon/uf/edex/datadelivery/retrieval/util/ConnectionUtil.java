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
package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.uf.common.comm.ProxyConfiguration;
import com.raytheon.uf.common.comm.ProxyUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import dods.dap.DConnect;

/**
 * Utilities for datadelivery connections.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 28, 2012 819        djohnson     Initial creation
 * Apr 01, 2013 1786       mpduff       Pulled proxy settings out to util class.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ConnectionUtil {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ColorMapManager.class);

    private static final String PROXY_PROPERTIES_FILE = "datadelivery"
            + File.separator + "proxy.properties";

    static ConnectionUtil instance = new ConnectionUtil();

    static volatile boolean initialized;

    private ProxyConfiguration proxySettings;

    static void clearSettings() {
        System.clearProperty(ProxyUtil.HTTP_PROXY_HOST);
        System.clearProperty(ProxyUtil.HTTP_PROXY_PORT);
        instance = new ConnectionUtil();
        initialized = false;
    }

    /**
     * Retrieve a DConnect instance.
     * 
     * @param urlString
     * @return DConnect instance
     * @throws FileNotFoundException
     *             rethrown from DConnect
     */
    public static DConnect getDConnect(String urlString)
            throws FileNotFoundException {
        if (!initialized) {
            initialize();
        }
        ProxyConfiguration proxyInformation = instance.getProxyInformation();
        if (proxyInformation != null) {
            return new DConnect(urlString, proxyInformation.getHost(),
                    proxyInformation.getPortString());
        } else {
            return new DConnect(urlString);
        }
    }

    /**
     * Returns the proxy information.
     * 
     * @return [0] = proxy host, [1] proxy port or null if there is no proxy
     *         information
     */
    public static ProxyConfiguration getProxyParameters() {
        if (!initialized) {
            initialize();
        }
        return instance.getProxyInformation();
    }

    private static synchronized void initialize() {
        ProxyConfiguration proxyInformation = instance.getProxyInformation();

        if (proxyInformation != null) {
            System.setProperty(ProxyUtil.HTTP_PROXY_HOST,
                    proxyInformation.getHost());
            System.setProperty(ProxyUtil.HTTP_PROXY_PORT,
                    proxyInformation.getPortString());
        }
        initialized = true;
    }

    /**
     * Package level constructor so test can call.
     */
    ConnectionUtil() {
    }

    /**
     * Returns the proxy information.
     * 
     * @return [0] = proxy host, [1] proxy port or null if there is no proxy
     *         information
     */
    ProxyConfiguration getProxyInformation() {
        if (proxySettings == null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext context = pathMgr.getContext(
                    LocalizationContext.LocalizationType.COMMON_STATIC,
                    LocalizationContext.LocalizationLevel.CONFIGURED);

            try {
                File proxyFile = pathMgr
                        .getFile(context, PROXY_PROPERTIES_FILE);
                // If the configured version doesn't exist, default to the base
                // version
                if (!proxyFile.exists()) {
                    context = pathMgr.getContext(
                            LocalizationContext.LocalizationType.COMMON_STATIC,
                            LocalizationContext.LocalizationLevel.BASE);
                    proxyFile = pathMgr.getFile(context, PROXY_PROPERTIES_FILE);
                }

                proxySettings = ProxyUtil.getProxySettings(proxyFile);
            } catch (FileNotFoundException e) {
                statusHandler.error("Unable to find a file with name "
                        + PROXY_PROPERTIES_FILE + "!", e);
            } catch (IOException e) {
                statusHandler.error("Unable to read file "
                        + PROXY_PROPERTIES_FILE + "!", e);
            }
        }

        return proxySettings;
    }
}