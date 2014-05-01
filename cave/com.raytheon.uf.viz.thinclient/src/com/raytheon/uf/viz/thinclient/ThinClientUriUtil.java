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
package com.raytheon.uf.viz.thinclient;

/**
 * Utilities for manipulating thin client URIs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2014            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ThinClientUriUtil {

    private static String services = "services";

    private static String pypies = "pypies";

    static {
        services = System.getProperty("thinclient.proxy.services", services);
        pypies = System.getProperty("thinclient.proxy.pypies", pypies);
    }

    private ThinClientUriUtil() {

    }

    /**
     * Returns the address to connect to EDEX services when going through the
     * thin client proxy
     * 
     * @param proxyAddress
     *            the address of the proxy server
     * @return
     */
    public static String getServicesAddress(String proxyAddress) {
        return appendTrailingSlash(proxyAddress) + services;
    }

    /**
     * Returns the address to connect to PyPIES when going through the thin
     * client proxy
     * 
     * @param proxyAddress
     *            the address of the proxy server
     * @return
     */
    public static String getPypiesAddress(String proxyAddress) {
        return appendTrailingSlash(proxyAddress) + pypies;
    }

    /**
     * Appends a trailing slash to an address if the address does not have one
     * 
     * @param proxyAddress
     * @return
     */
    private static String appendTrailingSlash(String proxyAddress) {
        if (!proxyAddress.endsWith("/")) {
            proxyAddress += "/";
        }
        return proxyAddress;
    }

}
