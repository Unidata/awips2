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
package com.raytheon.uf.edex.registry.ebxml.util;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * 
 * Bean used to dynamically assign the URL for a registry service via the spring
 * configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2012 #184       bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ServiceUrlFactory {

    /**
     * Gets a service address for the given service
     * 
     * @param service
     *            The name of the service
     * @param port
     *            The port on which the service will listen
     * @return The URL String of the service endpoint
     * @throws UnknownHostException
     *             If problems occur determining the host name
     */
    public String getServiceAddress(String service, String port)
            throws UnknownHostException {
        String hostName = InetAddress.getLocalHost().getHostName();
        return "http://" + hostName + ":" + port + "/" + service;
    }
}
