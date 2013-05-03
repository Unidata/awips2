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
package com.raytheon.uf.common.registry;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

/**
 * 
 * 
 * Service class for submitting registry requests to the EDEX ebxml registry via
 * JAX-WS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/3/2013     1948        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryRequestService extends Service {

    /** The service name */
    private static final QName SERVICE_NAME = new QName(
            "http://registry.common.uf.raytheon.com/", "RegistryRequestService");

    /** The location of the wsdl */
    private static final String WSDL_LOCATION = "/registryRequest?wsdl";

    /**
     * Creates a new RegistryRequestService service residing on the given server
     * on the given port
     * 
     * @param server
     *            The server
     * @param port
     *            The port number
     * @throws MalformedURLException
     *             If errors occur constructing the service URL
     */
    public RegistryRequestService(String server, String port)
            throws MalformedURLException {
        super(new URL("http://" + server + ":" + port + WSDL_LOCATION),
                SERVICE_NAME);
    }

    /**
     * Gets the IRegistryRequestService port
     * 
     * @return The IRegistryRequestService port
     */
    public IRegistryRequestService getService() {
        return super.getPort(IRegistryRequestService.class);
    }
}
