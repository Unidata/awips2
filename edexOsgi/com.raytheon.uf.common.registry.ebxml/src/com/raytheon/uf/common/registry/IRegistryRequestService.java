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

import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.jws.soap.SOAPBinding.Style;

/**
 * 
 * Web service interface accessible to process Thrift serialized server requests
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
@WebService(serviceName = "RegistryRequestService")
@SOAPBinding(style = Style.RPC)
public interface IRegistryRequestService {

    /**
     * Process the Thrift serialized request and return the Thrift serialized
     * response
     * 
     * @param data
     *            The Thrift serialized request
     * @return The Thrift serialized response
     */
    @WebMethod
    public byte[] request(byte[] data);
}
