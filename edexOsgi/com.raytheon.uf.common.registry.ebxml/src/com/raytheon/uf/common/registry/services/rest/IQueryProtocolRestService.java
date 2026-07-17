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
package com.raytheon.uf.common.registry.services.rest;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.UriInfo;
import jakarta.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

/**
 * 
 * Interface for the QueryProtocol rest service
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2013    2022        bphillip    Initial implementation
 * 10/30/2013   1538        bphillip    Changed REST path
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path("/search")
public interface IQueryProtocolRestService {

    /**
     * Executes a query based on the submitted query parameters
     * 
     * @param info
     *            The UriInfo containing the query parameters
     * @return The marshalled QueryResponse
     * @throws JAXBException
     *             If errors occur while marshalling the response
     * @throws MsgRegistryException
     *             If errors occur in the registry while querying for the
     *             objects
     */
    @GET
    @Produces("text/xml")
    public String executeQuery(@Context UriInfo info) throws JAXBException,
            MsgRegistryException;

}
