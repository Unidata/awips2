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

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;

/**
 * 
 * REST service interface for various registry data access methods
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public interface IRegistryDataAccessService {

    /**
     * Gets the ids of registry objects of the given object type
     * 
     * @param objectType
     *            The object type to get ids for
     * @return List of ids for registry objects of the given type
     */
    @GET
    @Path("/rest/dataAccess/getRegistryObjectIds/{objectType}")
    public RestCollectionResponse<String> getRegistryObjectIdsOfType(
            @PathParam("objectType") String objectType);

    /**
     * Removes any subscriptions for the given site
     * 
     * @param siteId
     *            The site to remove the subscriptions for
     * @throws RegistryException
     *             If errors occur while removing the subscriptions
     */
    @GET
    @Path("/rest/dataAccess/removeSubscriptionsFor/{siteId}")
    public void removeSubscriptionsForSite(@PathParam("siteId") String siteId)
            throws RegistryException;
}
