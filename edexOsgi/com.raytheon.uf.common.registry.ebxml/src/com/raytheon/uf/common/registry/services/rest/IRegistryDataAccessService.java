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
 * 9/20/2013    2385        bphillip    Added subscription backup functions
 * 10/8/2013    1682        bphillip    Added rest functions for use with the query web interface
 * 10/23/2013   2385        bphillip    restoreSubscriptions throws JAXBException
 * 10/30/2013   1538        bphillip    Moved data delivery specific servics out of registry plugin
 * 11/7/2013    1678        bphillip    Added get custom query method
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Path(IRegistryDataAccessService.DATA_ACCESS_PATH_PREFIX)
public interface IRegistryDataAccessService {

    /** Root path to this set of services */
    public static final String DATA_ACCESS_PATH_PREFIX = "/dataAccess";

    /**
     * Gets the ids of registry objects of the given object type
     * 
     * @param objectType
     *            The object type to get ids for
     * @return List of ids for registry objects of the given type
     */
    @GET
    @Path("getRegistryObjectIds/{objectType}")
    public RestCollectionResponse<String> getRegistryObjectIdsOfType(
            @PathParam("objectType")
            String objectType);

    /**
     * Gets the list of query definitions currently contained in the registry
     * 
     * @return The query definitions currently contained in the registry
     */
    @GET
    @Path("getQueries")
    public String getValidQueries();

    /**
     * Gets the list of non-canonical query types contained in the registry
     * 
     * @return The non-canonical query definitions
     */
    @GET
    @Path("getCustomQueries")
    public String getCustomQueries();

    /**
     * Gets the valid parameters for a given query definition
     * 
     * @param queryId
     *            The id of the query definition
     * @return The parameters for a given query definition
     */
    @GET
    @Path("getParametersForQuery/{queryId}")
    public String getParametersForQuery(@PathParam("queryId")
    String queryId);

}
