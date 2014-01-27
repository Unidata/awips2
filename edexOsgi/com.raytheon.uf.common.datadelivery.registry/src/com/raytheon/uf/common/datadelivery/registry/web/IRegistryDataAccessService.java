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
package com.raytheon.uf.common.datadelivery.registry.web;

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
 * 9/20/2013    2385        bphillip    Added subscription backup functions
 * 10/8/2013    1682        bphillip    Added rest functions for use with the query web interface
 * 10/23/2013   2385        bphillip    restoreSubscriptions throws JAXBException
 * 10/30/2013   1538        bphillip    Moved data delivery specific servics out of registry plugin
 * 11/7/2013    1678        bphillip    Added get custom query method
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
            @PathParam("objectType") String objectType);

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
    public String getParametersForQuery(@PathParam("queryId") String queryId);

    /**
     * Removes any subscriptions for the given site
     * 
     * @param siteId
     *            The site to remove the subscriptions for
     * @throws RegistryException
     *             If errors occur while removing the subscriptions
     */
    @GET
    @Path("removeSubscriptionsFor/{siteId}")
    public void removeSubscriptionsForSite(@PathParam("siteId") String siteId)
            throws RegistryException;

    /**
     * Gets the subscriptions that are currently in the registry and formats
     * them in HTML for viewing in a web browser
     * 
     * @return The page containing the subscriptions
     */
    @GET
    @Path("getSubscriptions")
    public String getSubscriptions();

    /**
     * 
     * Backs up the specified subscription to be restored at a later time
     * 
     * @param subscriptionName
     *            The subscription to be backed up
     * @return Status message about whether the backup was successful
     */
    @GET
    @Path("backupSubscription/{subscriptionName}")
    public String backupSubscription(
            @PathParam("subscriptionName") String subscriptionName);

    /**
     * Backs up all subscriptions currently in the registry
     * 
     * @return Status message about whether the backup was successful
     */
    @GET
    @Path("backupAllSubscriptions/")
    public String backupAllSubscriptions();

    /**
     * Restores the specified subscription
     * 
     * @param subscriptionName
     *            The name of the subscription to restore
     * @return Status message about whether the backup was successful
     */
    @GET
    @Path("restoreSubscription/{subscriptionName}")
    public String restoreSubscription(
            @PathParam("subscriptionName") String subscriptionName);

    /**
     * Restores any subscriptions that were previously backed up
     * 
     * @return Status messages relating to the success or failure of the restore
     */
    @GET
    @Path("restoreSubscriptions/")
    public String restoreSubscriptions();

    /**
     * Clears the backup file directory
     * 
     * @return Status message
     */
    @GET
    @Path("clearSubscriptionBackupFiles/")
    public String clearSubscriptionBackupFiles();

}
