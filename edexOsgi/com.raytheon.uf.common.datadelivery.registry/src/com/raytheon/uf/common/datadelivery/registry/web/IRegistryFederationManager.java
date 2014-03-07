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

/**
 * 
 * Interface for the registry federation manager for exposing web services for
 * the web interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/27/2014    2769        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
@Path(IRegistryFederationManager.REGISTRY_FEDERATION_MANAGER_PATH)
public interface IRegistryFederationManager {

    /** The path to these set of services */
    public static final String REGISTRY_FEDERATION_MANAGER_PATH = "/federation/";

    /**
     * Gets if this registry is participating in the federation
     * 
     * @return The value of the EBXML_REGISTRY_FEDERATION_ENABLED environment
     *         variable
     */
    @GET
    @Path("isFederated")
    public String isFederated();

    @GET
    @Path("dataDeliveryId")
    public String dataDeliveryId();

    @GET
    @Path("siteId")
    public String siteId();

    @GET
    @Path("getObjectTypesReplicated")
    public String getObjectTypesReplicated();

    @GET
    @Path("getFederationMembers")
    public String getFederationMembers() throws Exception;

    @GET
    @Path("getReplicatingTo")
    public String getReplicatingTo();

    @GET
    @Path("getReplicatingFrom")
    public String getReplicatingFrom() throws Exception;

    @GET
    @Path("subscribeToRegistry/{registryId}")
    public void subscribeToRegistry(@PathParam("registryId") String registryId)
            throws Exception;

    @GET
    @Path("unsubscribeFromRegistry/{registryId}")
    public void unsubscribeFromRegistry(
            @PathParam("registryId") String registryId) throws Exception;

    @GET
    @Path("addReplicationServer/{registryId}")
    public void addReplicationServer(@PathParam("registryId") String registryId)
            throws Exception;

    @GET
    @Path("removeReplicationServer/{registryId}")
    public void removeReplicationServer(
            @PathParam("registryId") String registryId) throws Exception;

    @GET
    @Path("synchronizeWithRegistry/{registryId}")
    public void synchronizeWithRegistry(
            @PathParam("registryId") String registryId) throws Exception;

}
