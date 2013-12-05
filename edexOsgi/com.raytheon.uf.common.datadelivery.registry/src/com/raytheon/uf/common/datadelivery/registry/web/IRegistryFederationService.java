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
import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import com.raytheon.uf.common.registry.services.RegistryServiceException;

/**
 * <pre>
 * Interface for Registry federation status
 *  
 *  SOFTWARE HISTORY
 *  
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  11/20/2013   2534        bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
@Path(IRegistryFederationService.REGISTRY_FEDERATION_STATUS_PATH)
public interface IRegistryFederationService {

    /** The path to these set of services */
    public static final String REGISTRY_FEDERATION_STATUS_PATH = "/status/";

    /**
     * Gets if this registry is participating in the federation
     * 
     * @return The value of the EBXML_REGISTRY_FEDERATION_ENABLED environment
     *         variable
     */
    @GET
    @Path("isFederated")
    public String isFederated();

    /**
     * Gets information about this registry
     * 
     * @return Information pertaining to the local registry
     */
    @GET
    @Path("getMyRegistryInfo")
    public String getMyRegistryInfo();

    /**
     * Queries the NCF registry to get a list of registries in the federation
     * 
     * @return The list of registries in the federation
     * @throws MsgRegistryException
     *             If an error occurs while querying the NCF registry
     */
    @GET
    @Path("getFederationMembers")
    public String getFederationMembers() throws MsgRegistryException;

    /**
     * Gets the list of registry that the local registry is subscribed to
     * 
     * @return The list of registries that the local registry is subscribed to
     */
    @GET
    @Path("getRegistriesSubscribedTo")
    public String getRegistriesSubscribedTo();

    /**
     * Gets a list of registries that are subscribing to the local registry
     * 
     * @return The list of registries that are subscribing to the local registry
     */
    @GET
    @Path("getRegistrySubscribing")
    public String getRegistrySubscribing();

    /**
     * Gets the list of object types that are currently being replicated
     * 
     * @return The object list
     */
    @GET
    @Path("getObjectTypesReplicated")
    public String getObjectTypesReplicated();

    /**
     * Kicks of a full registry sync with the specified registry
     * 
     * @param registryId
     *            The registry ID to sync with
     * @return status message
     */
    @GET
    @Path("syncWithRegistry/{registryId}")
    public String syncWithRegistry(@PathParam("registryId") String registryId);

    /**
     * Subscribes to replication notifications from the specified registry
     * 
     * @param registryId
     *            The ID of the registry to subscribe to
     * @return Status message
     * @throws JAXBException
     * @throws RegistryServiceException
     */
    @GET
    @Path("subscribeToRegistry/{registryId}")
    public String subscribeToRegistry(@PathParam("registryId") String registryId)
            throws RegistryServiceException, JAXBException;

    /**
     * Unsubscribes from the specified registry
     * 
     * @param registryId
     *            The ID of the registry to unsubscribe from
     * @return The status message
     */
    @GET
    @Path("unsubscribeFromRegistry/{registryId}")
    public String unsubscribeFromRegistry(
            @PathParam("registryId") String registryId);

}
