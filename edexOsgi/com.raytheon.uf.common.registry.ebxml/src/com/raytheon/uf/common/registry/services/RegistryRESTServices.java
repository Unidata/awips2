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
package com.raytheon.uf.common.registry.services;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.raytheon.uf.common.registry.constants.RegistryAvailability;
import com.raytheon.uf.common.registry.services.rest.IRegistryAvailableRestService;
import com.raytheon.uf.common.registry.services.rest.IRegistryObjectsRestService;
import com.raytheon.uf.common.registry.services.rest.IRepositoryItemsRestService;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Class used to access REST services provided by the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/21/2013    2022        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryRESTServices {

    /** Map of known registry object request services */
    private static LoadingCache<String, IRegistryObjectsRestService> registryObjectServiceMap = CacheBuilder
            .newBuilder().expireAfterAccess(1, TimeUnit.HOURS)
            .build(new CacheLoader<String, IRegistryObjectsRestService>() {
                public IRegistryObjectsRestService load(String key) {
                    return JAXRSClientFactory.create(key,
                            IRegistryObjectsRestService.class);
                }
            });

    /** Map of known repository item request services */
    private static LoadingCache<String, IRepositoryItemsRestService> repositoryItemServiceMap = CacheBuilder
            .newBuilder().expireAfterAccess(1, TimeUnit.HOURS)
            .build(new CacheLoader<String, IRepositoryItemsRestService>() {
                public IRepositoryItemsRestService load(String key) {
                    return JAXRSClientFactory.create(key,
                            IRepositoryItemsRestService.class);
                }
            });

    /** Map of known registry availability services */
    private static LoadingCache<String, IRegistryAvailableRestService> registryAvailabilityServiceMap = CacheBuilder
            .newBuilder().expireAfterAccess(1, TimeUnit.HOURS)
            .build(new CacheLoader<String, IRegistryAvailableRestService>() {
                public IRegistryAvailableRestService load(String key) {
                    return JAXRSClientFactory.create(key,
                            IRegistryAvailableRestService.class);
                }
            });

    /**
     * The logger
     */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryRESTServices.class);

    /**
     * Gets the registry object rest service implementation
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return The service implementation
     * @throws RegistryServiceException
     *             If an invalid URL is provided
     */
    public static IRegistryObjectsRestService getRegistryObjectService(
            String baseURL) throws RegistryServiceException {
        try {
            return registryObjectServiceMap.get(baseURL);
        } catch (ExecutionException e) {
            throw new RegistryServiceException(
                    "Error getting Registry Object Rest Service", e);
        }
    }

    /**
     * Gets a registry object via the rest service
     * 
     * @param <T>
     *            Type of object extending RegistryObjectType
     * @param baseURL
     *            The base URL of the registry
     * @param objectId
     *            The id of the object to retrieve
     * @return The object
     * @throws JAXBException
     *             If errors occur while serializing the object
     * @throws RegistryServiceException
     *             If an invalid URL is provided
     */
    public static <T extends RegistryObjectType> T getRegistryObject(
            Class<T> expectedType, String baseURL, String objectId)
            throws JAXBException, RegistryServiceException {
        return SerializationUtil.unmarshalFromXml(expectedType,
                getRegistryObjectService(baseURL).getRegistryObject(objectId));
    }

    /**
     * Gets the repository item rest service implementation
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return The service implementation
     * @throws RegistryServiceException
     *             If an invalid URL is provided
     */
    public static IRepositoryItemsRestService getRepositoryItemService(
            String baseURL) throws RegistryServiceException {
        try {
            return repositoryItemServiceMap.get(baseURL);
        } catch (ExecutionException e) {
            throw new RegistryServiceException(
                    "Error getting Repository Item Rest Service", e);
        }
    }

    /**
     * Gets a repository item via the rest service
     * 
     * @param baseURL
     *            The base URL of the registry
     * @param repositoryItemId
     *            The id of the object
     * @return The repository item
     * @throws RegistryServiceException
     *             If an invalid URL is provided
     */
    public static byte[] getRepositoryItem(String baseURL,
            String repositoryItemId) throws RegistryServiceException {
        return getRepositoryItemService(baseURL).getRepositoryItem(
                repositoryItemId);
    }

    /**
     * Gets the registry available service implementation
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return THe registry available service implementation
     * @throws RegistryServiceException
     *             If an invalid URL is provided
     */
    public static IRegistryAvailableRestService getRegistryAvailableService(
            String baseURL) throws RegistryServiceException {
        try {
            return registryAvailabilityServiceMap.get(baseURL);
        } catch (ExecutionException e) {
            throw new RegistryServiceException(
                    "Error getting Registry Availability Rest Service", e);
        }
    }

    /**
     * Check if the registry at the given URL is available
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return True if the registry services are available
     */
    public static boolean isRegistryAvailable(String baseURL) {
        try {
            String response = getRegistryAvailableService(baseURL)
                    .isRegistryAvailable();
            return RegistryAvailability.AVAILABLE.equals(response);
        } catch (Throwable t) {
            statusHandler.error(
                    "Registry at [" + baseURL + "] not available: ",
                    t.getLocalizedMessage());
            return false;
        }
    }
}
