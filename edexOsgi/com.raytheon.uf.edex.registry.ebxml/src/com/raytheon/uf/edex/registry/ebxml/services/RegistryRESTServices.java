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
package com.raytheon.uf.edex.registry.ebxml.services;

import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.cxf.clustering.FailoverFeature;
import org.apache.cxf.clustering.SequentialStrategy;
import org.apache.cxf.feature.Feature;
import org.apache.cxf.jaxrs.client.JAXRSClientFactoryBean;

import com.google.common.io.Resources;
import com.raytheon.uf.common.registry.RegistryJaxbManager;
import com.raytheon.uf.common.registry.RegistryNamespaceMapper;
import com.raytheon.uf.common.registry.constants.RegistryAvailability;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.registry.services.rest.IRegistryAvailableRestService;
import com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService;
import com.raytheon.uf.common.registry.services.rest.IRegistryFederationManager;
import com.raytheon.uf.common.registry.services.rest.IRegistryObjectsRestService;
import com.raytheon.uf.common.registry.services.rest.IRepositoryItemsRestService;
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
 * 7/29/2013    2191        bphillip    Implemented registry data access service
 * 8/1/2013     1693        bphillip    Modified getregistry objects method to correctly handle response
 * 9/5/2013     1538        bphillip    Changed cache expiration timeout and added http header
 * 10/30/2013   1538        bphillip    Moved data delivery services out of registry plugin
 * 11/20/2013   2534        bphillip    Added HTTPClient policy for rest connections.  Eliminated service caching.
 * 12/2/2013    1829        bphillip    Removed expectedType argument on getRegistryObject method
 * 1/15/2014    2613        bphillip    Removed Service cache due to unexpected behavior
 * 2/19/2014    2769        bphillip    Added service cache
 * 6/5/2014     1712        bphillip    Moved configuration out to separate class
 * 7/10/2014    1717        bphillip    Added authorization policy
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryRESTServices {

    /**
     * The logger
     */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryRESTServices.class);

    /** The url path to this set of services */
    private static final String REGISTRY_REST_SERVICE_PATH = "/rest";

    /*
     * When separating Data Delivery from the pure EBXML registry pieces, some
     * REST services were moved. This causes issues when a version 16.1.1
     * communicates with previous versions. A server error (500) is returned.
     * Unfortunately, the failover feature of CXF does not failover to alternate
     * address on a 500 error. This dummy address is used to force an
     * IOException to be thrown (for which CXF does in fact failover for).
     */

    /** The dummy url part used for failover */
    private static final String DUMMY_PATH = "/dummy";

    /** In versions prior to 16.1.1, some rest services reside here */
    private static final String REGISTRY_REST_SERVICE_FAILOVER_PATH = "/dataDelivery";

    /** JAXB Manager */
    private RegistryJaxbManager jaxbManager;

    /**
     * Creates a new RegistryRESTServices object
     * 
     * @throws JAXBException
     *             If the RegistryJaxbManager is not initialized properly
     */
    public RegistryRESTServices() throws JAXBException {
        jaxbManager = new RegistryJaxbManager(new RegistryNamespaceMapper());
    }

    /**
     * Gets the registry object rest service implementation
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return The service implementation
     */
    public IRegistryObjectsRestService getRegistryObjectService(String baseURL) {
        return createService(baseURL, IRegistryObjectsRestService.class);
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
     */
    @SuppressWarnings("unchecked")
    public <T extends RegistryObjectType> T getRegistryObject(String baseURL,
            String objectId) throws JAXBException, RegistryServiceException {
        String objStr = getRegistryObjectService(baseURL).getRegistryObject(
                objectId);
        Object retVal = jaxbManager.unmarshalFromXml(objStr);
        if (retVal instanceof JAXBElement<?>) {
            return (T) ((JAXBElement<?>) retVal).getValue();
        }
        return (T) jaxbManager.unmarshalFromXml(objStr);
    }

    /**
     * Gets the repository item rest service implementation
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return The service implementation
     */
    public IRepositoryItemsRestService getRepositoryItemService(String baseURL) {
        return createService(baseURL, IRepositoryItemsRestService.class);
    }

    /**
     * Gets a repository item via the rest service
     * 
     * @param baseURL
     *            The base URL of the registry
     * @param repositoryItemId
     *            The id of the object
     * @return The repository item
     */
    public byte[] getRepositoryItem(String baseURL, String repositoryItemId) {
        return getRepositoryItemService(baseURL).getRepositoryItem(
                repositoryItemId);
    }

    /**
     * Gets the federation manager rest services
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return The federation manager rest services
     */
    public IRegistryFederationManager getRegistryFederationManager(
            String baseURL) {
        return createService(baseURL, IRegistryFederationManager.class);
    }

    /**
     * Gets the registry available service implementation
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return THe registry available service implementation
     */
    public IRegistryAvailableRestService getRegistryAvailableService(
            String baseURL) {
        return createService(baseURL, IRegistryAvailableRestService.class);
    }

    /**
     * Check if the registry at the given URL is available
     * 
     * @param baseURL
     *            The base URL of the registry
     * @return True if the registry services are available
     */
    public boolean isRegistryAvailable(String baseURL) {
        String response = null;
        try {
            response = getRegistryAvailableService(baseURL)
                    .isRegistryAvailable();
            if (RegistryAvailability.AVAILABLE.equals(response)) {
                return true;
            } else {
                statusHandler.info("Registry at [" + baseURL
                        + "] not available: " + response);
            }
            return RegistryAvailability.AVAILABLE.equals(response);
        } catch (Throwable t) {
            if (response == null) {
                response = ExceptionUtils.getRootCauseMessage(t);
            }
            statusHandler.error("Registry at [" + baseURL + "] not available: "
                    + response);
            return false;
        }
    }

    /**
     * Gets the data access service for the specified registry URL
     * 
     * @param baseURL
     *            The baseURL of the registry
     * @return The data access service for the specified registry URL
     */
    public IRegistryDataAccessService getRegistryDataAccessService(
            String baseURL) {
        return createService(baseURL, IRegistryDataAccessService.class);

    }

    /**
     * Accesses a rest service at the provided URL. This method is primarily
     * used for resolving remote object references which use a REST service
     * 
     * @param url
     *            The URL of the rest service
     * @return
     */
    public Object accessXMLRestService(String url) {
        String response = null;
        try {
            response = Resources
                    .toString(new URL(url), Charset.forName("UTF8"));
        } catch (Exception e) {
            throw new RegistryServiceException(
                    "Error accessing REST service at URL: [" + url + "]", e);
        }
        try {
            return jaxbManager.unmarshalFromXml(response);
        } catch (JAXBException e) {
            throw new RegistryServiceException(
                    "Error unmarshalling xml response from REST Service at URL: ["
                            + url + "]");
        }
    }

    /**
     * Creates a REST service proxy object for the given service residing at the
     * given URL
     * 
     * @param url
     *            The url hosting the REST service
     * @param serviceClass
     *            The service class interface
     * @return A REST service proxy object
     */
    public static <T extends Object> T createService(final String url,
            final Class<T> serviceClass) {
        String dummyUrl = url + DUMMY_PATH;
        JAXRSClientFactoryBean clientFactory = new JAXRSClientFactoryBean();

        /*
         * Creates the failover feature. A dummy address is used as the primary
         * URL and the valid addresses are provided as alternates. This is an
         * unfortunate feature of the CXF feature. CXF only fails over to
         * alternate addresses if it encounters an IOException.
         */
        List<Feature> features = new ArrayList<Feature>();
        FailoverFeature failover = new FailoverFeature();
        RegistryFailoverTargetSelector targetSelector = new RegistryFailoverTargetSelector();
        failover.setTargetSelector(targetSelector);
        List<String> alternate = new ArrayList<String>(2);
        alternate.add(dummyUrl.replace(DUMMY_PATH, REGISTRY_REST_SERVICE_PATH));
        alternate.add(dummyUrl.replace(DUMMY_PATH,
                REGISTRY_REST_SERVICE_FAILOVER_PATH));
        SequentialStrategy strategy = new SequentialStrategy();
        strategy.setAlternateAddresses(alternate);
        failover.setStrategy(strategy);
        features.add(failover);

        clientFactory.setResourceClass(serviceClass);
        clientFactory.setAddress(dummyUrl);
        clientFactory.setFeatures(features);
        return clientFactory.create(serviceClass);
    }
}
