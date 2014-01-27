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

import java.lang.reflect.Proxy;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.cxf.jaxrs.client.Client;
import org.apache.cxf.jaxrs.client.ClientConfiguration;
import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.ConnectionType;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.io.Resources;
import com.raytheon.uf.common.comm.ProxyConfiguration;
import com.raytheon.uf.common.registry.RegistryJaxbManager;
import com.raytheon.uf.common.registry.RegistryNamespaceMapper;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.rest.IRegistryObjectsRestService;
import com.raytheon.uf.common.registry.services.rest.IRepositoryItemsRestService;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryRESTServices {

    /** The url path to this set of services */
    private static final String REGISTRY_REST_SERVICE_PATH = "/rest";

    /** JAXB Manager */
    private RegistryJaxbManager jaxbManager;

    /** Policy used for rest connections */
    private static final HTTPClientPolicy restPolicy;

    static {
        ProxyConfiguration proxyConfig = RegistrySOAPServices
                .getProxyConfiguration();
        restPolicy = new HTTPClientPolicy();
        restPolicy.setConnection(ConnectionType.CLOSE);
        restPolicy.setConnectionTimeout(2000);
        restPolicy.setReceiveTimeout(30000);
        restPolicy.setMaxRetransmits(1);
        if (proxyConfig != null) {
            restPolicy.setProxyServer(proxyConfig.getHost());
            restPolicy.setProxyServerPort(proxyConfig.getPort());
            restPolicy.setNonProxyHosts(proxyConfig.getNonProxyHosts());
        }
    }

    private Map<Class<?>, LoadingCache<String, ?>> serviceCache = new HashMap<Class<?>, LoadingCache<String, ?>>();

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
        return getPort(baseURL + REGISTRY_REST_SERVICE_PATH,
                IRegistryObjectsRestService.class);
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
        return getPort(baseURL + REGISTRY_REST_SERVICE_PATH,
                IRepositoryItemsRestService.class);
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

    @SuppressWarnings("unchecked")
    protected <T extends Object> T getPort(String serviceUrl,
            final Class<T> serviceInterface) {
        LoadingCache<String, ?> cache = serviceCache.get(serviceInterface);
        if (cache == null) {
            cache = CacheBuilder.newBuilder()
                    .expireAfterAccess(1, TimeUnit.MINUTES)
                    .build(new CacheLoader<String, T>() {
                        public T load(String key) {
                            return createService(key, serviceInterface);
                        }
                    });
            serviceCache.put(serviceInterface, cache);
        }
        try {
            return (T) cache.get(serviceUrl);
        } catch (ExecutionException e) {
            throw new RuntimeException("Error getting service at ["
                    + serviceUrl + "]", e);
        }
    }

    protected <T extends Object> T createService(String url,
            Class<T> serviceClass) {
        T service = JAXRSClientFactory.create(url, serviceClass);
        Client client = (Client) Proxy.getInvocationHandler((Proxy) service);
        ClientConfiguration config = WebClient.getConfig(service);
        HTTPConduit conduit = config.getHttpConduit();
        conduit.setClient(restPolicy);

        // Create HTTP header containing the calling registry
        client.header(RegistryUtil.CALLING_REGISTRY_SOAP_HEADER_NAME,
                RegistryUtil.LOCAL_REGISTRY_ADDRESS);
        return service;
    }
}
