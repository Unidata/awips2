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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.ws.wsaddressing.W3CEndpointReference;
import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import com.raytheon.uf.common.registry.constants.RegistryResponseStatus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * 
 * Convenience class used for accessing the registry soap services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistrySOAPServices {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistrySOAPServices.class);

    /** The HTTP prefix */
    private static final String HTTP = "http://";

    /** Path separator */
    private static final String PATH_SEPARATOR = "/";

    /** WSDL suffix */
    private static final String WSDL = "?wsdl";

    /** The name of the notification listener service */
    private static final String NOTIFICATION_SERVICE_NAME = "notificationListener";

    /** The name of the lifecycle manager service */
    private static final String LIFECYCLE_MANAGER_SERVICE_NAME = "lifecycleManager";

    /** The name of the cataloger service */
    private static final String CATALOGER_SERVICE_NAME = "cataloger";

    /** The name of the query service */
    private static final String QUERY_SERVICE_NAME = "queryManager";

    /** The name of the validator service */
    private static final String VALIDATOR_SERVICE_NAME = "validator";

    /** Map of known notification services */
    private static Map<URL, NotificationListener> notificationServiceMap = new ConcurrentHashMap<URL, NotificationListener>();

    /** Map of known lifecycle manager services */
    private static Map<URL, LifecycleManager> lifecycleManagerServiceMap = new ConcurrentHashMap<URL, LifecycleManager>();

    /** Map of known cataloger services */
    private static Map<URL, Cataloger> catalogerServiceMap = new ConcurrentHashMap<URL, Cataloger>();

    /** Map of known query services */
    private static Map<URL, QueryManager> queryServiceMap = new ConcurrentHashMap<URL, QueryManager>();

    /** Map of known validator services */
    private static Map<URL, Validator> validatorServiceMap = new ConcurrentHashMap<URL, Validator>();

    /**
     * Gets the notification listener service URL for the given host
     * 
     * @param host
     *            The host
     * @return The notification listener service URL for the given host
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static String getNotificationListenerServiceUrl(final String host)
            throws MalformedURLException {
        return new URL(HTTP + host + PATH_SEPARATOR + NOTIFICATION_SERVICE_NAME)
                .toString();
    }

    /**
     * Gets the notification listener service for the given host
     * 
     * @param host
     *            The host to get the notification listener service for
     * @return The notification listener service for the given host
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static NotificationListener getNotificationListenerServiceForHost(
            final String host) throws MalformedURLException {
        return getNotificationListenerServiceForUrl(getNotificationListenerServiceUrl(host));
    }

    /**
     * Gets the notification listener service at the given URL string
     * 
     * @param serviceUrl
     *            The url
     * @return The notification listener service at the given URL
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static NotificationListener getNotificationListenerServiceForUrl(
            final String url) throws MalformedURLException {
        return getNotificationListener(new URL(url));
    }

    /**
     * Gets the notification listener service at the given URL
     * 
     * @param url
     *            The notification listener service URL
     * @return The notification listener service at the given URL
     */
    private static NotificationListener getNotificationListener(URL url) {
        NotificationListener notificationListener = notificationServiceMap
                .get(url);
        if (notificationListener == null) {
            notificationListener = getPort(url, NotificationListener.class);
            notificationServiceMap.put(url, notificationListener);
        }
        return notificationListener;
    }

    /**
     * Gets the lifecycle manager service for the given host
     * 
     * @param host
     *            The host to get the lifecycle manager service for
     * @return The lifecycle manager service for the given host
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static LifecycleManager getLifecycleManagerServiceForHost(
            final String host) throws MalformedURLException {
        return getLifecycleManager(new URL(HTTP + host + PATH_SEPARATOR
                + LIFECYCLE_MANAGER_SERVICE_NAME));
    }

    /**
     * Gets the lifecycle manager service for the given URL string
     * 
     * @param serviceUrl
     *            The service URL
     * @return The lifecycle manager service at the given URL string
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static LifecycleManager getLifecycleManagerServiceForUrl(
            final String url) throws MalformedURLException {
        return getLifecycleManager(new URL(url));
    }

    /**
     * Gets the lifecycle manager service for at the given URL
     * 
     * @param url
     *            The url
     * @return The lifecycle manager service at the given URL
     */
    private static LifecycleManager getLifecycleManager(URL url) {
        LifecycleManager lcm = lifecycleManagerServiceMap.get(url);
        if (lcm == null) {
            lcm = getPort(url, LifecycleManager.class);
            lifecycleManagerServiceMap.put(url, lcm);
        }
        return lcm;
    }

    /**
     * Gets the cataloger service for the given host
     * 
     * @param host
     *            The host to get the cataloger service for
     * @return The cataloger service at the given host
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static Cataloger getCatalogerServiceForHost(final String host)
            throws MalformedURLException {
        return getCataloger(new URL(HTTP + host + PATH_SEPARATOR
                + CATALOGER_SERVICE_NAME));
    }

    /**
     * Gets the cataloger service for the given url string
     * 
     * @param serviceUrl
     *            the url string
     * @return The cataloger service
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static Cataloger getCatalogerServiceForUrl(final String url)
            throws MalformedURLException {
        return getCataloger(new URL(url));
    }

    /**
     * Gets the cataloger service at the given URL
     * 
     * @param url
     *            The URL
     * @return The cataloger service at the given URL
     */
    private static Cataloger getCataloger(URL url) {
        Cataloger cataloger = catalogerServiceMap.get(url);
        if (cataloger == null) {
            cataloger = getPort(url, Cataloger.class);
            catalogerServiceMap.put(url, cataloger);
        }
        return cataloger;
    }

    /**
     * Gets the query manager service at the given host
     * 
     * @param host
     *            The host name
     * @return The query manager service
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static QueryManager getQueryServiceForHost(final String host)
            throws MalformedURLException {
        return getQueryManager(new URL(HTTP + host + PATH_SEPARATOR
                + QUERY_SERVICE_NAME));
    }

    /**
     * Gets the query manager service at the given url string
     * 
     * @param serviceUrl
     *            The url string
     * @return The query manager service at the given url string
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static QueryManager getQueryServiceForUrl(final String url)
            throws MalformedURLException {
        return getQueryManager(new URL(url));
    }

    /**
     * Gets the query manager service at the given url
     * 
     * @param url
     *            The url
     * @return The query manager service at the give URL
     */
    private static QueryManager getQueryManager(URL url) {
        QueryManager queryManager = queryServiceMap.get(url);
        if (queryManager == null) {
            queryManager = getPort(url, QueryManager.class);
            queryServiceMap.put(url, queryManager);
        }
        return queryManager;
    }

    /**
     * Gets the validator service for the given host
     * 
     * @param host
     *            The host
     * @return The validator service for the given host
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static Validator getValidatorServiceForHost(final String host)
            throws MalformedURLException {
        return getValidator(new URL(HTTP + host + PATH_SEPARATOR
                + VALIDATOR_SERVICE_NAME));
    }

    /**
     * Gets the validator service for the given url string
     * 
     * @param serviceUrl
     *            The url string
     * @return The validator service for the given url string
     * @throws MalformedURLException
     *             If errors occur creating the URL object
     */
    public static Validator getValidatorServiceForUrl(final String url)
            throws MalformedURLException {
        return getValidator(new URL(url));
    }

    /**
     * Gets the validator service for the given URL
     * 
     * @param url
     *            The URL
     * @return The validator service at the given URL
     */
    private static Validator getValidator(URL url) {
        Validator validator = validatorServiceMap.get(url);
        if (validator == null) {
            validator = getPort(url, Validator.class);
            validatorServiceMap.put(url, validator);
        }
        return validator;
    }

    /**
     * 
     * Sends a SubmitObjects request to the lifecycle manager service at the
     * given host
     * 
     * @param request
     *            The request
     * @param host
     *            The host to send the request to
     * @throws RegistryServiceException
     *             If errors occur during request submission
     */
    public static void sendSubmitObjectsRequest(SubmitObjectsRequest request,
            String host) throws RegistryServiceException {

        LifecycleManager lcm;
        try {
            lcm = getLifecycleManagerServiceForHost(host);
        } catch (MalformedURLException e) {
            throw new RegistryServiceException(
                    "Error getting lifecyclemanager for host at [" + host + "]",
                    e);
        }
        RegistryResponseType response;
        try {
            response = lcm.submitObjects(request);
        } catch (MsgRegistryException e) {
            throw new RegistryServiceException(
                    "Error executing submitObjects!", e);
        }
        String status = response.getStatus();
        if (status.equals(RegistryResponseStatus.SUCCESS)) {
            statusHandler.info("Submit Objects request ["
                    + response.getRequestId() + "] successful");
        } else if (status.equals(RegistryResponseStatus.PARTIAL_SUCCESS)) {
            statusHandler.warn("Submit Objects request ["
                    + response.getRequestId() + "] partially successful");
        } else if (status.equals(RegistryResponseStatus.FAILURE)) {
            statusHandler.error("Submit Objects request ["
                    + response.getRequestId() + "] failed!");
            StringBuilder exceptionMessage = new StringBuilder();
            for (RegistryExceptionType exc : response.getException()) {
                exceptionMessage.append(exc).append("\n");
            }
            throw new RegistryServiceException("Submit Objects Request ["
                    + response.getRequestId()
                    + "] failed with the following exceptions:"
                    + exceptionMessage.toString());
        }
    }

    /**
     * Creates a SubmitObjectsRequest with the given parameters
     */
    public static SubmitObjectsRequest createSubmitObjectRequest(String id,
            Mode mode, String comment, boolean checkReferences,
            RegistryObjectListType objectList, List<SlotType> slots) {
        SubmitObjectsRequest request = new SubmitObjectsRequest();
        request.setId(id);
        request.setMode(mode);
        request.setRegistryObjectList(objectList);
        request.setComment(comment);
        request.setCheckReferences(checkReferences);
        if (slots != null) {
            request.getSlot().addAll(slots);
        }
        return request;
    }

    /**
     * Creates a RemoveObjectsRequest with the given parameters
     */
    public static RemoveObjectsRequest createRemoveObjectsRequest(String id,
            String comment, boolean deleteChildren, String deletionScope,
            ObjectRefListType objectRefs, QueryType query,
            Collection<SlotType> slots) {
        RemoveObjectsRequest request = new RemoveObjectsRequest();
        request.setId(id);
        request.setComment(comment);
        request.setDeleteChildren(deleteChildren);
        request.setDeletionScope(deletionScope);
        request.setObjectRefList(objectRefs);
        if (query != null) {
            request.setQuery(query);
        }
        if (slots != null) {
            request.getSlot().addAll(slots);
        }
        return request;
    }

    /**
     * Creates a QueryRequest with the given parameters
     */
    public static QueryRequest createQueryRequest(String id, String comment,
            QueryType query, ResponseOptionType responseOption) {
        QueryRequest request = new QueryRequest();
        request.setId(id);
        request.setComment(comment);
        request.setQuery(query);
        request.setResponseOption(responseOption);
        return request;
    }

    /**
     * Creates a QueryType object with the given parameters
     */
    public static QueryType createQueryType(String queryDefinition,
            List<SlotType> slots) {
        QueryType query = new QueryType();
        query.setQueryDefinition(queryDefinition);
        if (slots != null) {
            query.getSlot().addAll(slots);
        }
        return query;
    }

    @SuppressWarnings("unchecked")
    private static <T extends Object> T getPort(URL serviceUrl,
            Class<?> serviceInterface) {
        W3CEndpointReferenceBuilder endpointBuilder = new W3CEndpointReferenceBuilder();
        endpointBuilder.wsdlDocumentLocation(serviceUrl.toString() + WSDL);
        endpointBuilder.address(serviceUrl.toString());
        W3CEndpointReference ref = endpointBuilder.build();
        return (T) ref.getPort(serviceInterface);

    }
}
