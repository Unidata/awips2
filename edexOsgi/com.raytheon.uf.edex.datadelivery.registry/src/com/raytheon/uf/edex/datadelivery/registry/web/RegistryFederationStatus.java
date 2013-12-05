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
package com.raytheon.uf.edex.datadelivery.registry.web;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.xml.bind.JAXBException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.ws.wsaddressing.W3CEndpointReference;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.datadelivery.registry.web.IRegistryFederationService;
import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.datadelivery.registry.federation.RegistryFederationManager;
import com.raytheon.uf.edex.datadelivery.registry.replication.NotificationHostConfiguration;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.SubscriptionDao;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistrySubscriptionManager;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * <pre>
 * Set of services used by the federation status web interface to modify federation associations
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/30/2013    1538       bphillip    Initial Creation
 * 11/20/2013   2534        bphillip    Added interface
 * 12/2/2013    1829        bphillip    Changed to use modified call to getRegistryObject
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
@Path(IRegistryFederationService.REGISTRY_FEDERATION_STATUS_PATH)
@Transactional
public class RegistryFederationStatus implements IRegistryFederationService {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryFederationStatus.class);

    /** Comparator to sort registry instances */
    private static final Comparator<RegistryObjectType> REGISTRY_COMPARATOR = new Comparator<RegistryObjectType>() {
        @Override
        public int compare(RegistryObjectType o1, RegistryObjectType o2) {
            return o1.getId().compareTo(o2.getId());
        }
    };

    /** Data Access object for registry objects */
    private RegistryDao registryDao;

    /** Data Access object for registry subscription objects */
    private SubscriptionDao subscriptionDao;

    /** The registry replication manager */
    private RegistryFederationManager federationManager;

    /** Data Delivery rest services client */
    private DataDeliveryRESTServices dataDeliveryRestClient;

    /** Registry soap services client */
    private RegistrySOAPServices registrySoapServices;

    /**
     * The address to the NCF registry loaded from the NCF_ADDRESS item in
     * setup.env
     */
    private String ncfAddress = System.getenv("NCF_ADDRESS");

    @GET
    @Path("isFederated")
    public String isFederated() {
        return System.getenv("EBXML_REGISTRY_FEDERATION_ENABLED");
    }

    @GET
    @Path("getMyRegistryInfo")
    public String getMyRegistryInfo() {
        StringBuilder builder = new StringBuilder();
        RegistryType myRegistry = registryDao
                .getRegistryByBaseURL(RegistryUtil.LOCAL_REGISTRY_ADDRESS);
        appendRegistryInfo(myRegistry, builder);
        return builder.toString();

    }

    @GET
    @Path("getFederationMembers")
    public String getFederationMembers() throws MsgRegistryException {
        StringBuilder builder = new StringBuilder();
        QueryType query = new QueryType(
                CanonicalQueryTypes.FIND_ASSOCIATED_OBJECTS);
        query.getSlot().add(
                new SlotType(QueryConstants.ASSOCIATION_TYPE,
                        new StringValueType(
                                AssociationTypes.HAS_FEDERATION_MEMBER)));
        query.getSlot().add(
                new SlotType(QueryConstants.SOURCE_OBJECT_ID,
                        new StringValueType(
                                RegistryFederationManager.FEDERATION_ID)));

        QueryRequest queryRequest = new QueryRequest("Get Federation Members",
                query, new ResponseOptionType(QueryReturnTypes.REGISTRY_OBJECT,
                        true));
        List<RegistryObjectType> federationMembers = registrySoapServices
                .getQueryServiceForHost(ncfAddress).executeQuery(queryRequest)
                .getRegistryObjects();
        Collections.sort(federationMembers, REGISTRY_COMPARATOR);
        for (RegistryObjectType obj : federationMembers) {
            if (obj instanceof RegistryType) {
                RegistryType registry = (RegistryType) obj;
                appendRegistryInfo(registry, builder);
            }
        }

        return builder.toString();
    }

    @GET
    @Path("getRegistriesSubscribedTo")
    public String getRegistriesSubscribedTo() {
        StringBuilder builder = new StringBuilder();
        if (this.federationManager.getServers() != null
                && !CollectionUtil.isNullOrEmpty(this.federationManager
                        .getServers().getRegistryReplicationServers())) {
            List<RegistryObjectType> registries = new ArrayList<RegistryObjectType>();
            for (NotificationHostConfiguration hostConfig : this.federationManager
                    .getServers().getRegistryReplicationServers()) {

                SlotType queryLanguageSlot = new SlotType(
                        QueryConstants.QUERY_LANGUAGE, new StringValueType(
                                QueryLanguages.HQL));
                SlotType queryExpressionSlot = new SlotType(
                        QueryConstants.QUERY_EXPRESSION, new StringValueType(
                                "FROM SubscriptionType sub where sub.id like 'Replication Subscription for%"
                                        + RegistryUtil.LOCAL_REGISTRY_ADDRESS
                                        + "%'"));
                QueryType query = new QueryType();
                query.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);
                query.getSlot().add(queryLanguageSlot);
                query.getSlot().add(queryExpressionSlot);

                QueryRequest request = new QueryRequest();
                request.setResponseOption(new ResponseOptionType(
                        QueryReturnTypes.REGISTRY_OBJECT, true));
                request.setId("Replication Subscription Verification Query");
                request.setQuery(query);
                try {
                    if (!registrySoapServices
                            .getQueryServiceForHost(
                                    hostConfig.getRegistryBaseURL())
                            .executeQuery(request).getRegistryObjects()
                            .isEmpty()) {
                        RegistryType registry = registryDao
                                .getRegistryByBaseURL(hostConfig
                                        .getRegistryBaseURL());
                        if (registry != null) {
                            registries.add(registry);
                        }
                    }
                } catch (Exception e) {
                    statusHandler.error("Error querying remote registry", e);
                }

            }
            Collections.sort(registries, REGISTRY_COMPARATOR);
            for (RegistryObjectType reg : registries) {
                appendRegistryInfo((RegistryType) reg, builder);
            }
        }
        return builder.toString();
    }

    @GET
    @Path("getRegistrySubscribing")
    public String getRegistrySubscribing() {
        StringBuilder builder = new StringBuilder();
        List<RegistryObjectType> registries = new ArrayList<RegistryObjectType>();
        for (SubscriptionType sub : subscriptionDao.getAll()) {
            DeliveryInfoType deliveryInfo = sub.getDeliveryInfo().get(0);
            W3CEndpointReference endpointReference = deliveryInfo.getNotifyTo();
            DOMResult dom = new DOMResult();
            endpointReference.writeTo(dom);
            Document doc = (Document) dom.getNode();
            NodeList nodes = doc.getElementsByTagNameNS(
                    EbxmlNamespaces.ADDRESSING_URI,
                    RegistrySubscriptionManager.ADDRESS_TAG);
            Node addressNode = nodes.item(0);
            String serviceAddress = addressNode.getTextContent().trim();
            String endpointType = addressNode
                    .getAttributes()
                    .getNamedItemNS(EbxmlNamespaces.RIM_URI,
                            RegistrySubscriptionManager.ENDPOINT_TAG)
                    .getNodeValue();
            if (endpointType.equals(DeliveryMethodTypes.SOAP)) {
                RegistryType registry = registryDao
                        .getRegistryByBaseURL(serviceAddress.replace(
                                "/notificationListener", ""));
                if (registry != null && !registries.contains(registry)) {
                    registries.add(registry);
                }

            }
        }
        Collections.sort(registries, REGISTRY_COMPARATOR);
        for (RegistryObjectType reg : registries) {
            appendRegistryInfo((RegistryType) reg, builder);
        }
        return builder.toString();
    }

    @GET
    @Path("getObjectTypesReplicated")
    public String getObjectTypesReplicated() {
        StringBuilder builder = new StringBuilder();
        for (String objectType : RegistryFederationManager.getObjectTypes()) {
            builder.append(objectType).append(StringUtil.NEWLINE);
        }
        return builder.toString();
    }

    @GET
    @Path("syncWithRegistry/{registryId}")
    public String syncWithRegistry(@PathParam("registryId") String registryId) {
        StringBuilder builder = new StringBuilder();
        RegistryType registry = registryDao.getById(registryId);
        if (registry == null) {
            builder.append("Registry [" + registryId
                    + "] not in federation. Unable to synchronize.");
        } else {
            try {
                federationManager.synchronizeRegistryWithFederation(registry
                        .getBaseURL());
            } catch (Exception e) {
                statusHandler.error("Error synchronizing registry!", e);
                builder.append("Error synchronizing registry [" + registryId
                        + "]: " + e.getLocalizedMessage());
            }
        }
        return builder.toString();
    }

    @GET
    @Path("subscribeToRegistry/{registryId}")
    public String subscribeToRegistry(@PathParam("registryId") String registryId)
            throws RegistryServiceException, JAXBException {
        StringBuilder builder = new StringBuilder();
        RegistryType registry = dataDeliveryRestClient.getRegistryObject(
                ncfAddress, registryId);
        if (registry == null) {
            builder.append("Registry [")
                    .append(registryId)
                    .append("] not in federation. Unable to submit subscriptions.");
        } else {
            if (!federationManager.isSubscribedTo(registry)) {
                federationManager.submitSubscriptionsToRegistry(registry);
                builder.append("Successfully subscribed to registry [")
                        .append(registryId).append("]");
                this.federationManager.addNotificationServer(registry);
                federationManager.saveNotificationServers();
            }
        }
        return builder.toString();
    }

    @GET
    @Path("unsubscribeFromRegistry/{registryId}")
    public String unsubscribeFromRegistry(
            @PathParam("registryId") String registryId) {
        StringBuilder builder = new StringBuilder();
        RegistryType registry = registryDao.getById(registryId);
        if (registry == null) {
            builder.append("Registry [" + registryId
                    + "] not in federation. Unable to unsubscribe.");
        } else {
            RegistryType localRegistry = registryDao
                    .getRegistryByBaseURL(RegistryUtil.LOCAL_REGISTRY_ADDRESS);
            dataDeliveryRestClient.getRegistryDataAccessService(
                    registry.getBaseURL()).removeSubscriptionsForSite(
                    localRegistry.getOwner());
            builder.append("Successfully unsubscribed from registry ["
                    + registryId + "]");
            federationManager.removeNotificationServer(registry.getBaseURL());
            federationManager.saveNotificationServers();
        }
        return builder.toString();
    }

    /**
     * Appends registry information to the stringbuilder
     * 
     * @param registry
     *            The registry to get information for
     * @param builder
     *            The string builder to append to
     */
    private void appendRegistryInfo(RegistryType registry, StringBuilder builder) {
        builder.append(registry.getId()).append(",");
        builder.append(registry.getBaseURL()).append(",");
        builder.append(registry.getConformanceProfile()).append(",");
        builder.append(registry.getSpecificationVersion());
        builder.append(StringUtil.NEWLINE);
    }

    public void setFederationManager(RegistryFederationManager federationManager) {
        this.federationManager = federationManager;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

    public void setSubscriptionDao(SubscriptionDao subscriptionDao) {
        this.subscriptionDao = subscriptionDao;
    }

    public void setDataDeliveryRestClient(
            DataDeliveryRESTServices dataDeliveryRestClient) {
        this.dataDeliveryRestClient = dataDeliveryRestClient;
    }

    public void setRegistrySoapServices(
            RegistrySOAPServices registrySoapServices) {
        this.registrySoapServices = registrySoapServices;
    }
}
