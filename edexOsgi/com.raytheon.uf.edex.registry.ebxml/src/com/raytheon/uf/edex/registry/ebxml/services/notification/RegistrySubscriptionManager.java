package com.raytheon.uf.edex.registry.ebxml.services.notification;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.AuditableEventTypeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

@Transactional(propagation = Propagation.REQUIRED)
public class RegistrySubscriptionManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistrySubscriptionManager.class);

    private RegistryNotificationManager notificationManager;

    private QueryManagerImpl queryManager;

    private RegistryObjectTypeDao subscriptionDao;

    private AuditableEventTypeDao auditableEventDao;

    public RegistrySubscriptionManager() {

    }

    public void processSubscriptions() throws EbxmlRegistryException,
            MsgRegistryException {
        // statusHandler
        // .info("---------------------PROCESSING SUBSCRIPTIONS----------------------------");

        // List<SubscriptionType> subs = subscriptionDao
        // .getAllObjectsOfType(SubscriptionType.class);
        // for (SubscriptionType sub : subs) {
        // processSubscription(sub);
        // }
    }

    private void processSubscription(SubscriptionType sub)
            throws MsgRegistryException, EbxmlRegistryException {
        // Get objects that match selector query
        QueryType selectorQuery = sub.getSelector();
        ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                .createResponseOptionType();
        responseOption.setReturnType(RegistryObjectType.class.getSimpleName());
        QueryResponse queryResponse = queryManager.executeQuery(responseOption,
                selectorQuery);
        List<RegistryObjectType> objectsOfInterest = queryResponse
                .getRegistryObjectList().getRegistryObject();

        if (!objectsOfInterest.isEmpty()) {
            // Now get AuditableEvents that affected objectsOfInterest
            // MUST not include AuditableEvents that have already been delivered
            // to this subscriber
            List<AuditableEventType> eventsOfInterest = auditableEventDao
                    .getEventsOfInterest(objectsOfInterest);
            if (!eventsOfInterest.isEmpty()) {
                NotificationType notification = notificationManager
                        .createNotification(sub, objectsOfInterest,
                                eventsOfInterest);
                List<String[]> addresses = null;
                try {
                    addresses = getServiceAddressFromDeliveryInfo(sub);
                } catch (Exception e) {
                    throw new EbxmlRegistryException(
                            "Error extracting service addresses from subscription delivery info!",
                            e);
                }
                if (addresses.isEmpty()) {
                    statusHandler
                            .warn("No destinations found for notification!");
                } else {
                    for (String[] deliveryInfo : addresses) {
                        if (deliveryInfo[0]
                                .equals("urn:oasis:names:tc:ebxml-regrep:endPointType:soap")) {
                            notificationManager.sendNotificationViaSoap(
                                    notification, deliveryInfo[1]);
                        } else {
                            statusHandler.warn("Unsupported delivery type: "
                                    + deliveryInfo[1]
                                    + ". Notification will not be delivered!");
                        }
                    }
                }
            }
        }
    }

    public List<String[]> getServiceAddressFromDeliveryInfo(SubscriptionType sub)
            throws Exception {
        List<String[]> addresses = new ArrayList<String[]>();
        DocumentBuilderFactory docFactory = DocumentBuilderFactory
                .newInstance();
        docFactory.setNamespaceAware(true);
        DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
        Document doc = docBuilder.parse(new InputSource(new StringReader(
                SerializationUtil.getJaxbManager().marshalToXml(sub))));

        NodeList nodes = doc.getElementsByTagNameNS(
                "http://www.w3.org/2005/08/addressing", "Address");
        System.out.println(nodes.getLength());
        for (int i = 0; i < nodes.getLength(); i++) {
            Node addressNode = nodes.item(i);
            String endpointType = addressNode
                    .getAttributes()
                    .getNamedItemNS(
                            "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0",
                            "endpointType").getNodeValue();
            String serviceAddress = addressNode.getNodeValue();
            addresses.add(new String[] { endpointType, serviceAddress });
        }
        return addresses;
    }

    public void setQueryManager(QueryManagerImpl queryManager) {
        this.queryManager = queryManager;
    }

    public void setSubscriptionDao(RegistryObjectTypeDao subscriptionDao) {
        this.subscriptionDao = subscriptionDao;
    }

    public void setAuditableEventDao(AuditableEventTypeDao auditableEventDao) {
        this.auditableEventDao = auditableEventDao;
    }

    public void setNotificationManager(
            RegistryNotificationManager notificationManager) {
        this.notificationManager = notificationManager;
    }
}
