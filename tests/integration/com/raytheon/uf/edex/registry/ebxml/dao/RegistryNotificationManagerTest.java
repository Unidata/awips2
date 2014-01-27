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
package com.raytheon.uf.edex.registry.ebxml.dao;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;

import java.util.Arrays;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.ws.wsaddressing.W3CEndpointReference;
import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.event.EventBusTest;
import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.NotificationOptionTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.event.NonTransactionalSynchronousEventBusHandler;
import com.raytheon.uf.edex.registry.ebxml.services.notification.MockNotificationListenerFactory;
import com.raytheon.uf.edex.registry.ebxml.services.notification.RegistryNotificationManager;

/**
 * Test {@link RegistryNotificationManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013 1672       djohnson    Initial creation
 * Jun 24, 2013 2106       djohnson    Set explicitly non-transactional event bus handler.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@ContextConfiguration(inheritLocations = true, locations = {
        SpringFiles.UNIT_TEST_EBXML_PLUGIN_NOTIFICATION_LISTENER_XML,
        SpringFiles.UNIT_TEST_EBXML_REPLICATION_BEANS_XML })
public class RegistryNotificationManagerTest extends AbstractRegistryTest {

    private static final String PLUGIN_SUBSCRIBED_LISTENER_ADDRESS = "pluginSubscribedListener";

    private static final String WEB_SERVICE_ADDRESS = "http://myWebServiceAddress";

    @Autowired
    private MockNotificationListenerFactory notificationListenerFactory;

    @Autowired
    @Qualifier("pluginSubscribedListener")
    private PluginSubscribedListener pluginSubscribedListener;

    @Autowired
    @Qualifier("pluginSubscribedListener2")
    private PluginSubscribedListener pluginSubscribedListener2;

    @BeforeClass
    public static void classSetUp() {
        EventBusTest
                .useExplicitEventBusHandler(new NonTransactionalSynchronousEventBusHandler());
    }

    @Test
    public void webServiceDestinationIsNotifiedOnSubscribedObjectInsert()
            throws MsgRegistryException {

        final SubscriptionType subscription = createSubscriptionForNotifications(
                WEB_SERVICE_ADDRESS, DeliveryMethodTypes.SOAP);
        final SubmitObjectsRequest submitObjects = createSubmitObjectsRequestForSubscription(subscription);

        lifecycleManager.submitObjects(submitObjects);

        final SubmitObjectsRequest objectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);
        lifecycleManager.submitObjects(objectsRequest);

        final NotificationListener notificationListenerForWebService = notificationListenerFactory
                .getMockForDestination(WEB_SERVICE_ADDRESS);
        verify(notificationListenerForWebService).onNotification(
                any(NotificationType.class));
    }

    @Test
    public void webServiceDestinationIsNotNotifiedOnNonSubscribedObjectInsert()
            throws MsgRegistryException {

        final SubscriptionType subscription = createSubscriptionForNotifications(
                WEB_SERVICE_ADDRESS, DeliveryMethodTypes.SOAP);
        final SubmitObjectsRequest submitObjects = createSubmitObjectsRequestForSubscription(subscription);

        lifecycleManager.submitObjects(submitObjects);

        final SubmitObjectsRequest objectsRequest = createSubmitObjectsRequest(
                "thisIsADifferentRegistryObjectId", "SomeRandomObjectType",
                Mode.CREATE_OR_REPLACE);
        lifecycleManager.submitObjects(objectsRequest);

        final NotificationListener notificationListenerForWebService = notificationListenerFactory
                .getMockForDestination(WEB_SERVICE_ADDRESS);
        verifyZeroInteractions(notificationListenerForWebService);
    }

    @Test
    public void pluginDestinationIsNotifiedOnSubscribedObjectInsert()
            throws MsgRegistryException {
        final SubscriptionType subscription = createSubscriptionForNotifications(
                PLUGIN_SUBSCRIBED_LISTENER_ADDRESS, DeliveryMethodTypes.PLUGIN);
        final SubmitObjectsRequest submitObjects = createSubmitObjectsRequestForSubscription(subscription);

        lifecycleManager.submitObjects(submitObjects);

        final SubmitObjectsRequest objectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);
        lifecycleManager.submitObjects(objectsRequest);

        assertThat(pluginSubscribedListener.hasBeenNotified(), is(true));
    }

    @Ignore
    @Test
    public void pluginDestinationIsNotNotifiedOnNonSubscribedObjectInsert()
            throws MsgRegistryException {

        final SubscriptionType subscription = createSubscriptionForNotifications(
                PLUGIN_SUBSCRIBED_LISTENER_ADDRESS, DeliveryMethodTypes.PLUGIN);
        final SubmitObjectsRequest submitObjects = createSubmitObjectsRequestForSubscription(subscription);

        lifecycleManager.submitObjects(submitObjects);

        final SubmitObjectsRequest objectsRequest = createSubmitObjectsRequest(
                "thisIsADifferentRegistryObjectId", "SomeRandomObjectType",
                Mode.CREATE_OR_REPLACE);
        lifecycleManager.submitObjects(objectsRequest);

        assertThat(pluginSubscribedListener2.hasBeenNotified(), is(false));
    }

    private SubmitObjectsRequest createSubmitObjectsRequestForSubscription(
            SubscriptionType subscription) {
        final SubmitObjectsRequest submitObjects = createSubmitObjectsRequest(
                "theSubmitObjectsRequestId", SubscriptionType.class.getName(),
                Mode.CREATE_ONLY);
        submitObjects.getRegistryObjectList().getRegistryObject().clear();
        submitObjects.getRegistryObjectList().getRegistryObject()
                .add(subscription);
        return submitObjects;
    }

    private SubscriptionType createSubscriptionForNotifications(
            final String webServiceAddress, final String deliveryMethod) {
        QueryType selectorQuery = new QueryType();
        selectorQuery.setQueryDefinition(CanonicalQueryTypes.BASIC_QUERY);
        SlotType slot = new SlotType();
        StringValueType valType = new StringValueType();
        valType.setStringValue(REGISTRY_OBJECT_TYPE);
        slot.setName("objectType");
        slot.setSlotValue(valType);
        selectorQuery.getSlot().add(slot);

        W3CEndpointReferenceBuilder builder = new W3CEndpointReferenceBuilder();
        builder.address(webServiceAddress);
        W3CEndpointReference ref = builder.build();
        DOMResult dom = new DOMResult();
        ref.writeTo(dom);
        Document doc = (Document) dom.getNode();
        NodeList nodes = doc.getElementsByTagNameNS(
                EbxmlNamespaces.ADDRESSING_URI, "Address");
        for (int i = 0; i < nodes.getLength(); i++) {
            Node addressNode = nodes.item(i);
            Attr endpointTypeAttr = doc.createAttributeNS(
                    EbxmlNamespaces.RIM_URI, "endpointType");
            endpointTypeAttr.setValue(deliveryMethod);
            addressNode.getAttributes().setNamedItemNS(endpointTypeAttr);
        }
        ref = new W3CEndpointReference(new DOMSource(dom.getNode()));

        // Set subscription specific fields
        DeliveryInfoType deliveryInfo = new DeliveryInfoType();
        deliveryInfo.setNotificationOption(NotificationOptionTypes.OBJECT_REFS);
        deliveryInfo.setNotifyTo(ref);

        SubscriptionType subscription = new SubscriptionType();
        try {
            subscription.setStartTime(DatatypeFactory.newInstance()
                    .newXMLGregorianCalendar());
        } catch (DatatypeConfigurationException e) {
            throw new RuntimeException(e);
        }
        subscription.setId("someSubscriptionId");
        subscription.setLid("someSubscriptionId");
        subscription.setName(RegistryUtil
                .getInternationalString("subscriptionName"));
        subscription.setObjectType(RegistryObjectTypes.SUBSCRIPTION);
        subscription.setSelector(selectorQuery);
        subscription.setDeliveryInfo(Arrays.asList(deliveryInfo));
        return subscription;
    }

}
