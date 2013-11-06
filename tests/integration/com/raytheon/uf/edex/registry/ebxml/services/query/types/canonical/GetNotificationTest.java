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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import static org.junit.Assert.assertEquals;

import java.util.List;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.ws.wsaddressing.W3CEndpointReference;
import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeliveryMethodTypes;
import com.raytheon.uf.common.registry.constants.NotificationOptionTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.GetNotification;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Test for the Canonical GetNotification query defined by the EBXML 4.0 spec
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GetNotificationTest extends QueryTest {

    private static final String SUBSCRIPTION_ID = "Test Subscription";

    @Autowired
    private GetNotification getNotification;

    @Autowired
    private RegistrySOAPServices registrySoapClient;

    @Before
    public void createSubscription() throws Exception {
        // Set normal registry object fields
        SubscriptionType sub = new SubscriptionType();
        sub.setId(SUBSCRIPTION_ID);
        sub.setLid(SUBSCRIPTION_ID);
        sub.setObjectType(RegistryObjectTypes.SUBSCRIPTION);
        sub.setName(RegistryUtil.getInternationalString(SUBSCRIPTION_ID));
        sub.setDescription(RegistryUtil.getInternationalString(SUBSCRIPTION_ID));
        VersionInfoType version = new VersionInfoType();
        version.setVersionName("1");
        version.setUserVersionName("1");
        sub.setVersionInfo(version);
        sub.setOwner("TEST OWNER");
        sub.setStatus(StatusTypes.APPROVED);

        sub.setStartTime(EbxmlObjectUtil.getTimeAsXMLGregorianCalendar(0));

        QueryType selectorQuery = createQuery(
                CanonicalQueryTypes.GET_OBJECT_BY_ID, QueryConstants.ID,
                "urn:oasis:names:tc:ebxml-regrep:classificationScheme:ActionType")
                .getQuery();
        sub.setSelector(selectorQuery);

        Duration notificationInterval = DatatypeFactory.newInstance()
                .newDuration(0);
        sub.setNotificationInterval(notificationInterval);

        String endpointType = DeliveryMethodTypes.SOAP;
        W3CEndpointReferenceBuilder builder = new W3CEndpointReferenceBuilder();
        builder.address(registrySoapClient.getNotificationListenerServiceUrl(
                "http://someaddress.com").toString());
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
            endpointTypeAttr.setValue(endpointType);
            addressNode.getAttributes().setNamedItemNS(endpointTypeAttr);
        }
        ref = new W3CEndpointReference(new DOMSource(dom.getNode()));

        // Set subscription specific fields
        DeliveryInfoType deliveryInfo = new DeliveryInfoType();
        deliveryInfo.setNotificationOption(NotificationOptionTypes.OBJECT_REFS);
        deliveryInfo.setNotifyTo(ref);
        sub.getDeliveryInfo().add(deliveryInfo);

        submitRegistryObjectToRegistry(sub);
    }

    @Test
    public void testGetNotification() throws MsgRegistryException {

        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_NOTIFICATION,
                QueryConstants.SUBSCRIPTION_ID, SUBSCRIPTION_ID);
        List<RegistryObjectType> result = executeQuery(getNotification, request);
        assertEquals(1, result.size());

    }
}
