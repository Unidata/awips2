package com.raytheon.uf.edex.registry.ebxml.services.notification;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.dom.DOMSource;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.RequestEntity;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.commons.lang.StringEscapeUtils;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryResponseStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

@Transactional
public class RegistryNotificationManager {

    private DefaultHttpMethodRetryHandler httpMethodRetryHandler;

    private JAXBContext jaxbResponseContext;

    private Unmarshaller jaxbResponseUnmarshaller;

    public RegistryNotificationManager() throws JAXBException {
        httpMethodRetryHandler = new DefaultHttpMethodRetryHandler(3, false);
        jaxbResponseContext = JAXBContext
                .newInstance(RegistryResponseType.class);
        jaxbResponseUnmarshaller = jaxbResponseContext.createUnmarshaller();
    }

    public NotificationType createNotification(SubscriptionType subscription,
            List<RegistryObjectType> objectsOfInterest,
            List<AuditableEventType> eventsOfInterest) {

        List<RegistryObjectType> objectsToRemove = new ArrayList<RegistryObjectType>();
        for (AuditableEventType event : eventsOfInterest) {
            List<ActionType> actionList = event.getAction();
            for (ActionType action : actionList) {
                objectsToRemove.clear();
                List<RegistryObjectType> regObjs = action.getAffectedObjects()
                        .getRegistryObject();
                for (RegistryObjectType obj : regObjs) {
                    boolean found = false;
                    for (RegistryObjectType objOfInterest : objectsOfInterest) {
                        if (objOfInterest.getId().equals(obj.getId())) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        objectsToRemove.add(obj);
                    }
                }
                regObjs.removeAll(objectsToRemove);
            }
        }

        // RegistryObjectType specific Fields
        NotificationType notification = new NotificationType();
        notification.setId(EbxmlObjectUtil.getUUID());
        notification.setLid(notification.getId());
        notification.setName(RegistryUtil
                .getInternationalString("Notification name"));
        notification.setDescription(RegistryUtil
                .getInternationalString("Notification Description"));
        notification.setObjectType(RegistryObjectTypes.NOTIFICATION);
        notification.setStatus(StatusTypes.APPROVED);
        notification.setOwner("EDEX_Internal_User");

        // NotificationType specific fields
        notification.setSubscription(subscription.getId());
        notification.setEvent(eventsOfInterest);
        return notification;

    }

    public void sendNotificationViaSoap(NotificationType notification,
            String serviceAddress) throws EbxmlRegistryException {
        String notificationXml = null;
        SOAPMessage msg = null;

        /*
         * Generate a new SOAP Message
         */
        try {
            MessageFactory factory = MessageFactory
                    .newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
            msg = factory.createMessage();
        } catch (SOAPException e) {
            throw new EbxmlRegistryException(
                    "Error creating notification SOAP message", e);
        }

        /*
         * Marshal the notification to XML string
         */
        try {
            notificationXml = SerializationUtil.marshalToXml(notification);
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error marshalling notification",
                    e);
        }

        /*
         * Put the notification XML string into the soap message
         */
        try {
            msg.getSOAPBody().setTextContent(notificationXml);
        } catch (SOAPException e) {
            throw new EbxmlRegistryException(
                    "Error setting text content of SOAP Message", e);
        }

        /*
         * Send the SOAP request
         */
        sendSoapMessage(msg, serviceAddress);
    }

    private void sendSoapMessage(SOAPMessage message, String serviceAddress)
            throws EbxmlRegistryException {
        System.out.println("Sending SOAP Message to [" + serviceAddress
                + "]...");

        HostConfiguration hostConfig = new HostConfiguration();
        HttpClient client = new HttpClient();
        client.setHostConfiguration(hostConfig);
        PostMethod postMethod = null;

        /*
         * Format soap message for transmission
         */
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        RequestEntity reqEntity = null;
        try {
            message.writeTo(baos);
            reqEntity = new StringRequestEntity(
                    StringEscapeUtils.unescapeXml(baos.toString()), "text/xml",
                    "utf-8");
        } catch (SOAPException e) {
            throw new EbxmlRegistryException(
                    "Error externalizing Soap message!", e);
        } catch (IOException e) {
            throw new EbxmlRegistryException(
                    "Error writing soap message to byte array output stream!",
                    e);
        } finally {
            if (baos != null) {
                try {
                    baos.close();
                } catch (IOException e) {
                    throw new EbxmlRegistryException(
                            "Error closing ByteArrayOutputStream!", e);
                }
            }
        }

        /*
         * Send the soap message via HTTP to desired service address
         */
        try {
            postMethod = new PostMethod(serviceAddress);
            postMethod.setRequestEntity(reqEntity);
            postMethod.getParams().setParameter(HttpMethodParams.RETRY_HANDLER,
                    httpMethodRetryHandler);

            /*
             * Post the message to the service address and check the response
             */
            int httpStatus = client.executeMethod(postMethod);
            if (httpStatus == HttpStatus.SC_OK) {
                System.out.println("Soap Message transmission to ["
                        + serviceAddress + "] successful!");
            } else {
                throw new EbxmlRegistryException(
                        "Soap message transmission to [" + serviceAddress
                                + "] failed with HTTP Status [" + httpStatus
                                + "]");
            }

            /*
             * Retrieve the server response and validate
             */
            byte[] responseBytes = null;
            try {
                responseBytes = postMethod.getResponseBody();
            } catch (Exception e) {
                throw new EbxmlRegistryException("Error reading response from "
                        + serviceAddress, e);
            }

            ByteArrayInputStream responseInputStream = new ByteArrayInputStream(
                    responseBytes);
            try {
                DocumentBuilderFactory dbf = DocumentBuilderFactory
                        .newInstance();
                dbf.setNamespaceAware(true);
                DocumentBuilder db = dbf.newDocumentBuilder();
                Document d = db.parse(responseInputStream);
                NodeList responseNodeList = d.getElementsByTagNameNS(
                        "urn:oasis:names:tc:ebxml-regrep:xsd:rs:4.0",
                        "RegistryResponse");
                if (responseNodeList.getLength() == 0) {
                    throw new EbxmlRegistryException(
                            "Invalid response returned from server!");
                }

                RegistryResponseType registryResponse = jaxbResponseUnmarshaller
                        .unmarshal(new DOMSource(responseNodeList.item(0)),
                                RegistryResponseType.class).getValue();
                if (registryResponse.getStatus().equals(
                        RegistryResponseStatus.SUCCESS)) {
                    System.out.println("Soap service at [" + serviceAddress
                            + "] returned success response");
                } else {
                    throw new EbxmlRegistryException("Soap service at ["
                            + serviceAddress + "] returned response: "
                            + registryResponse.getStatus());
                }
            } finally {
                if (responseInputStream != null) {
                    responseInputStream.close();
                }
            }
        } catch (Exception e) {
            throw new EbxmlRegistryException(
                    "Error detected sending soap message to " + serviceAddress,
                    e);
        } finally {
            postMethod.releaseConnection();
        }

    }
}
