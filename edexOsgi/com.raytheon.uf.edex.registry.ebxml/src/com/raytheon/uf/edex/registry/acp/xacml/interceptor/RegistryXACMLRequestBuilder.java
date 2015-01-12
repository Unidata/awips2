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
package com.raytheon.uf.edex.registry.acp.xacml.interceptor;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryRequestType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.CatalogObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.FilterObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;

import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.interceptor.security.SAMLSecurityContext;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageContentsList;
import org.apache.cxf.rt.security.xacml.RequestComponentBuilder;
import org.apache.cxf.rt.security.xacml.XACMLConstants;
import org.apache.cxf.security.SecurityContext;
import org.apache.ws.security.WSSecurityException;
import org.apache.ws.security.saml.ext.AssertionWrapper;
import org.joda.time.DateTime;
import org.opensaml.xacml.ctx.ActionType;
import org.opensaml.xacml.ctx.AttributeType;
import org.opensaml.xacml.ctx.AttributeValueType;
import org.opensaml.xacml.ctx.EnvironmentType;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResourceType;
import org.opensaml.xacml.ctx.SubjectType;
import org.w3c.dom.Element;

/**
 * 
 * This class generates XACML authorization requests from SOAP and REST requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/11/201r    1712          bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryXACMLRequestBuilder {

    /**
     * Creates and empty XACMLRequestBuilder
     */
    public RegistryXACMLRequestBuilder() {
    }

    /**
     * Creates a list of requests for the given resources. A request per
     * resource is created due to the fact that individual registry objects may
     * have specific access control policies assigned to them.
     * 
     * @param isSoapCall
     *            True if this is a SOAP request
     * @param principal
     *            The principal on the request
     * @param roles
     *            The role of the requesting user
     * @param message
     *            The actual message
     * @param resources
     *            The resources involved with the request
     * @return A list of
     * @throws Exception
     */
    public List<RequestType> createRequestList(boolean isSoapCall,
            Principal principal, List<String> roles, Message message,
            List<RegistryObjectType> resources) throws Exception {

        // Gets the issuer of the message if one is specified
        String issuer = getIssuer(message);
        // Gets the action being executed
        String actionToUse = getAction(message, isSoapCall);
        List<RequestType> requests = new ArrayList<RequestType>(
                resources.size());
        
        /*
         * Create a request per resource
         */
        for (RegistryObjectType registryObject : resources) {
            requests.add(createRequest(principal, roles, issuer, actionToUse,
                    registryObject, isSoapCall));
        }
        return requests;

    }

    /**
     * Creates an XACML request for the given resource
     * @param principal The principal on the message
     * @param roles The role(s) of the user making the request
     * @param issuer The issuer of the message
     * @param actionToUse The action being executed by the request
     * @param registryObject The registry object involved with the request
     * @param isSoapCall True if this is a soap call
     * @return An XACML request using the given resource
     */
    private RequestType createRequest(Principal principal, List<String> roles,
            String issuer, String actionToUse,
            RegistryObjectType registryObject, boolean isSoapCall) {

        // Build the Subject
        List<AttributeType> attributes = new ArrayList<AttributeType>();
        AttributeValueType subjectIdAttributeValue = RequestComponentBuilder
                .createAttributeValueType(principal.getName());
        AttributeType subjectIdAttribute = RequestComponentBuilder
                .createAttributeType(XACMLConstants.SUBJECT_ID,
                        XACMLConstants.XS_STRING, issuer,
                        Collections.singletonList(subjectIdAttributeValue));
        attributes.add(subjectIdAttribute);

        if (roles != null) {
            List<AttributeValueType> roleAttributes = new ArrayList<AttributeValueType>(roles.size());
            for (String role : roles) {
                if (role != null) {
                    AttributeValueType subjectRoleAttributeValue = RequestComponentBuilder
                            .createAttributeValueType(role);
                    roleAttributes.add(subjectRoleAttributeValue);
                }
            }

            if (!roleAttributes.isEmpty()) {
                AttributeType subjectRoleAttribute = RequestComponentBuilder
                        .createAttributeType(XACMLConstants.SUBJECT_ROLE,
                                XACMLConstants.XS_ANY_URI, issuer,
                                roleAttributes);
                attributes.add(subjectRoleAttribute);
            }
        }
        SubjectType subjectType = RequestComponentBuilder.createSubjectType(
                attributes, null);

        attributes.clear();

        // Build the Resource
        attributes.add(RequestComponentBuilder.createAttributeType(
                XACMLConstants.RESOURCE_ID, XACMLConstants.XS_STRING, null,
                Collections.singletonList(RequestComponentBuilder
                        .createAttributeValueType(registryObject.getId()))));
        ResourceType resourceType = RequestComponentBuilder.createResourceType(
                attributes, null);

        // Build the Action
        AttributeValueType actionAttributeValue = RequestComponentBuilder
                .createAttributeValueType(actionToUse);
        AttributeType actionAttribute = RequestComponentBuilder
                .createAttributeType(XACMLConstants.ACTION_ID,
                        XACMLConstants.XS_STRING, null,
                        Collections.singletonList(actionAttributeValue));
        attributes.clear();
        attributes.add(actionAttribute);
        ActionType actionType = RequestComponentBuilder
                .createActionType(attributes);

        // Environment
        attributes.clear();
        DateTime dateTime = new DateTime();
        AttributeValueType environmentAttributeValue = RequestComponentBuilder
                .createAttributeValueType(dateTime.toString());
        AttributeType environmentAttribute = RequestComponentBuilder
                .createAttributeType(XACMLConstants.CURRENT_DATETIME,
                        XACMLConstants.XS_DATETIME, null,
                        Collections.singletonList(environmentAttributeValue));
        attributes.add(environmentAttribute);

        EnvironmentType environmentType = RequestComponentBuilder
                .createEnvironmentType(attributes);

        // Build the Request
        RequestType request = RequestComponentBuilder.createRequestType(
                Collections.singletonList(subjectType),
                Collections.singletonList(resourceType), actionType,
                environmentType);

        return request;

    }

    /**
     * Gets the registry request object from the soap message
     * @param message The message to get the registry request object from
     * @return The registry request message extracted from the received xml message
     */
    private RegistryRequestType getRequestFromSoapMessage(Message message) {
        if (message instanceof SoapMessage) {
            MessageContentsList content = (MessageContentsList) ((SoapMessage) message)
                    .getContent(List.class);
            if (!content.isEmpty()) {
                Object obj = content.get(0);
                if (obj instanceof RegistryRequestType) {
                    return (RegistryRequestType) obj;
                }
            }
        }
        return null;
    }

    /**
     * Gets the issuer from the message
     * @param message The message to get the issuer from 
     * @return The issuer of the message
     * @throws WSSecurityException If errors occur while extracting the issuer
     */
    private String getIssuer(Message message) throws WSSecurityException {
        SecurityContext sc = message.get(SecurityContext.class);

        if (sc instanceof SAMLSecurityContext) {
            Element assertionElement = ((SAMLSecurityContext) sc)
                    .getAssertionElement();
            if (assertionElement != null) {
                AssertionWrapper wrapper = new AssertionWrapper(
                        assertionElement);
                return wrapper.getIssuerString();
            }
        }
        return null;
    }

    /**
     * Gets the action from the message
     * @param message The message to get the action from 
     * @param isSoapCall True if this is a soap call
     * @return The action contained in the message
     */
    private String getAction(Message message, boolean isSoapCall) {
        String actionToUse = null;
        RegistryRequestType request = getRequestFromSoapMessage(message);

        if (isSoapCall) {
            if (request instanceof CatalogObjectsRequest) {
                actionToUse = "catalog";
            } else if (request instanceof FilterObjectsRequest) {
                actionToUse = "filter";
            } else if (request instanceof QueryRequest) {
                actionToUse = "read";
            } else if (request instanceof RemoveObjectsRequest) {
                actionToUse = "delete";
            } else if (request instanceof SubmitObjectsRequest) {
                actionToUse = "create";
            } else if (request instanceof UpdateObjectsRequest) {
                actionToUse = "update";
            } else if (request instanceof ValidateObjectsRequest) {
                actionToUse = "validate";
            } else {
                throw new IllegalArgumentException("Unsupported request type: "
                        + request.getClass());
            }
        } else {
            // For REST use the HTTP Verb
            if (message.get(Message.WSDL_OPERATION) == null
                    && message.get(Message.HTTP_REQUEST_METHOD) != null) {
                actionToUse = (String) message.get(Message.HTTP_REQUEST_METHOD);
            }
        }
        return actionToUse;
    }
}
