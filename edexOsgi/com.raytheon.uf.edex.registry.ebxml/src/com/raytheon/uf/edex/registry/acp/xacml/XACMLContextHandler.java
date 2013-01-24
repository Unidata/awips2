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
package com.raytheon.uf.edex.registry.acp.xacml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.commons.beanutils.PropertyUtils;
import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.ActionType;
import org.opensaml.xacml.ctx.AttributeType;
import org.opensaml.xacml.ctx.AttributeValueType;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResourceType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.ctx.SubjectType;
import org.opensaml.xacml.ctx.impl.ActionTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.AttributeTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.AttributeValueTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.RequestTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.ResourceTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.SubjectTypeImplBuilder;
import org.opensaml.xacml.policy.ObligationType;

import com.raytheon.uf.common.registry.IRegistryRequest;
import com.raytheon.uf.common.registry.IRegistryRequest.Action;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.acp.xacml.conformance.DataTypes;
import com.raytheon.uf.edex.registry.acp.xacml.conformance.Identifiers;
import com.raytheon.uf.edex.registry.acp.xacml.engine.obligation.XACMLObligationEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLNotApplicableException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryResponseStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Context handler - The system entity that converts decision requests in the
 * native request format to the XACML canonical form and converts authorization
 * decisions in the XACML canonical form to the native response format
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * Oct 01, 2012 1187         djohnson    Commented out code throwing {@link ClassCastException}s.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class XACMLContextHandler {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XACMLContextHandler.class);

    /** The singleton instance */
    private static XACMLContextHandler instance = new XACMLContextHandler();

    private QueryManagerImpl queryManager;

    /**
     * Private constructor
     */
    private XACMLContextHandler() {

    }

    /**
     * Gets the singleton instance of the XACMLContextHandler
     * 
     * @return The singleton instance of the XACMLContextHandler
     */
    public static XACMLContextHandler getInstance() {
        return instance;
    }

    /**
     * Authorizes a request for the given object for the given user
     * 
     * @param object
     *            The object to authorize
     * @param user
     *            The user making the request
     * @return The authorization response
     * @throws EbxmlRegistryException
     * @throws MsgRegistryException
     */
    public ResponseType authorize(String userName, Object object)
            throws MsgRegistryException, EbxmlRegistryException {

        RequestType request = constructRequest(userName, object);

        XACMLObject policy = XACMLPolicyAdministrator
                .getInstance()
                .getPolicyObject(
                        "urn:oasis:names:tc:xacml:2.0:data-delivery:default-policySet");
        XACMLPolicyDecisionPoint pdp = new XACMLPolicyDecisionPoint(policy,
                request);

        ResponseType response = pdp.evaluate();
        List<ObligationType> obligations = pdp.getObligations();
        if (obligations != null) {
            for (ObligationType obligation : obligations) {
                try {
                    XACMLObligationEvaluator.getInstance().evaluate(obligation,
                            request);
                } catch (XACMLException e) {
                    e.printStackTrace();
                }
            }
        }

        return response;
    }

    public RequestType constructRequest(String user, Object nativeRequest)
            throws MsgRegistryException, EbxmlRegistryException {
        RequestType request = new RequestTypeImplBuilder().buildObject();

        /*
         * Construct the subject component of the request
         */
        SubjectType subject = new SubjectTypeImplBuilder().buildObject();
        AttributeType subjectAttribute = new AttributeTypeImplBuilder()
                .buildObject();
        subjectAttribute.setAttributeID(Identifiers.SUBJECT_ID);
        subjectAttribute.setDataType(DataTypes.STRING);
        AttributeValueType subjectAttributeValue = new AttributeValueTypeImplBuilder()
                .buildObject();
        subjectAttributeValue.setValue(user);
        subjectAttribute.getAttributeValues().add(subjectAttributeValue);
        subject.getAttributes().add(subjectAttribute);
        request.getSubjects().add(subject);

        /*
         * Construct the resource component of the request
         */
        List<RegistryObjectType> regObjs = getRegistryObjects(nativeRequest);
        for (RegistryObjectType obj : regObjs) {
            ResourceType resource = new ResourceTypeImplBuilder().buildObject();
            addIdToResource(resource, obj.getId());
            addAttributeToResource(resource, "owner", obj.getOwner());
            addAttributeToResource(resource, "name", obj.getName()
                    .getLocalizedString().get(0).getValue());
            addAttributeToResource(resource, "description", obj
                    .getDescription().getLocalizedString().get(0).getValue());
            addAttributeToResource(resource, "status", obj.getStatus());
            addAttributeToResource(resource, "objectType", obj.getObjectType());
        }
        /*
         * Construct the action component of the request
         */
        ActionType action = new ActionTypeImplBuilder().buildObject();
        AttributeType attribute = new AttributeTypeImplBuilder().buildObject();
        attribute.setAttributeID(Identifiers.ACTION_ID);
        attribute.setDataType(DataTypes.STRING);
        AttributeValueType actionAttribute = new AttributeValueTypeImplBuilder()
                .buildObject();
        if (nativeRequest instanceof SubmitObjectsRequest) {
            actionAttribute.setValue("create");
        } else if (nativeRequest instanceof UpdateObjectsRequest) {
            actionAttribute.setValue("update");
        } else if (nativeRequest instanceof RemoveObjectsRequest) {
            actionAttribute.setValue("delete");
        } else if (nativeRequest instanceof QueryRequest) {
            actionAttribute.setValue("read");
        } else if (nativeRequest instanceof IRegistryRequest) {
            switch (((IRegistryRequest<?>) nativeRequest).getAction()) {
            case QUERY:
                actionAttribute.setValue("read");
                break;
            case REMOVE:
                actionAttribute.setValue("delete");
                break;
            case STORE_OR_REPLACE:
            case STORE:
                actionAttribute.setValue("create");
                break;
            }
        }
        attribute.getAttributeValues().add(actionAttribute);
        action.getAttributes().add(attribute);
        request.setAction(action);
        return request;
    }

    private void addIdToResource(ResourceType resource, String name) {
        addResourceAttribute(resource, Identifiers.RESOURCE_ID, name);
    }

    private void addAttributeToResource(ResourceType resource, String name,
            String value) {
        addResourceAttribute(resource, "" + name, value);
    }

    private void addResourceAttribute(ResourceType resource, String name,
            String value) {
        AttributeType attr = new AttributeTypeImplBuilder().buildObject();
        attr.setAttributeID(name);
        attr.setDataType(DataTypes.STRING);
        AttributeValueType attrValue = new AttributeValueTypeImplBuilder()
                .buildObject();
        attrValue.setValue(value);
        attr.getAttributeValues().add(attrValue);
    }

    private List<RegistryObjectType> getRegistryObjects(Object nativeRequest)
            throws MsgRegistryException, EbxmlRegistryException {
        List<RegistryObjectType> registryObjects = new ArrayList<RegistryObjectType>();
        if (nativeRequest instanceof SubmitObjectsRequest) {
            registryObjects.addAll(((SubmitObjectsRequest) nativeRequest)
                    .getRegistryObjectList().getRegistryObject());
        } else if (nativeRequest instanceof UpdateObjectsRequest) {
            // TODO: Handle object update requests
            throw new UnsupportedOperationException(
                    "This registry currently does not support object update requests");
        } else if (nativeRequest instanceof RemoveObjectsRequest) {
            RemoveObjectsRequest removeRequest = (RemoveObjectsRequest) nativeRequest;
            registryObjects.addAll(evaluateQuery(removeRequest.getQuery()));
            registryObjects.addAll(getObjectsFromRefs(removeRequest
                    .getObjectRefList()));
        } else if (nativeRequest instanceof QueryRequest) {
            QueryRequest queryRequest = (QueryRequest) nativeRequest;
            registryObjects.addAll(evaluateQuery(queryRequest.getQuery()));
        } else if (nativeRequest instanceof IRegistryRequest<?>) {
            IRegistryRequest<?> registryRequest = (IRegistryRequest<?>) nativeRequest;
            Action requestAction = registryRequest.getAction();
            switch (requestAction) {
            case QUERY:
                QueryRequest queryRequest = RegistryUtil
                        .getQuery(registryRequest.getQuery());
                QueryResponse queryResponse = queryManager
                        .executeQuery(queryRequest);
                if (queryResponse.getRegistryObjectList() != null) {
                    return queryResponse.getRegistryObjectList()
                            .getRegistryObject();
                } else {
                    return Collections.emptyList();
                }
            case REMOVE:
            case STORE_OR_REPLACE:
            case STORE:
                // TODO:Change to do something other than throw
                // ClassCastException
                // return (List<RegistryObjectType>)
                // registryRequest.getObjects();
                return Collections.emptyList();
            }
        } else {
            throw new UnsupportedOperationException("Unsupported operation ["
                    + nativeRequest.getClass() + "]");
        }
        return registryObjects;
    }

    private List<RegistryObjectType> evaluateQuery(QueryType query)
            throws MsgRegistryException {
        if (query == null) {
            return Collections.emptyList();
        }

        ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                .createResponseOptionType();
        QueryResponse queryResponse = queryManager.executeQuery(responseOption,
                query);
        if (queryResponse.getStatus().equals(RegistryResponseStatus.SUCCESS)
                || queryResponse.getStatus().equals(
                        RegistryResponseStatus.PARTIAL_SUCCESS)) {
            statusHandler.info("Remove objects query successful");
        }
        if (queryResponse.getRegistryObjectList() == null) {
            return Collections.emptyList();
        } else {
            return queryResponse.getRegistryObjectList().getRegistryObject();
        }
    }

    private List<RegistryObjectType> getObjectsFromRefs(
            ObjectRefListType objRefList) throws EbxmlRegistryException {
        List<ObjectRefType> refs = objRefList.getObjectRef();
        List<String> ids = new ArrayList<String>();
        for (ObjectRefType ref : refs) {
            ids.add(ref.getId());
        }
        return new RegistryObjectTypeDao().getById(ids);

    }

    /**
     * Gets an attribute for an object
     * 
     * @param objId
     *            The id of the object
     * @param attributeName
     *            The name of the object to retrieve
     * @return The attribute from the given object
     * @throws XACMLProcessingException
     *             If errors occur getting the desired object
     * @throws XACMLNotApplicableException
     *             If errors occur getting the attribute from the desired object
     */
    public Object getAttribute(Object objId, String attributeName)
            throws XACMLProcessingException, XACMLNotApplicableException {

        String attrName = XACMLObjectUtil.stripIdentifierPrefix(attributeName);
        if (attrName.equals("subject-id") || attrName.equals("resource-id")) {
            attrName = "id";
        } else if (attrName.contains("attribute:")) {
            String[] attrTokens = attrName.split(":");
            attrName = attrTokens[attrTokens.length - 1];
        }

        Object repoItem = null;
        try {
            repoItem = new RegistryObjectTypeDao().getById(objId.toString());
        } catch (EbxmlRegistryException e) {
            throw new XACMLProcessingException(
                    "Unable to fetch attribute from repository object. Error querying for resource",
                    e);
        }
        if (repoItem == null) {
            throw new XACMLProcessingException(
                    "Unable to fetch attribute from repository object. No object exists with id ["
                            + objId + "].");
        }

        try {
            Object returnedProperty = PropertyUtils.getProperty(repoItem,
                    attrName);
            return returnedProperty;
        } catch (Exception e) {
            throw new XACMLNotApplicableException(
                    "Unable to fetch attribute from repository object. Error getting attribute ["
                            + attrName + "] for object with id [" + objId
                            + "]. Attribute not found");
        }
    }

    public QueryManagerImpl getQueryManager() {
        return queryManager;
    }

    public void setQueryManager(QueryManagerImpl queryManager) {
        this.queryManager = queryManager;
    }

}
