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
package com.raytheon.uf.edex.registry.acp.xacml.engine.expression.impl;

import java.util.ArrayList;
import java.util.List;

import org.opensaml.xacml.ctx.ActionType;
import org.opensaml.xacml.ctx.AttributeType;
import org.opensaml.xacml.ctx.AttributeValueType;
import org.opensaml.xacml.ctx.EnvironmentType;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResourceType;
import org.opensaml.xacml.ctx.SubjectType;
import org.opensaml.xacml.policy.AttributeDesignatorType;
import org.opensaml.xacml.policy.ExpressionType;
import org.opensaml.xacml.policy.SubjectAttributeDesignatorType;
import org.opensaml.xacml.policy.impl.AttributeDesignatorTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.XACMLContextHandler;
import com.raytheon.uf.edex.registry.acp.xacml.conformance.DataTypes;
import com.raytheon.uf.edex.registry.acp.xacml.conformance.Identifiers;
import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.engine.function.Environment;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLMissingAttributeException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLNotApplicableException;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil.IDENTIFIER_TYPE;

/**
 * The AttributeDesignatorType complex type is the type for elements that
 * identify attributes by name. It contains the information required to match
 * attributes in the request context. See Section 7.2.4.
 * <p>
 * It also contains information to control behaviour in the event that no
 * matching attribute is present in the context.
 * <p>
 * Elements of this type SHALL NOT alter the match semantics of named
 * attributes, but MAY narrow the search space.
 * <p>
 * A named attribute SHALL match an attribute if the values of their respective
 * AttributeId, DataType and Issuer attributes match. The attribute designator’s
 * AttributeId MUST match, by URI equality, the AttributeId of the attribute.
 * The attribute designator’s DataType MUST match, by URI equality, the DataType
 * of the same attribute.
 * <p>
 * If the Issuer attribute is present in the attribute designator, then it MUST
 * match, using the “urn:oasis:names:tc:xacml:1.0:function:string-equal”
 * function, the Issuer of the same attribute. If the Issuer is not present in
 * the attribute designator, then the matching of the attribute to the named
 * attribute SHALL be governed by AttributeId and DataType attributes alone.
 * <p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class AttributeDesignatorExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return AttributeDesignatorTypeImpl.class.getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        AttributeDesignatorType designator = (AttributeDesignatorType) XACMLObjectUtil
                .validateElement(
                        expression,
                        SubjectAttributeDesignatorType.DEFAULT_ELEMENT_LOCAL_NAME);
        String designatorAttributeId = XACMLObjectUtil.validateElement(
                designator.getAttributeId(), "AttributeId");
        String designatorDataType = XACMLObjectUtil.validateElement(
                designator.getDataType(), "DataType");
        String issuer = designator.getIssuer();
        @SuppressWarnings("unused")
        boolean designatorMustBePresentXS = designator
                .getMustBePresentXSBoolean().getValue();
        Boolean designatorMustBePresent = designator.getMustBePresent();

        String subjectCategory = null;
        String subjectIssuer = null;
        if (expression instanceof SubjectAttributeDesignatorType) {
            subjectCategory = ((SubjectAttributeDesignatorType) expression)
                    .getSubjectCategory();
            if (subjectCategory == null) {
                subjectCategory = Identifiers.ACCESS_SUBJECT;
            }
            subjectIssuer = ((SubjectAttributeDesignatorType) expression)
                    .getIssuer();
        }

        IDENTIFIER_TYPE idType = XACMLObjectUtil
                .getIdentifierType(designatorAttributeId);

        switch (idType) {
        case subject:
            return getAttributeFromSubject(request, designatorAttributeId,
                    designatorDataType, subjectCategory, subjectIssuer,
                    designatorMustBePresent);
        case resource:
            return getAttributeFromResource(request, designatorAttributeId,
                    designatorDataType, issuer, designatorMustBePresent);
        case action:
            return getAttributeFromAction(request, designatorAttributeId,
                    designatorDataType, issuer, designatorMustBePresent);
        case environment:
            return getAttributeFromEnvironment(request, designatorAttributeId);
        case unknown:
            String elementQName = expression.getElementQName().getLocalPart();
            if (elementQName
                    .equals(SubjectAttributeDesignatorType.DEFAULT_ELEMENT_LOCAL_NAME)) {
                return getAttributeFromSubject(request, designatorAttributeId,
                        designatorDataType, subjectCategory, subjectIssuer,
                        designatorMustBePresent);
            } else if (elementQName.equals("ResourceAttributeDesignator")) {
                return getAttributeFromResource(request, designatorAttributeId,
                        designatorDataType, issuer, designatorMustBePresent);
            } else if (elementQName.equals("ActionAttributeDesignator")) {
                return getAttributeFromAction(request, designatorAttributeId,
                        designatorDataType, issuer, designatorMustBePresent);
            } else if (elementQName.equals("EnvironmentAttributeDesignator")) {
                return getAttributeFromEnvironment(request,
                        designatorAttributeId);
            } else {
                throw new UnsupportedOperationException(
                        "Unsupported IDENTIFIER: " + designatorAttributeId);
            }
        default:
            throw new UnsupportedOperationException(
                    "Unsupported type of IDENTIFIER_TYPE:"
                            + designatorAttributeId);
        }
    }

    /**
     * Gets the attribute from the environment
     * 
     * @param request
     *            The current request object
     * @param id
     *            The id of the environment attribute to be retrieved
     * @return The value of the environment attribute
     * @throws XACMLException
     *             If errors occur while retrieving the environment attribute
     */
    protected Object getAttributeFromEnvironment(RequestType request, String id)
            throws XACMLException {
        List<Object> retVal = new ArrayList<Object>();
        EnvironmentType env = request.getEnvironment();
        List<AttributeType> attrs = env.getAttributes();
        for (AttributeType attr : attrs) {
            String attributeId = attr.getAttributeID();
            if (attributeId.equals(id)) {
                String attributeDataType = XACMLObjectUtil.validateElement(
                        attr.getDataType(), "DataType");
                for (AttributeValueType attribute : attr.getAttributeValues()) {
                    retVal.add(DataTypes.castDataType(attribute.getValue(),
                            attributeDataType));
                }
            }
        }
        if (retVal.isEmpty()) {
            return requestAttribute(request, IDENTIFIER_TYPE.environment, id);
        } else if (retVal.size() == 1) {
            return retVal.get(0);
        }
        return retVal;
    }

    /**
     * Gets the attribute from the subject
     * 
     * @param request
     *            The current request object
     * @param id
     *            The id of the subject attribute to be retrieved
     * @return The value of the subject attribute
     * @throws XACMLException
     *             If errors occur while retrieving the subject attribute
     */
    protected Object getAttributeFromSubject(RequestType request, String id,
            String attributeType, String subjectCategory, String subjectIssuer,
            Boolean designatorMustBePresent) throws XACMLException {
        List<SubjectType> subjects = XACMLObjectUtil.validateElement(
                request.getSubjects(), "Subject");
        List<Object> retVal = new ArrayList<Object>();
        String requestSubjectCategory = null;
        for (SubjectType subj : subjects) {
            requestSubjectCategory = subj.getSubjectCategory();
            if (subjectCategory.equals(requestSubjectCategory)) {

                List<AttributeType> attrs = XACMLObjectUtil.validateElement(
                        subj.getAttributes(), "Attribute");
                for (AttributeType attr : attrs) {
                    if (subjectIssuer != null
                            && !subjectIssuer.equals(attr.getIssuer())) {
                        throw new XACMLNotApplicableException(
                                "Issuer doesn't match");
                    }
                    String attributeId = XACMLObjectUtil.validateElement(
                            attr.getAttributeID(), "AttributeID");
                    String attributeDataType = XACMLObjectUtil.validateElement(
                            attr.getDataType(), "DataType");
                    if (attributeId.equals(id)
                            && attributeDataType.equals(attributeType)) {

                        for (AttributeValueType attribute : attr
                                .getAttributeValues()) {
                            retVal.add(DataTypes.castDataType(
                                    attribute.getValue(), attributeDataType));
                        }
                    }

                }
            }
        }
        if (retVal.isEmpty()) {
            if (designatorMustBePresent != null
                    && designatorMustBePresent.booleanValue()) {
                throw new XACMLMissingAttributeException(
                        "Subject Missing Required Attribute [" + id + "]");
            } else {
                return requestAttribute(request, IDENTIFIER_TYPE.subject, id);
            }
        } else if (retVal.size() == 1) {
            return retVal.get(0);
        }
        return retVal;
    }

    /**
     * Gets the attribute from the resource
     * 
     * @param request
     *            The current request object
     * @param id
     *            The id of the resource attribute to be retrieved
     * @return The value of the resource attribute
     * @throws XACMLException
     *             If errors occur while retrieving the resource attribute
     */
    protected Object getAttributeFromResource(RequestType request, String id,
            String attributeType, String issuer, Boolean designatorMustBePresent)
            throws XACMLException {
        List<Object> retVal = new ArrayList<Object>();
        List<ResourceType> resources = request.getResources();
        for (ResourceType resource : resources) {
            List<AttributeType> attrs = resource.getAttributes();
            for (AttributeType attr : attrs) {
                if (issuer != null && !issuer.equals(attr.getIssuer())) {
                    throw new XACMLNotApplicableException(
                            "Issuer doesn't match");
                }
                String attributeId = attr.getAttributeID();
                if (attributeId
                        .equals("urn:oasis:names:tc:xacml:1.0:resource:scope")) {
                    throw new UnsupportedOperationException(
                            "Unsupported Attribute: urn:oasis:names:tc:xacml:1.0:resource:scope");
                }
                String attributeDataType = XACMLObjectUtil.validateElement(
                        attr.getDataType(), "DataType");

                for (AttributeValueType attribute : attr.getAttributeValues()) {
                    if (attributeId.equals(id)
                            && attributeDataType.equals(attributeType)) {
                        retVal.add(DataTypes.castDataType(attribute.getValue(),
                                attributeDataType));
                    }
                }

            }
        }
        if (retVal.isEmpty()) {
            if (designatorMustBePresent != null
                    && designatorMustBePresent.booleanValue()) {
                throw new XACMLMissingAttributeException(
                        "Resource Missing Required Attribute [" + id + "]");
            } else {
                return requestAttribute(request, IDENTIFIER_TYPE.resource, id);
            }
        } else if (retVal.size() == 1) {
            return retVal.get(0);
        }
        return retVal;
    }

    /**
     * Gets the attribute from the action
     * 
     * @param request
     *            The current request object
     * @param id
     *            The id of the action attribute to be retrieved
     * @return The value of the action attribute
     * @throws XACMLException
     *             If errors occur while retrieving the action attribute
     */
    protected Object getAttributeFromAction(RequestType request, String id,
            String attributeType, String issuer, Boolean designatorMustBePresent)
            throws XACMLException {
        List<Object> retVal = new ArrayList<Object>();
        ActionType action = request.getAction();
        List<AttributeType> attrs = action.getAttributes();
        for (AttributeType attr : attrs) {
            if (issuer != null && !issuer.equals(attr.getIssuer())) {
                throw new XACMLNotApplicableException("Issuer doesn't match");
            }
            String attributeId = XACMLObjectUtil.validateElement(
                    attr.getAttributeID(), "AttributeID");
            String attributeDataType = XACMLObjectUtil.validateElement(
                    attr.getDataType(), "DataType");
            if (attributeId.equals(id)
                    && attributeDataType.equals(attributeType)) {

                for (AttributeValueType attribute : attr.getAttributeValues()) {
                    retVal.add(DataTypes.castDataType(attribute.getValue(),
                            attributeDataType));
                }
            }

        }

        if (retVal.isEmpty()) {
            if (designatorMustBePresent != null
                    && designatorMustBePresent.booleanValue()) {
                throw new XACMLMissingAttributeException(
                        "Action Missing Required Attribute [" + id + "]");
            } else {
                return requestAttribute(request, IDENTIFIER_TYPE.action, id);
            }
        } else if (retVal.size() == 1) {
            return retVal.get(0);
        }
        return retVal;
    }

    /**
     * Requests attribute from the registry
     * 
     * @param request
     *            The current request object
     * @param idType
     *            The component from which to get the attribute
     * @param requestedAttribute
     *            The name of the requested attribute
     * @return The value of the attribute
     * @throws XACMLException
     *             If errors occur while accessing the registry
     */
    private Object requestAttribute(RequestType request,
            IDENTIFIER_TYPE idType, String requestedAttribute)
            throws XACMLException {
        String requestAttributeId = null;
        String requestAttributeValue = null;
        switch (idType) {
        case subject:
            for (SubjectType subj : request.getSubjects()) {
                for (AttributeType reqSubAttr : subj.getAttributes()) {
                    requestAttributeId = reqSubAttr.getAttributeID();
                    requestAttributeValue = reqSubAttr.getAttributeValues()
                            .get(0).getValue();
                    if (requestAttributeId.equals(requestedAttribute)
                            || XACMLObjectUtil
                                    .isAttributeRequest(requestedAttribute)) {
                        return XACMLContextHandler.getInstance().getAttribute(
                                requestAttributeValue, requestedAttribute);
                    }
                }
            }
            throw new XACMLNotApplicableException("Attribute not found");
        case resource:
            for (ResourceType resource : request.getResources()) {
                for (AttributeType reqSubAttr : resource.getAttributes()) {
                    requestAttributeId = reqSubAttr.getAttributeID();
                    requestAttributeValue = reqSubAttr.getAttributeValues()
                            .get(0).getValue();
                    if (requestAttributeId.equals(requestedAttribute)
                            || XACMLObjectUtil
                                    .isAttributeRequest(requestedAttribute)) {
                        return XACMLContextHandler.getInstance().getAttribute(
                                requestAttributeValue, requestedAttribute);
                    }
                }
            }
            throw new XACMLNotApplicableException("Attribute not found");
        case action:
            for (AttributeType reqSubAttr : request.getAction().getAttributes()) {
                requestAttributeId = reqSubAttr.getAttributeID();
                requestAttributeValue = reqSubAttr.getAttributeValues().get(0)
                        .getValue();
                if (requestAttributeId.equals(requestedAttribute)
                        || XACMLObjectUtil
                                .isAttributeRequest(requestedAttribute)) {
                    return XACMLContextHandler.getInstance().getAttribute(
                            requestAttributeValue, requestedAttribute);
                }
            }
            throw new XACMLNotApplicableException("Attribute not found");
        case environment:
            for (AttributeType reqSubAttr : request.getEnvironment()
                    .getAttributes()) {
                requestAttributeId = reqSubAttr.getAttributeID();
                requestAttributeValue = reqSubAttr.getAttributeValues().get(0)
                        .getValue();
                return Environment.getInstance().getEnvironmentValue(
                        requestAttributeId, reqSubAttr.getDataType());
            }
            return Environment.getInstance().getEnvironmentValue(
                    requestedAttribute, null);
        default:
            throw new UnsupportedOperationException(
                    "Unsupported Operation Type:  " + idType);
        }
    }
}
