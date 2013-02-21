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
package com.raytheon.uf.edex.registry.acp.xacml.util;

import org.opensaml.xacml.ctx.DecisionType;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.ctx.ResultType;
import org.opensaml.xacml.ctx.StatusCodeType;
import org.opensaml.xacml.ctx.StatusDetailType;
import org.opensaml.xacml.ctx.StatusMessageType;
import org.opensaml.xacml.ctx.StatusType;
import org.opensaml.xacml.ctx.impl.DecisionTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.ResponseTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.ResultTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.StatusCodeTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.StatusDetailTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.StatusMessageTypeImplBuilder;
import org.opensaml.xacml.ctx.impl.StatusTypeImplBuilder;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.IdentifierPrefixes;
import com.raytheon.uf.edex.registry.acp.xacml.conformance.SchemaPrefixes;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLSyntaxException;

/**
 * 
 * Utility methods for building XACML response objects.
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
public class XACMLObjectUtil {

    /** The types of identifiers in the XACML spec */
    public enum IDENTIFIER_TYPE {
        subject, subject_category, resource, action, environment, unknown
    }

    /** XACML builder for building ResponseType objects */
    private static ResponseTypeImplBuilder responseBuilder = new ResponseTypeImplBuilder();

    /** XACML builder for building ResultType objects */
    private static ResultTypeImplBuilder resultBuilder = new ResultTypeImplBuilder();

    /** XACML builder for building DecisionType objects */
    private static DecisionTypeImplBuilder decisionBuilder = new DecisionTypeImplBuilder();

    /** XACML builder for building StatusType objects */
    private static StatusTypeImplBuilder statusBuilder = new StatusTypeImplBuilder();

    /** XACML builder for building StatusCodeType objects */
    private static StatusCodeTypeImplBuilder statusCodeBuilder = new StatusCodeTypeImplBuilder();

    /** XACML builder for building StatusMessageType objects */
    private static StatusMessageTypeImplBuilder statusMessageBuilder = new StatusMessageTypeImplBuilder();

    /** XACML builder for building StatusDetailType objects */
    private static StatusDetailTypeImplBuilder statusDetailBuilder = new StatusDetailTypeImplBuilder();

    /**
     * Builds a ResponseType object with the given result
     * 
     * @param result
     *            The result
     * @return The ResponseType object
     */
    public static ResponseType buildResponse(ResultType result) {
        ResponseType response = responseBuilder.buildObject();
        response.setResult(result);
        return response;
    }

    /**
     * Builds a ResponseType object with the given parameters
     * 
     * @param decision
     *            The decision object
     * @param statusCode
     *            The status code
     * @param statusMessage
     *            The status message
     * @param statusDetail
     *            The status detail
     * @return The ResponseType object
     */
    public static ResponseType buildResponse(DECISION decision,
            String statusCode, String statusMessage, String statusDetail) {
        ResponseType response = responseBuilder.buildObject();
        response.setResult(buildResult(decision, statusCode, statusMessage,
                statusDetail));
        return response;
    }

    /**
     * Builds a ResponseType object for a decision and no message.
     * 
     * @param decision
     *            The decision
     * @return The ResponseType object
     */
    public static ResponseType buildOkResponse(DECISION decision) {
        return buildResponse(buildOkResult(decision));
    }

    /**
     * Builds a result using the XACML object builders
     * 
     * @param decision
     *            The decision
     * @param statusCode
     *            The status code
     * @param statusMessage
     *            The status message
     * @param statusDetail
     *            The status detail
     * @return The constructed ResultType object
     */
    public static ResultType buildResult(DECISION decision, String statusCode,
            String statusMessage, String statusDetail) {
        ResultType resultObj = resultBuilder.buildObject();
        DecisionType decisionObj = decisionBuilder.buildObject();
        StatusType statusObj = statusBuilder.buildObject();
        StatusCodeType statusCodeObj = statusCodeBuilder.buildObject();
        StatusMessageType statusMessageObj = statusMessageBuilder.buildObject();
        StatusDetailType statusDetailObj = statusDetailBuilder.buildObject();

        // Assign the decision to the decision object
        decisionObj.setDecision(decision);

        // Assign the decision to result object
        resultObj.setDecision(decisionObj);

        // Populate the status items and assign the status to the result
        if (statusCode == null) {
            statusCode = StatusCodeType.SC_OK;
        }
        statusCodeObj.setValue(statusCode);
        statusObj.setStatusCode(statusCodeObj);
        if (!statusCode.equals(StatusCodeType.SC_OK)) {
            statusMessageObj.setValue(statusMessage);
            statusObj.setStatusMessage(statusMessageObj);
            statusObj.setStatusDetail(statusDetailObj);
        }
        resultObj.setStatus(statusObj);

        // Return the response
        return resultObj;
    }

    /**
     * Builds an ok result with the given decision
     * 
     * @param decision
     *            The decision
     * @return The ResultType object
     */
    public static ResultType buildOkResult(DECISION decision) {
        ResultType resultObj = resultBuilder.buildObject();
        DecisionType decisionObj = decisionBuilder.buildObject();
        StatusType statusObj = statusBuilder.buildObject();
        StatusCodeType statusCodeObj = statusCodeBuilder.buildObject();

        // Assign the decision to the decision object
        decisionObj.setDecision(decision);

        // Assign the decision to result object
        resultObj.setDecision(decisionObj);

        // Populate the status items and assign the status to the result
        statusCodeObj.setValue(StatusCodeType.SC_OK);
        statusObj.setStatusCode(statusCodeObj);
        resultObj.setStatus(statusObj);

        // Return the response
        return resultObj;
    }

    /**
     * Validates an object to ensure correct syntax
     * 
     * @param <T>
     *            Object
     * @param element
     *            The element object
     * @param elementName
     *            The element name
     * @return The element if it's valid
     * @throws XACMLSyntaxException
     *             If the object is null
     */
    public static <T extends Object> T validateElement(T element,
            String elementName) throws XACMLSyntaxException {

        if (element == null) {
            throw new XACMLSyntaxException("Required element [" + elementName
                    + "] not present");
        }
        return element;
    }

    /**
     * Strips the schema prefixes from an identifier
     * 
     * @param identifier
     *            The identifier string
     * @return The identifier string with the schema prefixes removed
     */
    public static String stripSchemaPrefixes(final String identifier) {
        return identifier.replace(SchemaPrefixes.XACML_1_0_PREFIX, "").replace(
                SchemaPrefixes.XACML_2_0_PREFIX, "");
    }

    /**
     * Strips the identifier prefixes from an identifier string
     * 
     * @param identifier
     *            The identifier string
     * @return The identifier string with the prefix removed
     */
    public static String stripIdentifierPrefix(final String identifier) {

        String retVal = identifier;
        for (String prefix : IdentifierPrefixes.prefixes) {
            retVal = retVal.replace(prefix, "");
        }
        if (retVal.startsWith(":")) {
            retVal = retVal.substring(1, retVal.length());
        }
        if (retVal.endsWith(":")) {
            retVal = retVal.substring(0, retVal.length() - 1);
        }

        return retVal;
    }

    /**
     * Gets the type of identifier from the given identifier string
     * 
     * @param identifier
     *            The identifier string
     * @return The type of identifier
     */
    public static IDENTIFIER_TYPE getIdentifierType(final String identifier) {
        String typeStr = XACMLObjectUtil.stripSchemaPrefixes(identifier)
                .replace(XACMLObjectUtil.stripIdentifierPrefix(identifier), "");
        if (typeStr.startsWith(":")) {
            typeStr = typeStr.substring(1, typeStr.length());
        }
        if (typeStr.endsWith(":")) {
            typeStr = typeStr.substring(0, typeStr.length() - 1);
        }
        for (IDENTIFIER_TYPE idType : IDENTIFIER_TYPE.values()) {
            if (idType.toString().equals(typeStr)) {
                return idType;
            }
        }
        return IDENTIFIER_TYPE.unknown;
    }

    /**
     * Checks if this attribute id denotes an attribute request
     * 
     * @param attribute
     *            The attribute id
     * @return True if this is an attribute request, else false
     */
    public static boolean isAttributeRequest(String attribute) {
        return stripIdentifierPrefix(attribute).startsWith("attribute")
                || attribute.startsWith(IdentifierPrefixes.CONFORMANCE_TEST);
    }
}
