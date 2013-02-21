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

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import org.apache.cxf.binding.soap.Soap12;
import org.apache.cxf.binding.soap.SoapFault;
import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.ResponseType;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.registry.IRegistryRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * 
 * Policy enforcement point (PEP) - The system entity that performs access
 * control, by making decision requests and enforcing authorization decisions.
 * This term is defined in a joint effort by the IETF Policy Framework Working
 * Group and the Distributed Management Task Force (DMTF)/Common Information
 * Model (CIM) in [RFC3198]. This term corresponds to "Access Enforcement
 * Function" (AEF) in [ISO10181-3].
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
public class XACMLPolicyEnforcementPoint extends
        AbstractPhaseInterceptor<SoapMessage> {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XACMLPolicyEnforcementPoint.class);

    /** The singleton instance */
    private static XACMLPolicyEnforcementPoint instance = new XACMLPolicyEnforcementPoint();

    /**
     * Private constructor
     */
    private XACMLPolicyEnforcementPoint() {
        super(Phase.PRE_INVOKE);
    }

    /**
     * Gets the singleton instance of the XACMLPolicyEnforcementPoint
     * 
     * @return
     */
    public static XACMLPolicyEnforcementPoint getInstance() {
        return instance;
    }

    /**
     * Handles requests from the Thrift client
     * 
     * @param user
     *            The user making the request
     * @param request
     *            The request object
     * @return The authorization response
     */
    public AuthorizationResponse handleRegistryRequest(IUser user,
            IRegistryRequest<?> request) {
        AuthorizationResponse retVal;
        String theUser = user.uniqueId().toString();
        String requestId = EbxmlObjectUtil.getUUID();
        logRequest(theUser, "IRegistryRequest", requestId);
        try {
            ResponseType response = XACMLContextHandler.getInstance()
                    .authorize(theUser, request);
            DECISION decision = response.getResult().getDecision()
                    .getDecision();
            logResult(decision, requestId);
            if (decision.equals(DECISION.Permit)) {
                retVal = new AuthorizationResponse(true,
                        "Registry Access Granted");
            } else {
                retVal = new AuthorizationResponse(false,
                        "Registry Access Denied. XACML Decision: " + decision);
            }
        } catch (Exception e) {
            statusHandler.error("Errors occurred during XACML authentication",
                    e);
            retVal = new AuthorizationResponse(false,
                    "Errors occurred during XACML authentication: "
                            + e.getLocalizedMessage()
                            + ". Registry Access Denied");
        }

        return retVal;

    }

    @Override
    public void handleMessage(SoapMessage msg) throws Fault {
        String authorizationId = EbxmlObjectUtil.getUUID();
        Object content = msg.getContent(List.class).get(0);
        // String userName = ((WSUsernameTokenPrincipal) msg
        // .getContextualProperty("wss4j.principal.result")).getName();
        // TODO: Get the user name
        String userName = "dummy";
        logRequest(userName, "SOAP", authorizationId);
        String requestUri = ((String) msg
                .getContextualProperty("org.apache.cxf.request.uri"))
                .replaceFirst("/", "");
        statusHandler.info("Authorizing Soap Request. User: [" + userName
                + "] Enpoint: [" + requestUri + "]");
        ResponseType response;
        try {
            response = XACMLContextHandler.getInstance().authorize(userName,
                    content);
        } catch (MsgRegistryException e) {
            throw new SoapFault("Error processing XACML request. "
                    + e.getLocalizedMessage(), Soap12.getInstance()
                    .getReceiver());
        } catch (EbxmlRegistryException e) {
            throw new SoapFault("Error processing XACML request. "
                    + e.getLocalizedMessage(), Soap12.getInstance()
                    .getReceiver());
        }
        DECISION decision = response.getResult().getDecision().getDecision();
        logResult(decision, authorizationId);
        if (!decision.equals(DECISION.Permit)) {
            throw new org.apache.cxf.binding.soap.SoapFault(
                    "XACML decision was: [" + decision
                            + "]. Registry Access Denied.", Soap12
                            .getInstance().getReceiver());
        }
    }

    /**
     * Convenience method for logging authorization requests
     * 
     * @param userName
     *            The user name
     * @param type
     *            The source of
     * @param id
     *            The ID of the request
     */
    private void logRequest(String userName, String type, String id) {
        statusHandler.info("Authorizing Request: Type [" + type + "] User: ["
                + userName + "] ID: [" + id + "]");
    }

    /**
     * Convenience method for logging authorization results
     * 
     * @param decision
     *            The decision
     * @param id
     *            The request ID
     */
    private void logResult(DECISION decision, String id) {
        statusHandler.info("Authorization Result for Authorization Request ["
                + id + "]:  " + decision);
    }
}
