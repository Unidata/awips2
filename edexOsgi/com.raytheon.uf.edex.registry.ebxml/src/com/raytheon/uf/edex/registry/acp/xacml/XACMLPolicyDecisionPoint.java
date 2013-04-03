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

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.ctx.StatusCodeType;
import org.opensaml.xacml.policy.ObligationType;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.Evaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil;

/**
 * 
 * Policy decision point (PDP) - The system entity that evaluates applicable
 * policy and renders an authorization decision. This term is defined in a joint
 * effort by the IETF Policy Framework Working Group and the Distributed
 * Management Task Force (DMTF)/Common Information Model (CIM) in [RFC3198].
 * This term corresponds to "Access Decision Function" (ADF) in [ISO10181-3].
 * 
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
public class XACMLPolicyDecisionPoint {

    /** The obligations to evaluate */
    private List<ObligationType> obligations;

    /** The Policy or Policy Set object being used to evaluate the request */
    private XACMLObject policyObject;

    /** The Request being evaluated */
    private RequestType request;

    /**
     * Creates a new XACMLPolicyDecisionPoint
     * 
     * @param policyObject
     *            The policy object used to evaluate the request against
     * @param request
     *            The request being evaluated
     */
    public XACMLPolicyDecisionPoint(XACMLObject policyObject,
            RequestType request) {
        this.policyObject = policyObject;
        this.request = request;
    }

    /**
     * Evaluates the policy object
     * 
     * @return The response
     */
    public ResponseType evaluate() {
        Match match;
        try {
            match = Evaluator.getInstance().evaluate(policyObject, request);
        } catch (XACMLProcessingException e) {
            return XACMLObjectUtil.buildResponse(DECISION.Deny,
                    StatusCodeType.SC_PROCESSING_ERROR,
                    e.getLocalizedMessage(), "");
        }
        this.obligations = match.getObligations();
        return XACMLObjectUtil.buildResponse(match.getMatch(),
                match.getStatusCode(), match.getMessage(), "");
    }

    /**
     * @return the obligations
     */
    public List<ObligationType> getObligations() {
        return obligations;
    }

    /**
     * @param obligations
     *            the obligations to set
     */
    public void setObligations(List<ObligationType> obligations) {
        this.obligations = obligations;
    }

    /**
     * @return the policyObject
     */
    public XACMLObject getPolicyObject() {
        return policyObject;
    }

    /**
     * @param policyObject
     *            the policyObject to set
     */
    public void setPolicyObject(XACMLObject policyObject) {
        this.policyObject = policyObject;
    }

    /**
     * @return the request
     */
    public RequestType getRequest() {
        return request;
    }

    /**
     * @param request
     *            the request to set
     */
    public void setRequest(RequestType request) {
        this.request = request;
    }

}
