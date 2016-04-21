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

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.ResponseType;
import org.opensaml.xacml.ctx.StatusCodeType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.util.CollectionUtil;
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
 * 3/18/2013    1802         bphillip    Modified to use transaction boundaries and spring injection
 * 7/10/2014    1717         bphillip    Removed unneccessary methods
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Service
@Transactional
public class XACMLPolicyDecisionPoint {

    public XACMLPolicyDecisionPoint() {

    }

    /**
     * Creates a new XACMLPolicyDecisionPoint
     * 
     * @param policyObject
     *            The policy object used to evaluate the request against
     * @param request
     *            The request being evaluated
     */
    public XACMLPolicyDecisionPoint(XACMLObject policyObject) {
    }

    /**
     * Evaluates the policy object
     * 
     * @return The response
     */
    public ResponseType evaluate(XACMLObject policy, RequestType request) {
        ResponseType response = null;
        Match match;
        try {
            match = Evaluator.getInstance().evaluate(policy, request);
        } catch (XACMLProcessingException e) {
            response = XACMLObjectUtil.buildResponse(DECISION.Deny,
                    StatusCodeType.SC_PROCESSING_ERROR,
                    e.getLocalizedMessage(), "");
            return response;
        }

        response = XACMLObjectUtil.buildResponse(match.getMatch(),
                match.getStatusCode(), match.getMessage(), "");
        if (!CollectionUtil.isNullOrEmpty(match.getObligations())) {
            response.getResult().getObligations().getObligations()
                    .addAll(match.getObligations());
        }
        return response;
    }

}
