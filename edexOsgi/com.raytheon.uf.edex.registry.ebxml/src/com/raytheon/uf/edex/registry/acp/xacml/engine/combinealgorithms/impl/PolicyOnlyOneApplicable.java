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
package com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms.impl;

import java.util.List;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.StatusCodeType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.PolicyType;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.RuleCombinerAlgorithms;
import com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms.CombinerAlgorithm;
import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.Evaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * Section C.6 of the XACML 2.0 core spec defines the only-one-applicable
 * combiner algorithm:
 * <p>
 * In the entire set of policies in the policy set, if no policy is considered
 * applicable by virtue of its target, then the result of the policy combination
 * algorithm SHALL be "NotApplicable". If more than one policy is considered
 * applicable by virtue of its target, then the result of the policy combination
 * algorithm SHALL be "Indeterminate".
 * <p>
 * If only one policy is considered applicable by evaluation of its target, then
 * the result of the policy-combining algorithm SHALL be the result of
 * evaluating the policy.
 * <p>
 * If an error occurs while evaluating the target of a policy, or a reference to
 * a policy is considered invalid or the policy evaluation results in
 * "Indeterminate, then the policy set SHALL evaluate to "Indeterminate", with
 * the appropriate error status.
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
public class PolicyOnlyOneApplicable implements CombinerAlgorithm {

    @Override
    public String getAlgorithmId() {
        return RuleCombinerAlgorithms.COMBINE_POLICY_ONLY_ONE_APPLICABLE;
    }

    @Override
    public Match evaluate(Match[] decisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException {
        boolean atLeastOne = false;
        XACMLObject selectedPolicy = null;
        Match appResult = null;

        for (int i = 0; i < decisions.length; i++) {
            appResult = isPolicyApplicable(objs.get(i), request);

            switch (appResult.getMatch()) {
            case Indeterminate:
                return Match.indeterminate(decisions[i].getStatusCode());
            case Permit:
                if (atLeastOne) {
                    return Match
                            .indeterminate(StatusCodeType.SC_PROCESSING_ERROR);
                } else {
                    atLeastOne = true;
                    selectedPolicy = objs.get(i);
                }
            case Deny:
                continue;
            }
        }
        if (atLeastOne) {
            return Evaluator.getInstance().evaluate(selectedPolicy, request);
        } else {
            return Match.NOT_APPLICABLE;
        }
    }

    private Match isPolicyApplicable(XACMLObject obj, RequestType request)
            throws XACMLProcessingException {
        if (!(obj instanceof PolicyType)) {
            throw new XACMLProcessingException("Wrong type");
        }
        PolicyType policy = (PolicyType) obj;
        PolicySetType parentPolicySet = (PolicySetType) obj.getParent();
        Match targetMatches = Match.PERMIT;
        if (policy.getTarget() == null) {
            if (parentPolicySet != null && parentPolicySet.getTarget() == null) {
                targetMatches = Match.PERMIT;
            } else {
                targetMatches = Evaluator.getInstance().evaluate(
                        parentPolicySet.getTarget(), request);
            }
        } else {
            targetMatches = Evaluator.getInstance().evaluate(
                    policy.getTarget(), request);
        }
        return targetMatches;
    }

}
