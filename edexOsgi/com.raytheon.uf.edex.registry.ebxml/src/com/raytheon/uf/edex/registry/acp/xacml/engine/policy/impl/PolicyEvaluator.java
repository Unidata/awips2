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
package com.raytheon.uf.edex.registry.acp.xacml.engine.policy.impl;

import java.util.ArrayList;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.policy.EffectType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.PolicyType;
import org.opensaml.xacml.policy.impl.PolicyTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms.CombinerAlgorithmEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluates the <Policy> element in a policy.
 * <p>
 * According to the XACML 2.0 core spec, policy evaluation is as follows:
 * <p>
 * The value of a policy SHALL be determined only by its contents, considered in
 * relation to the contents of the request context. A policy's value SHALL be
 * determined by evaluation of the policy's target and rules.
 * <p>
 * The policy's target SHALL be evaluated to determine the applicability of the
 * policy. If the target evaluates to "Match", then the value of the policy
 * SHALL be determined by evaluation of the policy's rules, according to the
 * specified rule-combining algorithm. If the target evaluates to "No-match",
 * then the value of the policy SHALL be "NotApplicable". If the target
 * evaluates to "Indeterminate", then the value of the policy SHALL be
 * "Indeterminate".
 * <p>
 * The policy truth table is shown below (Table 5 from the spec)
 * <p>
 * <table border="1">
 * <tr>
 * <th>Target</th>
 * <th>Rule Values</th>
 * <th>Policy Value</th>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>At least one rule value is its Effect</td>
 * <td>Specified by the rule-combining-algorithm</td>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>All rule values are "NotApplicable"</td>
 * <td>"NotApplicable"</td>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>At least one rule value is "Indeterminate"</td>
 * <td>Specified by the rule-combining-algorithm</td>
 * </tr>
 * <tr>
 * <td>"No-Match"</td>
 * <td>Don't Care</td>
 * <td>"NotApplicable"</td>
 * </tr>
 * <tr>
 * <td>"Indeterminate"</td>
 * <td>Don't Care</td>
 * <td>"Indeterminate"</td>
 * </tr>
 * </table>
 * <p>
 * A rules value of "At least one rule value is its Effect" means either that
 * the <Rule> element is absent, or one or more of the rules contained in the
 * policy is applicable to the decision request (i.e., it returns the value of
 * its “Effect”; see Section 7.9). A rules value of “All rule values are
 * ‘NotApplicable’” SHALL be used if no rule contained in the policy is
 * applicable to the request and if no rule contained in the policy returns a
 * value of “Indeterminate”. If no rule contained in the policy is applicable to
 * the request, but one or more rule returns a value of “Indeterminate”, then
 * the rules SHALL evaluate to "At least one rule value is ‘Indeterminate’".
 * <p>
 * If the target value is "No-match" or “Indeterminate” then the policy value
 * SHALL be “NotApplicable” or “Indeterminate”, respectively, regardless of the
 * value of the rules. For these cases, therefore, the rules need not be
 * evaluated.
 * <p>
 * If the target value is “Match” and the rule value is “At least one rule value
 * is it’s Effect” or “At least one rule value is ‘Indeterminate’”, then the
 * rule-combining algorithm specified in the policy SHALL determine the policy
 * value.
 * <p>
 * Note that none of the rule-combining algorithms defined by XACML 2.0 take
 * parameters. However, non-standard combining algorithms MAY take parameters.
 * In such a case, the values of these parameters associated with the rules,
 * MUST be taken into account when evaluating the policy. The parameters and
 * their types should be defined in the specification of the combining
 * algorithm. If the implementation supports combiner parameters and if combiner
 * parameters are present in a policy, then the parameter values MUST be
 * supplied to the combining algorithm implementation.
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
public class PolicyEvaluator extends ElementEvaluator<PolicyType> {

    public String getId() {
        return PolicyTypeImpl.class.getName();
    }

    public Match evaluate(PolicyType policy) throws XACMLProcessingException {
        PolicySetType parentPolicySet = (PolicySetType) policy.getParent();
        Match retVal = null;
        Match targetMatches = Match.PERMIT;
        if (policy.getTarget() == null) {
            if (parentPolicySet.getTarget() != null) {
                targetMatches = evaluateElement(parentPolicySet.getTarget());
            }
        } else {
            targetMatches = evaluateElement(policy.getTarget());
        }
        if (targetMatches.getMatch().equals(DECISION.Indeterminate)) {
            retVal = targetMatches;
        } else if (targetMatches.getMatch().equals(DECISION.Deny)) {
            retVal = Match.NOT_APPLICABLE;
        } else {
            Match[] ruleDecisions = new Match[policy.getRules().size()];
            int notApplicableCount = 0;
            boolean atLeastOneIndeterminate = false;
            boolean atLeastOneIsInEffect = false;
            for (int i = 0; i < policy.getRules().size(); i++) {
                DECISION desiredEffect = DECISION.Deny;
                if (policy.getRules().get(i).getEffect()
                        .equals(EffectType.Permit)) {
                    desiredEffect = DECISION.Permit;
                }
                ruleDecisions[i] = evaluateElement(policy.getRules().get(i));
                if (ruleDecisions[i].getMatch().equals(DECISION.NotApplicable)) {
                    notApplicableCount++;
                } else if (ruleDecisions[i].getMatch().equals(
                        DECISION.Indeterminate)) {
                    atLeastOneIndeterminate = true;
                } else if (ruleDecisions[i].getMatch().equals(desiredEffect)) {
                    atLeastOneIsInEffect = true;
                }
            }
            if (notApplicableCount == policy.getRules().size()) {
                retVal = Match.NOT_APPLICABLE;
            } else if (atLeastOneIndeterminate || atLeastOneIsInEffect) {
                retVal = CombinerAlgorithmEvaluator.getInstance().combine(
                        policy.getRuleCombiningAlgoId(), ruleDecisions,
                        new ArrayList<XACMLObject>(policy.getRules()), request);
            }
        }
        retVal.setApplicableObligations(policy.getObligations());
        return retVal;
    }
}
