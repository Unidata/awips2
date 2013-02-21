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
import java.util.List;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.policy.IdReferenceType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.impl.PolicySetTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms.CombinerAlgorithmEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation for evaluating the <PolicySet> element of a policy.
 * <p>
 * From the XACML 2.0 core spec:
 * <p>
 * The value of a policy set SHALL be determined by its contents, considered in
 * relation to the contents of the request context. A policy set's value SHALL
 * be determined by evaluation of the policy set's target, policies and policy
 * sets, according to the specified policy-combining algorithm.
 * <p>
 * The policy set's target SHALL be evaluated to determine the applicability of
 * the policy set. If the target evaluates to "Match" then the value of the
 * policy set SHALL be determined by evaluation of the policy set's policies and
 * policy sets, according to the specified policy-combining algorithm. If the
 * target evaluates to "No-match", then the value of the policy set shall be
 * "NotApplicable". If the target evaluates to "Indeterminate", then the value
 * of the policy set SHALL be "Indeterminate".
 * <p>
 * The policy set truth table is show below (Table 6 from section 7.11 of the
 * XACML 2.0 core spec):
 * <p>
 * *
 * <table border="1">
 * <tr>
 * <th>Target</th>
 * <th>Rule Values</th>
 * <th>Policy Value</th>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>At least one policy value is Decision</td>
 * <td>Specified by the policy-combining algorithm</td>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>All policy values are "NotApplicable"</td>
 * <td>"NotApplicable"</td>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>At least one policy value is "Indeterminate"</td>
 * <td>Specified by the policy-combining algorithm</td>
 * </tr>
 * <tr>
 * <td>"No-match"</td>
 * <td>Don't care</td>
 * <td>"NotApplicable"</td>
 * </tr>
 * <tr>
 * <td>"Indeterminate"</td>
 * <td>Don't care</td>
 * <td>"indeterminate"</td>
 * </tr>
 * </table>
 * <p>
 * A policies value of "At least one policy value is its Decision" SHALL be used
 * if there are no contained or referenced policies or policy sets, or if one or
 * more of the policies or policy sets contained in or referenced by the policy
 * set is applicable to the decision request (i.e., returns a value determined
 * by its combining algorithm) A policies value of “All policy values are
 * ‘NotApplicable’” SHALL be used if no policy or policy set contained in or
 * referenced by the policy set is applicable to the request and if no policy or
 * policy set contained in or referenced by the policy set returns a value of
 * “Indeterminate”. If no policy or policy set contained in or referenced by the
 * policy set is applicable to the request but one or more policy or policy set
 * returns a value of “Indeterminate”, then the policies SHALL evaluate to "At
 * least one policy value is ‘Indeterminate’".
 * <p>
 * If the target value is "No-match" or “Indeterminate” then the policy set
 * value SHALL be “NotApplicable” or “Indeterminate”, respectively, regardless
 * of the value of the policies. For these cases, therefore, the policies need
 * not be evaluated.
 * <p>
 * If the target value is “Match” and the policies value is “At least one policy
 * value is its Decision” or “At least one policy value is ‘Indeterminate’”,
 * then the policy-combining algorithm specified in the policy set SHALL
 * determine the policy set value.
 * <p>
 * Note that none of the policy-combining algorithms defined by XACML 2.0 take
 * parameters.
 * <p>
 * However, non-standard combining algorithms MAY take parameters. In such a
 * case, the values of these parameters associated with the policies, MUST be
 * taken into account when evaluating the policy set. The parameters and their
 * types should be defined in the specification of the combining algorithm. If
 * the implementation supports combiner parameters and if combiner parameters
 * are present in a policy, then the parameter values MUST be supplied to the
 * combining algorithm implementation.
 * 
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
public class PolicySetEvaluator extends ElementEvaluator<PolicySetType> {

    private List<XACMLObject> elements;

    public String getId() {
        return PolicySetTypeImpl.class.getName();
    }

    public Match evaluate(PolicySetType policySet)
            throws XACMLProcessingException {
        elements = new ArrayList<XACMLObject>();
        Match retVal = null;
        List<IdReferenceType> policyRefs = policySet.getPolicyIdReferences();
        List<IdReferenceType> policySetRefs = policySet
                .getPolicySetIdReferences();
        List<PolicySetType> policySets = policySet.getPolicySets();

        if (policyRefs != null) {
            elements.addAll(policyRefs);
        }
        if (policySetRefs != null) {
            elements.addAll(policySetRefs);
        }
        if (policySets != null) {
            elements.addAll(policySets);
        }

        elements.addAll(policySet.getPolicies());

        Match targetMatch = evaluateElement(policySet.getTarget());
        if (targetMatch.getMatch().equals(DECISION.Indeterminate)) {
            retVal = targetMatch;
        } else if (targetMatch.getMatch().equals(DECISION.Deny)) {
            retVal = Match.NOT_APPLICABLE;
        } else {
            Match[] decisions = new Match[elements.size()];
            int notApplicableCount = 0;
            boolean atLeastOneIndeterminate = false;
            boolean atLeastOneIsInEffect = false;
            for (int i = 0; i < elements.size(); i++) {
                decisions[i] = evaluateElement(elements.get(i));
                switch (decisions[i].getMatch()) {
                case NotApplicable:
                    notApplicableCount++;
                    break;
                case Indeterminate:
                    atLeastOneIndeterminate = true;
                    break;
                case Permit:
                case Deny:
                    atLeastOneIsInEffect = true;
                    break;
                }
            }
            if (notApplicableCount == elements.size()) {
                return Match.NOT_APPLICABLE;
            } else if (atLeastOneIndeterminate || atLeastOneIsInEffect) {
                retVal = CombinerAlgorithmEvaluator.getInstance().combine(
                        policySet.getPolicyCombiningAlgoId(), decisions,
                        elements, request);
            }
        }

        retVal.setApplicableObligations(policySet.getObligations());
        return retVal;
    }
}
