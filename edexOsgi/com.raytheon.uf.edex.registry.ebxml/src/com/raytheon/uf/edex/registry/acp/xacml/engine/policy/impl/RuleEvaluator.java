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

import org.opensaml.xacml.policy.EffectType;
import org.opensaml.xacml.policy.PolicySetType;
import org.opensaml.xacml.policy.PolicyType;
import org.opensaml.xacml.policy.RuleType;
import org.opensaml.xacml.policy.impl.RuleTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation for evaluating the <Rule> element of a policy.
 * <p>
 * From the XACML 2.0 core spec:
 * <p>
 * A rule has a value that can be calculated by evaluating its contents. Rule
 * evaluation involves separate evaluation of the rule's target and condition.
 * The rule truth table is shown below:
 * 
 * <table border="1">
 * <tr>
 * <th>Target</th>
 * <th>Condition</th>
 * <th>Rule Value</th>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>"True"</td>
 * <td>Effect</td>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>"False"</td>
 * <td>"NotApplicable"</td>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>"Indeterminate"</td>
 * <td>"Indeterminate"</td>
 * </tr>
 * <tr>
 * <td>"No-match"</td>
 * <td>Don't care</td>
 * <td>"NotApplicable</td>
 * </tr>
 * <tr>
 * <td>"Indeterminate"</td>
 * <td>Don't care</td>
 * <td>"Indeterminate"</td>
 * </tr>
 * </table>
 * <p>
 * If the target value is "No-match" or “Indeterminate” then the rule value
 * SHALL be “NotApplicable” or “Indeterminate”, respectively, regardless of the
 * value of the condition. For these cases, therefore, the condition need not be
 * evaluated.
 * <p>
 * If the target value is “Match” and the condition value is “True”, then the
 * effect specified in the enclosing <Rule> element SHALL determine the rule’s
 * value.
 * <p>
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
public class RuleEvaluator extends ElementEvaluator<RuleType> {

    public String getId() {
        return RuleTypeImpl.class.getName();
    }

    @Override
    public Match evaluate(RuleType rule) throws XACMLProcessingException {
        PolicyType parentPolicy = (PolicyType) rule.getParent();
        PolicySetType parentPolicySet = null;
        if (parentPolicy != null) {
            parentPolicySet = (PolicySetType) parentPolicy.getParent();
        }
        Match retVal = null;
        EffectType effect = rule.getEffect();
        Match targetMatches = Match.PERMIT;
        if (rule.getTarget() == null) {
            if (parentPolicy != null && parentPolicy.getTarget() == null) {
                if (parentPolicySet != null
                        && parentPolicySet.getTarget() != null) {
                    targetMatches = evaluateElement(parentPolicySet.getTarget());
                }
            } else {
                targetMatches = evaluateElement(parentPolicy.getTarget());
            }
        } else {
            targetMatches = evaluateElement(rule.getTarget());
        }
        Match conditionMatches = evaluateElement(rule.getCondition());

        switch (targetMatches.getMatch()) {
        case Deny:
            retVal = Match.NOT_APPLICABLE;
            break;
        case Indeterminate:
            retVal = targetMatches;
            break;
        case Permit:
            switch (conditionMatches.getMatch()) {
            case Indeterminate:
                retVal = conditionMatches;
                break;
            case Permit:
                if (effect.equals(EffectType.Permit)) {
                    retVal = Match.PERMIT;
                } else {
                    retVal = Match.DENY;
                }
                break;
            case Deny:
                retVal = Match.NOT_APPLICABLE;
                break;
            }
        }
        return retVal;
    }
}
