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
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.RequestType;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.RuleCombinerAlgorithms;
import com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms.CombinerAlgorithm;
import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.Evaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Section C.1 of the XACML 2.0 core spec defines the rule deny overrides
 * combiner algorithm:
 * <p>
 * The following specification defines the “Deny-overrides” rule-combining
 * algorithm of a policy.
 * <p>
 * In the entire set of rules in the policy, if any rule evaluates to "Deny",
 * then the result of the rule combination SHALL be "Deny". If any rule
 * evaluates to "Permit" and all other rules evaluate to "NotApplicable", then
 * the result of the rule combination SHALL be "Permit". In other words, "Deny"
 * takes precedence, regardless of the result of evaluating any of the other
 * rules in the combination. If all rules are found to be "NotApplicable" to the
 * decision request, then the rule combination SHALL evaluate to
 * "NotApplicable".
 * <p>
 * If an error occurs while evaluating the target or condition of a rule that
 * contains an effect value of "Deny" then the evaluation SHALL continue to
 * evaluate subsequent rules, looking for a result of "Deny". If no other rule
 * evaluates to "Deny", then the combination SHALL evaluate to "Indeterminate",
 * with the appropriate error status.
 * <p>
 * If at least one rule evaluates to "Permit", all other rules that do not have
 * evaluation errors evaluate to "Permit" or "NotApplicable" and all rules that
 * do have evaluation errors contain effects of "Permit", then the result of the
 * combination SHALL be "Permit".
 * 
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
public class RuleDenyOverrides implements CombinerAlgorithm {

    @Override
    public String getAlgorithmId() {
        return RuleCombinerAlgorithms.COMBINE_RULES_DENY_OVERRIDES;
    }

    @Override
    public Match evaluate(Match[] ruleDecisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException {
        boolean atLeastOneError = false;
        boolean potentialDeny = false;
        boolean atLeastOnePermit = false;
        String statusCode = null;
        for (int i = 0; i < ruleDecisions.length; i++) {
            switch (ruleDecisions[i].getMatch()) {
            case Deny:
                return Match.DENY;
            case Permit:
                atLeastOnePermit = true;
                continue;
            case NotApplicable:
                continue;
            case Indeterminate:
                atLeastOneError = true;
                statusCode = ruleDecisions[i].getStatusCode();
                if (Evaluator.getInstance().evaluate(objs.get(i), request)
                        .getMatch().equals(DECISION.Deny)) {
                    potentialDeny = true;
                }
                continue;
            }
        }

        if (potentialDeny) {
            return Match.indeterminate(statusCode);
        }
        if (atLeastOnePermit) {
            return Match.PERMIT;
        }
        if (atLeastOneError) {
            return Match.indeterminate(statusCode);
        }
        return Match.NOT_APPLICABLE;
    }
}
