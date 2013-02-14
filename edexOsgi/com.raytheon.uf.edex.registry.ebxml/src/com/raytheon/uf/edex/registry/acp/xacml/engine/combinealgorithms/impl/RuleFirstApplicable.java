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

import com.raytheon.uf.edex.registry.acp.xacml.conformance.RuleCombinerAlgorithms;
import com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms.CombinerAlgorithm;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * Section C.5 of the XACML 2.0 core spec defines the rule first applicable
 * combiner algorithm:
 * <p>
 * The following specification defines the "First-Applicable " rule-combining
 * algorithm of a policy.
 * <p>
 * Each rule SHALL be evaluated in the order in which it is listed in the
 * policy. For a particular rule, if the target matches and the condition
 * evaluates to "True", then the evaluation of the policy SHALL halt and the
 * corresponding effect of the rule SHALL be the result of the evaluation of the
 * policy (i.e. "Permit" or "Deny"). For a particular rule selected in the
 * evaluation, if the target evaluates to "False" or the condition evaluates to
 * "False", then the next rule in the order SHALL be evaluated. If no further
 * rule in the order exists, then the policy SHALL evaluate to "NotApplicable".
 * <p>
 * If an error occurs while evaluating the target or condition of a rule, then
 * the evaluation SHALL halt, and the policy shall evaluate to "Indeterminate",
 * with the appropriate error status.
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
public class RuleFirstApplicable implements CombinerAlgorithm {

    @Override
    public String getAlgorithmId() {
        return RuleCombinerAlgorithms.COMBINE_RULES_FIRST_APPLICABLE;
    }

    @Override
    public Match evaluate(Match[] ruleDecisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException {
        for (int i = 0; i < objs.size(); i++) {
            switch (ruleDecisions[i].getMatch()) {
            case Deny:
                return Match.DENY;
            case Permit:
                return Match.PERMIT;
            case Indeterminate:
                return Match.indeterminate(ruleDecisions[i].getStatusCode());
            case NotApplicable:
                continue;

            }
        }
        return Match.NOT_APPLICABLE;
    }
}
