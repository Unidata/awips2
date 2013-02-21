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
 * Section C.4 of the XACML 2.0 core spec defines the rule
 * ordered-permit-overrides combiner algorithm:
 * <p>
 * The following specification defines the "Ordered-permit-overrides"
 * rule-combining algorithm of a policy.
 * <p>
 * The behavior of this algorithm is identical to that of the Permit-overrides
 * rule-combining algorithm with one exception. The order in which the
 * collection of rules is evaluated SHALL match the order as listed in the
 * policy.
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
public class RuleOrderedPermitOverrides implements CombinerAlgorithm {

    @Override
    public String getAlgorithmId() {
        return RuleCombinerAlgorithms.COMBINE_RULES_ORDERED_PERMIT_OVERRIDES;
    }

    @Override
    public Match evaluate(Match[] ruleDecisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException {
        boolean atLeastOneError = false;
        boolean potentialPermit = false;
        boolean atLeastOneDeny = false;
        String statusCode = null;
        for (int i = 0; i < ruleDecisions.length; i++) {
            switch (ruleDecisions[i].getMatch()) {
            case Deny:
                atLeastOneDeny = true;
                continue;
            case Permit:
                return Match.PERMIT;
            case NotApplicable:
                continue;
            case Indeterminate:
                statusCode = ruleDecisions[i].getStatusCode();
                atLeastOneError = true;
                if (Evaluator.getInstance().evaluate(objs.get(i), request)
                        .getMatch().equals(DECISION.Permit)) {
                    potentialPermit = true;
                }
                continue;
            }
        }
        if (potentialPermit) {
            return Match.indeterminate(statusCode);
        }
        if (atLeastOneDeny) {
            return Match.DENY;
        }
        if (atLeastOneError) {
            return Match.indeterminate(statusCode);
        }
        return Match.NOT_APPLICABLE;
    }

}
