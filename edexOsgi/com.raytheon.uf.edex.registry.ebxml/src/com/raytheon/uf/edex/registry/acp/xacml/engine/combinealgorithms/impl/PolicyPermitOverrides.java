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
 * Section C.3 of the XACML 2.0 core spec defines the permit-overrides combiner
 * algorithm:
 * <p>
 * The following specification defines the “Permit-overrides” policy-combining
 * algorithm of a policy set.
 * <p>
 * In the entire set of policies in the policy set, if any policy evaluates to
 * "Permit", then the result of the policy combination SHALL be "Permit". In
 * other words, "Permit" takes precedence, regardless of the result of
 * evaluating any of the other policies in the policy set. If all policies are
 * found to be "NotApplicable" to the decision request, then the policy set
 * SHALL evaluate to "NotApplicable".
 * <p>
 * If an error occurs while evaluating the target of a policy, a reference to a
 * policy is considered invalid or the policy evaluation results in
 * "Indeterminate", then the policy set SHALL evaluate to "Indeterminate", with
 * the appropriate error status, provided no other policies evaluate to "Permit"
 * or "Deny".
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
public class PolicyPermitOverrides implements CombinerAlgorithm {

    @Override
    public String getAlgorithmId() {
        return RuleCombinerAlgorithms.COMBINE_POLICY_PERMIT_OVERRIDES;
    }

    @Override
    public Match evaluate(Match[] decisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException {
        boolean atLeastOneError = false;
        boolean atLeastOneDeny = false;
        String statusCode = null;
        for (int i = 0; i < objs.size(); i++) {
            switch (decisions[i].getMatch()) {
            case Deny:
                atLeastOneDeny = true;
                continue;
            case Permit:
                return Match.PERMIT;
            case NotApplicable:
                continue;
            case Indeterminate:
                statusCode = decisions[i].getStatusCode();
                atLeastOneError = true;
                continue;
            }
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
