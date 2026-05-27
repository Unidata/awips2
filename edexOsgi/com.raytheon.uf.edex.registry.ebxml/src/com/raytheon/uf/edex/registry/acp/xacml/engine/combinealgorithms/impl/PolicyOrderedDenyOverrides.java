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
 * Section C.2 of the XACML 2.0 core spec defines the ordered deny overrides
 * combiner algorithm:
 * <p>
 * The following specification defines the "Ordered-deny-overrides"
 * policy-combining algorithm of a policy set.
 * <p>
 * The behavior of this algorithm is identical to that of the Deny-overrides
 * policy-combining algorithm with one exception. The order in which the
 * collection of policies is evaluated SHALL match the order as listed in the
 * policy set.
 * <p>
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
public class PolicyOrderedDenyOverrides implements CombinerAlgorithm {

    @Override
    public String getAlgorithmId() {
        return RuleCombinerAlgorithms.COMBINE_POLICY_ORDERED_DENY_OVERRIDES;
    }

    @Override
    public Match evaluate(Match[] decisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException {
        boolean atLeastOnePermit = false;
        for (int i = 0; i < objs.size(); i++) {
            switch (decisions[i].getMatch()) {
            case Deny:
                return Match.DENY;
            case Permit:
                atLeastOnePermit = true;
                continue;
            case NotApplicable:
                continue;
            case Indeterminate:
                return Match.DENY;
            }
        }
        if (atLeastOnePermit) {
            return Match.PERMIT;
        }
        return Match.NOT_APPLICABLE;
    }

}
