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

import java.util.List;

import org.opensaml.xacml.policy.ActionType;
import org.opensaml.xacml.policy.ActionsType;
import org.opensaml.xacml.policy.impl.ActionsTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation used to evaluate the <Actions> element of a policy
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
public class ActionsEvaluator extends ElementEvaluator<ActionsType> {

    public String getId() {
        return ActionsTypeImpl.class.getName();
    }

    @Override
    protected Match evaluate(ActionsType actions)
            throws XACMLProcessingException {
        Match retVal = Match.PERMIT;
        if (actions != null) {
            List<ActionType> actionTypes = actions.getActions();
            Match[] matches = new Match[actionTypes.size()];
            for (int i = 0; i < actionTypes.size(); i++) {
                matches[i] = evaluateElement(actionTypes.get(i));
            }
            retVal = applyPolicyElementTruthTable(matches);
        }
        return retVal;
    }
}
