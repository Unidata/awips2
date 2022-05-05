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

import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.policy.TargetType;
import org.opensaml.xacml.policy.impl.TargetTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation for evaluating the <Target> element of a policy.
 * <p>
 * From the XACML 2.0 core spec:
 * <p>
 * The target value SHALL be "Match" if the subjects, resources, actions and
 * environments specified in the target all match values in the request context.
 * If any one of the subjects, resources, actions and environments specified in
 * the target are “Indeterminate”, then the target SHALL be “Indeterminate”.
 * Otherwise, the target SHALL be “No match”. The target match table is shown
 * below.
 * <p>
 * <table border="1">
 * <tr>
 * <th>Subjects Value</th>
 * <th>Resources Value</th>
 * <th>Actions Value</th>
 * <th>Environments Value</th>
 * <th>Target Value</th>
 * </tr>
 * <tr>
 * <td>"Match"</td>
 * <td>"Match"</td>
 * <td>"Match"</td>
 * <td>"Match"</td>
 * <td>"Match"</td>
 * </tr>
 * <tr>
 * <td>"No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"No Match"</td>
 * </tr>
 * <tr>
 * <td>"Match" or "No Match"</td>
 * <td>"No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"No Match"</td>
 * </tr>
 * <tr>
 * <td>"Match" or "No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"No Match"</td>
 * </tr>
 * <tr>
 * <td>"Match" or "No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"Match" or "No Match"</td>
 * <td>"No Match"</td>
 * <td>"No Match"</td>
 * </tr>
 * <tr>
 * <td>"Indeterminate"</td>
 * <td>Don't Care</td>
 * <td>Don't Care</td>
 * <td>Don't Care</td>
 * <td>"Indeterminate"</td>
 * </tr>
 * <tr>
 * <td>Don't Care</td>
 * <td>"Indeterminate"</td>
 * <td>Don't Care</td>
 * <td>Don't Care</td>
 * <td>"Indeterminate</td>
 * </tr>
 * <tr>
 * <td>Don't Care</td>
 * <td>Don't Care</td>
 * <td>"Indeterminate"</td>
 * <td>Don't Care</td>
 * <td>"Indeterminate"</td>
 * </tr>
 * <tr>
 * <td>Don't Care</td>
 * <td>Don't Care</td>
 * <td>Don't Care</td>
 * <td>"Indeterminate"</td>
 * <td>"Indeterminate"</td>
 * </tr>
 * 
 * </table>
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
public class TargetEvaluator extends ElementEvaluator<TargetType> {

    public String getId() {
        return TargetTypeImpl.class.getName();
    }

    @Override
    protected Match evaluate(TargetType target) throws XACMLProcessingException {
        Match retVal = Match.DENY;
        Match subjectsMatch = Match.PERMIT;
        Match resourcesMatch = Match.PERMIT;
        Match actionsMatch = Match.PERMIT;
        Match environmentsMatch = Match.PERMIT;

        if (target.getSubjects() != null) {
            subjectsMatch = evaluateElement(target.getSubjects());
        }
        if (target.getResources() != null) {
            resourcesMatch = evaluateElement(target.getResources());
        }
        if (target.getActions() != null) {
            actionsMatch = evaluateElement(target.getActions());
        }
        if (target.getEnvironments() != null) {
            environmentsMatch = evaluateElement(target.getEnvironments());
        }
        if (subjectsMatch.getMatch().equals(DECISION.Permit)
                && resourcesMatch.getMatch().equals(DECISION.Permit)
                && actionsMatch.getMatch().equals(DECISION.Permit)
                && environmentsMatch.getMatch().equals(DECISION.Permit)) {
            retVal = Match.PERMIT;
        } else if (subjectsMatch.getMatch().equals(DECISION.Indeterminate)) {
            retVal = Match.indeterminate(subjectsMatch.getStatusCode());
        } else if (resourcesMatch.getMatch().equals(DECISION.Indeterminate)) {
            retVal = Match.indeterminate(resourcesMatch.getStatusCode());
        } else if (actionsMatch.getMatch().equals(DECISION.Indeterminate)) {
            retVal = Match.indeterminate(actionsMatch.getStatusCode());
        } else if (environmentsMatch.getMatch().equals(DECISION.Indeterminate)) {
            retVal = Match.indeterminate(environmentsMatch.getStatusCode());
        }
        return retVal;
    }
}
