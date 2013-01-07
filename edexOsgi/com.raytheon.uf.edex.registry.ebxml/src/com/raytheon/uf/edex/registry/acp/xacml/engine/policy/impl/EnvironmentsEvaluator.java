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

import org.opensaml.xacml.policy.EnvironmentType;
import org.opensaml.xacml.policy.EnvironmentsType;
import org.opensaml.xacml.policy.impl.EnvironmentsTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation for evaluating the <Environments> element of a
 * policy
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
public class EnvironmentsEvaluator extends ElementEvaluator<EnvironmentsType> {

    public String getId() {
        return EnvironmentsTypeImpl.class.getName();
    }

    @Override
    protected Match evaluate(EnvironmentsType environments)
            throws XACMLProcessingException {
        Match retVal = Match.PERMIT;
        if (environments != null) {

            List<EnvironmentType> environmentTypes = environments
                    .getEnvrionments();
            Match[] matches = new Match[environmentTypes.size()];
            for (int i = 0; i < environmentTypes.size(); i++) {
                matches[i] = evaluateElement(environmentTypes.get(i));
            }
            retVal = applyPolicyElementTruthTable(matches);
        }
        return retVal;
    }

}
