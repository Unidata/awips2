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
import org.opensaml.xacml.ctx.StatusCodeType;
import org.opensaml.xacml.policy.ConditionType;
import org.opensaml.xacml.policy.impl.ConditionTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLNotApplicableException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation used to evaluate the <Condition> element of a policy
 * <p>
 * Section 7.8 of the XACML 2.0 core spec defines evaluation of the <Condition>
 * element as follows<br>
 * The condition value SHALL be "True" if the <Condition> element is absent, or
 * if it evaluates to "True". Its value SHALL be "False" if the <Condition>
 * element evaluates to "False". The condition value SHALL be "Indeterminate",
 * if the expression contained in the <Condtion> element evaluates to
 * "Indeterminate."
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
public class ConditionEvaluator extends ElementEvaluator<ConditionType> {

    public String getId() {
        return ConditionTypeImpl.class.getName();
    }

    @Override
    public Match evaluate(ConditionType condition) {
        Match retVal = null;
        Boolean result = null;
        String statusCode = StatusCodeType.SC_OK;
        try {
            result = evaluateExpression(condition);
        } catch (Exception e) {
            e.printStackTrace();
            if (e instanceof XACMLException) {
                statusCode = ((XACMLException) e).getStatusCode();
                if (e instanceof XACMLNotApplicableException) {
                    result = false;
                }
            } else {
                statusCode = StatusCodeType.SC_PROCESSING_ERROR;
            }
        }
        if (result == null) {
            retVal = new Match(DECISION.Indeterminate, statusCode);
        } else if (result) {
            retVal = Match.PERMIT;
        } else {
            retVal = Match.DENY;
        }
        return retVal;
    }
}
