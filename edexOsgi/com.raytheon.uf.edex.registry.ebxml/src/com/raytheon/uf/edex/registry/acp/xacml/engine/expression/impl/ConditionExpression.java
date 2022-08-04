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
package com.raytheon.uf.edex.registry.acp.xacml.engine.expression.impl;

import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.policy.ConditionType;
import org.opensaml.xacml.policy.ExpressionType;

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpressionEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * The <Condition> element is a Boolean function over subject, resource, action
 * and environment attributes or functions of attributes.
 * 
 * The <Condition> contains one <Expression> element, with the restriction that
 * the <Expression> return data-type MUST be
 * “http://www.w3.org/2001/XMLSchema#boolean”. Evaluation of the <Condition>
 * element is described in Section 7.8.
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
public class ConditionExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return org.opensaml.xacml.policy.impl.ConditionTypeImpl.class.getName();
    }

    @Override
    public Boolean evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        return XACMLExpressionEvaluator.getInstance().evaluate(
                ((ConditionType) expression).getExpression(), request);
    }
}
