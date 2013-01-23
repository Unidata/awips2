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

import java.util.List;

import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.policy.ApplyType;
import org.opensaml.xacml.policy.ExpressionType;
import org.opensaml.xacml.policy.impl.ApplyTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpressionEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.engine.function.XACMLFunctionEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * The <Apply> element denotes application of a function to its arguments, thus
 * encoding a function call. The <Apply> element can be applied to any
 * combination of the members of the <Expression> element substitution group.
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
public class ApplyExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return ApplyTypeImpl.class.getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        ApplyType apply = (ApplyType) expression;
        // Gets the function ID
        String functionId = apply.getFunctionId();
        List<ExpressionType> expressions = apply.getExpressions();
        Object[] args = new Object[expressions.size()];
        /*
         * Iterate over the expressions contained in the Apply expression and
         * assign them to an array to be passed as arguments to the function
         */
        for (int i = 0; i < expressions.size(); i++) {
            args[i] = XACMLExpressionEvaluator.getInstance().evaluate(
                    expressions.get(i), request);
        }
        return XACMLFunctionEvaluator.getInstance().evaluate(functionId, args,
                request);
    }

}
