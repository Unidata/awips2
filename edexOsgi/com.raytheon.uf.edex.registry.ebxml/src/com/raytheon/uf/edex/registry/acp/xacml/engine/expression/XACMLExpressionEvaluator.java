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
package com.raytheon.uf.edex.registry.acp.xacml.engine.expression;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.policy.ExpressionType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * Class used to evaluate policy expressions.
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
public class XACMLExpressionEvaluator {

    /** Map of known expression types keyed on the ID of the expression type */
    private static Map<String, XACMLExpression> expressionMap = new HashMap<String, XACMLExpression>();

    /** The singleton instance */
    private static XACMLExpressionEvaluator instance = new XACMLExpressionEvaluator();

    private List<XACMLExpression> expressionList;

    /**
     * Gets the singleton instance of XACMLExpressionEvaluator
     * 
     * @return The singleton instance of XACMLExpressionEvaluator
     */
    public static XACMLExpressionEvaluator getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private XACMLExpressionEvaluator() {

    }

    /**
     * Evaluates the provided expression
     * 
     * @param <T>
     *            Object type
     * @param expression
     *            The expression to be evaluated
     * @param request
     *            The request object
     * @return The result of evaluating the expression
     * @throws XACMLException
     *             If errors occur during evaluation of the expression
     */
    @SuppressWarnings("unchecked")
    public <T> T evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        XACMLExpression expr = expressionMap.get(expression.getClass()
                .getName());
        if (expr == null) {
            throw new IllegalArgumentException("Unknown expression type ["
                    + expression.getClass().getName());
        }
        return (T) expr.evaluate(expression, request);
    }

    public List<XACMLExpression> getExpressionList() {
        return expressionList;
    }

    public void setExpressionList(List<XACMLExpression> expressionList)
            throws Exception {
        this.expressionList = expressionList;
        for (XACMLExpression expression : expressionList) {
            expressionMap.put(expression.getExpressionId(), expression);
        }
    }

}
