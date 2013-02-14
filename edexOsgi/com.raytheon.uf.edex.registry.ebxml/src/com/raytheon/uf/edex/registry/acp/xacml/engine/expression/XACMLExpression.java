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

import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.policy.ExpressionType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * Interface used by expression implementations.
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
public interface XACMLExpression {

    /** Gets the ID of this expression */
    public String getExpressionId() throws Exception;

    /**
     * Evaluates this expression
     * 
     * @param expression
     *            The XACML expression to be evaluated
     * @param request
     *            The current request object
     * @return The result of evaluation of the expression
     * @throws XACMLException
     *             If errors occur while evaluating the expression
     */
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException;

}
