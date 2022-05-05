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
import org.opensaml.xacml.policy.ExpressionType;
import org.opensaml.xacml.policy.FunctionType;
import org.opensaml.xacml.policy.impl.FunctionTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * The <Function> element SHALL be used to name a function as an argument to the
 * function defined by the parent <Apply> element. In the case where the parent
 * <Apply> element is a higher-order bag function, the named function is applied
 * to every element of the bag or bags identified in the other arguments of the
 * parent element. The higher-order bag functions are described in Section
 * A3A.3.12.
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
public class FunctionExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return org.opensaml.xacml.policy.impl.FunctionTypeImpl.class.getName();
    }

    @Override
    public FunctionType evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        if (expression instanceof FunctionType) {
            return (FunctionType) expression;
        } else {
            throw new IllegalArgumentException(
                    "Illegal argument submitted to ["
                            + FunctionTypeImpl.class.getName() + "]. Expected "
                            + FunctionType.class.getName() + ". Got "
                            + expression.getClass().getName());
        }
    }

}
