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
import org.opensaml.xacml.policy.impl.VariableReferenceTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * The <VariableReference> element is used to reference a value defined within
 * the same encompassing <Policy> element. The <VariableReference> element SHALL
 * refer to the <VariableDefinition> element by string equality on the value of
 * their respective VariableId attributes. There SHALL exist one and only one
 * <VariableDefinition> within the same encompassing <Policy> element to which
 * the <VariableReference> refers. There MAY be zero or more <VariableReference>
 * elements that refer to the same <VariableDefinition> element.
 * <p>
 * The <VariableReference> element is of the VariableReferenceType complex type,
 * which is of the ExpressionType complex type and is a member of the
 * <Expression> element substitution group. The <VariableReference> element MAY
 * appear any place where an <Expression> element occurs in the schema.
 * <p>
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
public class VariableReferenceExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return VariableReferenceTypeImpl.class.getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        throw new UnsupportedOperationException("Expression ["
                + expression.getClass() + "] not implemented!");
    }

}
