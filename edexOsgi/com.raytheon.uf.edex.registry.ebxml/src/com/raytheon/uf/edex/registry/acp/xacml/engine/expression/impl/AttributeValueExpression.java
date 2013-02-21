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
import org.opensaml.xacml.policy.AttributeValueType;
import org.opensaml.xacml.policy.ExpressionType;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.DataTypes;
import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil;

/**
 * 
 * The <xacml:AttributeValue> element SHALL contain a literal attribute value.
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
public class AttributeValueExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return org.opensaml.xacml.policy.impl.AttributeValueTypeImpl.class
                .getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        AttributeValueType attr = (AttributeValueType) expression;
        return DataTypes
                .castDataType(attr.getValue(), XACMLObjectUtil.validateElement(
                        attr.getDataType(), "DataType"));
    }

}
