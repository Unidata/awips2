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
import org.opensaml.xacml.policy.AttributeAssignmentType;
import org.opensaml.xacml.policy.ExpressionType;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.DataTypes;
import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * The <AttributeAssignment> element is used for including arguments in
 * obligations. It SHALL contain an AttributeId and the corresponding attribute
 * value, by extending the AttributeValueType type definition. The
 * <AttributeAssignment> element MAY be used in any way that is consistent with
 * the schema syntax, which is a sequence of <xs:any> elements. The value
 * specified SHALL be understood by the PEP, but it is not further specified by
 * XACML. See Section 7.14. Section 4.2.4.3 provides a number of examples of
 * arguments included in obligations.
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
public class AttributeAssignmentExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return org.opensaml.xacml.policy.impl.AttributeAssignmentTypeImpl.class
                .getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        AttributeAssignmentType attrAssignment = (AttributeAssignmentType) expression;
        // String attrId = attrAssignment.getAttributeId();
        String attrDataType = attrAssignment.getDataType();
        String attrValue = attrAssignment.getValue();

        // TODO: Handle unknown attributes and objects
        // AttributeMap unknownAttributes =
        // attrAssignment.getUnknownAttributes();
        // List<XMLObject> unknownObjects =
        // attrAssignment.getUnknownXMLObjects();

        return DataTypes.castDataType(attrValue, attrDataType);
    }

}
