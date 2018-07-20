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

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpression;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * This element is currently not supported.
 * <p>
 * 
 * The <AttributeSelector> element identifies attributes by their location in
 * the request context. Support for the <AttributeSelector> element is OPTIONAL.
 * <p>
 * The <AttributeSelector> element's RequestContextPath XML attribute SHALL
 * contain a legal XPath expression whose context node is the
 * <xacml-context:Request> element. The AttributeSelector element SHALL evaluate
 * to a bag of values whose data-type is specified by the element’s DataType
 * attribute. If the DataType specified in the AttributeSelector is a primitive
 * data type defined in [XF] or [XS], then the value returned by the XPath
 * expression SHALL be converted to the DataType specified in the
 * <AttributeSelector> using the constructor function below [XF Section 4] that
 * corresponds to the DataType. If an error results from using the constructor
 * function, then the value of the <AttributeSelector> SHALL be "Indeterminate".
 * <p>
 * xs:string()<br>
 * xs:boolean()<br>
 * xs:integer()<br>
 * xs:double()<br>
 * xs:dateTime()<br>
 * xs:date()<br>
 * xs:time()<br>
 * xs:hexBinary()<br>
 * xs:base64Binary()<br>
 * xs:anyURI()<br>
 * xf:yearMonthDuration()<br>
 * xf:dayTimeDuration()<br>
 * <p>
 * If the DataType specified in the AttributeSelector is not one of the
 * preceding primitive DataTypes, then the AttributeSelector SHALL return a bag
 * of instances of the specified DataType. If an error occurs when converting
 * the values returned by the XPath expression to the specified DataType, then
 * the result of the AttributeSelector SHALL be "Indeterminate".
 * <p>
 * Each node selected by the specified XPath expression MUST be either a text
 * node, an attribute node, a processing instruction node or a comment node. The
 * string representation of the value of each node MUST be converted to an
 * attribute value of the specified data-type, and the result of the
 * AttributeSelector is the bag of the attribute values generated from all the
 * selected nodes.
 * <p>
 * If the node selected by the specified XPath expression is not one of those
 * listed above (i.e. a text node, an attribute node, a processing instruction
 * node or a comment node), then the result of the enclosing policy SHALL be
 * "Indeterminate" with a StatusCode value of
 * "urn:oasis:names:tc:xacml:1.0:status:syntax-error".
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
public class AttributeSelectorExpression implements XACMLExpression {

    @Override
    public String getExpressionId() throws Exception {
        return org.opensaml.xacml.policy.impl.AttributeSelectorTypeImpl.class
                .getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {
        throw new UnsupportedOperationException("Expression ["
                + expression.getClass() + "] not implemented!");
    }

}
