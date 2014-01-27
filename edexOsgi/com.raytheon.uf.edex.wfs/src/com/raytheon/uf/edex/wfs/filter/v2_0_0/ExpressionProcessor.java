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
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBElement;

/**
 * Top level visitor pattern support class for expressions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class ExpressionProcessor {

    protected static final Map<String, AbsExpressionOp> expressionMap;
    static {
        expressionMap = new HashMap<String, AbsExpressionOp>();
        expressionMap.put("Literal", new AbsExpressionOp.Literal());
        expressionMap.put("ValueReference", new AbsExpressionOp.ValueRef());
        expressionMap.put("Function", new AbsExpressionOp.Function());
    }

    protected JAXBElement<?> expression;

    /**
     * @param expression
     */
    public ExpressionProcessor(JAXBElement<?> expression) {
        this.expression = expression;
    }

    /**
     * Entry point for visitor
     * 
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public Object accept(IExpressionVisitor visitor, Object obj)
            throws Exception {
        AbsExpressionOp op = expressionMap.get(expression.getName()
                .getLocalPart());
        return op.visit(expression, visitor, obj);
    }

}
