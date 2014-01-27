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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.FunctionType;
import net.opengis.filter.v_2_0_0.LiteralType;

/**
 * Visitor pattern support classes for filter expression operations
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
public abstract class AbsExpressionOp {

    /**
     * @param element
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public abstract Object visit(JAXBElement<?> element,
            IExpressionVisitor visitor, Object obj) throws Exception;

    /**
     * processes literal values
     */
    public static class Literal extends AbsExpressionOp {
        @Override
        public Object visit(JAXBElement<?> element, IExpressionVisitor visitor,
                Object obj) throws Exception {
            LiteralType literal = (LiteralType) element.getValue();
            return visitor.literal(literal.getContent(), obj);
        }
    }

    /**
     * processes value references
     */
    public static class ValueRef extends AbsExpressionOp {
        @Override
        public Object visit(JAXBElement<?> element, IExpressionVisitor visitor,
                Object obj) throws Exception {
            Object value = element.getValue();
            return visitor.valueRef(value.toString(), obj);
        }

    }

    /**
     * processes custom functions
     */
    public static class Function extends AbsExpressionOp {
        @Override
        public FilterFunction visit(JAXBElement<?> element,
                IExpressionVisitor visitor, Object obj) throws Exception {
            FunctionType f = (FunctionType) element.getValue();
            String name = f.getName();
            List<JAXBElement<?>> exprs = f.getExpression();
            List<ExpressionProcessor> procs = new ArrayList<ExpressionProcessor>(
                    exprs.size());
            for (JAXBElement<?> expr : exprs) {
                procs.add(new ExpressionProcessor(expr));
            }
            return visitor.function(procs, name, obj);
        }
    }
}
