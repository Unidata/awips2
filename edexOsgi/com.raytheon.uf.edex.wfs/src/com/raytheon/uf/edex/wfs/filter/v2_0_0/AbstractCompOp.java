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
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.BinaryComparisonOpType;
import net.opengis.filter.v_2_0_0.ComparisonOpsType;
import net.opengis.filter.v_2_0_0.PropertyIsBetweenType;
import net.opengis.filter.v_2_0_0.PropertyIsLikeType;
import net.opengis.filter.v_2_0_0.PropertyIsNilType;
import net.opengis.filter.v_2_0_0.PropertyIsNullType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Visitor pattern support classes for comparison operators
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
public abstract class AbstractCompOp {

    protected static final Map<String, AbstractCompOp> binaryMap;

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    static {
        binaryMap = new HashMap<String, AbstractCompOp>();
        binaryMap.put("PropertyIsEqualTo", new Equal());
        binaryMap.put("PropertyIsNotEqualTo", new NotEqual());
        binaryMap.put("PropertyIsLessThan", new LessThan());
        binaryMap.put("PropertyIsGreaterThan", new GreaterThan());
        binaryMap.put("PropertyIsLessThanOrEqualTo", new LessThanEqual());
        binaryMap.put("PropertyIsGreaterThanOrEqualTo", new GreaterThanEqual());
    }

    /**
     * @param op
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public abstract Object visit(JAXBElement<? extends ComparisonOpsType> op,
            IFilter2Visitor visitor, Object obj) throws Exception;

    /**
     * like expression akin to SQL LIKE
     */
    public static class Like extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            return visitor.isLike((PropertyIsLikeType) op.getValue(), obj);
        }
    }

    /**
     * test if property is null
     */
    public static class Null extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            return visitor.isNull((PropertyIsNullType) op.getValue(), obj);
        }
    }

    /**
     * test if property is nil
     */
    public static class Nil extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            return visitor.isNil((PropertyIsNilType) op.getValue(), obj);
        }
    }

    /**
     * test if property's value is between two values
     */
    public static class Between extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            PropertyIsBetweenType between = (PropertyIsBetweenType) op
                    .getValue();
            ExpressionProcessor exp = new ExpressionProcessor(
                    between.getExpression());
            ExpressionProcessor lower = new ExpressionProcessor(between
                    .getLowerBoundary().getExpression());
            ExpressionProcessor upper = new ExpressionProcessor(between
                    .getUpperBoundary().getExpression());
            return visitor.between(lower, exp, upper, obj);
        }
    }

    /**
     * top level binary operator visitation site, delegates to other classes
     */
    public static class BinaryOp extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            String name = op.getName().getLocalPart();
            AbstractCompOp compOp = binaryMap.get(name);
            if (compOp != null) {
                return compOp.visit(op, visitor, obj);
            } else {
                throw new Exception("Unknown binary operator: " + name);
            }
        }
    }

    public static class Equal extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryComparisonOpType binary = (BinaryComparisonOpType) op
                    .getValue();
            List<JAXBElement<?>> expressions = binary.getExpression();
            return visitor.equal(new ExpressionProcessor(expressions.get(0)),
                    new ExpressionProcessor(expressions.get(1)),
                    binary.isMatchCase(), obj);
        }
    }

    public static class NotEqual extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryComparisonOpType binary = (BinaryComparisonOpType) op
                    .getValue();
            List<JAXBElement<?>> expressions = binary.getExpression();
            return visitor.notEqual(
                    new ExpressionProcessor(expressions.get(0)),
                    new ExpressionProcessor(expressions.get(1)),
                    binary.isMatchCase(), obj);
        }
    }

    public static class LessThan extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryComparisonOpType binary = (BinaryComparisonOpType) op
                    .getValue();
            List<JAXBElement<?>> expressions = binary.getExpression();
            return visitor.lessThan(
                    new ExpressionProcessor(expressions.get(0)),
                    new ExpressionProcessor(expressions.get(1)),
                    binary.isMatchCase(), obj);
        }
    }

    public static class GreaterThan extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryComparisonOpType binary = (BinaryComparisonOpType) op
                    .getValue();
            List<JAXBElement<?>> expressions = binary.getExpression();
            return visitor.greaterThan(
                    new ExpressionProcessor(expressions.get(0)),
                    new ExpressionProcessor(expressions.get(1)),
                    binary.isMatchCase(), obj);
        }
    }

    public static class LessThanEqual extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryComparisonOpType binary = (BinaryComparisonOpType) op
                    .getValue();
            List<JAXBElement<?>> expressions = binary.getExpression();
            return visitor.lessThanEqual(
                    new ExpressionProcessor(expressions.get(0)),
                    new ExpressionProcessor(expressions.get(1)),
                    binary.isMatchCase(), obj);
        }
    }

    public static class GreaterThanEqual extends AbstractCompOp {
        @Override
        public Object visit(JAXBElement<? extends ComparisonOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryComparisonOpType binary = (BinaryComparisonOpType) op
                    .getValue();
            List<JAXBElement<?>> expressions = binary.getExpression();
            return visitor.greaterThanEqual(
                    new ExpressionProcessor(expressions.get(0)),
                    new ExpressionProcessor(expressions.get(1)),
                    binary.isMatchCase(), obj);
        }
    }

}
