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
package com.raytheon.uf.edex.wfs.filter.v1_1_0;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.BinaryOperatorType;
import net.opengis.filter.v_1_1_0.FunctionType;
import net.opengis.filter.v_1_1_0.LiteralType;
import net.opengis.filter.v_1_1_0.PropertyNameType;

/**
 * OGC Filter parsing for expression operators.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbsExpressionOp {

	public abstract Object visit(JAXBElement<?> element,
			OgcExpressionVisitor visitor, Object obj) throws Exception;

	public static class Literal extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
			LiteralType literal = (LiteralType) element.getValue();
			return visitor.literal(literal.getContent(), obj);
		}
	}

	public static class Add extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
			BinaryOperatorType binary = (BinaryOperatorType) element.getValue();
			List<JAXBElement<?>> exprs = binary.getExpression();
			ExpressionProcessor left = new ExpressionProcessor(exprs.get(0));
			ExpressionProcessor right = new ExpressionProcessor(exprs.get(1));
			return visitor.add(left, right, obj);
		}
	}

	public static class Sub extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
			BinaryOperatorType binary = (BinaryOperatorType) element.getValue();
			List<JAXBElement<?>> exprs = binary.getExpression();
			ExpressionProcessor left = new ExpressionProcessor(exprs.get(0));
			ExpressionProcessor right = new ExpressionProcessor(exprs.get(1));
			return visitor.sub(left, right, obj);
		}
	}

	public static class Mul extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
			BinaryOperatorType binary = (BinaryOperatorType) element.getValue();
			List<JAXBElement<?>> exprs = binary.getExpression();
			ExpressionProcessor left = new ExpressionProcessor(exprs.get(0));
			ExpressionProcessor right = new ExpressionProcessor(exprs.get(1));
			return visitor.mul(left, right, obj);
		}
	}

	public static class Div extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
			BinaryOperatorType binary = (BinaryOperatorType) element.getValue();
			List<JAXBElement<?>> exprs = binary.getExpression();
			ExpressionProcessor left = new ExpressionProcessor(exprs.get(0));
			ExpressionProcessor right = new ExpressionProcessor(exprs.get(1));
			return visitor.div(left, right, obj);
		}
	}

	public static class Property extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
			PropertyNameType prop = (PropertyNameType) element.getValue();
			return visitor.property(prop, obj);
		}
	}

	public static class Function extends AbsExpressionOp {
		@Override
		public Object visit(JAXBElement<?> element,
				OgcExpressionVisitor visitor, Object obj) throws Exception {
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
