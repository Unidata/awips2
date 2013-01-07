/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.filter;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.BinaryOperatorType;
import net.opengis.filter.v_1_1_0.FunctionType;
import net.opengis.filter.v_1_1_0.LiteralType;
import net.opengis.filter.v_1_1_0.PropertyNameType;

/**
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
