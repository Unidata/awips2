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
 * Apr 20, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.filter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.BinaryComparisonOpType;
import net.opengis.filter.v_1_1_0.ComparisonOpsType;
import net.opengis.filter.v_1_1_0.PropertyIsBetweenType;
import net.opengis.filter.v_1_1_0.PropertyIsLikeType;
import net.opengis.filter.v_1_1_0.PropertyIsNullType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractCompOp {

	protected static final Map<String, AbstractCompOp> binaryMap;

	protected Log log = LogFactory.getLog(this.getClass());

	static {
		binaryMap = new HashMap<String, AbstractCompOp>();
		binaryMap.put("PropertyIsEqualTo", new Equal());
		binaryMap.put("PropertyIsNotEqualTo", new NotEqual());
		binaryMap.put("PropertyIsLessThan", new LessThan());
		binaryMap.put("PropertyIsGreaterThan", new GreaterThan());
		binaryMap.put("PropertyIsLessThanOrEqualTo", new LessThanEqual());
		binaryMap.put("PropertyIsGreaterThanOrEqualTo", new GreaterThanEqual());
	}

	public abstract Object visit(JAXBElement<? extends ComparisonOpsType> op,
			OgcFilterVisitor visitor, Object obj) throws Exception;

	public static class Like extends AbstractCompOp {
		@Override
		public Object visit(JAXBElement<? extends ComparisonOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			return visitor.isLike((PropertyIsLikeType) op.getValue(), obj);
		}
	}

	public static class Null extends AbstractCompOp {
		@Override
		public Object visit(JAXBElement<? extends ComparisonOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			return visitor.isNull((PropertyIsNullType) op.getValue(), obj);
		}
	}

	public static class Between extends AbstractCompOp {
		@Override
		public Object visit(JAXBElement<? extends ComparisonOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
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

	public static class BinaryOp extends AbstractCompOp {
		@Override
		public Object visit(JAXBElement<? extends ComparisonOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
				OgcFilterVisitor visitor, Object obj) throws Exception {
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
