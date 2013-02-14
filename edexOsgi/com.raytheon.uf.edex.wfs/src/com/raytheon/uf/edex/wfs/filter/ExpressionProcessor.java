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
 * Apr 21, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.filter;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBElement;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class ExpressionProcessor {

	protected static final Map<String, AbsExpressionOp> expressionMap;
	static {
		expressionMap = new HashMap<String, AbsExpressionOp>();
		expressionMap.put("Literal", new AbsExpressionOp.Literal());
		expressionMap.put("Add", new AbsExpressionOp.Add());
		expressionMap.put("Sub", new AbsExpressionOp.Sub());
		expressionMap.put("Mul", new AbsExpressionOp.Mul());
		expressionMap.put("Div", new AbsExpressionOp.Div());
		expressionMap.put("Function", new AbsExpressionOp.Function());
		expressionMap.put("PropertyName", new AbsExpressionOp.Property());
	}

	JAXBElement<?> expression;

	/**
	 * @param expression
	 */
	public ExpressionProcessor(JAXBElement<?> expression) {
		super();
		this.expression = expression;
	}

	public Object accept(OgcExpressionVisitor visitor, Object obj)
			throws Exception {
		AbsExpressionOp op = expressionMap.get(expression.getName()
				.getLocalPart());
		return op.visit(expression, visitor, obj);
	}

}
