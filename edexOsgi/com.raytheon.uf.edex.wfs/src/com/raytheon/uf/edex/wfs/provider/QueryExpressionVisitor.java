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
package com.raytheon.uf.edex.wfs.provider;

import java.util.List;

import net.opengis.filter.v_1_1_0.PropertyNameType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.wfs.filter.ExpressionProcessor;
import com.raytheon.uf.edex.wfs.filter.OgcExpressionVisitor;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class QueryExpressionVisitor implements OgcExpressionVisitor {

	protected Log log = LogFactory.getLog(this.getClass());

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#add(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object add(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: add");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#sub(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object sub(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: sub");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#mul(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object mul(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: mul");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#div(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object div(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: div");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#literal(java.util.List,
	 * java.lang.Object)
	 */
	@Override
	public Object literal(List<Object> values, Object obj) throws Exception {
		if (values.size() != 1) {
			log.warn("Unsupported literal values: " + values.toArray());
		}
		return values.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#property(net.opengis
	 * .filter.v_1_1_0.PropertyNameType, java.lang.Object)
	 */
	@Override
	public Object property(PropertyNameType prop, Object obj) throws Exception {
		List<Object> content = prop.getContent();
		if (content.size() != 1) {
			log.warn("Unsupported property name values: " + content.toArray());
		}
		return content.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#function(java.util.List,
	 * java.lang.String, java.lang.Object)
	 */
	@Override
	public Object function(List<ExpressionProcessor> expressions, String name,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression type: function");
	}

}
