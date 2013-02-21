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

import java.util.List;

import net.opengis.filter.v_1_1_0.BBOXType;
import net.opengis.filter.v_1_1_0.BinarySpatialOpType;
import net.opengis.filter.v_1_1_0.DistanceBufferType;
import net.opengis.filter.v_1_1_0.PropertyIsLikeType;
import net.opengis.filter.v_1_1_0.PropertyIsNullType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public interface OgcFilterVisitor {

	// comparison

	public Object equal(ExpressionProcessor left, ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception;

	public Object notEqual(ExpressionProcessor left, ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception;

	public Object lessThan(ExpressionProcessor left, ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception;

	public Object greaterThan(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception;

	public Object greaterThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception;

	public Object lessThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception;

	public Object isLike(PropertyIsLikeType op, Object obj) throws Exception;

	public Object isNull(PropertyIsNullType op, Object obj) throws Exception;

	public Object between(ExpressionProcessor lower, ExpressionProcessor exp,
			ExpressionProcessor upper, Object obj) throws Exception;

	// logic

	public Object and(List<FilterProcessor> filters, Object obj)
			throws Exception;

	public Object or(List<FilterProcessor> filters, Object obj)
			throws Exception;

	public Object not(FilterProcessor filter, Object obj) throws Exception;

	// spatial

	public Object spatialEquals(BinarySpatialOpType op, Object obj)
			throws Exception;

	public Object disjoint(BinarySpatialOpType op, Object obj) throws Exception;

	public Object touches(BinarySpatialOpType op, Object obj) throws Exception;

	public Object within(BinarySpatialOpType op, Object obj) throws Exception;

	public Object overlaps(BinarySpatialOpType op, Object obj) throws Exception;

	public Object crosses(BinarySpatialOpType op, Object obj) throws Exception;

	public Object intersects(BinarySpatialOpType op, Object obj)
			throws Exception;

	public Object contains(BinarySpatialOpType op, Object obj) throws Exception;

	public Object dWithin(DistanceBufferType op, Object obj) throws Exception;

	public Object beyond(DistanceBufferType op, Object obj) throws Exception;

	public Object bbox(BBOXType op, Object obj) throws Exception;

}
