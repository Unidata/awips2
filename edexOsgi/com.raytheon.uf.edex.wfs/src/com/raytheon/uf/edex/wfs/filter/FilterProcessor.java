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
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.BinaryComparisonOpType;
import net.opengis.filter.v_1_1_0.ComparisonOpsType;
import net.opengis.filter.v_1_1_0.FilterType;
import net.opengis.filter.v_1_1_0.LogicOpsType;
import net.opengis.filter.v_1_1_0.PropertyIsBetweenType;
import net.opengis.filter.v_1_1_0.PropertyIsLikeType;
import net.opengis.filter.v_1_1_0.PropertyIsNullType;
import net.opengis.filter.v_1_1_0.SpatialOpsType;

import com.raytheon.uf.edex.wfs.filter.AbstractCompOp.BinaryOp;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class FilterProcessor {

	protected static final Map<Class<? extends ComparisonOpsType>, AbstractCompOp> compMap;
	static {
		compMap = new HashMap<Class<? extends ComparisonOpsType>, AbstractCompOp>();
		compMap.put(BinaryComparisonOpType.class, new BinaryOp());
		compMap.put(PropertyIsLikeType.class, new AbstractCompOp.Like());
		compMap.put(PropertyIsNullType.class, new AbstractCompOp.Null());
		compMap.put(PropertyIsBetweenType.class, new AbstractCompOp.Between());
	}

	protected static final Map<String, AbstractLogicOp> logicMap;
	static {
		logicMap = new HashMap<String, AbstractLogicOp>();
		logicMap.put("And", new AbstractLogicOp.And());
		logicMap.put("Or", new AbstractLogicOp.Or());
		logicMap.put("Not", new AbstractLogicOp.Not());
	}

	protected static final Map<String, AbstractSpatialOp> spatialMap;
	static {
		spatialMap = new HashMap<String, AbstractSpatialOp>();
		spatialMap.put("Equals", new AbstractSpatialOp.SpatialEquals());
		spatialMap.put("Disjoint", new AbstractSpatialOp.Disjoint());
		spatialMap.put("Touches", new AbstractSpatialOp.Touches());
		spatialMap.put("Within", new AbstractSpatialOp.Within());
		spatialMap.put("Overlaps", new AbstractSpatialOp.Overlaps());
		spatialMap.put("Crosses", new AbstractSpatialOp.Crosses());
		spatialMap.put("Intersects", new AbstractSpatialOp.Intersects());
		spatialMap.put("Contains", new AbstractSpatialOp.Contains());
		spatialMap.put("DWithin", new AbstractSpatialOp.DWithin());
		spatialMap.put("Beyond", new AbstractSpatialOp.Beyond());
		spatialMap.put("BBOX", new AbstractSpatialOp.Bbox());
	}

	protected FilterType filter;

	public FilterProcessor(FilterType filter) {
		this.filter = filter;
	}

	public static FilterProcessor newFromLogic(
			JAXBElement<? extends LogicOpsType> ops) {
		FilterType f = new FilterType();
		f.setLogicOps(ops);
		return new FilterProcessor(f);
	}

	public static FilterProcessor newFromComparison(
			JAXBElement<? extends ComparisonOpsType> ops) {
		FilterType f = new FilterType();
		f.setComparisonOps(ops);
		return new FilterProcessor(f);
	}

	public static FilterProcessor newFromSpatial(
			JAXBElement<? extends SpatialOpsType> ops) {
		FilterType f = new FilterType();
		f.setSpatialOps(ops);
		return new FilterProcessor(f);
	}

	public Object accept(OgcFilterVisitor visitor, Object obj) throws Exception {
		JAXBElement<? extends ComparisonOpsType> comps = filter
				.getComparisonOps();
		JAXBElement<? extends LogicOpsType> logics = filter.getLogicOps();
		JAXBElement<? extends SpatialOpsType> spats = filter.getSpatialOps();
		Object rval;
		if (logics != null && !logics.isNil()) {
			String name = logics.getName().getLocalPart();
			AbstractLogicOp op = logicMap.get(name);
			rval = op.visit(logics, visitor, obj);
		} else if (comps != null && !comps.isNil()) {
			Class<? extends ComparisonOpsType> type = comps.getDeclaredType();
			AbstractCompOp op = compMap.get(type);
			rval = op.visit(comps, visitor, obj);
		} else if (spats != null && !spats.isNil()) {
			String name = spats.getName().getLocalPart();
			AbstractSpatialOp op = spatialMap.get(name);
			rval = op.visit(spats, visitor, obj);
		} else {
			rval = null;
		}
		return rval;
	}

}
