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
* Apr 2, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.wms.styling;

import java.util.Iterator;
import java.util.List;

import org.opengis.filter.And;
import org.opengis.filter.BinaryComparisonOperator;
import org.opengis.filter.BinaryLogicOperator;
import org.opengis.filter.ExcludeFilter;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterVisitor;
import org.opengis.filter.Id;
import org.opengis.filter.IncludeFilter;
import org.opengis.filter.Not;
import org.opengis.filter.Or;
import org.opengis.filter.PropertyIsBetween;
import org.opengis.filter.PropertyIsEqualTo;
import org.opengis.filter.PropertyIsGreaterThan;
import org.opengis.filter.PropertyIsGreaterThanOrEqualTo;
import org.opengis.filter.PropertyIsLessThan;
import org.opengis.filter.PropertyIsLessThanOrEqualTo;
import org.opengis.filter.PropertyIsLike;
import org.opengis.filter.PropertyIsNotEqualTo;
import org.opengis.filter.PropertyIsNull;
import org.opengis.filter.spatial.BBOX;
import org.opengis.filter.spatial.Beyond;
import org.opengis.filter.spatial.BinarySpatialOperator;
import org.opengis.filter.spatial.Contains;
import org.opengis.filter.spatial.Crosses;
import org.opengis.filter.spatial.DWithin;
import org.opengis.filter.spatial.Disjoint;
import org.opengis.filter.spatial.Equals;
import org.opengis.filter.spatial.Intersects;
import org.opengis.filter.spatial.Overlaps;
import org.opengis.filter.spatial.Touches;
import org.opengis.filter.spatial.Within;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public class FilterLabeler implements FilterVisitor {

	protected boolean shortVersion = true;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visitNullFilter(java.lang.Object)
	 */
	@Override
	public Object visitNullFilter(Object extraData) {
		return "null";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.ExcludeFilter,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(ExcludeFilter filter, Object extraData) {
		return filter.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.IncludeFilter,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(IncludeFilter filter, Object extraData) {
		return filter.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.And,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(And filter, Object extraData) {
		return binaryLogic(filter, extraData, "and");
	}

	protected String binaryLogic(BinaryLogicOperator filter, Object extraData,
			String op) {
		List<Filter> children = filter.getChildren();
		if (children == null || children.isEmpty()) {
			return "";
		}
		Iterator<Filter> i = children.iterator();
		Filter next = i.next();
		if (!i.hasNext()) {
			return (String) next.accept(this, extraData);
		}
		StringBuilder sb = new StringBuilder();
		sb.append(next.accept(this, extraData));
		while (i.hasNext()) {
			next = i.next();
			sb.append(" ").append(op).append(" ");
			sb.append(next.accept(this, extraData));
		}
		sb.append(" ");
		return sb.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.Id,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(Id filter, Object extraData) {
		return filter.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.Not,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(Not filter, Object extraData) {
		return "not " + filter.getFilter().accept(this, extraData);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.Or,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(Or filter, Object extraData) {
		return binaryLogic(filter, extraData, "or");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.PropertyIsBetween
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsBetween filter, Object extraData) {
		return filter.getExpression();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.PropertyIsEqualTo
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsEqualTo filter, Object extraData) {
		return binaryComp(filter, extraData, shortVersion ? "=" : "equal to");
	}

	protected Object binaryComp(BinaryComparisonOperator filter,
			Object extraData, String op) {
		String left = filter.getExpression1().toString();
		String right = filter.getExpression2().toString();
		return left + " " + op + " " + right;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.
	 * PropertyIsNotEqualTo, java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsNotEqualTo filter, Object extraData) {
		return binaryComp(filter, extraData, shortVersion ? "!="
				: "not equal to");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.
	 * PropertyIsGreaterThan, java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsGreaterThan filter, Object extraData) {
		return binaryComp(filter, extraData, shortVersion ? ">"
				: "greater than");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.
	 * PropertyIsGreaterThanOrEqualTo, java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsGreaterThanOrEqualTo filter, Object extraData) {
		return binaryComp(filter, extraData, shortVersion ? ">="
				: "greater than or equal to");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.PropertyIsLessThan
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsLessThan filter, Object extraData) {
		return binaryComp(filter, extraData, shortVersion ? "<" : "less than");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.opengis.filter.FilterVisitor#visit(org.opengis.filter.
	 * PropertyIsLessThanOrEqualTo, java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsLessThanOrEqualTo filter, Object extraData) {
		return binaryComp(filter, extraData, shortVersion ? "<="
				: "less than or equal to");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.PropertyIsLike,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsLike filter, Object extraData) {
		return filter.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.PropertyIsNull,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(PropertyIsNull filter, Object extraData) {
		String prop = filter.getExpression().toString();
		return prop + " is null";
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.BBOX,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(BBOX filter, Object extraData) {
		return binarySpatial(filter, extraData, "within");
	}

	protected Object binarySpatial(BinarySpatialOperator filter,
			Object extraData, String op) {
		String left = filter.getExpression1().toString();
		String right = filter.getExpression2().toString();
		return left + " " + op + " " + right;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Beyond,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(Beyond filter, Object extraData) {
		return binarySpatial(filter, extraData, "beyond");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Contains
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(Contains filter, Object extraData) {
		return binarySpatial(filter, extraData, "contains");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Crosses
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(Crosses filter, Object extraData) {
		return binarySpatial(filter, extraData, "crosses");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Disjoint
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(Disjoint filter, Object extraData) {
		return binarySpatial(filter, extraData, "disjoint");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.DWithin
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(DWithin filter, Object extraData) {
		return binarySpatial(filter, extraData, "distance within");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Equals,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(Equals filter, Object extraData) {
		return binarySpatial(filter, extraData, "equals");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Intersects
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(Intersects filter, Object extraData) {
		return binarySpatial(filter, extraData, "intersects");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Overlaps
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(Overlaps filter, Object extraData) {
		return binarySpatial(filter, extraData, "overlaps");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Touches
	 * , java.lang.Object)
	 */
	@Override
	public Object visit(Touches filter, Object extraData) {
		return binarySpatial(filter, extraData, "touches");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.opengis.filter.FilterVisitor#visit(org.opengis.filter.spatial.Within,
	 * java.lang.Object)
	 */
	@Override
	public Object visit(Within filter, Object extraData) {
		return binarySpatial(filter, extraData, "within");
	}

	/**
	 * @return the shortVersion
	 */
	public boolean isShortVersion() {
		return shortVersion;
	}

	/**
	 * @param shortVersion
	 *            the shortVersion to set
	 */
	public void setShortVersion(boolean shortVersion) {
		this.shortVersion = shortVersion;
	}

}
