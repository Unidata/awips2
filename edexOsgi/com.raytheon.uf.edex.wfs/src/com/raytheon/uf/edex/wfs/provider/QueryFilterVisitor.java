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

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.BBOXType;
import net.opengis.filter.v_1_1_0.BinarySpatialOpType;
import net.opengis.filter.v_1_1_0.DistanceBufferType;
import net.opengis.filter.v_1_1_0.PropertyIsLikeType;
import net.opengis.filter.v_1_1_0.PropertyIsNullType;
import net.opengis.gml.v_3_1_1.AbstractGeometryType;
import net.opengis.gml.v_3_1_1.EnvelopeType;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.jts.JTS;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Junction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.hibernatespatial.criterion.SpatialRestrictions;

import com.raytheon.uf.common.util.ConvertUtil;
import com.raytheon.uf.edex.wfs.filter.ExpressionProcessor;
import com.raytheon.uf.edex.wfs.filter.FilterProcessor;
import com.raytheon.uf.edex.wfs.filter.OgcFilterVisitor;
import com.raytheon.uf.edex.wfs.gml.EnvelopeConverter;
import com.raytheon.uf.edex.wfs.gml.GeometryConverter;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class QueryFilterVisitor implements OgcFilterVisitor {

	protected Log log = LogFactory.getLog(this.getClass());

	protected GeometryConverter geomConverter = new GeometryConverter();

	protected EnvelopeConverter envConverter = new EnvelopeConverter();

	protected QueryExpressionVisitor exprVisitor = new QueryExpressionVisitor();

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#equal(com.raytheon.uf.edex
	 * .filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
	 * java.lang.Object)
	 */
	@Override
	public Criterion equal(ExpressionProcessor left,
			ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Object> e = getBinaryProps(left, right, bag);
		return Restrictions.eq(e.getKey(), e.getValue());
	}

	protected Entry<String, Object> getBinaryProps(ExpressionProcessor left,
			ExpressionProcessor right, VisitorBag bag) throws Exception {
		String prop = (String) left.accept(exprVisitor, bag);
		String value = (String) right.accept(exprVisitor, bag);
		Class<?> ent = bag.getRootEntity();
		String[] path = parseProp(prop);
		Object val = ConvertUtil.convertAsType(value, ent, path);
		String field = StringUtils.join(path, ".");
		return new SimpleEntry<String, Object>(bag.filterField(field), val);
	}

	protected String[] parseProp(String prop) {
		// TODO we may want to keep the namespaces
		String[] rval = prop.trim().split("\\/");
		for (int i = 0; i < rval.length; ++i) {
			int index = rval[i].lastIndexOf(':');
			if (index > -1) {
				rval[i] = rval[i].substring(index + 1);
			}
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#notEqual(com.raytheon.uf
	 * .edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
	 * java.lang.Object)
	 */
	@Override
	public Criterion notEqual(ExpressionProcessor left,
			ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Object> e = getBinaryProps(left, right, bag);
		return Restrictions.ne(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#lessThan(com.raytheon.uf
	 * .edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
	 * java.lang.Object)
	 */
	@Override
	public Criterion lessThan(ExpressionProcessor left,
			ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Object> e = getBinaryProps(left, right, bag);
		return Restrictions.lt(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#greaterThan(com.raytheon
	 * .uf.edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
	 * java.lang.Object)
	 */
	@Override
	public Criterion greaterThan(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Object> e = getBinaryProps(left, right, bag);
		return Restrictions.gt(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#greaterThanEqual(com.raytheon
	 * .uf.edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
	 * java.lang.Object)
	 */
	@Override
	public Criterion greaterThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Object> e = getBinaryProps(left, right, bag);
		return Restrictions.ge(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#lessThanEqual(com.raytheon
	 * .uf.edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
	 * java.lang.Object)
	 */
	@Override
	public Criterion lessThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Object> e = getBinaryProps(left, right, bag);
		return Restrictions.le(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#isLike(net.opengis.filter
	 * .v_1_1_0.PropertyIsLikeType, java.lang.Object)
	 */
	@Override
	public Criterion isLike(PropertyIsLikeType op, Object obj)
			throws Exception {
		// FIXME this is not correct, needs to take wildcard, anychar and
		// escapes into account
		VisitorBag bag = (VisitorBag) obj;
		String prop = (String) op.getPropertyName().getContent().get(0);
		String value = (String) op.getLiteral().getContent().get(0);
		return Restrictions.like(bag.filterField(prop), value,
				MatchMode.ANYWHERE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#isNull(net.opengis.filter
	 * .v_1_1_0.PropertyIsNullType, java.lang.Object)
	 */
	@Override
	public Criterion isNull(PropertyIsNullType op, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		String field = (String) op.getPropertyName().getContent().get(0);
		return Restrictions.isNull(bag.filterField(field));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#between(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Criterion between(ExpressionProcessor lower,
			ExpressionProcessor exp,
			ExpressionProcessor upper, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;

		Entry<String, Object> lowerPart = getBinaryProps(exp, lower, bag);
		Entry<String, Object> upperPart = getBinaryProps(exp, upper, bag);

		return Restrictions.between(lowerPart.getKey(), lowerPart.getValue(),
				upperPart.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.filter.OgcFilterVisitor#and(java.util.List,
	 * java.lang.Object)
	 */
	@Override
	public Criterion and(List<FilterProcessor> filters, Object obj)
			throws Exception {
		Conjunction rval = Restrictions.conjunction();
		acceptAll(filters, obj, rval);
		return rval;
	}

	protected void acceptAll(List<FilterProcessor> filters, Object obj,
			Junction junc) throws Exception {
		Iterator<FilterProcessor> i = filters.iterator();
		while (i.hasNext()) {
			junc.add((Criterion) i.next().accept(this, obj));
		}
	}

	protected <T> List<T> addAll(List<T> l1, List<T> l2) {
		if (l1 == null) {
			return l2;
		}
		if (l2 == null) {
			return l1;
		}
		ArrayList<T> rval = new ArrayList<T>(l1.size() + l2.size());
		rval.addAll(l1);
		rval.addAll(l2);
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.filter.OgcFilterVisitor#or(java.util.List,
	 * java.lang.Object)
	 */
	@Override
	public Criterion or(List<FilterProcessor> filters, Object obj)
			throws Exception {
		Disjunction rval = Restrictions.disjunction();
		acceptAll(filters, obj, rval);
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#not(com.raytheon.uf.edex
	 * .filter.FilterProcessor, java.lang.Object)
	 */
	@Override
	public Criterion not(FilterProcessor filter, Object obj) throws Exception {
		return Restrictions.not((Criterion) filter.accept(this, obj));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#spatialEquals(net.opengis
	 * .filter.v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion spatialEquals(BinarySpatialOpType op, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Geometry> e = getBinarySpatial(op, bag);
		return SpatialRestrictions.eq(bag.filterField(e.getKey()), e.getValue());
	}


	protected Entry<String, Geometry> getBinarySpatial(
			BinarySpatialOpType binary, VisitorBag bag) throws Exception {
		List<Object> lst = binary.getPropertyName().getContent();
		String str = getStringWarn(lst, "Unsupported property name type");
		String prop = StringUtils.join(parseProp(str), '.');
		Geometry shape = getGeometry(binary);
		return new SimpleEntry<String, Geometry>(bag.filterField(prop), shape);
	}

	protected Geometry getGeometry(BinarySpatialOpType binary) throws Exception {
		JAXBElement<EnvelopeType> env = binary.getEnvelope();
		JAXBElement<AbstractGeometryType> geom = binary.getGeometry();
		Geometry shape;
		if (env != null && !env.isNil()) {
			Envelope envelope = envConverter.convert(env.getValue());
			shape = geomConverter.convert(envelope);
		} else if (geom != null && !geom.isNil()) {
			shape = geomConverter.convert(geom.getValue());
		} else {
			throw new Exception("Unsupported geometry format");
		}
		return shape;
	}

	protected String getStringWarn(List<Object> lst, String msg) {
		if (lst.size() != 1) {
			log.warn(msg);
		}
		return (String) lst.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#disjoint(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion disjoint(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.disjoint(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#touches(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion touches(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.touches(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#within(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion within(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.within(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#overlaps(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion overlaps(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.overlaps(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#crosses(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion crosses(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.crosses(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#intersects(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion intersects(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.intersects(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#contains(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion contains(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.contains(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#dWithin(net.opengis.filter
	 * .v_1_1_0.DistanceBufferType, java.lang.Object)
	 */
	@Override
	public Object dWithin(DistanceBufferType op, Object obj)
			throws Exception {
		throw new Exception("dWithin queries not supported");
	}

	// public RangeParameter getDistance(DistanceBufferType dist, RangeOperand
	// op,
	// VisitorBag bag) throws Exception {
	// List<Object> lst = dist.getPropertyName().getContent();
	// String str = getStringWarn(lst, "Unsupported property name type");
	// String prop = StringUtils.join(parseProp(str), '.');
	// AbstractGeometryType geom = dist.getGeometry().getValue();
	// Geometry res = geomConverter.convert(geom);
	// if (!(res instanceof Point)) {
	// throw new Exception("Unsupported distance geometry"
	// + res.getClass());
	// }
	// double distance = 0;// FIXME jaxb classes do not contain distance value
	// RangeParameter param = new RangeParameter(prop, (Point) res, distance,
	// op);
	// throw new Exception("Distance types not supported");
	// // return rval;
	// }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#beyond(net.opengis.filter
	 * .v_1_1_0.DistanceBufferType, java.lang.Object)
	 */
	@Override
	public Object beyond(DistanceBufferType op, Object obj)
			throws Exception {
		throw new Exception("Beyond queries not supported");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#bbox(net.opengis.filter.
	 * v_1_1_0.BBOXType, java.lang.Object)
	 */
	@Override
	public Criterion bbox(BBOXType op, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		List<Object> lst = op.getPropertyName().getContent();
		String str = getStringWarn(lst, "Unsupported property name type");
		String prop = StringUtils.join(parseProp(str), '.');
		EnvelopeType value = op.getEnvelope().getValue();
		Envelope env = envConverter.convert(value);
		return SpatialRestrictions.within(bag.filterField(prop),
				JTS.toGeometry(env));
	}

}
