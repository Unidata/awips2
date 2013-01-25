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
 * Apr 25, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.gml;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.gml.v_3_1_1.AbstractCurveType;
import net.opengis.gml.v_3_1_1.AbstractGeometryType;
import net.opengis.gml.v_3_1_1.AbstractRingPropertyType;
import net.opengis.gml.v_3_1_1.AbstractRingType;
import net.opengis.gml.v_3_1_1.CoordType;
import net.opengis.gml.v_3_1_1.CurvePropertyType;
import net.opengis.gml.v_3_1_1.DirectPositionListType;
import net.opengis.gml.v_3_1_1.DirectPositionType;
import net.opengis.gml.v_3_1_1.LineStringPropertyType;
import net.opengis.gml.v_3_1_1.LineStringType;
import net.opengis.gml.v_3_1_1.LinearRingType;
import net.opengis.gml.v_3_1_1.MultiLineStringType;
import net.opengis.gml.v_3_1_1.MultiPointType;
import net.opengis.gml.v_3_1_1.MultiPolygonType;
import net.opengis.gml.v_3_1_1.PointArrayPropertyType;
import net.opengis.gml.v_3_1_1.PointPropertyType;
import net.opengis.gml.v_3_1_1.PointType;
import net.opengis.gml.v_3_1_1.PolygonPropertyType;
import net.opengis.gml.v_3_1_1.PolygonType;
import net.opengis.gml.v_3_1_1.RingType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class GmlWktWriter {

	protected static final Map<Class<? extends AbstractGeometryType>, GmlWktWriter> geomMap;
	static {
		geomMap = new HashMap<Class<? extends AbstractGeometryType>, GmlWktWriter>();
		geomMap.put(PolygonType.class, new Polygon());
		geomMap.put(MultiPolygonType.class, new MultiPolygon());
		geomMap.put(PointType.class, new Point());
		geomMap.put(MultiPointType.class, new MultiPoint());
		geomMap.put(LinearRingType.class, new LinearRing());
		geomMap.put(LineStringType.class, new LineString());
		geomMap.put(MultiLineStringType.class, new MultiLineString());
	}

	public abstract String write(AbstractGeometryType geom) throws Exception;

	public static String write(JAXBElement<AbstractGeometryType> elem)
			throws Exception {
		GmlWktWriter writer = geomMap.get(elem.getDeclaredType());
		if (writer != null) {
			return writer.write(elem.getValue());
		} else {
			throw new Exception("Unsupported geometry type: "
					+ elem.getDeclaredType());
		}
	}

	protected void addPolygon(PolygonType poly, StringBuilder sb)
			throws Exception {
		sb.append('(');
		AbstractRingPropertyType value = poly.getExterior().getValue();
		AbstractRingType ring = value.getRing().getValue();
		addAbstractRing(ring, sb);
		List<JAXBElement<AbstractRingPropertyType>> interior = poly
				.getInterior();
		if (interior != null && !interior.isEmpty()) {
			sb.append(',');
			addInterior(interior, sb);
		}
		sb.append(')');
	}

	/**
	 * @param interior
	 * @param sb
	 * @throws Exception
	 */
	protected void addInterior(
			List<JAXBElement<AbstractRingPropertyType>> interior,
			StringBuilder sb) throws Exception {
		Iterator<JAXBElement<AbstractRingPropertyType>> i = interior.iterator();
		sb.append('(');
		addAbstractRing(i.next().getValue().getRing().getValue(), sb);
		while (i.hasNext()) {
			sb.append(',');
			addAbstractRing(i.next().getValue().getRing().getValue(), sb);
		}
	}

	protected void addAbstractRing(AbstractRingType ring, StringBuilder sb)
			throws Exception {
		if (ring instanceof LinearRingType) {
			addLinearRing((LinearRingType) ring, sb);
		} else if (ring instanceof RingType) {
			addRing((RingType) ring, sb);
		} else {
			throw new Exception("Unsupported ring type" + ring.getClass());
		}
	}

	/**
	 * @param ring
	 * @param sb
	 * @throws Exception
	 */
	private void addRing(RingType ring, StringBuilder sb) throws Exception {
		List<CurvePropertyType> curvProps = ring.getCurveMember();
		Iterator<CurvePropertyType> i = curvProps.iterator();
		AbstractCurveType curve = i.next().getCurve().getValue();
		if (curve instanceof LineStringType) {
			addLineString((LineStringType) curve, sb);
		} else {
			throw new Exception("Unsupported curve type: " + curve.getClass());
		}
	}

	/**
	 * @param ring
	 * @param sb
	 * @throws Exception
	 */
	private void addLinearRing(LinearRingType ring, StringBuilder sb)
			throws Exception {
		DirectPositionListType posList = ring.getPosList();
		List<CoordType> coord = ring.getCoord();
		List<JAXBElement<?>> elems = ring.getPosOrPointPropertyOrPointRep();
		addLine(posList, elems, coord, sb);
	}

	protected void addLine(DirectPositionListType posList,
			List<JAXBElement<?>> elems, List<CoordType> coord, StringBuilder sb)
			throws Exception {
		sb.append("(");
		if (posList != null) {
			addPosList(posList, sb);
		} else if (elems != null) {
			addMixedPos(elems, sb);
		} else if (coord != null) {
			addCoords(coord, sb);
		} else {
			throw new Exception("Unsupported LineString format");
		}
		sb.append(")");
	}

	/**
	 * @param coord
	 * @param sb
	 */
	protected void addCoords(List<CoordType> coord, StringBuilder sb) {
		Iterator<CoordType> i = coord.iterator();
		addPoint(convert(i.next()), sb);
		while (i.hasNext()) {
			sb.append(", ");
			addPoint(convert(i.next()), sb);
		}
	}

	protected void addLineString(LineStringType lst, StringBuilder sb)
			throws Exception {
		DirectPositionListType posList = lst.getPosList();
		List<JAXBElement<?>> elems = lst.getPosOrPointPropertyOrPointRep();
		addLine(posList, elems, null, sb);
	}

	protected void addMixedPos(List<JAXBElement<?>> elems, StringBuilder sb)
			throws Exception {
		Iterator<JAXBElement<?>> i = elems.iterator();
		while (i.hasNext()) {
			JAXBElement<?> elem = i.next();
			Object value = elem.getValue();
			List<Double> point = null;
			if (value instanceof DirectPositionType) {
				DirectPositionType pos = (DirectPositionType) value;
				point = pos.getValue();
			} else if (value instanceof PointPropertyType) {
				PointPropertyType pointProp = (PointPropertyType) value;
				DirectPositionType pos = pointProp.getPoint().getPos();
				CoordType coord = pointProp.getPoint().getCoord();
				if (pos != null) {
					point = pos.getValue();
				} else if (coord != null) {
					point = convert(coord);
				}
			}
			if (point == null) {
				throw new Exception("Unsupported position type: "
						+ value.getClass());
			}
			addPoint(point, sb);
			if (i.hasNext()) {
				sb.append(", ");
			}
		}
	}

	public void addPointPropList(List<PointPropertyType> pointProps,
			StringBuilder sb) throws Exception {
		Iterator<PointPropertyType> i = pointProps.iterator();
		addPoint(i.next().getPoint(), sb);
		while (i.hasNext()) {
			sb.append(' ');
			addPoint(i.next().getPoint(), sb);
		}
	}

	public void addPointList(List<PointType> point, StringBuilder sb)
			throws Exception {
		Iterator<PointType> i = point.iterator();
		addPoint(i.next(), sb);
		while (i.hasNext()) {
			sb.append(' ');
			addPoint(i.next(), sb);
		}
	}

	protected void addPoint(PointType point, StringBuilder sb) throws Exception {
		DirectPositionType pos = point.getPos();
		CoordType coord = point.getCoord();
		if (pos != null) {
			addPoint(pos.getValue(), sb);
		} else if (coord != null) {
			addPoint(convert(coord), sb);
		} else {
			throw new Exception("Unsupported point format");
		}
	}

	protected void addPoint(List<Double> point, StringBuilder sb) {
		Iterator<Double> j = point.iterator();
		sb.append(j.next());
		while (j.hasNext()) {
			sb.append(" ");
			sb.append(j.next());
		}
	}

	protected List<Double> convert(CoordType coord) {
		ArrayList<Double> rval = new ArrayList<Double>(3);
		rval.add(coord.getX().doubleValue());
		BigDecimal y = coord.getY();
		BigDecimal z = coord.getZ();
		if (y != null) {
			rval.add(y.doubleValue());
			if (z != null) {
				rval.add(z.doubleValue());
			}
		}
		return rval;
	}

	protected void addPosList(DirectPositionListType poslist, StringBuilder sb)
			throws Exception {
		int dim = poslist.getSrsDimension().intValue();
		List<Double> value = poslist.getValue();
		if (dim < 2 || value.size() % dim != 0) {
			throw new Exception("Invalid dimensions for position list");
		}
		Iterator<Double> i = value.iterator();
		while (i.hasNext()) {
			sb.append(i.next());
			for (int j = 0; j < dim; ++j) {
				sb.append(" ");
				sb.append(i.next());
			}
			if (i.hasNext()) {
				sb.append(", ");
			}
		}
	}

	public static class LineString extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			LineStringType lst = (LineStringType) geom;
			StringBuilder sb = new StringBuilder("LINESTRING ");
			addLineString(lst, sb);
			return sb.toString();
		}
	}

	public static class MultiLineString extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			MultiLineStringType mlst = (MultiLineStringType) geom;
			StringBuilder sb = new StringBuilder("MULTILINESTRING (");
			Iterator<LineStringPropertyType> i = mlst.getLineStringMember()
					.iterator();
			addLineString(i.next().getLineString(), sb);
			while (i.hasNext()) {
				sb.append(',');
				addLineString(i.next().getLineString(), sb);
			}
			sb.append(')');
			return sb.toString();
		}
	}

	public static class Point extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			PointType p = (PointType) geom;
			StringBuilder sb = new StringBuilder("POINT (");
			addPoint(p, sb);
			sb.append(')');
			return sb.toString();
		}
	}

	public static class MultiPoint extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			MultiPointType mult = (MultiPointType) geom;
			StringBuilder sb = new StringBuilder("MULTIPOINT (");
			List<PointPropertyType> pointProps = mult.getPointMember();
			PointArrayPropertyType array = mult.getPointMembers();
			if (pointProps != null) {
				addPointPropList(pointProps, sb);
			}
			if (array != null) {
				addPointList(array.getPoint(), sb);
			}
			sb.append(')');
			return sb.toString();
		}
	}

	public static class Polygon extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			PolygonType poly = (PolygonType) geom;
			StringBuilder sb = new StringBuilder("POLYGON ");
			addPolygon(poly, sb);
			return sb.toString();
		}
	}

	public static class MultiPolygon extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			MultiPolygonType poly = (MultiPolygonType) geom;
			StringBuilder sb = new StringBuilder("MULTIPOLYGON (");
			List<PolygonPropertyType> value = poly.getPolygonMember();
			Iterator<PolygonPropertyType> i = value.iterator();
			addPolygon(i.next().getPolygon(), sb);
			while (i.hasNext()) {
				sb.append(',');
				addPolygon(i.next().getPolygon(), sb);
			}
			sb.append(')');
			return sb.toString();
		}
	}

	public static class LinearRing extends GmlWktWriter {
		@Override
		public String write(AbstractGeometryType geom) throws Exception {
			LinearRingType ring = (LinearRingType) geom;
			StringBuilder sb = new StringBuilder("LINEARRING ");
			addAbstractRing(ring, sb);
			return sb.toString();
		}
	}
}
