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
 * Aug 9, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.json.geo;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.geotools.data.store.EmptyFeatureCollection;
import org.geotools.feature.FeatureCollection;
import org.geotools.feature.FeatureIterator;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.AttributeDescriptor;
import org.opengis.feature.type.GeometryDescriptor;
import org.opengis.feature.type.Name;
import org.opengis.geometry.BoundingBox;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.json.JsonException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GeoJsonMapUtil {

	public enum FeatureOpt {
		ENCODE_CRS, ENCODE_BOUNDS
	};

	public static final String TYPE_KEY = "type";

	public static final String GEOM_KEY = "geometry";

	public static final String PROP_KEY = "properties";

	public static final String COORD_KEY = "coordinates";

	public static final String POINT_TYPE = "Point";

	public static final String LINE_STR_TYPE = "LineString";

	public static final String POLY_TYPE = "Polygon";

	public static final String MULT_POINT_TYPE = "Multi" + POINT_TYPE;

	public static final String MULT_LINE_STR_TYPE = "Multi" + LINE_STR_TYPE;

	public static final String MULT_POLY_TYPE = "Multi" + POLY_TYPE;

	public static final String GEOM_COLL_TYPE = "GeometryCollection";

	public static final String FEATURE_TYPE = "Feature";

	public static final String ID_KEY = "id";

	public static final String FEATURE_COLL_TYPE = "FeatureCollection";

	public static final String CRS_KEY = "crs";

	public static final String NAME_KEY = "name";

	public static final String LINK_KEY = "link";

	public static final String HREF_KEY = "href";

	public static final String BBOX_KEY = "bbox";

	public static final String FEATURES_KEY = "features";

	public static final String GEOMS_KEY = "geometries";

	private GeometryFactory _geomFact;

	private Object _mutex = new Object();

	protected GeometryFactory getGeomFact() {
		if (_geomFact == null) {
			synchronized (_mutex) {
				// check again
				if (_geomFact == null) {
					_geomFact = new GeometryFactory();
				}
			}
		}
		return _geomFact;
	}

	public Map<String, Object> extract(Geometry geom) throws JsonException {
		if (geom instanceof Point) {
			return extractPoint((Point) geom);
		} else if (geom instanceof LineString) {
			return extractLineString((LineString) geom);
		} else if (geom instanceof Polygon) {
			return extractPolygon((Polygon) geom);
		} else if (geom instanceof MultiPoint) {
			return extractMultiPoint((MultiPoint) geom);
		} else if (geom instanceof MultiLineString) {
			return extractMultiLineString((MultiLineString) geom);
		} else if (geom instanceof MultiPolygon) {
			return extractMultiPolygon((MultiPolygon) geom);
		} else if (geom instanceof GeometryCollection) {
			return extractGeometryCollection((GeometryCollection) geom);
		} else {
			throw new JsonException("Unkown geometry type: " + geom.getClass());
		}
	}

	public Geometry populateGeometry(Map<String, Object> jsonObj)
			throws JsonException {
		String type = (String) jsonObj.get(TYPE_KEY);
		if (type == null) {
			throw new JsonException("Unable to find type in json map");
		}
		if (POINT_TYPE.equals(type)) {
			return populatePoint(jsonObj);
		} else if (LINE_STR_TYPE.equals(type)) {
			return populateLineString(jsonObj);
		} else if (POLY_TYPE.equals(type)) {
			return populatePolygon(jsonObj);
		} else if (MULT_POINT_TYPE.equals(type)) {
			return populateMultiPoint(jsonObj);
		} else if (MULT_LINE_STR_TYPE.equals(type)) {
			return populateMultiLineString(jsonObj);
		} else if (MULT_POLY_TYPE.equals(type)) {
			return populateMultiPolygon(jsonObj);
		} else if (GEOM_COLL_TYPE.equals(type)) {
			return populateGeomCollection(jsonObj);
		} else {
			throw new JsonException("Unkown GeoJson geometry type: " + type);
		}
	}

	public Map<String, Object> extract(SimpleFeature feature)
			throws JsonException {
		return extract(feature, EnumSet.noneOf(FeatureOpt.class));
	}

	public Map<String, Object> extract(SimpleFeature feature,
			EnumSet<FeatureOpt> opts) throws JsonException {
		Object geom = feature.getDefaultGeometry();
		Map<String, Object> rval = new LinkedHashMap<String, Object>(6);
		rval.put(TYPE_KEY, FEATURE_TYPE);
		SimpleFeatureType type = feature.getFeatureType();
		if (type == null) {
			throw new JsonException("Feature type cannot be null");
		}
		String id = feature.getID();
		if (id != null) {
			rval.put(ID_KEY, id);
		}
		if (opts.contains(FeatureOpt.ENCODE_CRS)) {
			CoordinateReferenceSystem crs = type.getCoordinateReferenceSystem();
			if (crs != null) {
				rval.put(CRS_KEY, extract(crs));
			}
		}
		if (opts.contains(FeatureOpt.ENCODE_BOUNDS)) {
			BoundingBox bounds = feature.getBounds();
			if (bounds != null) {
				rval.put(BBOX_KEY, getBounds(bounds));
			}
		}
		int geomIndex = -1;
		if (geom != null) {
			rval.put(GEOM_KEY, extract((Geometry) geom));
			geomIndex = type.indexOf(feature.getDefaultGeometryProperty()
					.getName());
		}
		int count = type.getAttributeCount();
		Map<String, Object> props = new LinkedHashMap<String, Object>(count);
		for (int i = 0; i < count; ++i) {
			if (i == geomIndex) {
				continue;
			}
			Object value = feature.getAttribute(i);
			if (value == null) {
				continue;
			}
			AttributeDescriptor desc = type.getDescriptor(i);
			String name = desc.getLocalName();
			if (value instanceof Geometry) {
				props.put(name, extract((Geometry) value));
			} else if (value instanceof Envelope) {
				props.put(name, getEnv((Envelope) value));
			} else if (value instanceof BoundingBox) {
				props.put(name, getBounds((BoundingBox) value));
			} else {
				props.put(name, value);
			}
		}
		rval.put(PROP_KEY, props);
		return rval;
	}

	public Map<String, Object> extract(
			FeatureCollection<SimpleFeatureType, SimpleFeature> coll)
			throws JsonException {
		return extract(coll, EnumSet.noneOf(FeatureOpt.class));
	}

	public Map<String, Object> extract(
			FeatureCollection<SimpleFeatureType, SimpleFeature> coll,
			EnumSet<FeatureOpt> opts) throws JsonException {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(4);
		rval.put(TYPE_KEY, FEATURE_COLL_TYPE);
		ReferencedEnvelope bounds = coll.getBounds();
		if (opts.contains(FeatureOpt.ENCODE_CRS) && bounds != null
				&& !bounds.isNull()) {
			CoordinateReferenceSystem crs = bounds
					.getCoordinateReferenceSystem();
			if (crs != null) {
				rval.put(CRS_KEY, extract(crs));
			}
		}
		if (opts.contains(FeatureOpt.ENCODE_BOUNDS) && bounds != null
				&& !bounds.isNull()) {
			rval.put(BBOX_KEY, getBounds(bounds));
		}
		FeatureIterator<SimpleFeature> iter = coll.features();
		Object[] features = new Object[coll.size()];
		for (int i = 0; iter.hasNext(); ++i) {
			SimpleFeature feature = iter.next();
			features[i] = extract(feature, opts);
		}
		rval.put(FEATURES_KEY, features);
		return rval;
	}

	protected double[] getBounds(BoundingBox bbox) {
		double minX = bbox.getMinX();
		double minY = bbox.getMinY();
		double maxX = bbox.getMaxX();
		double maxY = bbox.getMaxY();
		return new double[] { minX, minY, maxX, maxY };
	}

	protected double[] getEnv(Envelope bbox) {
		double minX = bbox.getMinX();
		double minY = bbox.getMinY();
		double maxX = bbox.getMaxX();
		double maxY = bbox.getMaxY();
		return new double[] { minX, minY, maxX, maxY };
	}

	public Map<String, Object> extract(CoordinateReferenceSystem crs)
			throws JsonException {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		// TODO supported linked crs
		rval.put(TYPE_KEY, NAME_KEY);
		Map<String, Object> props = new LinkedHashMap<String, Object>(1);
		try {
			props.put(NAME_KEY, CRS.lookupIdentifier(crs, true));
		} catch (FactoryException e) {
			throw new JsonException("Problem looking up CRS ID", e);
		}
		rval.put(PROP_KEY, props);
		return rval;
	}

	@SuppressWarnings("unchecked")
	public FeatureCollection<SimpleFeatureType, SimpleFeature> populateFeatureCollection(
			Map<String, Object> jsonObj, SimpleFeatureType type)
			throws JsonException {
		Object obj = jsonObj.get(FEATURES_KEY);
		if (obj == null) {
			if (type == null) {
				type = new SimpleFeatureTypeBuilder().buildFeatureType();
			}
			return new EmptyFeatureCollection(type);
		}
		if (!(obj instanceof List<?>)) {
			throw new JsonException("Expected List for features, got "
					+ obj.getClass());
		}
		List<?> features = (List<?>) obj;
		List<SimpleFeature> rval = new ArrayList<SimpleFeature>(features.size());
		for (Object f : features) {
			if (!(f instanceof Map)) {
				throw new JsonException("Expected Map for feature got "
						+ f.getClass());
			}
			Map<String, Object> fmap = (Map<String, Object>) f;
			if (type == null) {
				rval.add(populateFeature(fmap));
			} else {
				rval.add(populateFeature(fmap, type));
			}
		}
		SimpleFeature sample = rval.get(0);
		MemoryFeatureCollection coll = new MemoryFeatureCollection(
				sample.getFeatureType());
		coll.addAll(rval);
		return coll;
	}

	protected FeatureCollection<SimpleFeatureType, SimpleFeature> getCollection(
			List<SimpleFeature> features, SimpleFeatureType type) {
		if (type != null) {
			MemoryFeatureCollection coll = new MemoryFeatureCollection(type);
			coll.addAll(features);
			return coll;
		}
		Map<SimpleFeatureType, List<SimpleFeature>> sorted = new HashMap<SimpleFeatureType, List<SimpleFeature>>();

		Iterator<SimpleFeature> i = features.iterator();
		while (i.hasNext()) {
			SimpleFeature next = i.next();
			List<SimpleFeature> list = sorted.get(next.getFeatureType());
			if (list == null) {
				list = new ArrayList<SimpleFeature>();
			}
			list.add(next);
		}
		if (sorted.size() == 1) {
			SimpleFeatureType t = features.get(0).getFeatureType();
			MemoryFeatureCollection coll = new MemoryFeatureCollection(t);
			coll.addAll(features);
			return coll;
		} else {
			Set<SimpleFeatureType> keySet = sorted.keySet();
			List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls = new ArrayList<FeatureCollection<SimpleFeatureType, SimpleFeature>>(
					keySet.size());
			for (SimpleFeatureType key : keySet) {
				MemoryFeatureCollection coll = new MemoryFeatureCollection(key);
				coll.addAll(sorted.get(key));
				colls.add(coll);
			}
			return new MixedFeatureCollection(colls);
		}
	}

	public FeatureCollection<SimpleFeatureType, SimpleFeature> populateFeatureCollection(
			Map<String, Object> jsonObj) throws JsonException {
		return populateFeatureCollection(jsonObj, null);
	}

	public SimpleFeature populateFeature(Map<String, Object> jsonObj)
			throws JsonException {
		return populateFeature(jsonObj, "the_geom", "feature");
	}

	public SimpleFeature populateFeature(Map<String, Object> jsonObj,
			SimpleFeatureType type) throws JsonException {
		String id = (String) jsonObj.get(ID_KEY);
		Geometry geom = getFeatureGeom(jsonObj);
		SimpleFeatureBuilder builder = new SimpleFeatureBuilder(type);
		if (geom != null) {
			GeometryDescriptor geomDesc = type.getGeometryDescriptor();
			Name geomName = geomDesc.getName();
			builder.set(geomName, geom);
		}
		Map<String, Object> props = getProps(jsonObj.get(PROP_KEY));
		for (AttributeDescriptor desc : type.getAttributeDescriptors()) {
			String name = desc.getLocalName();
			Object value = props.get(name);
			if (value != null) {
				builder.set(name, value);
			}
		}
		return builder.buildFeature(id);
	}

	@SuppressWarnings("unchecked")
	protected Geometry getFeatureGeom(Map<String, Object> jsonObj)
			throws JsonException {
		Object obj = jsonObj.get(GEOM_KEY);
		Geometry geom = null;
		if (obj != null) {
			if (obj instanceof Map) {
				geom = populateGeometry((Map<String, Object>) obj);
			} else {
				throw new JsonException("Unexpected type for geometry: "
						+ obj.getClass());
			}
		}
		return geom;
	}

	@SuppressWarnings("unchecked")
	public SimpleFeature populateFeature(Map<String, Object> jsonObj,
			String geomName, String featureName) throws JsonException {
		String id = (String) jsonObj.get(ID_KEY);
		SimpleFeatureTypeBuilder typeBuilder = new SimpleFeatureTypeBuilder();
		typeBuilder.setName(featureName);
		Geometry geom = getFeatureGeom(jsonObj);
		if (geom != null) {
			typeBuilder.setDefaultGeometry(geomName);
			typeBuilder.add(geomName, geom.getClass());
		}
		Object crsObj = jsonObj.get(CRS_KEY);
		CoordinateReferenceSystem crs = null;
		if (crsObj != null) {
			if (crsObj instanceof Map) {
				crs = populateCrs((Map<String, Object>) crsObj);
				typeBuilder.setCRS(crs);
			} else {
				throw new JsonException(
						"Expected Map<String, Object> for CRS got"
								+ crsObj.getClass());
			}
		}
		List<Object> values = new ArrayList<Object>(0);
		Object propObj = jsonObj.get(PROP_KEY);
		if (propObj != null) {
			Map<String, Object> props = getProps(propObj);
			Set<String> keySet = props.keySet();
			values = new ArrayList<Object>(keySet.size());
			for (String key : keySet) {
				Object val = props.get(key);
				typeBuilder.add(key, val.getClass());
				values.add(val);
			}
		}
		SimpleFeatureType type = typeBuilder.buildFeatureType();
		SimpleFeatureBuilder featureBuilder = new SimpleFeatureBuilder(type);
		if (geom != null) {
			featureBuilder.add(geom);
		}
		featureBuilder.addAll(values);
		return featureBuilder.buildFeature(id);
	}

	@SuppressWarnings("unchecked")
	protected Map<String, Object> getProps(Object propObj) throws JsonException {
		if (!(propObj instanceof Map)) {
			throw new JsonException(
					"Expected Map<String, Object> for properties got "
							+ propObj.getClass());
		}
		return (Map<String, Object>) propObj;
	}

	public CoordinateReferenceSystem populateCrs(Map<String, Object> jsonObj)
			throws JsonException {
		String type = (String) jsonObj.get(TYPE_KEY);
		if (!NAME_KEY.equals(type)) {
			// TODO handle link crs
			throw new JsonException("Only named CRS supported");
		}
		Object propObj = jsonObj.get(PROP_KEY);
		if (propObj == null) {
			throw new JsonException("Properties required for CRS");
		}
		Map<String, Object> props = getProps(propObj);
		String name = (String) props.get(NAME_KEY);
		try {
			return CRS.decode(name);
		} catch (Exception e) {
			throw new JsonException("Unable to decode namded CRS", e);
		}
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	public Point populatePoint(Map<String, Object> jsonObj)
			throws JsonException {
		Coordinate c = getCoord(jsonObj);
		return getGeomFact().createPoint(c);
	}

	protected Coordinate getSingleCoordinate(double[] c) throws JsonException {
		Coordinate coord;
		if (c.length == 3) {
			coord = new Coordinate(c[0], c[1], c[2]);
		} else if (c.length == 2) {
			coord = new Coordinate(c[0], c[1]);
		} else {
			throw new JsonException("Unsupported coordinate dimention: "
					+ c.length);
		}
		return coord;
	}

	protected Coordinate getSingleCoordinate(List<?> l) throws JsonException {
		Coordinate coord;
		double x = (Double) l.get(0);
		double y = (Double) l.get(1);
		if (l.size() == 3) {
			double z = (Double) l.get(2);
			coord = new Coordinate(x, y, z);
		} else if (l.size() == 2) {
			coord = new Coordinate(x, y);
		} else {
			throw new JsonException("Unsupported coordinate dimention: "
					+ l.size());
		}
		return coord;
	}

	protected Coordinate getCoord(Map<String, Object> jsonObj)
			throws JsonException {
		Object obj = jsonObj.get(COORD_KEY);
		if (obj == null) {
			throw new JsonException("Unable to find coordinate array");
		}
		if (obj instanceof List) {
			return getSingleCoordinate((List<?>) obj);
		} else if (obj instanceof double[]) {
			return getSingleCoordinate((double[]) obj);
		} else {
			throw new JsonException("Unexpected value for coordinates: "
					+ obj.getClass());
		}
	}

	protected Coordinate[] get2DArr(Map<String, Object> jsonObj)
			throws JsonException {
		Object obj = jsonObj.get(COORD_KEY);
		if (obj == null) {
			throw new JsonException("Unable to find coordinate array");
		}
		if (obj instanceof List) {
			return wrap2D((List<?>) obj);
		} else if (obj instanceof double[][]) {
			return wrap((double[][]) obj);
		} else {
			throw new JsonException("Unexpected value for coordinates: "
					+ obj.getClass());
		}
	}

	protected Coordinate[][] get3DArr(Map<String, Object> jsonObj)
			throws JsonException {
		Object obj = jsonObj.get(COORD_KEY);
		if (obj == null) {
			throw new JsonException("Unable to find coordinate array");
		}
		if (obj instanceof List) {
			List<?> list = (List<?>) obj;
			return wrap3D(list);
		} else if (obj instanceof double[][][]) {
			double[][][] arr = (double[][][]) obj;
			Coordinate[][] rval = new Coordinate[arr.length][];
			for (int i = 0; i < arr.length; ++i) {
				rval[i] = wrap(arr[i]);
			}
			return rval;
		} else {
			throw new JsonException("Unexpected value for coordinates: "
					+ obj.getClass());
		}
	}

	protected Coordinate[][] wrap3D(List<?> list) throws JsonException {
		Coordinate[][] rval = new Coordinate[list.size()][];
		Iterator<?> iterator = list.iterator();
		for (int i = 0; iterator.hasNext(); ++i) {
			List<?> next = (List<?>) iterator.next();
			rval[i] = wrap2D(next);
		}
		return rval;
	}

	protected Coordinate[][][] get4DArr(Map<String, Object> jsonObj)
			throws JsonException {
		Object obj = jsonObj.get(COORD_KEY);
		if (obj == null) {
			throw new JsonException("Unable to find coordinate array");
		}
		if (obj instanceof List) {
			List<?> list = (List<?>) obj;
			Coordinate[][][] rval = new Coordinate[list.size()][][];
			Iterator<?> iterator = list.iterator();
			for (int i = 0; iterator.hasNext(); ++i) {
				List<?> next = (List<?>) iterator.next();
				rval[i] = wrap3D(next);
			}
			return rval;
		} else {
			throw new JsonException("Unexpected value for coordinates: "
					+ obj.getClass());
		}
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	public LineString populateLineString(Map<String, Object> jsonObj)
			throws JsonException {
		GeometryFactory geomFact = getGeomFact();
		Coordinate[] coordinates = get2DArr(jsonObj);
		return geomFact.createLineString(coordinates);
	}

	protected Coordinate[] wrap2D(List<?> coords) throws JsonException {
		int count = coords.size();
		Coordinate[] rval = new Coordinate[count];
		Iterator<?> iterator = coords.iterator();
		for (int i = 0; iterator.hasNext(); ++i) {
			List<?> inner = (List<?>) iterator.next();

			rval[i] = getSingleCoordinate(inner);
		}
		return rval;
	}

	protected Coordinate[] wrap(double[][] coords) throws JsonException {
		int count = coords.length;
		Coordinate[] rval = new Coordinate[count];
		for (int i = 0; i < count; ++i) {
			rval[i] = getSingleCoordinate(coords[i]);
		}
		return rval;
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	public Geometry populatePolygon(Map<String, Object> jsonObj)
			throws JsonException {
		Coordinate[][] coords = get3DArr(jsonObj);
		return getPolygon(coords);
	}

	protected Polygon getPolygon(Coordinate[][] coords) throws JsonException {
		GeometryFactory geomFact = getGeomFact();
		LinearRing shell = getLinearRing(coords[0]);
		LinearRing[] holes = getInteriors(coords);
		return geomFact.createPolygon(shell, holes);
	}

	protected LinearRing[] getInteriors(Coordinate[][] coords)
			throws JsonException {
		LinearRing[] rval = new LinearRing[coords.length - 1];
		for (int i = 1; i < coords.length; ++i) {
			rval[i - 1] = getLinearRing(coords[i]);
		}
		return rval;
	}

	protected LinearRing getLinearRing(Coordinate[] coords)
			throws JsonException {
		GeometryFactory geomFact = getGeomFact();
		return geomFact.createLinearRing(coords);
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	public MultiPoint populateMultiPoint(Map<String, Object> jsonObj)
			throws JsonException {
		GeometryFactory geomFact = getGeomFact();
		return geomFact.createMultiPoint(get2DArr(jsonObj));
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	public MultiLineString populateMultiLineString(Map<String, Object> jsonObj)
			throws JsonException {
		Coordinate[][] coords = get3DArr(jsonObj);
		GeometryFactory geomFact = getGeomFact();
		LineString[] lines = new LineString[coords.length];
		for (int i = 0; i < lines.length; ++i) {
			Coordinate[] wrap = coords[i];
			lines[i] = geomFact.createLineString(wrap);
		}
		return geomFact.createMultiLineString(lines);
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	public MultiPolygon populateMultiPolygon(Map<String, Object> jsonObj)
			throws JsonException {
		Coordinate[][][] coords = get4DArr(jsonObj);
		Polygon[] polys = new Polygon[coords.length];
		for (int i = 0; i < coords.length; ++i) {
			polys[i] = getPolygon(coords[i]);
		}
		return getGeomFact().createMultiPolygon(polys);
	}

	/**
	 * @param jsonObj
	 * @return
	 * @throws JsonException
	 */
	@SuppressWarnings("unchecked")
	public GeometryCollection populateGeomCollection(Map<String, Object> jsonObj)
			throws JsonException {
		Object obj = jsonObj.get(GEOMS_KEY);
		if (obj == null) {
			throw new JsonException("Unable to find geometries in collection");
		}
		if (!(obj instanceof List)) {
			throw new JsonException("Expected List for geometries got: "
					+ obj.getClass());
		}
		List<?> geoms = (List<?>) obj;
		Geometry[] rval = new Geometry[geoms.size()];
		Iterator<?> iterator = geoms.iterator();
		for (int i = 0; iterator.hasNext(); ++i) {
			Object o = iterator.next();
			if (o instanceof Map) {
				rval[i] = populateGeometry((Map<String, Object>) o);
			} else {
				throw new JsonException(
						"Expected Map<String, Object> for each geometry, got "
								+ o.getClass());
			}
		}
		return getGeomFact().createGeometryCollection(rval);
	}

	/**
	 * @param geom
	 * @return
	 * @throws JsonException
	 */
	public Map<String, Object> extractGeometryCollection(GeometryCollection coll)
			throws JsonException {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		int count = coll.getNumGeometries();
		Object[] geoms = new Object[count];
		for (int i = 0; i < count; ++i) {
			Geometry geom = coll.getGeometryN(i);
			geoms[i] = extract(geom);
		}
		rval.put(TYPE_KEY, GEOM_COLL_TYPE);
		rval.put(GEOMS_KEY, geoms);
		return rval;
	}

	/**
	 * @param geom
	 * @return
	 */
	public Map<String, Object> extractMultiPolygon(MultiPolygon mpoly) {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		int count = mpoly.getNumGeometries();
		double[][][][] coords = new double[count][][][];
		for (int i = 0; i < count; ++i) {
			Polygon poly = (Polygon) mpoly.getGeometryN(i);
			coords[i] = getPolyCoords(poly);
		}
		rval.put(TYPE_KEY, MULT_POLY_TYPE);
		rval.put(COORD_KEY, coords);
		return rval;
	}

	/**
	 * @param geom
	 * @return
	 */
	public Map<String, Object> extractMultiLineString(MultiLineString mlineStr) {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		int count = mlineStr.getNumGeometries();
		double[][][] coords = new double[count][][];
		for (int i = 0; i < count; ++i) {
			LineString lineStr = (LineString) mlineStr.getGeometryN(i);
			coords[i] = toArray(lineStr.getCoordinates());
		}
		rval.put(TYPE_KEY, MULT_LINE_STR_TYPE);
		rval.put(COORD_KEY, coords);
		return rval;
	}

	/**
	 * @param geom
	 * @return
	 */
	public Map<String, Object> extractMultiPoint(MultiPoint mpoint) {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		int count = mpoint.getNumGeometries();
		double[][] coords = new double[count][];
		for (int i = 0; i < count; ++i) {
			Point p = (Point) mpoint.getGeometryN(i);
			coords[i] = toArray(p.getCoordinate());
		}
		rval.put(TYPE_KEY, MULT_POINT_TYPE);
		rval.put(COORD_KEY, coords);
		return rval;
	}

	/**
	 * @param geom
	 * @return
	 */
	public Map<String, Object> extractPolygon(Polygon poly) {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		double[][][] coords = getPolyCoords(poly);
		rval.put(TYPE_KEY, POLY_TYPE);
		rval.put(COORD_KEY, coords);
		return rval;
	}

	protected double[][][] getPolyCoords(Polygon poly) {
		LineString exterior = poly.getExteriorRing();
		int count = poly.getNumInteriorRing() + 1;
		double[][][] coords = new double[count][][];
		coords[0] = toArray(exterior.getCoordinates());
		for (int i = 1; i < count; ++i) {
			LineString interior = poly.getInteriorRingN(i - 1);
			coords[i] = toArray(interior.getCoordinates());
		}
		return coords;
	}

	/**
	 * @param geom
	 * @return
	 */
	public Map<String, Object> extractLineString(LineString lineStr) {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		double[][] coords = toArray(lineStr.getCoordinates());
		rval.put(TYPE_KEY, LINE_STR_TYPE);
		rval.put(COORD_KEY, coords);
		return rval;
	}

	/**
	 * @param geom
	 * @return
	 */
	public Map<String, Object> extractPoint(Point point) {
		Map<String, Object> rval = new LinkedHashMap<String, Object>(2);
		double[] coords = toArray(point.getCoordinate());
		rval.put(TYPE_KEY, POINT_TYPE);
		rval.put(COORD_KEY, coords);
		return rval;
	}

	protected double[] toArray(Coordinate c) {
		if (!Double.isNaN(c.z)) {
			return new double[] { c.x, c.y, c.z };
		} else {
			return new double[] { c.x, c.y };
		}
	}

	protected double[][] toArray(Coordinate[] coords) {
		double[][] rval = new double[coords.length][];
		for (int i = 0; i < rval.length; ++i) {
			rval[i] = toArray(coords[i]);
		}
		return rval;
	}
}
