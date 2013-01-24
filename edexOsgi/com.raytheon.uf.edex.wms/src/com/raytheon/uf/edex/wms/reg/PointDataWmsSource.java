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
 * Jul 25, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.reg;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateUtils;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.CRS;
import org.geotools.styling.StyledLayerDescriptor;
import org.hibernate.Criteria;
import org.hibernate.QueryException;
import org.hibernate.classic.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.hibernatespatial.criterion.SpatialRestrictions;
import org.opengis.feature.GeometryAttribute;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.springframework.context.ApplicationContext;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer.TimeFormat;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.sld.SldParser;
import com.raytheon.uf.edex.wms.sld.SldParserRegistry;
import com.raytheon.uf.edex.wms.util.StyleLibrary;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class PointDataWmsSource extends FeatureWmsSource {

	protected String timeField = "dataTime.refTime";

	public PointDataWmsSource(PluginProperties props, String key,
			LayerTransformer transformer, FeatureFactory featureFactory) {
		super(props, key, transformer, featureFactory);
		super.setTimeFormat(TimeFormat.HOUR_RANGES);
	}

	/**
	 * @param layer
	 * @param spatial
	 * @param params
	 * @return an empty list if no features match the query
	 */
	@SuppressWarnings("unchecked")
	protected List<SimpleFeature> getLayerFeatures(String layer,
			Criterion criterion) throws Exception {
		PluginFactory factory = PluginFactory.getInstance();
		PluginDao dao = factory.getPluginDao(props.getPluginName());
		Session sess = null;
		try {
			sess = dao.getSessionFactory().openSession();
			Criteria crit = sess.createCriteria(props.getRecord());
			crit = addAliases(crit);
			crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			crit.add(criterion);
			List<? extends PluginDataObject> pdos = crit.list();
			return convert(pdos);
		} catch (QueryException e) {
			log.error("Problem querying for feature data", e);
			return new ArrayList<SimpleFeature>(0);
		} finally {
			if (sess != null) {
				sess.close();
			}
		}
	}

	protected Criteria addAliases(Criteria crit) {
		return crit;
	}

	protected List<SimpleFeature> convert(List<? extends PluginDataObject> pdos) {
		return featureFactory.convert(pdos.toArray(new PluginDataObject[pdos
				.size()]));
	}

	protected abstract String getGeometryField(String layer);

	protected abstract CoordinateReferenceSystem getCRS(String layer);

	@Override
	public List<SimpleFeature> getFeatureInfo(String rawLayer,
			CoordinateReferenceSystem crs, Envelope bbox, String time,
			String elevation, Map<String, String> dimensions, Coordinate c,
			double scale) throws WmsException {
		// TODO add hook for dimensions
		// TODO handle more than CRS:84
		String layer = parseIncomingLayerName(rawLayer);
		Criterion spatial = getSpatialPoint(layer, c, scale);
		List<SimpleFeature> features = timeQuery(layer, time, spatial);
		return filterByNearest(features, c);
	}

	protected List<SimpleFeature> filterByNearest(List<SimpleFeature> features,
			Coordinate c) {
		if (features.isEmpty()) {
			return features;
		}
		Point center = new GeometryFactory().createPoint(c);
		TreeMap<Double, List<SimpleFeature>> distMap = new TreeMap<Double, List<SimpleFeature>>();
		for (SimpleFeature f : features) {
			GeometryAttribute geomProp = f.getDefaultGeometryProperty();
			Point p = (Point) f.getAttribute(geomProp.getName());
			double fdist = p.distance(center);
			List<SimpleFeature> list = distMap.get(fdist);
			if (list == null) {
				list = new ArrayList<SimpleFeature>(2);
				distMap.put(fdist, list);
			}
			list.add(f);
		}
		return distMap.firstEntry().getValue();
	}

	@Override
	protected List<SimpleFeature> getFeatures(String layer,
			CoordinateReferenceSystem crs, Envelope bbox, String time,
			String elevation, Map<String, String> dimensions, double scale)
			throws WmsException {
		Criterion spatial = getSpatial(layer, crs, bbox, scale);
		return timeQuery(layer, time, spatial);
	}

	protected List<SimpleFeature> timeQuery(String layer, String time,
			Criterion spatial) throws WmsException {
		List<SimpleFeature> rval = new ArrayList<SimpleFeature>();
		if (time != null) {
			String[] parts = StringUtils.split(time.trim());
			for (String part : parts) {
				// since DatabaseQuery doesn't handle 'or' we need to do a query
				// for each time range/instance
				Criterion crit = getTimeCrit(part, spatial);
				rval.addAll(getFeatInt(layer, crit));
			}
		} else {
			// get default
			Criterion crit = getDefaultTimeCrit(layer, spatial);
			rval.addAll(getFeatInt(layer, crit));
		}
		return rval;
	}

	protected List<SimpleFeature> getFeatInt(String layer, Criterion criterion)
			throws WmsException {
		try {
			return getLayerFeatures(layer, criterion);
		} catch (Exception e) {
			log.error("Problem getting features for layer " + layer, e);
			throw new WmsException(Code.InternalServerError);
		}
	}

	protected Criterion getDefaultTimeCrit(String layer, Criterion spatial)
			throws WmsException {
		Date start = getDefaultDate(layer);
		start = DateUtils.truncate(start, Calendar.HOUR);
		Date end = DateUtils.addHours(start, 1);
		return getRangeQuery(start, end, spatial);
	}

	protected Criterion getTimeCrit(String time, Criterion spatial)
			throws WmsException {
		String[] parts = StringUtils.split(time, '/');
		Conjunction con = Restrictions.conjunction();
		if (spatial != null) {
			con.add(spatial);
		}
		if (parts.length == 1) {
			// instance
			Calendar c = parseTimeString(parts[0]);
			Date date = c.getTime();
			// rval = new QueryParam(timeField, date.getTime());
			con.add(Restrictions.eq(timeField, date));
		} else {
			// range
			Calendar c = parseTimeString(parts[0]);
			Date start = c.getTime();
			c = parseTimeString(parts[1]);
			Date end = c.getTime();
			con.add(Restrictions.ge(timeField, start));
			con.add(Restrictions.lt(timeField, end));
			// TODO check resolution
		}
		return con;
	}

	protected Criterion getRangeQuery(Date start, Date end, Criterion spatial) {
		// spec is unclear about inclusiveness of time ranges, default to
		// inclusive bottom exclusive top
		Conjunction con = Restrictions.conjunction();
		if (spatial != null) {
			con.add(spatial);
		}
		con.add(Restrictions.ge(timeField, start));
		con.add(Restrictions.lt(timeField, end));
		return con;
	}

	protected Criterion getSpatialPoint(String layer, Coordinate c,
			double scale) throws WmsException {
		CoordinateReferenceSystem layerCrs = getCRS(layer);
		DirectPosition2D src = new DirectPosition2D(c.x, c.y);
		DirectPosition2D dst = new DirectPosition2D();
		try {
			MathTransform fromLatLon = MapUtil.getTransformFromLatLon(layerCrs);
			fromLatLon.transform(src, dst);
		} catch (Exception e) {
			log.error("Unable to transform query point", e);
			throw new WmsException(Code.InternalServerError);
		}
		Coordinate res = new Coordinate(dst.x, dst.y);
		Point p = new GeometryFactory().createPoint(res);

		Envelope env = p.getEnvelopeInternal();
		env.expandBy(scale * 2);
		return SpatialRestrictions.within(getGeometryField(layer),
				JTS.toGeometry(env));
	}

	protected Criterion getSpatial(String layer,
			CoordinateReferenceSystem crs, Envelope bbox, double scale)
			throws WmsException {
		Criterion rval;
		if (CRS.equalsIgnoreMetadata(crs, getCRS(layer))) {
			// spatial = new BboxParameter(getGeometryField(layer), bbox);
			Polygon poly = JTS.toGeometry(bbox);
			rval = SpatialRestrictions.within(getGeometryField(layer), poly);
		} else {
			// FIXME
			log.error("Attempted to query with non-native bounds");
			throw new WmsException(Code.InvalidParameterValue,
					"Layer backed by feature data must be queried with native CRS bbox");
		}
		return rval;
	}

	protected static StyleLibrary getStyleLib(InputStream in, String sldVersion)
			throws Exception {
		ApplicationContext ctx = EDEXUtil.getSpringContext();
		String[] beans = ctx.getBeanNamesForType(SldParserRegistry.class);
		SldParserRegistry sldParserRegistry = (SldParserRegistry) ctx
				.getBean(beans[0]);
		SldParser parser = sldParserRegistry.getParser(sldVersion);
		StyledLayerDescriptor sld = parser.parse(in);
		return new StyleLibrary(sld);
	}

	public String getTimeField() {
		return timeField;
	}

	public void setTimeField(String timeField) {
		this.timeField = timeField;
	}

}
