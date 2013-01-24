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
 * Jun 13, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.ReferenceIdentifier;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcDimension;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class LayerTransformer {

	public enum TimeFormat {
		LIST, HOUR_RANGES
	};

	protected String key;

	protected static String timeUnit = "ISO8601";

	protected Class<? extends SimpleLayer> layerClass = SimpleLayer.class;

	protected static Log log = LogFactory.getLog(LayerTransformer.class);

	protected static final int MILLIS_IN_HOUR = 60 * 60 * 1000;

	public static final Pattern frontDot = Pattern
			.compile("^(-?[0-9]*\\.?[0-9]+)(.*)$");
	public static final Pattern backDot = Pattern
			.compile("^(-?[0-9]+\\.?[0-9]*)(.*)$");

	/**
	 * Construct a LayerTransformer that uses a different layerClass and
	 * layerWrapperClass than the default SimpleLayer and LayerHolder
	 * 
	 * @param key
	 * @param layerTable
	 * @param das
	 * @param layerClass
	 * @param layerHolderClass
	 */
	public LayerTransformer(String key, Class<? extends SimpleLayer> layerClass) {
		this.key = key;
		this.layerClass = layerClass;
	}

	/**
	 * @param name
	 * @return null if layer not found
	 * @throws DataAccessLayerException
	 */
	public SimpleLayer find(String name) throws DataAccessLayerException {
		String field = "name";
		List<QueryParam> params = Arrays.asList(new QueryParam(field, name));
		List<? extends SimpleLayer> res;
		res = query(layerClass, params);
		if (res == null || res.isEmpty()) {
			return null;
		}
		if (res.size() > 1) {
			log.warn("Multiple layers found with the same name, returning first");
		}
		return layerClass.cast(res.get(0));
	}

	/**
	 * @param layer
	 * @param dimension
	 * @return null if layer/dimension not found
	 * @throws DataAccessLayerException
	 */
	public SimpleDimension getDimension(String layer, String dimension)
			throws DataAccessLayerException {
		SimpleLayer l = find(layer);
		return getDimension(l, dimension);
	}

	/**
	 * @param layer
	 * @param dimension
	 * @return null if layer/dimension not found
	 */
	public static SimpleDimension getDimension(SimpleLayer layer,
			String dimension) {
		if (layer == null) {
			return null;
		}
		SimpleDimension rval = null;
		for (SimpleDimension d : layer.getDimensions()) {
			if (d.getName().equalsIgnoreCase(dimension)) {
				rval = d;
				break;
			}
		}
		return rval;
	}

	/**
	 * @param dim
	 * @return empty set if dim is null or doesn't have any parsable values
	 */
	public static TreeSet<Double> getDimValuesAsDouble(SimpleDimension dim) {
		TreeSet<Double> rval = new TreeSet<Double>();
		if (dim == null) {
			return rval;
		}
		for (String val : dim.getValues()) {
			try {
				Matcher m = frontDot.matcher(val);
				if (m.matches()) {
					val = m.group(1);
				} else {
					m = backDot.matcher(val);
					if (m.matches()) {
						val = m.group(1);
					}
				}
				rval.add(Double.parseDouble(val));
			} catch (Throwable e) {
				// continue
			}
		}
		return rval;
	}

	/**
	 * @param layer
	 * @return
	 */
	public List<OgcBoundingBox> getBoundingBoxes(SimpleLayer layer) {
		String crs = layer.getTargetCrsCode();
		double minx = layer.getTargetMinx();
		double maxx = layer.getTargetMaxx();
		double miny = layer.getTargetMiny();
		double maxy = layer.getTargetMaxy();
		OgcBoundingBox rval = new OgcBoundingBox(crs, minx, maxx, miny, maxy);
		return Arrays.asList(rval);
	}

	public static String getCrsName(CoordinateReferenceSystem crs) {
		if (crs == null) {
			return null;
		}
		Set<ReferenceIdentifier> ids = crs.getIdentifiers();
		if (ids == null || ids.isEmpty()) {
			return crs.getName().toString();
		} else {
			return ids.iterator().next().toString();
		}
	}

	/**
	 * @param layer
	 * @return
	 */
	public static OgcGeoBoundingBox getGeoBoundingBox(SimpleLayer layer) {
		Polygon crs84Bounds = layer.getCrs84Bounds();
		if (crs84Bounds == null) {
			return null;
		}
		ReferencedEnvelope env = JTS.toEnvelope(crs84Bounds);
		return new OgcGeoBoundingBox(env);
	}

	/**
	 * @param layer
	 * @param tformat
	 * @return
	 */
	public static List<String> getTimes(SimpleLayer layer) {
		return getTimes(layer, TimeFormat.LIST);
	}

	/**
	 * @param layer
	 * @param tformat
	 * @return
	 */
	public static List<String> getTimes(SimpleLayer layer, TimeFormat tformat) {
		List<String> rval;
		// TODO this could be adapted to a pattern that allows for formatters to
		// be externally added
		switch (tformat) {
		case LIST:
			rval = getTimesAsList(layer);
			break;
		case HOUR_RANGES:
			rval = getTimesAsHourRanges(layer);
			break;
		default:
			throw new IllegalArgumentException("No handler for time format: "
					+ tformat);
		}
		return rval;
	}

	/**
	 * @param layer
	 * @return
	 */
	protected static List<String> getTimesAsHourRanges(SimpleLayer layer) {
		Set<Date> times = layer.getTimes();
		if (times == null || times.isEmpty()) {
			return new ArrayList<String>(0);
		}
		Set<Date> set = new TreeSet<Date>();
		set.addAll(times);
		Iterator<Date> i = set.iterator();
		StringBuilder sb = new StringBuilder();
		List<String> rval = new ArrayList<String>();
		Date curr = i.next();
		while (curr != null) {
			startRange(sb, curr);
			curr = endRange(curr, i, sb);
			if (curr != null) {
				rval.add(sb.toString());
				sb = new StringBuilder();
			}
		}
		String last = sb.toString();
		if (!last.isEmpty()) {
			rval.add(last);
		}
		return rval;
	}

	/**
	 * End a range started by startRange()
	 * 
	 * @param d
	 *            start of range
	 * @param i
	 *            iterator to rest of times that possibly include the rest of
	 *            the range
	 * @param sb
	 *            where the formatted output goes
	 * @return the start of the next range, null if there are no more ranges
	 */
	protected static Date endRange(Date d, Iterator<Date> i, StringBuilder sb) {
		long milliStart = d.getTime();
		long milliPrev = milliStart;
		Date rval = null;
		Date prev = null;
		Date curr = null;
		while (i.hasNext()) {
			curr = i.next();
			if (curr.getTime() - milliPrev > MILLIS_IN_HOUR) {
				// we've reached the end of the range return rval
				rval = curr;
				break;
			}
			prev = curr;
			milliPrev = prev.getTime();
		}
		if (prev == null) {
			// iterator didn't have anything
			prev = new Date(milliStart + MILLIS_IN_HOUR);
		} else {
			// we want the range to end at the next hour
			prev = new Date(prev.getTime() + MILLIS_IN_HOUR);
		}
		sb.append(format(prev));
		// FIXME 0 indicates a continuum range, we should support discrete
		// periods in the range
		sb.append("/0");
		return rval;
	}

	public static String format(Date d) {
		Calendar c = GregorianCalendar.getInstance(TimeZone.getTimeZone("GMT"));
		c.setTime(d);
		return DatatypeConverter.printDateTime(c);
	}

	protected static void startRange(StringBuilder sb, Date d) {
		sb.append(format(d));
		sb.append('/');
	}

	protected static List<String> getTimesAsList(SimpleLayer layer) {
		Set<Date> times = layer.getTimes();
		if (times == null || times.isEmpty()) {
			return new ArrayList<String>(0);
		}
		TreeSet<Date> sorted = new TreeSet<Date>(times);
		Iterator<Date> i = sorted.iterator();
		List<String> rval = new ArrayList<String>(sorted.size());
		while (i.hasNext()) {
			rval.add(format(i.next()));
		}
		return rval;
	}

	public TreeSet<Date> getAllTimes() {
		TreeSet<Date> rval = new TreeSet<Date>();
		List<SimpleLayer> layers = getLayers();
		for (SimpleLayer l : layers) {
			rval.addAll(l.getTimes());
		}
		return rval;
	}

	@SuppressWarnings("unchecked")
	public List<SimpleLayer> getLayers() {
		List<SimpleLayer> rval;
		rval = (List<SimpleLayer>) query(layerClass);
		return rval;
	}

	public List<OgcLayer> getLayersAsOgc(TimeFormat tformat, StyleLookup lookup) {
		return transform(getLayers(), tformat, lookup);
	}

	public List<OgcLayer> getLayersAsOgc(StyleLookup lookup) {
		return getLayersAsOgc(TimeFormat.LIST, lookup);
	}

	/**
	 * @param layers
	 * @return
	 */
	protected List<OgcLayer> transform(List<? extends SimpleLayer> layers,
			TimeFormat tformat, StyleLookup lookup) {
		List<OgcLayer> rval = new ArrayList<OgcLayer>(layers.size());
		for (SimpleLayer simple : layers) {
			rval.add(transform(simple, tformat, lookup));
		}
		return rval;
	}

	/**
	 * Transform a simple layer as represented in the data storage area to an
	 * OgcLayer that can be viewed through getCapabilities
	 * <p>
	 * Override this method to add additional Dimensions.
	 * 
	 * @param layer
	 * @return
	 */
	public OgcLayer transform(SimpleLayer layer, TimeFormat tformat,
			StyleLookup lookup) {
		OgcLayer rval = new OgcLayer();
		String name = layer.getName();
		rval.setName(key, name);
		rval.setTitle(name);
		setStylesForLayer(rval.getName(), layer, rval, lookup);
		OgcDimension timeDim = new OgcDimension("time", timeUnit, getTimes(layer,
				tformat));
		Date time = layer.getDefaultTime();
		String defaultTime;
		if (tformat.equals(TimeFormat.HOUR_RANGES)) {
			defaultTime = getTimeRange(time);
		} else {
			defaultTime = format(time);
		}
		timeDim.setDefaultVal(defaultTime);
		rval.addDimension(timeDim);
		for (OgcDimension dim : getDims(layer, layer.getDimensions())) {
			rval.addDimension(dim);
		}
		rval.setGeoBoundingBox(getGeoBoundingBox(layer));
		rval.setBoundingBox(getBoundingBoxes(layer));
		// rval.setCrs(Arrays.asList(layer.getTargetCrs()));
		return rval;
	}

	public String getTimeRange(Date d) {
		Date start = DateUtils.truncate(d, Calendar.HOUR);
		Date end = DateUtils.addHours(start, 1);
		return format(start) + '/' + format(end);
	}

	protected void setStylesForLayer(String layerName, SimpleLayer layer,
			OgcLayer rval, StyleLookup lookup) {
		if (lookup != null) {
			String style = lookup.lookup(layerName);
			if (style != null) {
				OgcStyle ogcstyle = new OgcStyle(style);
				String url = "&layer=" + layerName + "&style=" + style
						+ "&format=image/png";
				ogcstyle.setLegendUrl(url.replaceAll(" ", "%20"));
				ogcstyle.setDefault(true);
				rval.setStyles(Arrays.asList(ogcstyle));
			}
		}
	}

	/**
	 * @param layer
	 * @return
	 */
	protected OgcDimension[] getDims(SimpleLayer layer,
			Set<? extends SimpleDimension> dims) {
		if (dims == null) {
			return new OgcDimension[0];
		}
		ArrayList<OgcDimension> rval = new ArrayList<OgcDimension>(dims.size());
		for (SimpleDimension dim : dims) {
			OgcDimension ogcdim = transform(layer, dim);
			if (ogcdim != null) {
				rval.add(ogcdim);
			}
		}
		return rval.toArray(new OgcDimension[rval.size()]);
	}

	/**
	 * @param simpleDimension
	 * @return
	 */
	protected OgcDimension transform(SimpleLayer layer, SimpleDimension dim) {
		if (dim == null || dim.getName() == null || dim.getValues() == null) {
			return null;
		}
		OgcDimension rval;
		List<String> values = new ArrayList<String>(dim.getValues());
		if (dim.getUnits() == null) {
			rval = new OgcDimension(dim.getName(), values);
		} else {
			rval = new OgcDimension(dim.getName(), dim.getUnits(), values);
		}
		rval.setDefaultVal(dim.getDefaultValue(layer));
		return rval;
	}

	protected OgcLayer transform(SimpleLayer layer, StyleLookup lookup) {
		return transform(layer, TimeFormat.LIST, lookup);
	}

	protected <T> List<T> query(Class<T> c) {
		return query(c, null);
	}

	@SuppressWarnings("unchecked")
	protected <T> List<T> query(Class<T> c, List<QueryParam> params) {
		try {
			DatabaseQuery query = new DatabaseQuery(c);
			if (params != null && !params.isEmpty()) {
				for (QueryParam p : params) {
					query.addQueryParam(p);
				}
			}
			DaoConfig config = DaoConfig.forClass(layerClass);
			CoreDao dao = new CoreDao(config);
			return (List<T>) dao.queryByCriteria(query);
		} catch (Exception e) {
			log.error("Unable to query entity: " + c, e);
			return new ArrayList<T>(0);
		}
	}

	protected <H extends LayerHolder<? extends SimpleLayer>> List<SimpleLayer> unwrap(
			List<H> holders) {
		List<SimpleLayer> rval = new ArrayList<SimpleLayer>(holders.size());
		for (H holder : holders) {
			rval.add(holder.getValue());
		}
		return rval;
	}

	/**
	 * @param layerName
	 * @return null if layer name isn't found
	 * @throws DataAccessLayerException
	 */
	public Date getLatestTime(String layerName) throws DataAccessLayerException {
		SimpleLayer simpleLayer = find(layerName);
		return getLatestTime(simpleLayer);
	}

	/**
	 * @param layer
	 * @return null if layer name isn't found
	 */
	public static Date getLatestTime(SimpleLayer layer) {
		if (layer == null) {
			return null;
		}
		Set<Date> times = layer.getTimes();
		TreeSet<Date> sorted = new TreeSet<Date>(times);
		return sorted.last();
	}

	/**
	 * @return the key
	 */
	public String getKey() {
		return key;
	}

	/**
	 * @param key
	 *            the key to set
	 */
	public void setKey(String key) {
		this.key = key;
	}

	/**
	 * @param layerClass
	 *            the layerClass to set
	 */
	public void setLayerClass(Class<? extends SimpleLayer> layerClass) {
		this.layerClass = layerClass;
	}

	/**
	 * @return the layerClass
	 */
	public Class<? extends SimpleLayer> getLayerClass() {
		return layerClass;
	}

	/**
	 * @return
	 */
	public TreeSet<Date> getLatestTimes() {
		List<SimpleLayer> layers = getLayers();
		TreeSet<Date> rval = new TreeSet<Date>();
		for (SimpleLayer l : layers) {
			TreeSet<Date> times = new TreeSet<Date>(l.getTimes());
			rval.add(times.last());
		}
		return rval;
	}

}
