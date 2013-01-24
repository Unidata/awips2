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
 * Aug 3, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.plugin.obs.ogc.metar;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.quantity.Pressure;
import javax.measure.unit.Unit;

import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.edex.plugin.obs.metar.MetarPointDataTransform;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.dataplugin.obs.metar.util.SkyCover;
import com.raytheon.uf.common.dataplugin.obs.metar.util.WeatherCondition;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class MetarFeatureFactory implements FeatureFactory {

	private static final String TEMP_KEY = MetarPointDataTransform.TEMPERATURE;

	private static final String WX_KEY = MetarPointDataTransform.PRES_WEATHER;

	private static final String PRESS_KEY = MetarPointDataTransform.SEA_LEVEL_PRESS;

	private static final String DP_KEY = MetarPointDataTransform.DEWPOINT;

	private static final String HR_PRESS_KEY = MetarPointDataTransform.PRESS_CHANGE3_HOUR;

	private static final String VIZ_KEY = MetarPointDataTransform.VISIBILITY;

	private static final String SKY_COND_KEY = MetarPointDataTransform.SKY_COVER;

	private static final String GUST_KEY = MetarPointDataTransform.WIND_GUST;

	private static final String WSPD_KEY = MetarPointDataTransform.WIND_SPEED;

	private static final String WDIR_KEY = MetarPointDataTransform.WIND_DIR_STR;

	private static final String STATION_ID_KEY = MetarPointDataTransform.STATION_ID;

	private static final String SKYLAYERBASE_KEY = MetarPointDataTransform.SKY_LAYER_BASE;

	private static final String GEOM_KEY = "location";

	private static final String TIME_KEY = "timeObs";

	private static final String METAR_NS = "http://metar.edex.uf.raytheon.com";

	private static SimpleFeatureType _metarType;

	private static String name = "metar";

	private static Integer skyCondDefault = 53;

	private static final Map<String, Integer> skyMap;
	static {
        Map<String, Integer> temp = new HashMap<String, Integer>();
        temp.put("BLNK", 32);
        temp.put("SKC", 48);
        temp.put("CLR", 48);
        temp.put("FEW", 49);
        temp.put("SCT", 64);
        temp.put("BKN", 50);
        temp.put("OVC", 56);
        temp.put("OBS", 52);
        temp.put("VV", 52);
        skyMap = Collections.unmodifiableMap(temp);
	}

	public static SimpleFeatureType getFeatureType() {
		if (_metarType == null) {
			SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
			builder.setCRS(MapUtil.LATLON_PROJECTION);
			builder.setName(name);
			builder.setNamespaceURI(METAR_NS);
			builder.setDefaultGeometry(GEOM_KEY);
			builder.add(TEMP_KEY, Integer.class);
			builder.add(WX_KEY, String.class);
			builder.add(DP_KEY, Integer.class);
			builder.add(PRESS_KEY, Float.class);
			builder.add(HR_PRESS_KEY, String.class);
			builder.add(VIZ_KEY, Float.class);
			builder.add(SKY_COND_KEY, Integer.class);
			builder.add(GUST_KEY, Integer.class);
			builder.add(WSPD_KEY, Integer.class);
			builder.add(WDIR_KEY, String.class);
			builder.add(STATION_ID_KEY, String.class);
			builder.add(SKYLAYERBASE_KEY, Integer.class);
			builder.add(GEOM_KEY, Point.class);
			builder.add(TIME_KEY, Date.class);
			_metarType = builder.buildFeatureType();
		}
		return _metarType;
	}

	/**
	 * Converts MetarRecords to SimpleFeatures
	 * 
	 * @param pdos
	 * @return
	 */
	@Override
	public List<SimpleFeature> convert(PluginDataObject[] pdos) {
		if (CollectionUtil.isNullOrEmpty(pdos)) {
			return new ArrayList<SimpleFeature>(0);
		}
		ArrayList<SimpleFeature> rval = new ArrayList<SimpleFeature>(
				pdos.length);
		for (PluginDataObject pdo : pdos) {
			if (pdo instanceof MetarRecord) {
				rval.add(getAsFeature((MetarRecord) pdo));
			}
		}
		return rval;
	}

	/**
	 * @param pdo
	 * @return
	 */
	public static SimpleFeature getAsFeature(MetarRecord record) {
		
		SimpleFeatureBuilder builder = new SimpleFeatureBuilder(
				getFeatureType());
		builder.set(TEMP_KEY, record.getTemperature());
		builder.set(DP_KEY, record.getDewPoint());
		builder.set(GEOM_KEY, record.getLocation().getLocation());
		builder.set(PRESS_KEY, record.getSeaLevelPress());
		builder.set(GUST_KEY, record.getWindGust());
		builder.set(WDIR_KEY, record.getWindDir());
		builder.set(STATION_ID_KEY, record.getLocation().getStationId());
		builder.set(SKYLAYERBASE_KEY, record.getSkyLayerBase());
		builder.set(WSPD_KEY, record.getWindSpeed());
		builder.set(VIZ_KEY, record.getVisibility());
		builder.set(SKY_COND_KEY, getSkyCond(record.getSkyCoverage()));
		builder.set(HR_PRESS_KEY, get3HrPress(record.getPressChange3Hour()));
		builder.set(WX_KEY, getWxCondition(record.getWeatherCondition()));
		builder.set(TIME_KEY, record.getTimeObs().getTime());
		
		return builder.buildFeature(record.getDataURI());
	}

	/**
	 * @param weatherCondition
	 * @return
	 */
	protected static String getWxCondition(List<WeatherCondition> wxs) {
		if (CollectionUtil.isNullOrEmpty(wxs)) {
			return null;
		}
		StringBuilder rval = new StringBuilder();
		Iterator<WeatherCondition> i = wxs.iterator();
		WeatherCondition wx = i.next();
		appendFromWc(rval, wx);
		for (; i.hasNext(); wx = i.next()) {
			rval.append(' ');
			appendFromWc(rval, wx);
		}
		return rval.toString();
	}

	protected static void appendFromWc(StringBuilder sb, WeatherCondition wc) {
		sb.append(wc.getIntensityProximity());
		sb.append(wc.getDescriptor());
		sb.append(wc.getPrecipitation());
		sb.append(wc.getObscuration());
		sb.append(wc.getOther());
	}

	/**
	 * @param pressChange3Hour
	 * @return
	 */
	protected static String get3HrPress(float press) {
		Unit<Pressure> unit = MetarRecord.PRESSURE_UNIT;
		return String.format("%f %s", press, unit.toString());
	}

	protected static Integer getSkyCond(Set<SkyCover> cond) {
		if (CollectionUtil.isNullOrEmpty(cond)) {
			return skyCondDefault;
		}
		Integer rval = skyCondDefault;
		for (SkyCover sc : cond) {
			Integer i = skyMap.get(sc.getType());
			if (i != null) {
				rval = i;
				// FIXME we probably don't want to just take the first one
				break;
			}
		}
		return rval;
	}

}
