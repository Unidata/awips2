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
 * Aug 10, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.json.geo;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedHashMap;
import java.util.Map;

import org.geotools.feature.FeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.json.JsonException;
import com.raytheon.uf.common.json.JsonService;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Simple implementation of geojson. Uses maps as intermediate representations.
 * 
 * @author bclement
 * @version 1.0
 */
public class GeoJsonUtilSimpleImpl implements GeoJsonUtil {

	private static JsonService _jservice;

	protected GeoJsonMapUtil mapUtil = new GeoJsonMapUtil();

	protected boolean pretty = true;

	private static Object _mutex = new Object();

	protected static JsonService getJsonService() {
		if (_jservice == null) {
			synchronized (_mutex) {
				if (_jservice == null) {
					// we need a json service that does not enable default
					// typing
					_jservice = new BasicJsonService();
				}
			}
		}
		return _jservice;
	}

	@Override
	public void serialize(Geometry geom, OutputStream out) throws JsonException {
		JsonService service = getJsonService();
		Map<String, Object> map = mapUtil.extract(geom);
		service.serialize(map, out, pretty);
	}

	@Override
	public String serialize(Geometry geom) throws JsonException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		serialize(geom, baos);
		return baos.toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Geometry deserializeGeom(InputStream in) throws JsonException {
		JsonService service = getJsonService();
		Map<String, Object> map = (Map<String, Object>) service.deserialize(in,
				LinkedHashMap.class);
		return mapUtil.populateGeometry(map);
	}

	@Override
	public void serialize(SimpleFeature feature, OutputStream out)
			throws JsonException {
		Map<String, Object> map = mapUtil.extract(feature);
		getJsonService().serialize(map, out, pretty);
	}

	@Override
	public String serialize(SimpleFeature feature) throws JsonException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		serialize(feature, baos);
		return baos.toString();
	}

	@SuppressWarnings("unchecked")
	@Override
	public SimpleFeature deserializeFeature(InputStream in)
			throws JsonException {
		Map<String, Object> map = (Map<String, Object>) getJsonService()
				.deserialize(in, LinkedHashMap.class);
		return mapUtil.populateFeature(map);
	}

	@SuppressWarnings("unchecked")
	@Override
	public SimpleFeature deserializeFeature(InputStream in,
			SimpleFeatureType type) throws JsonException {
		Map<String, Object> map = (Map<String, Object>) getJsonService()
				.deserialize(in, LinkedHashMap.class);
		return mapUtil.populateFeature(map, type);
	}

	@Override
	public void serialize(
			FeatureCollection<SimpleFeatureType, SimpleFeature> coll,
			OutputStream out)
			throws JsonException {
		Map<String, Object> map = mapUtil.extract(coll);
		getJsonService().serialize(map, out, pretty);
	}

	@SuppressWarnings("unchecked")
	@Override
	public FeatureCollection<SimpleFeatureType, SimpleFeature> deserializeFeatureCollection(
			InputStream in)
			throws JsonException {
		Map<String, Object> map = (Map<String, Object>) getJsonService()
				.deserialize(in, LinkedHashMap.class);
		return mapUtil.populateFeatureCollection(map);
	}

	@SuppressWarnings("unchecked")
	@Override
	public FeatureCollection<SimpleFeatureType, SimpleFeature> deserializeFeatureCollection(
			InputStream in,
			SimpleFeatureType type) throws JsonException {
		Map<String, Object> map = (Map<String, Object>) getJsonService()
				.deserialize(in, LinkedHashMap.class);
		return mapUtil.populateFeatureCollection(map, type);
	}

	/**
	 * @return the pretty
	 */
	public boolean isPretty() {
		return pretty;
	}

	/**
	 * @param pretty
	 *            the pretty to set
	 */
	public void setPretty(boolean pretty) {
		this.pretty = pretty;
	}

}
