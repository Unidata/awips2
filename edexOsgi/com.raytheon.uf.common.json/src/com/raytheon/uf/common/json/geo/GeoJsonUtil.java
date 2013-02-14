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

import java.io.InputStream;
import java.io.OutputStream;

import org.geotools.feature.FeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

import com.raytheon.uf.common.json.JsonException;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public interface GeoJsonUtil {

	/**
	 * @param geom
	 * @param out
	 * @throws JsonException
	 */
	public void serialize(Geometry geom, OutputStream out) throws JsonException;

	/**
	 * @param geom
	 * @return
	 * @throws JsonException
	 */
	public String serialize(Geometry geom) throws JsonException;

	/**
	 * @param in
	 * @return
	 * @throws JsonException
	 */
	public Geometry deserializeGeom(InputStream in) throws JsonException;

	/**
	 * @param feature
	 * @param out
	 * @throws JsonException
	 */
	public void serialize(SimpleFeature feature, OutputStream out)
			throws JsonException;

	/**
	 * @param feature
	 * @return
	 * @throws JsonException
	 */
	public String serialize(SimpleFeature feature) throws JsonException;

	/**
	 * @param in
	 * @return
	 * @throws JsonException
	 */
	public SimpleFeature deserializeFeature(InputStream in)
			throws JsonException;

	/**
	 * @param coll
	 * @param out
	 * @throws JsonException
	 */
	public void serialize(
			FeatureCollection<SimpleFeatureType, SimpleFeature> coll,
			OutputStream out)
			throws JsonException;

	/**
	 * @param in
	 * @return
	 * @throws JsonException
	 */
	public FeatureCollection<SimpleFeatureType, SimpleFeature> deserializeFeatureCollection(
			InputStream in)
			throws JsonException;

	/**
	 * @param in
	 * @param type
	 * @return
	 * @throws JsonException
	 */
	public SimpleFeature deserializeFeature(InputStream in,
			SimpleFeatureType type) throws JsonException;

	/**
	 * @param in
	 * @param type
	 * @return
	 * @throws JsonException
	 */
	public FeatureCollection<SimpleFeatureType, SimpleFeature> deserializeFeatureCollection(
			InputStream in,
			SimpleFeatureType type) throws JsonException;

}
