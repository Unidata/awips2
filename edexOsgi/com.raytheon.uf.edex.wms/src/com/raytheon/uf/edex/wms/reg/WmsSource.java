/**********************************************************************
 *
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
 **********************************************************************/
package com.raytheon.uf.edex.wms.reg;

import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Map;

import org.opengis.feature.simple.SimpleFeature;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.edex.ogc.common.OgcLayer;
import com.raytheon.uf.edex.wms.WmsException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

public interface WmsSource {

	/**
	 * @return a List of OgcLayers. Every layer in the list must use the same
	 *         key in the layer name.
	 */
	public List<OgcLayer> listLayers();

	/**
	 * @param layerName
	 * @return null if layer is not found
	 */
	public OgcLayer getLayer(String layerName) throws WmsException;

	/**
	 * @param layer
	 * @param crs
	 * @param bbox
	 * @param time
	 * @param elevation
	 * @param dimensions
	 * @param scale
	 * @return null if bounding box is disjoint with dataset bounds
	 * @throws WmsException
	 */
	public WmsImage getImage(String layer, String style, boolean defaultStyle,
			CoordinateReferenceSystem targetCRS, Envelope bbox, String time,
			String elevation, Map<String, String> dimensions, double scale)
			throws WmsException;

	public boolean hasUpdated();

	public List<SimpleFeature> getFeatureInfo(String layer,
			CoordinateReferenceSystem crs, Envelope bbox, String time,
			String elevation, Map<String, String> dimensions, Coordinate c,
			double scale) throws WmsException;

	public BufferedImage getLegend(String layer, String style, String time,
			String elevation, Map<String, String> dimensions, Integer height,
			Integer width) throws WmsException;

	/**
	 * @return a unique key that identifies this source
	 */
	public String getKey();

	public boolean isWmtsCapable();

}
