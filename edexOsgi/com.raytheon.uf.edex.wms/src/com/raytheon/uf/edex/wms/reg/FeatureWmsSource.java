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
* Dec 1, 2011            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.wms.reg;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.feature.FeatureFactory;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.styling.IFeatureStyleProvider;
import com.vividsolutions.jts.geom.Envelope;

/**
 *
 * @author bclement
 * @version 1.0	
 */
public abstract class FeatureWmsSource extends AbstractWmsSource {

	protected FeatureFactory featureFactory;

	/**
	 * @param props
	 * @param key
	 * @param layerTable
	 * @param styles
	 */
	public FeatureWmsSource(PluginProperties props, String key,
			LayerTransformer transformer, FeatureFactory featureFactory) {
		super(props, key, transformer);
		this.featureFactory = featureFactory;
	}

	protected abstract List<SimpleFeature> getFeatures(String layer,
			CoordinateReferenceSystem crs, Envelope bbox, String time,
			String elevation, Map<String, String> dimensions, double scale)
			throws WmsException;

	/**
	 * @param layer
	 * @return default styler if layer is null
	 */
	protected abstract IFeatureStyleProvider getStyleProvider(String layer)
			throws WmsException;

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.AbstractWmsSource#getStyles()
	 */
	@Override
	public List<OgcStyle> getStyles() {
		IFeatureStyleProvider styler;
		try {
			styler = getStylerInternal(null);
		} catch (WmsException e) {
			log.error("Problem getting styler", e);
			return new ArrayList<OgcStyle>(0);
		}
		return styler.getStyles();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.AbstractWmsSource#getStyleLookup()
	 */
	@Override
	protected StyleLookup getStyleLookup() throws WmsException {
		return getStylerInternal(null);
	}

	protected IFeatureStyleProvider getStylerInternal(String layer)
			throws WmsException {
		IFeatureStyleProvider rval = getStyleProvider(layer);
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#getImage(java.lang.String,
	 * java.lang.String, org.opengis.referencing.crs.CoordinateReferenceSystem,
	 * com.vividsolutions.jts.geom.Envelope, java.lang.String, java.lang.String,
	 * java.util.Map, org.geotools.styling.Style)
	 */
	@Override
	public WmsImage getImage(String rawLayer, String style,
			boolean defaultStyle, CoordinateReferenceSystem targetCRS,
			Envelope bbox, String time, String elevation,
			Map<String, String> dimensions, double scale) throws WmsException {
		String layer = parseIncomingLayerName(rawLayer);
		List<SimpleFeature> features = getFeatures(layer, targetCRS, bbox,
				time, elevation, dimensions, scale);
		if (features.isEmpty()) {
			return null;
		}
		SimpleFeature sample = features.get(0);
		MemoryFeatureCollection coll = new MemoryFeatureCollection(
				sample.getFeatureType());
		coll.addAll(features);
		return getStylerInternal(layer).styleData(coll, layer, style,
				defaultStyle);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#getLegend(java.lang.String,
	 * java.lang.String, boolean, java.lang.String, java.lang.String,
	 * java.util.Map, java.lang.Integer, java.lang.Integer)
	 */
	@Override
	public BufferedImage getLegend(String rawLayer, String style, String time,
			String elevation, Map<String, String> dimensions, Integer height,
			Integer width) throws WmsException {
		String layer = parseIncomingLayerName(rawLayer);
		IFeatureStyleProvider styler = getStyleProvider(layer);
		if (styler == null) {
			throw new WmsException(Code.LayerNotDefined);
		}
		return styler.getLegend(layer, style, dimensions,
				new HashMap<String, String>(0), width, height);
	}

}
