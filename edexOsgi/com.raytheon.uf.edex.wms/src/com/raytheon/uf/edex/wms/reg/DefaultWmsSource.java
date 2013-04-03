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
package com.raytheon.uf.edex.wms.reg;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.styling.Style;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.ogc.common.OgcNamespace;
import com.raytheon.uf.edex.ogc.common.OgcStyle;
import com.raytheon.uf.edex.ogc.common.StyleLookup;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.WmsException.Code;
import com.raytheon.uf.edex.wms.styling.CoverageStyleProvider;
import com.raytheon.uf.edex.wms.styling.WmsDataRetriever;
import com.raytheon.uf.edex.wms.styling.WmsStyleChoice;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class DefaultWmsSource extends AbstractWmsSource implements
		WmsDataRetriever {

	public DefaultWmsSource(PluginProperties props, String key,
			LayerTransformer transformer) {
		super(props, key, transformer);
	}

	@Override
	public List<SimpleFeature> getFeatureInfo(String rawLayer,
			CoordinateReferenceSystem crs, Envelope bbox, String time,
			String elevation, Map<String, String> dimensions, Coordinate c,
			double scale) throws WmsException {
		String layer = parseIncomingLayerName(rawLayer);
		PluginDataObject record = getRecord(layer, time, elevation, dimensions,
				null);
		double value;
		try {
			PluginDao dao = getDao();
			value = dao.getHDF5Value(record, crs, c, 0);
		} catch (Exception e) {
			log.error("Problem retrieving feature data", e);
			throw new WmsException(Code.InternalServerError);
		}
		return Arrays.asList(wrapInFeature(layer, value, record));
	}

	protected SimpleFeature wrapInFeature(String name, double value,
			PluginDataObject record) {
		SimpleFeatureTypeBuilder tbuilder = new SimpleFeatureTypeBuilder();
		tbuilder.setName(key);
		tbuilder.setNamespaceURI(OgcNamespace.EDEX);
		tbuilder.add("value", Double.class);
		SimpleFeatureType type = tbuilder.buildFeatureType();
		SimpleFeatureBuilder fbuilder = new SimpleFeatureBuilder(type);
		fbuilder.add(value);
		return fbuilder.buildFeature(name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.WmsSource#getImage(java.lang.String,
	 * java.lang.String, org.opengis.referencing.crs.CoordinateReferenceSystem,
	 * com.vividsolutions.jts.geom.Envelope, java.lang.String,
	 * java.lang.Integer, java.util.Map)
	 */
	@Override
	public WmsImage getImage(String rawLayer, String style,
			boolean defaultStyle, CoordinateReferenceSystem targetCRS,
			Envelope bbox, String time, String elevation,
			Map<String, String> dimensions, double scale) throws WmsException {
		WmsImage rval;
		String layer = parseIncomingLayerName(rawLayer);
		Map<String, String> levelUnits = new HashMap<String, String>(2);
		PluginDataObject record = getRecord(layer, time, elevation, dimensions,
				levelUnits);
		CoverageStyleProvider styler = getStylerInternal(layer);
		WmsStyleChoice choice;
		if (!defaultStyle && style == null) {
			// return a coverage and let them handle the style
			choice = new WmsStyleChoice((Style) null);
		} else {
			// set style to null so we dont try to load an empty string as a
			// colormap name
			if (defaultStyle) {
				style = null;
			}
			choice = styler.getStyle(layer, style, dimensions, levelUnits);
		}
		ReferencedEnvelope envelope = new ReferencedEnvelope(bbox, targetCRS);
		rval = styler.styleData(this, choice, record, envelope);

		return rval;
	}

	/**
	 * @param layer
	 * @return default style provider if layer is null
	 * @throws WmsException
	 */
	protected abstract CoverageStyleProvider getStyleProvider(String layer)
			throws WmsException;

	@Override
	protected StyleLookup getStyleLookup() throws WmsException {
		return getStylerInternal(null);
	}

	protected CoverageStyleProvider getStylerInternal(String layer)
			throws WmsException {
		CoverageStyleProvider rval = getStyleProvider(layer);
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wms.reg.AbstractWmsSource#getStyles()
	 */
	@Override
	public List<OgcStyle> getStyles() {
		CoverageStyleProvider styler;
		try {
			styler = getStylerInternal(null);
		} catch (WmsException e) {
			log.error("Problem getting style provider", e);
			return new ArrayList<OgcStyle>(0);
		}
		return styler.getStyles();
	}

	@Override
	public ReferencedDataRecord getDataRecord(PluginDataObject record,
			ReferencedEnvelope envelope) throws WmsException {
		try {
			PluginDao dao = getDao();
			return dao.getProjected(record, envelope);
		} catch (Exception e) {
			log.error("Unable to get reprojected data for record: " + record, e);
			throw new WmsException(Code.InternalServerError);
		}
	}

	@Override
	public GridCoverage2D getGridCoverage(PluginDataObject record,
			ReferencedEnvelope envelope) throws WmsException {
		try {
			PluginDao dao = getDao();
			return dao.getProjectedCoverage(record, envelope);
		} catch (Exception e) {
			log.error("Unable to get reprojected data for record: " + record, e);
			throw new WmsException(Code.InternalServerError);
		}
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
		CoverageStyleProvider styler = getStylerInternal(layer);
		if (styler == null) {
			throw new WmsException(Code.LayerNotDefined);
		}
		// TODO have a way to populate units
		Map<String, String> levelUnits = new HashMap<String, String>(2);
		return styler.getLegend(layer, style, dimensions, levelUnits, width,
				height);
	}

}
