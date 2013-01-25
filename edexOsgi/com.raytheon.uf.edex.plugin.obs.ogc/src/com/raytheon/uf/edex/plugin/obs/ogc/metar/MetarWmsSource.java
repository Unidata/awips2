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

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.PointDataWmsSource;
import com.raytheon.uf.edex.wms.styling.FeatureStyleProvider;


/**
 * 
 * @author bclement
 * @version 1.0
 */
public class MetarWmsSource extends PointDataWmsSource {

	private static final String geometryField = "location.location";

	private static final FeatureStyleProvider styler = new FeatureStyleProvider(
			"sld/metar/defaultMetar.sld");

	/**
	 * @param props
	 * @param key
	 * @param layerTable
	 * @param styles
	 * @throws Exception
	 */
	public MetarWmsSource(PluginProperties props, LayerTransformer transformer)
			throws Exception {
		super(props, "metar", transformer,
				new MetarFeatureFactory());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.reg.FeatureWmsSource#getGeometryField(java.lang
	 * .String)
	 */
	@Override
	protected String getGeometryField(String layer) {
		// metar has only one layer
		return geometryField;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.reg.FeatureWmsSource#getCRS(java.lang.String)
	 */
	@Override
	protected CoordinateReferenceSystem getCRS(String layer) {
		
		return MapUtil.LATLON_PROJECTION;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.reg.FeatureWmsSource#getStyleProvider(java.lang
	 * .String)
	 */
	@Override
	protected FeatureStyleProvider getStyleProvider(String layer)
			throws WmsException {
		return styler;
	}

}
