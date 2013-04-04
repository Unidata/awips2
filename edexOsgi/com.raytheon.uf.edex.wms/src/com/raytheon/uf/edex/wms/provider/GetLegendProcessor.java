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
 */
package com.raytheon.uf.edex.wms.provider;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.styling.StyledLayerDescriptor;

import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.WmsSource;
import com.raytheon.uf.edex.wms.styling.SldStyleProvider;
import com.raytheon.uf.edex.wms.util.StyleLibrary;

public class GetLegendProcessor {

	protected String time;

	protected String elevation;

	protected Map<String, String> dimensions;

	protected Integer width;

	protected Integer height;

	protected String username;

	protected Set<String> roles;

	protected WmsLayerManager layerManager;

	protected Log log = LogFactory.getLog(this.getClass());

	public GetLegendProcessor(WmsLayerManager layerManager, String time,
			String elevation, Map<String, String> dimensions, Integer width,
			Integer height, String username, String[] roles) {
		super();
		this.layerManager = layerManager;
		this.time = time;
		this.elevation = elevation;
		this.dimensions = dimensions;
		this.width = width;
		this.height = height;
	}

	protected WmsSource getSource(String layer) throws WmsException {
		return layerManager.getSource(layer, username, roles);
	}

	public BufferedImage getLegend(String layerName, String styleName,
			boolean includeLabels) throws WmsException {
		WmsSource source = getSource(layerName);
		return source.getLegend(layerName, styleName, time, elevation,
				dimensions, height, width);
	}

	public BufferedImage getLegendSld(StyledLayerDescriptor sld)
			throws WmsException {
		StyleLibrary lib = new StyleLibrary(sld);
		return SldStyleProvider.getLegend(lib.getAny(), width, height);
	}

	/**
	 * @param layer
	 * @param style
	 * @param sld
	 * @return
	 * @throws WmsException
	 */
	public BufferedImage getLegendStyleLib(String layer, String style,
			StyledLayerDescriptor sld) throws WmsException {
		SldStyleProvider styler = new SldStyleProvider(sld);
		Map<String, String> dims = new HashMap<String, String>(0);
		Map<String, String> units = new HashMap<String, String>(0);
		return styler.getLegend(layer, style, dims, units, width, height);
	}

	public static BufferedImage applyBackground(BufferedImage img, Color bgColor) {
		BufferedImage rval = new BufferedImage(img.getWidth(), img.getHeight(),
				BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = rval.createGraphics();
		g.setColor(bgColor);
		g.fillRect(0, 0, img.getWidth(), img.getHeight());
		g.drawImage(img, 0, 0, null);
		g.dispose();
		return rval;
	}

}
