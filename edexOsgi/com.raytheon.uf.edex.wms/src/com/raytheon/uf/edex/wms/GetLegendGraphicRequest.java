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
package com.raytheon.uf.edex.wms;

import java.util.Map;

import org.geotools.styling.StyledLayerDescriptor;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.wms.WmsProvider.WmsOpType;

public class GetLegendGraphicRequest extends BaseRequest<WmsOpType> {

	protected String layer;

	protected String style;

	protected Integer width;

	protected Integer height;

	protected String time;

	protected String elevation;

	protected Map<String, String> dimensions;

	protected StyledLayerDescriptor sld;

	protected String rule;

	protected String scale;

	protected String featureType;

	protected Boolean transparent;

	protected String bgcolor;

	public GetLegendGraphicRequest(GetLegendGraphicRequest req) {
		this.layer = req.getLayer();
		this.style = req.getStyle();
		this.width = req.getWidth();
		this.height = req.getHeight();
		this.time = req.time;
		this.elevation = req.elevation;
		this.dimensions = req.dimensions;
		this.sld = req.getSld();
		this.rule = req.getRule();
		this.scale = req.getScale();
		this.featureType = req.getFeatureType();
	}

	public GetLegendGraphicRequest(String layer, String style, Integer width,
			Integer height, String time, String elevation,
			Map<String, String> dimensions, StyledLayerDescriptor sld,
			String rule, String scale, String featureType) {
		this.layer = layer;
		this.style = style;
		this.width = width;
		this.height = height;
		this.time = time;
		this.elevation = elevation;
		this.dimensions = dimensions;
		this.sld = sld;
		this.rule = rule;
		this.scale = scale;
		this.featureType = featureType;
	}

	/**
	 * @return the layers
	 */
	public String getLayer() {
		return layer;
	}

	/**
	 * @param layers
	 *            the layers to set
	 */
	public void setLayer(String layer) {
		this.layer = layer;
	}

	/**
	 * @return the styles
	 */
	public String getStyle() {
		return style;
	}

	/**
	 * @param styles
	 *            the styles to set
	 */
	public void setStyle(String style) {
		this.style = style;
	}

	/**
	 * @return the width
	 */
	public Integer getWidth() {
		return width;
	}

	/**
	 * @param width
	 *            the width to set
	 */
	public void setWidth(Integer width) {
		this.width = width;
	}

	/**
	 * @return the height
	 */
	public Integer getHeight() {
		return height;
	}

	/**
	 * @param height
	 *            the height to set
	 */
	public void setHeight(Integer height) {
		this.height = height;
	}

	/**
	 * @return the time
	 */
	public String getTime() {
		return time;
	}

	/**
	 * @param time
	 *            the time to set
	 */
	public void setTime(String time) {
		this.time = time;
	}

	/**
	 * @return the elevation
	 */
	public String getElevation() {
		return elevation;
	}

	/**
	 * @param elevation
	 *            the elevation to set
	 */
	public void setElevation(String elevation) {
		this.elevation = elevation;
	}

	/**
	 * @return the dimensions
	 */
	public Map<String, String> getDimensions() {
		return dimensions;
	}

	/**
	 * @param dimensions
	 *            the dimensions to set
	 */
	public void setDimensions(Map<String, String> dimensions) {
		this.dimensions = dimensions;
	}

	/**
	 * @return the sld
	 */
	public StyledLayerDescriptor getSld() {
		return sld;
	}

	/**
	 * @param sld
	 *            the sld to set
	 */
	public void setSld(StyledLayerDescriptor sld) {
		this.sld = sld;
	}

	public String getRule() {
		return rule;
	}

	public void setRule(String rule) {
		this.rule = rule;
	}

	public String getScale() {
		return scale;
	}

	public void setScale(String scale) {
		this.scale = scale;
	}

	public String getFeatureType() {
		return featureType;
	}

	public void setFeatureType(String featureType) {
		this.featureType = featureType;
	}

	/**
	 * @return the transparent
	 */
	public Boolean getTransparent() {
		return transparent;
	}

	/**
	 * @param transparent
	 *            the transparent to set
	 */
	public void setTransparent(Boolean transparent) {
		this.transparent = transparent;
	}

	/**
	 * @return the bgcolor
	 */
	public String getBgcolor() {
		return bgcolor;
	}

	/**
	 * @param bgcolor
	 *            the bgcolor to set
	 */
	public void setBgcolor(String bgcolor) {
		this.bgcolor = bgcolor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wms.BaseRequest#execute(com.raytheon.uf.edex.wms
	 * .WmsProvider)
	 */
	@Override
	public OgcResponse execute(WmsProvider provider) {
		return provider.getLegendGraphic(this);
	}
}
