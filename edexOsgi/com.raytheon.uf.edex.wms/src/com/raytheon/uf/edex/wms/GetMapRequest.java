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
package com.raytheon.uf.edex.wms;

import java.util.Map;

import org.geotools.styling.StyledLayerDescriptor;

import com.raytheon.uf.edex.ogc.common.OgcResponse;
import com.raytheon.uf.edex.wms.WmsProvider.WmsOpType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GetMapRequest extends BaseRequest<WmsOpType> {

	protected String[] layers;

	protected String[] styles;

	protected String crs;

	protected String bbox;

	protected Integer width;

	protected Integer height;

	protected Boolean transparent;

	protected String bgcolor;

	protected String time;

	protected String elevation;

	protected Map<String, String> dimensions;

	protected StyledLayerDescriptor sld;

	public GetMapRequest() {
	}

	public GetMapRequest(GetMapRequest req) {
		super(req.getVersion(), req.getFormat(), req.getUserName(), req
				.getRoles());
		this.layers = req.getLayers();
		this.styles = req.getStyles();
		this.crs = req.getCrs();
		this.crs = req.crs;
		this.bbox = req.bbox;
		this.width = req.width;
		this.height = req.height;
		this.transparent = req.transparent;
		this.bgcolor = req.bgcolor;
		this.time = req.time;
		this.elevation = req.elevation;
		this.dimensions = req.dimensions;
		this.sld = req.sld;
	}

	public GetMapRequest(String[] layers, String[] styles, String crs,
			String bbox, Integer width, Integer height, Boolean transparent,
			String bgcolor, String time, String elevation,
			Map<String, String> dimensions, StyledLayerDescriptor sld) {
		super();
		this.layers = layers;
		this.styles = styles;
		this.crs = crs;
		this.bbox = bbox;
		this.width = width;
		this.height = height;
		this.transparent = transparent;
		this.bgcolor = bgcolor;
		this.time = time;
		this.elevation = elevation;
		this.dimensions = dimensions;
		this.sld = sld;
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
		return provider.getMap(this);
	}

	public String[] getLayers() {
		return layers;
	}

	public void setLayers(String[] layers) {
		this.layers = layers;
	}

	public String[] getStyles() {
		return styles;
	}

	public void setStyles(String[] styles) {
		this.styles = styles;
	}

	public Integer getWidth() {
		return width;
	}

	public void setWidth(Integer width) {
		this.width = width;
	}

	public Integer getHeight() {
		return height;
	}

	public void setHeight(Integer height) {
		this.height = height;
	}

	public Boolean getTransparent() {
		return transparent;
	}

	public void setTransparent(Boolean transparent) {
		this.transparent = transparent;
	}

	public String getBgcolor() {
		return bgcolor;
	}

	public void setBgcolor(String bgcolor) {
		this.bgcolor = bgcolor;
	}

	public String getTime() {
		return time;
	}

	public void setTime(String time) {
		this.time = time;
	}

	public String getElevation() {
		return elevation;
	}

	public void setElevation(String elevation) {
		this.elevation = elevation;
	}

	public Map<String, String> getDimensions() {
		return dimensions;
	}

	public void setDimensions(Map<String, String> dimensions) {
		this.dimensions = dimensions;
	}

	public StyledLayerDescriptor getSld() {
		return sld;
	}

	public void setSld(StyledLayerDescriptor sld) {
		this.sld = sld;
	}

	public String getCrs() {
		return crs;
	}

	public void setCrs(String crs) {
		this.crs = crs;
	}

	public String getBbox() {
		return bbox;
	}

	public void setBbox(String bbox) {
		this.bbox = bbox;
	}

}
