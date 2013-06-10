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
 * Sep 8, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.provider;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.styling.NamedLayer;
import org.geotools.styling.Style;
import org.geotools.styling.StyledLayer;
import org.geotools.styling.StyledLayerDescriptor;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.edex.wms.WmsException;
import com.raytheon.uf.edex.wms.reg.WmsImage;
import com.raytheon.uf.edex.wms.reg.WmsSource;
import com.raytheon.uf.edex.wms.util.StyleLibrary;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class GetMapProcessor {

	protected CoordinateReferenceSystem crs;
	protected Envelope env;
	protected String time;
	protected String elevation;
	protected Map<String, String> dimensions;
	protected int width;
	protected int height;
	protected String username;
	protected Set<String> roles;
	protected WmsLayerManager layerManager;
	protected double scale;

	public GetMapProcessor(WmsLayerManager layerManager,
			CoordinateReferenceSystem crs, Envelope env, String time,
			String elevation, Map<String, String> dimensions, int width,
			int height, double scale, String username, String[] roles) {
		this(layerManager, crs, env, time, elevation, dimensions, width,
				height, scale);
		this.username = username;
		this.roles = getAsSet(roles);
	}

	public GetMapProcessor(WmsLayerManager layerManager,
			CoordinateReferenceSystem crs, Envelope env, String time,
			String elevation, Map<String, String> dimensions, int width,
			int height, double scale) {
		super();
		this.layerManager = layerManager;
		this.crs = crs;
		this.env = env;
		this.time = time;
		this.elevation = elevation;
		this.dimensions = dimensions;
		this.width = width;
		this.height = height;
		this.scale = scale;
	}

	public List<WmsImage> getMapSld(StyledLayerDescriptor sld)
			throws WmsException {
		StyledLayer[] layers = sld.getStyledLayers();
		ArrayList<WmsImage> rval = new ArrayList<WmsImage>(layers.length);
		for (StyledLayer sl : layers) {
			if (sl instanceof NamedLayer) {
				NamedLayer layer = (NamedLayer) sl;
				String layerName = layer.getName();
				WmsSource source = getSource(layerName);
				Style[] styles = layer.getStyles();
				if (styles == null || styles.length < 1) {
					// request a layer with default style
					WmsImage img = source.getImage(layerName, null, true, crs,
							env, time, elevation, dimensions, scale);
					rval.add(img);
				} else {
					for (Style s : styles) {
						WmsImage img = source.getImage(layerName, null, false,
								crs, env, time, elevation, dimensions, scale);
						img.setStyle(s);
						rval.add(img);
					}
				}
			}
		}
		return rval;
	}

	protected WmsSource getSource(String layer) throws WmsException {
		return layerManager.getSource(layer);
	}

	protected Set<String> getAsSet(String[] strs) {
		Set<String> rval = null;
		if (strs != null) {
			rval = new HashSet<String>(Arrays.asList(strs));
		}
		return rval;
	}

	public List<WmsImage> getMapStyleLib(String[] layers, String[] styles,
			StyledLayerDescriptor sld) throws WmsException {
		StyleLibrary lib = new StyleLibrary(sld);
		ArrayList<WmsImage> rval = new ArrayList<WmsImage>(layers.length);
		for (int i = 0; i < layers.length; ++i) {
			String layerName = layers[i];
			WmsSource source = getSource(layerName);
			String styleName = styles[i];
			Style style = null;
			if (styleName != null && styleName.trim().isEmpty()) {
				// use default
				style = lib.getDefault(layerName);
			} else {
				// use library
				style = lib.getNamedStyle(styleName);
			}
			WmsImage img;
			if (style == null) {
				// not in library, pass to source to see if they know it
				img = source.getImage(layerName, styleName, false, crs, env,
						time, elevation, dimensions, scale);
			} else {
				// get without style
				img = source.getImage(layerName, null, false, crs, env, time,
						elevation, dimensions, scale);
				img.setStyle(style);
			}
			rval.add(img);
		}
		return rval;
	}

	public List<WmsImage> getMap(String[] layers, String[] styles)
			throws WmsException {
		ArrayList<WmsImage> rval = new ArrayList<WmsImage>(layers.length);
		for (int i = 0; i < layers.length; ++i) {
			String layerName = layers[i];
			WmsSource source = getSource(layerName);
			String styleName = styles[i];
			boolean defaultStyle = (styleName == null || styleName.isEmpty());
			WmsImage img = source.getImage(layerName, styleName, defaultStyle,
					crs, env, time, elevation, dimensions, scale);
			if (img != null) {
				rval.add(img);
			}
		}
		return rval;
	}

}
