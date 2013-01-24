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
package com.raytheon.uf.edex.ogc.common;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

/**
 * @author bclement
 * 
 */
public class OgcLayer {

	protected OgcLayer parent;

	protected List<OgcLayer> children;

	protected String name;

	protected String title;

	protected List<String> keywords;

	protected String abs;

	protected List<OgcStyle> styles;

	protected OgcGeoBoundingBox geoBoundingBox;

	protected List<OgcBoundingBox> boundingBox;

	protected List<String> crs;

	protected double minScaleDenom = Double.NaN;

	protected double maxScaleDenom = Double.NaN;

	protected boolean opaque;

	protected int sizeRecord = 0;

	protected List<OgcDimension> dimensions;

	protected static String keySeparator = "/";

	public void addCRS(String crs) {
		this.crs = addToList(this.crs, crs);
	}

	protected <T> List<T> addToList(List<T> l, T item) {
		if (l == null) {
			l = new ArrayList<T>();
		}
		l.add(item);
		return l;
	}

	public void addBoundingBox(OgcBoundingBox bbox) {
		this.boundingBox = addToList(boundingBox, bbox);
	}

	public void addStyle(OgcStyle style) {
		this.styles = addToList(styles, style);
	}

	public void addChildLayer(OgcLayer child) {
		this.children = addToList(children, child);
	}

	public void addDimension(OgcDimension dimention) {
		this.dimensions = addToList(dimensions, dimention);
	}

	public void addKeyword(String keyword) {
		this.keywords = addToList(keywords, keyword);
	}

	public String getKey() {
		return getKey(name);
	}

	public String[] separateKey() {
		return separateKey(name);
	}

	/**
	 * @return the unique key for the source of the layer
	 */
	public static String getKey(String layerName) {
		if (layerName == null) {
			return null;
		}
		return separateKey(layerName)[0];
	}

	public static String[] separateKey(String layerName) {
		if (layerName == null) {
			return null;
		}
		String lname;
		try {
			lname = URLDecoder.decode(layerName, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
		return lname.split(OgcLayer.keySeparator, 2);
	}

	public String getFullTitle() {
		return getKey() + keySeparator + title;
	}

	protected String createName(String key, String name) {
		try {
			return URLEncoder.encode(key + keySeparator + name, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @return the dimentions
	 */
	public List<OgcDimension> getDimensions() {
		return dimensions;
	}

	/**
	 * @param dimensions
	 *            the dimentions to set
	 */
	public void setDimensions(List<OgcDimension> dimensions) {
		this.dimensions = dimensions;
	}

	public OgcLayer getParent() {
		return parent;
	}

	public void setParent(OgcLayer parent) {
		this.parent = parent;
	}

	public List<OgcLayer> getChildren() {
		return children;
	}

	public void setChildren(List<OgcLayer> children) {
		this.children = children;
	}

	public List<OgcStyle> getStyles() {
		return styles;
	}

	public void setStyles(List<OgcStyle> styles) {
		this.styles = styles;
	}

	/**
	 * @return the geoBoundingBox
	 */
	public OgcGeoBoundingBox getGeoBoundingBox() {
		return geoBoundingBox;
	}

	/**
	 * @param geoBoundingBox
	 *            the geoBoundingBox to set
	 */
	public void setGeoBoundingBox(OgcGeoBoundingBox geoBoundingBox) {
		this.geoBoundingBox = geoBoundingBox;
	}

	public List<String> getCrs() {
		return crs;
	}

	public void setCrs(List<String> crs) {
		this.crs = crs;
	}

	/**
	 * @return the boundingBox
	 */
	public List<OgcBoundingBox> getBoundingBox() {
		return boundingBox;
	}

	/**
	 * @param boundingBox
	 *            the boundingBox to set
	 */
	public void setBoundingBox(List<OgcBoundingBox> boundingBox) {
		this.boundingBox = boundingBox;
	}

	public double getMinScaleDenom() {
		return minScaleDenom;
	}

	public void setMinScaleDenom(double minScaleDenom) {
		this.minScaleDenom = minScaleDenom;
	}

	public double getMaxScaleDenom() {
		return maxScaleDenom;
	}

	public void setMaxScaleDenom(double maxScaleDenom) {
		this.maxScaleDenom = maxScaleDenom;
	}

	public boolean isOpaque() {
		return opaque;
	}

	public void setOpaque(boolean opaque) {
		this.opaque = opaque;
	}

	public String getName() {
		return name;
	}

	/**
	 * @param key
	 *            a key that is used in all layers from the layer's source
	 * @param name
	 */
	public void setName(String key, String name) {
		this.name = createName(key, name);
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public List<String> getKeywords() {
		return keywords;
	}

	public void setKeywords(List<String> keywords) {
		this.keywords = keywords;
	}

	public String getAbs() {
		return abs;
	}

	public void setAbs(String abs) {
		this.abs = abs;
	}

	public int getSizeRecord() {
		return sizeRecord;
	}

	public void setSizeRecord(int sizeRecord) {
		this.sizeRecord = sizeRecord;
	}

}
