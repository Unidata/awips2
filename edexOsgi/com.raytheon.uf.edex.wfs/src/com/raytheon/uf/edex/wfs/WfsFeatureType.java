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
 *
 */
package com.raytheon.uf.edex.wfs;

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.raytheon.uf.edex.wfs.request.QualifiedName;

/**
 * @author bclement
 * 
 */
public class WfsFeatureType {

	protected QualifiedName name;

	protected String title;

	protected String abs;

	protected String defaultSRS;

	protected List<String> keywords = new LinkedList<String>();

	protected List<String> otherSRS = new LinkedList<String>();

	protected OgcGeoBoundingBox bbox;

	public WfsFeatureType(QualifiedName name, String title, String defaultSRS,
			OgcGeoBoundingBox bbox) {
		super();
		this.name = name;
		this.title = title;
		this.defaultSRS = defaultSRS;
		this.bbox = bbox;
	}

	public void addKeyword(String keyword) {
		this.keywords.add(keyword);
	}

	public void addOtherSRS(String srs) {
		this.otherSRS.add(srs);
	}

	public QualifiedName getName() {
		return name;
	}

	public void setName(QualifiedName name) {
		this.name = name;
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @param title
	 *            the title to set
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * @return the abs
	 */
	public String getAbs() {
		return abs;
	}

	/**
	 * @param abs
	 *            the abs to set
	 */
	public void setAbs(String abs) {
		this.abs = abs;
	}

	/**
	 * @return the defaultSRS
	 */
	public String getDefaultSRS() {
		return defaultSRS;
	}

	/**
	 * @param defaultSRS
	 *            the defaultSRS to set
	 */
	public void setDefaultSRS(String defaultSRS) {
		this.defaultSRS = defaultSRS;
	}

	/**
	 * @return the keywords
	 */
	public List<String> getKeywords() {
		return keywords;
	}

	/**
	 * @param keywords
	 *            the keywords to set
	 */
	public void setKeywords(List<String> keywords) {
		this.keywords = keywords;
	}

	/**
	 * @return the otherSRS
	 */
	public List<String> getOtherSRS() {
		return otherSRS;
	}

	/**
	 * @param otherSRS
	 *            the otherSRS to set
	 */
	public void setOtherSRS(List<String> otherSRS) {
		this.otherSRS = otherSRS;
	}

	/**
	 * @return the bbox
	 */
	public OgcGeoBoundingBox getBbox() {
		return bbox;
	}

	/**
	 * @param bbox
	 *            the bbox to set
	 */
	public void setBbox(OgcGeoBoundingBox bbox) {
		this.bbox = bbox;
	}

}
