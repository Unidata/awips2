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
 * May 9, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.reg;

import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.ogc.common.OgcGeoBoundingBox;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class CoverageDescription {

	protected String identifier;

	protected String title;

	protected String abstractStr;

	protected List<String> crs;

	protected List<String> keywords;

	protected List<DataTime> times;

	protected List<OgcBoundingBox> bboxes;

	protected List<RangeField> rangeFields;

	protected OgcGeoBoundingBox crs84Bbox;

	protected Polygon polygon;

	protected List<Double> gridOffsets;

	protected String gridBaseCrs;

	protected String gridType;

	protected String gridCs;

	protected List<Double> gridOrigin;

	public OgcGeoBoundingBox getCrs84Bbox() {
		return crs84Bbox;
	}

	public void setCrs84Bbox(OgcGeoBoundingBox crs84Bbox) {
		this.crs84Bbox = crs84Bbox;
	}

	public List<Double> getGridOrigin() {
		return gridOrigin;
	}

	public void setGridOrigin(List<Double> gridOrigin) {
		this.gridOrigin = gridOrigin;
	}

	public String getGridType() {
		return gridType;
	}

	public void setGridType(String gridType) {
		this.gridType = gridType;
	}

	public String getGridCs() {
		return gridCs;
	}

	public void setGridCs(String gridCs) {
		this.gridCs = gridCs;
	}

	public String getGridBaseCrs() {
		return gridBaseCrs;
	}

	public void setGridBaseCrs(String gridBaseCrs) {
		this.gridBaseCrs = gridBaseCrs;
	}

	public List<Double> getGridOffsets() {
		return gridOffsets;
	}

	public void setGridOffsets(List<Double> offsets) {
		this.gridOffsets = offsets;
	}

	public Polygon getPolygon() {
		return polygon;
	}

	public void setPolygon(Polygon polygon) {
		this.polygon = polygon;
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getAbstractStr() {
		return abstractStr;
	}

	public void setAbstractStr(String abstractStr) {
		this.abstractStr = abstractStr;
	}

	public List<String> getCrs() {
		return crs;
	}

	public void setCrs(List<String> crs) {
		this.crs = crs;
	}

	public List<String> getKeywords() {
		return keywords;
	}

	public void setKeywords(List<String> keywords) {
		this.keywords = keywords;
	}

	public List<DataTime> getTimes() {
		return times;
	}

	public void setTimes(List<DataTime> times) {
		this.times = times;
	}

	public List<OgcBoundingBox> getBboxes() {
		return bboxes;
	}

	public void setBboxes(List<OgcBoundingBox> bboxes) {
		this.bboxes = bboxes;
	}

	public List<RangeField> getRangeFields() {
		return rangeFields;
	}

	public void setRangeFields(List<RangeField> rangeFields) {
		this.rangeFields = rangeFields;
	}

}
