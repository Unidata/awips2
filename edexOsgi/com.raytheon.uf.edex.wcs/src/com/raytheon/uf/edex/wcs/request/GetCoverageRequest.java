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
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.request;

import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.ogc.common.OgcBoundingBox;
import com.raytheon.uf.edex.wcs.reg.RangeField;

public class GetCoverageRequest extends WcsRequest {

	protected String identifier;

	protected String format;

	protected OgcBoundingBox bbox;

	protected List<RangeField> fields;

	protected boolean store = false;

	protected DataTime timeSequence;

	protected String gridBaseCrs;

	protected String gridType;

	protected List<Double> gridOrigin;

	protected List<Double> gridOffsets;

	protected boolean defacto = false;

	public GetCoverageRequest() {
		super(Type.GetCoverage);
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public OgcBoundingBox getBbox() {
		return bbox;
	}

	public void setBbox(OgcBoundingBox bbox) {
		this.bbox = bbox;
	}

	public List<RangeField> getFields() {
		return fields;
	}

	public void setFields(List<RangeField> fields) {
		this.fields = fields;
	}

	public boolean isStore() {
		return store;
	}

	public void setStore(boolean store) {
		this.store = store;
	}

	public DataTime getTimeSequence() {
		return timeSequence;
	}

	public void setTimeSequence(DataTime timeSequence) {
		this.timeSequence = timeSequence;
	}

	public String getGridBaseCrs() {
		return gridBaseCrs;
	}

	public void setGridBaseCrs(String gridBaseCrs) {
		this.gridBaseCrs = gridBaseCrs;
	}

	public String getGridType() {
		return gridType;
	}

	public void setGridType(String gridType) {
		this.gridType = gridType;
	}

	public List<Double> getGridOrigin() {
		return gridOrigin;
	}

	public void setGridOrigin(List<Double> gridOrigin) {
		this.gridOrigin = gridOrigin;
	}

	public List<Double> getGridOffsets() {
		return gridOffsets;
	}

	public void setGridOffsets(List<Double> gridOffsets) {
		this.gridOffsets = gridOffsets;
	}

	/**
	 * @param defacto
	 *            the defacto to set
	 */
	public void setDefacto(boolean defacto) {
		this.defacto = defacto;
	}

	/**
	 * @return the defacto
	 */
	public boolean isDefacto() {
		return defacto;
	}

}
