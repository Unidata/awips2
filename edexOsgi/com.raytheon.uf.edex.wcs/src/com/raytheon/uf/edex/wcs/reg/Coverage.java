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
 * May 5, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.reg;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class Coverage {

	protected IDataRecord dataRecord;
	private CoordinateReferenceSystem crs;
	private Envelope envelope;
	private String time;
	private GridGeometry2D gridGeometry;

	/**
	 * @return the crs
	 */
	public CoordinateReferenceSystem getCrs() {
		return crs;
	}

	/**
	 * @return the envelope
	 */
	public Envelope getEnvelope() {
		return envelope;
	}

	/**
	 * @return the dataRecord
	 */
	public IDataRecord getDataRecord() {
		return dataRecord;
	}

	/**
	 * @param dataRecord
	 *            the dataRecord to set
	 */
	public void setDataRecord(IDataRecord dataRecord) {
		this.dataRecord = dataRecord;
	}

	/**
	 * @param envelope
	 */
	public void setEnvelope(Envelope env) {
		this.envelope = env;
	}

	/**
	 * @param crs
	 */
	public void setCrs(CoordinateReferenceSystem crs) {
		this.crs = crs;
	}

	/**
	 * @param timeString
	 */
	public void setTime(String timeString) {
		this.time = timeString;
	}

	/**
	 * @return the time
	 */
	public String getTime() {
		return time;
	}

	/**
	 * @param gridGeometry the gridGeometry to set
	 */
	public void setGridGeometry(GridGeometry2D gridGeometry) {
		this.gridGeometry = gridGeometry;
	}

	/**
	 * @return the gridGeometry
	 */
	public GridGeometry2D getGridGeometry() {
		return gridGeometry;
	}

}
