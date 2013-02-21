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
 * Jun 15, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.spatial.reprojection;

import java.awt.image.DataBuffer;
import java.awt.image.WritableRaster;

import javax.media.jai.RasterFactory;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractDataReprojector<T extends IDataRecord> {

	public static class RequestWrapper {
		public Request req;
		public ReferencedEnvelope env;
	}

	/**
	 * Copy data record into geotools grid coverage object
	 * 
	 * @param dataRecord
	 *            datset
	 * @param env
	 *            geographics bounds for dataset
	 * @return
	 * @throws Exception
	 */
	protected abstract GridCoverage2D getGridCoverage(IDataRecord dataRecord,
			ReferencedEnvelope env) throws Exception;

    /**
     * Copy data record into geotools grid coverage object
     * 
     * @param dataRecord
     *            datset
     * @param env
     *            geographics bounds for dataset
     * @return
     * @throws Exception
     */
    protected abstract GridCoverage2D getMaskCoverage(IDataRecord dataRecord,
            ReferencedEnvelope env) throws Exception;

	/**
	 * Extract data from geotools coverage object into data record object
	 * 
	 * @param coverage
	 * @return
	 */
	protected abstract T extractData(GridCoverage2D coverage);

    /**
     * Extract data from geotools coverage object into data record object with a
     * mask for covering the non-data area.
     * 
     * @param coverage
     * @param maskCoverage
     * @return
     */
    protected abstract T extractData(GridCoverage2D coverage,
            GridCoverage2D maskCoverage);

	/**
	 * Apply slab request to data record, returning result in a new data record
	 * object.
	 * 
	 * @param dataRecord
	 * @param req
	 * @return
	 */
	protected abstract T getDataSlice(IDataRecord dataRecord, Request req);

	/**
	 * @param dataRecord
	 * @return true if this object can operate on native type of data record
	 */
	protected abstract boolean compatible(IDataRecord dataRecord);

	/**
	 * Apply point request to data record, returning result in a new data record
	 * object.
	 * 
	 * @param record
	 * @param req
	 * @return
	 */
	protected abstract IDataRecord getDataPoints(IDataRecord record, Request req);

	/**
	 * Construct a new geotools grid coverage object using data buffer
	 * 
	 * @param name
	 *            name of coverage
	 * @param data
	 *            raw data
	 * @param width
	 * @param height
	 * @param env
	 *            geographic bounds of coverage
	 * @return
	 * @throws Exception
	 */
	public static GridCoverage2D constructGridCoverage(String name,
			DataBuffer data, int width, int height, ReferencedEnvelope env)
			throws Exception {
		WritableRaster raster = RasterFactory.createBandedRaster(data, width,
				height, width, new int[] { 0 }, new int[] { 0 }, null);
		return new GridCoverageFactory().create(name, raster, env);
	}

}
