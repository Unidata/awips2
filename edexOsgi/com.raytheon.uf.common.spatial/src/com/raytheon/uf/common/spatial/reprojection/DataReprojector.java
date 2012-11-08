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
 * May 18, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.common.spatial.reprojection;

import java.awt.Point;
import java.io.FileNotFoundException;
import java.util.Iterator;
import java.util.Set;

import javax.media.jai.Interpolation;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridCoordinates2D;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.ViewType;
import org.geotools.coverage.processing.Operations;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.ReferenceIdentifier;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform2D;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.spatial.reprojection.AbstractDataReprojector.RequestWrapper;
import com.raytheon.uf.common.spatial.reprojection.KeyLocker.KeyLock;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class DataReprojector {

	protected IDataStore dataStore;

	protected String dataSetBase = "Data-";

	protected String dataSet = "Data";

	private AbstractDataReprojector<? extends IDataRecord> _typeProjector;

	protected static Log log = LogFactory.getLog(DataReprojector.class);

	protected static KeyLocker locker = new KeyLocker();

	public DataReprojector(IDataStore dataStore) {
		this.dataStore = dataStore;
	}

	/**
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param spatial
	 *            spatial object tied to requested dataset
	 * @param nativeEnv
	 *            native bounds of dataset
	 * @param crs
	 *            desired crs of returned data
	 * @param coords
	 *            coordinates of requested data in requested crs
	 * @return null if any of the coordinates are out of the bounds of the grid
	 *         geometry
	 * @throws Exception
	 */
	public IDataRecord getProjectedPoints(String group, ISpatialObject spatial,
			ReferencedEnvelope nativeEnv, CoordinateReferenceSystem crs,
			Coordinate[] coords) throws Exception {
		// get envelope in requested projection
		ReferencedEnvelope targetEnv = nativeEnv.transform(crs, true);
		// get target grid geometry
		GridGeometry2D geom = getGridGeometry(targetEnv, spatial.getNx(),
				spatial.getNy());
		Point[] points = new Point[coords.length];
		for (int i = 0; i < points.length; ++i) {
			Coordinate coord = coords[i];
			GridCoordinates2D point = getGridPoint(geom, coord);
			int nx = spatial.getNx();
			int ny = spatial.getNy();
			// coordinate was out of bounds, bail
			if (point.x < 0 || point.x > nx || point.y < 0 || point.y > ny) {
				return null;
			}
			// need to repackage point due to pypies not knowing about
			// gridcoordinates2d
			points[i] = new Point(point);
		}
		String reprojectedDataset = buildDatasetName(crs);
		Request req = Request.buildPointRequest(points);
		return getDataRecordWithReproject(group, reprojectedDataset, spatial,
				crs, req);
	}
	
	/**
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param reprojectedDataset
	 *            dataset name for reprojected data
	 * @param spatial
	 *            spatial object tied to requested dataset
	 * @param crs
	 *            desired crs of returned data
	 * @param req
	 *            datastore request object
	 * @return
	 * @throws Exception
	 */
	protected IDataRecord getDataRecordWithReproject(String group,
			String reprojectedDataset, ISpatialObject spatial,
			CoordinateReferenceSystem crs, Request req) throws Exception {
		IDataRecord dataRecord;
		// check if data has already been reprojected
		if (!datasetExists(group, reprojectedDataset)) {
			// it hasn't lock and reproject
			dataRecord = reprojectLocked(group, reprojectedDataset, spatial,
					crs, req);
		} else {
			// it has, just request the data
			dataRecord = getDataRecord(group, reprojectedDataset, req);
		}
		return dataRecord;
	}

	/**
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param reprojectedDataset
	 *            dataset name for reprojected data
	 * @param spatial
	 *            spatial object tied to requested dataset
	 * @param crs
	 *            desired crs of returned data
	 * @param req
	 *            datastore request object
	 * @return
	 * @throws Exception
	 */
	protected IDataRecord reprojectLocked(String group,
			String reprojectedDataset, ISpatialObject spatial,
			CoordinateReferenceSystem crs, Request req) throws Exception {
		KeyLock lock = null;
		IDataRecord dataRecord;
		try {
			// get reproject lock
			lock = locker.getLock(group + reprojectedDataset);
			lock.lock();
			// recheck that dataset still doesn't exist
			if (!datasetExists(group, reprojectedDataset)) {
				// still not there, reproject
				dataRecord = reprojectAndStore(spatial, group, crs, req);
			} else {
				// another thread created it, just grab it
				dataRecord = getDataRecord(group, reprojectedDataset, req);
			}
			lock.unlock();
			return dataRecord;
		} finally {
			if (lock != null) {
				lock.release();
			}
		}
	}

	/**
	 * @param group
	 * @param dataset
	 * @return true if dataset exists in datastore
	 * @throws FileNotFoundException
	 * @throws StorageException
	 */
	protected boolean datasetExists(String group, String dataset)
			throws FileNotFoundException, StorageException {
		String[] datasets = dataStore.getDatasets(group);
		return ArrayUtils.contains(datasets, dataset);
	}

	/**
	 * @param geom
	 *            Grid geometry
	 * @param coord
	 *            desired geographic coordinate
	 * @return grid point for coordinate
	 * @throws PluginException
	 */
	public static GridCoordinates2D getGridPoint(GridGeometry2D geom,
			Coordinate coord) throws PluginException {
		DirectPosition src = new DirectPosition2D(coord.x, coord.y);
		DirectPosition inGrid = new DirectPosition2D();
		try {
			MathTransform2D crsToGrid2D = geom
					.getCRSToGrid2D(PixelOrientation.UPPER_LEFT);
			crsToGrid2D.transform(src, inGrid);
		} catch (Exception e) {
			throw new PluginException("Unable to get grid point for geometry",
					e);
		}
		// floor of grid points should be upper left of pixel
		int x = (int) Math.floor(inGrid.getOrdinate(0));
		int y = (int) Math.floor(inGrid.getOrdinate(1));
		GridCoordinates2D rval = new GridCoordinates2D(x, y);
		return rval;
	}

	/**
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param spatial
	 *            spatial object tied to requested dataset
	 * @param nativeEnv
	 *            native bounds of dataset
	 * @param targetEnv
	 *            bounds of requested data
	 * @return null if target envelope is out of bounds for dataset
	 * @throws Exception
	 */
	public GridCoverage2D getReprojectedCoverage(String group,
			ISpatialObject spatial, ReferencedEnvelope nativeEnv,
			ReferencedEnvelope targetEnv)
			throws Exception {
		ReferencedDataRecord rep = getReprojected(group, spatial, nativeEnv,
				targetEnv);
		if (rep == null) {
			return null;
		}
		ReferencedEnvelope re = rep.getEnvelope();
		IDataRecord record = rep.getRecord();
		return getTypeProjector(record).getGridCoverage(rep.getRecord(), re);
	}

	/**
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param spatial
	 *            spatial object tied to requested dataset
	 * @param nativeEnvelope
	 *            native bounds of dataset
	 * @param targetEnvelope
	 *            bounds of requested data
	 * @return null if target envelope is out of bounds for dataset
	 * @throws Exception
	 */
	public ReferencedDataRecord getReprojected(String group,
			ISpatialObject spatial, ReferencedEnvelope nativeEnvelope,
			ReferencedEnvelope targetEnvelope)
			throws Exception {
		RequestWrapper req = getRequest(spatial, nativeEnvelope, targetEnvelope);
		if (req == null) {
			return null;
		}
		CoordinateReferenceSystem targetCrs = targetEnvelope
				.getCoordinateReferenceSystem();
		String reprojectedDataset = buildDatasetName(targetCrs);
		IDataRecord dataRecord = getDataRecordWithReproject(group,
				reprojectedDataset, spatial, targetCrs, req.req);
		return new ReferencedDataRecord(dataRecord, req.env);
	}

	/**
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param targetDataset
	 *            dataset name for requested data
	 * @param req
	 *            datastore request object
	 * @return
	 * @throws Exception
	 */
	protected IDataRecord getDataRecord(String group, String targetDataset,
			Request req) throws Exception {
		IDataRecord rval = dataStore.retrieve(group, targetDataset, req);
		return rval;
	}

	/**
	 * Get a projector that is compatible with record
	 * 
	 * @param record
	 * @return
	 * @throws Exception
	 */
	protected AbstractDataReprojector<? extends IDataRecord> getTypeProjector(
			IDataRecord record) throws Exception {
		if (_typeProjector == null || !_typeProjector.compatible(record)) {
			if (record instanceof ByteDataRecord) {
				_typeProjector = new ByteDataReprojector();
			} else if (record instanceof FloatDataRecord) {
				_typeProjector = new FloatDataReprojector();
			} else if (record instanceof ShortDataRecord) {
				_typeProjector = new ShortDataReprojector();
			} else if (record instanceof IntegerDataRecord) {
				_typeProjector = new IntDataReprojector();
			} else {
				throw new Exception("Unsupported data store type");
			}
		}
		return _typeProjector;
	}

	/**
	 * Gets the entire coverage from the store and reprojects it. Reprojected
	 * coverage is then stored under a name derived from the projected crs.
	 * 
	 * @param spatial
	 *            spatial object tied to requested dataset
	 * @param group
	 *            name of the datastore group that contains requested dataset
	 * @param targetCRS
	 *            desired crs of returned data
	 * @param req
	 *            datastore request object
	 * @return datarecord as per the request object
	 * @throws Exception
	 */
	protected IDataRecord reprojectAndStore(ISpatialObject spatial,
			String group, CoordinateReferenceSystem targetCRS, Request req)
			throws Exception {
		GridGeometry2D geom = MapUtil.getGridGeometry(spatial);
		IDataRecord original = getDataRecord(group, dataSet, Request.ALL);
		ReferencedEnvelope env = new ReferencedEnvelope(geom.getEnvelope());
		AbstractDataReprojector<? extends IDataRecord> typeProjector = getTypeProjector(original);
		GridCoverage2D cov = typeProjector.getGridCoverage(original, env);
		GridCoverage2D reprojected = MapUtil.reprojectCoverage(cov, targetCRS);
        IDataRecord rval;

        if (typeProjector instanceof FloatDataReprojector) {
            // TODO So far, the problem that this fixes has only appeared with
            // float data. If it happens with other data we can change this.
            GridCoverage2D maskCov = typeProjector.getMaskCoverage(original,
                    env);
            GridCoverage2D reprojectedMask = (GridCoverage2D) Operations.DEFAULT
                    .resample(maskCov.view(ViewType.GEOPHYSICS), targetCRS,
                            null, Interpolation
                                    .getInstance(Interpolation.INTERP_NEAREST));
            rval = typeProjector.extractData(reprojected, reprojectedMask);
        } else {
            rval = typeProjector.extractData(reprojected);
        }

		rval.setGroup(group);
		rval.setName(buildDatasetName(targetCRS));
		dataStore.addDataRecord(rval);
		dataStore.store();
		return getDataPerReq(rval, req);
	}

	/**
	 * @param record
	 *            data record containing full coverage
	 * @param req
	 *            datastore request object
	 * @return result of applying request object to data record
	 * @throws Exception
	 */
	protected IDataRecord getDataPerReq(IDataRecord record, Request req)
			throws Exception {
		AbstractDataReprojector<? extends IDataRecord> typeProjector = getTypeProjector(record);
		IDataRecord rval;
		switch (req.getType()) {
		case ALL:
			rval = record;
			break;
		case POINT:
			rval = typeProjector.getDataPoints(record, req);
			break;
		case SLAB:
			rval = typeProjector.getDataSlice(record, req);
			break;
		case XLINE:
		case YLINE:
		default:
			throw new Exception("Data reprojector " + req.getType()
					+ " not implemented");
		}
		return rval;
	}

	/**
	 * @param env
	 *            geographic bounds of data
	 * @param nx
	 *            length of x axis
	 * @param ny
	 *            length of y axis
	 * @return
	 */
	public static GridGeometry2D getGridGeometry(ReferencedEnvelope env,
			int nx, int ny) {
		// TODO cache
		GridGeometry2D mapGeom = null;
		mapGeom = new GridGeometry2D(new GeneralGridEnvelope(
				new int[] { 0, 0 }, new int[] { nx, ny }, false), env);
		return mapGeom;
	}

	/**
	 * Build up slice request for reprojected dataset
	 * 
	 * @param record
	 * @param crs
	 * @param targetEnvelope
	 *            bbox in crs
	 * @return null if envelope is outside of data bounds
	 * @throws TransformException
	 * @throws MismatchedDimensionException
	 * @throws FactoryException
	 */
	protected RequestWrapper getRequest(ISpatialObject spatial,
			ReferencedEnvelope nativeEnv, ReferencedEnvelope targetEnvelope)
			throws MismatchedDimensionException,
			TransformException, FactoryException {
		RequestWrapper rval = null;
		CoordinateReferenceSystem targetCrs = targetEnvelope
				.getCoordinateReferenceSystem();
		// get full bounds of reprojected dataset
		ReferencedEnvelope dataEnv = nativeEnv.transform(targetCrs, true);
		if (!dataEnv.intersects((Envelope) targetEnvelope)) {
			// request and data envelopes are disjoint, return null
			return null;
		}
		// get grid geometry for reprojected dataset
		GridGeometry2D geom = getGridGeometry(dataEnv, spatial.getNx(),
				spatial.getNy());
		int[] dims = { spatial.getNx(), spatial.getNy() };
		if (dataEnv.contains((Envelope) targetEnvelope)) {
			// requested slice is entirely inside data bounds
			// build slice based on requested bounds
			rval = getSubSlice(geom, targetEnvelope, dims);
		} else {
			// build slice based on intersection
			Envelope intersection = targetEnvelope.intersection(dataEnv);
			rval = getSubSlice(geom, intersection, dims);
		}
		return rval;
	}

	/**
	 * @param geom
	 *            grid geometry for projected dataset
	 * @param env
	 *            geographic bounds for slice
	 * @param dims
	 *            dimensions of dataset
	 * @return grid slice that corresponds to env
	 * @throws MismatchedDimensionException
	 * @throws TransformException
	 */
	protected RequestWrapper getSubSlice(GridGeometry2D geom, Envelope env,
			int[] dims) throws MismatchedDimensionException, TransformException {
		RequestWrapper rval = new RequestWrapper();
		MathTransform2D crsToGrid2D = geom
				.getCRSToGrid2D(PixelOrientation.UPPER_LEFT);
		// find a slice that has data for entire envelope (can have extra)
		int[][] minmax = transformEnv(crsToGrid2D, env, dims);
		MathTransform2D gridToCrs = crsToGrid2D.inverse();
		// find an envelope that matches the slice (could be a bit larger than
		// previous envelope)
		rval.env = transformGrid(gridToCrs, minmax,
				geom.getCoordinateReferenceSystem());
		rval.req = Request.buildSlab(minmax[0], minmax[1]);
		return rval;
	}

	/**
	 * @param gridToCrs
	 * @param minmax
	 *            2d array holding slice
	 * @param crs
	 * @return
	 * @throws MismatchedDimensionException
	 * @throws TransformException
	 */
	protected ReferencedEnvelope transformGrid(MathTransform2D gridToCrs,
			int[][] minmax, CoordinateReferenceSystem crs)
			throws MismatchedDimensionException, TransformException {
		int[] min = minmax[0];
		int[] max = minmax[1];
		DirectPosition lower = new DirectPosition2D(min[0], min[1]);
		DirectPosition upper = new DirectPosition2D(max[0], max[1]);
		DirectPosition lowerCrs = gridToCrs.transform(lower, null);
		DirectPosition upperCrs = gridToCrs.transform(upper, null);
		double x0 = lowerCrs.getOrdinate(0);
		double x1 = upperCrs.getOrdinate(0);
		// handle y axis flip
		double y0 = upperCrs.getOrdinate(1);
		double y1 = lowerCrs.getOrdinate(1);
		return new ReferencedEnvelope(x0, x1, y0, y1, crs);
	}

	/**
	 * transforms crs coordinates to grid indexes using given math transform
	 * 
	 * @param crsToGrid
	 * @param env
	 * @param dims
	 *            max bounds to be limited to
	 * @return an array with [[minx, miny], [maxx, maxy]]
	 * @throws MismatchedDimensionException
	 * @throws TransformException
	 */
	protected int[][] transformEnv(MathTransform2D crsToGrid, Envelope env,
			int[] dims) throws MismatchedDimensionException, TransformException {
		DirectPosition lower = new DirectPosition2D(env.getMinX(),
				env.getMinY());
		DirectPosition upper = new DirectPosition2D(env.getMaxX(),
				env.getMaxY());
		DirectPosition lowerGrid = crsToGrid.transform(lower, null);
		DirectPosition upperGrid = crsToGrid.transform(upper, null);
		int x0 = (int) Math.floor(lowerGrid.getOrdinate(0));
		// we want ceiling since slices are inclusive
		int x1 = (int) Math.ceil(upperGrid.getOrdinate(0));
		// handle y axis flip
		int y0 = (int) Math.floor(upperGrid.getOrdinate(1));
		// we want ceiling since slices are inclusive
		int y1 = (int) Math.ceil(lowerGrid.getOrdinate(1));
		// truncate requests to dataset dimensions
		if (x0 < 0) {
			x0 = 0;
		}
		if (y0 < 0) {
			y0 = 0;
		}
		if (x1 > dims[0]) {
			x1 = dims[0];
		}
		if (y1 > dims[1]) {
			y1 = dims[1];
		}
		return new int[][] { { x0, y0 }, { x1, y1 } };
	}

	/**
	 * construct the dataset name based on the name of the crs.
	 * 
	 * @param crs
	 * @return
	 */
	protected String buildDatasetName(CoordinateReferenceSystem crs) {
		Set<ReferenceIdentifier> ids = crs.getIdentifiers();
		String code;
		if (ids == null || ids.isEmpty()) {
			code = crs.getName().toString();
		} else {
			Iterator<ReferenceIdentifier> i = ids.iterator();
			code = i.next().toString();
			while (i.hasNext()) {
				code += "-" + i.next().toString();
			}
		}
		return dataSetBase + code;
	}

	public IDataStore getDataStore() {
		return dataStore;
	}

	public void setDataStore(IDataStore dataStore) {
		this.dataStore = dataStore;
	}

	public String getDataSetBase() {
		return dataSetBase;
	}

	public void setDataSetBase(String dataSetBase) {
		this.dataSetBase = dataSetBase;
	}

	public String getDataSet() {
		return dataSet;
	}

	public void setDataSet(String dataSet) {
		this.dataSet = dataSet;
	}

}
