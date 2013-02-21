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
 * Jun 30, 2011            jelkins     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.reg;

import java.lang.Thread.State;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransform2D;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.ogc.common.db.LayerTransformer;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.edex.wcs.WcsException;
import com.raytheon.uf.edex.wcs.WcsException.Code;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Default WcsSource
 * 
 * @author jelkins
 * @version 1.0
 */
public abstract class DefaultWcsSource implements WcsSource {

	protected LayerTransformer transformer;
	private PluginDao _dao;
	protected PluginProperties props;
	protected Log log = LogFactory.getLog(this.getClass());
	protected Set<String> idCache = new HashSet<String>();
	protected Thread idUpdateThread;

	abstract protected CoverageTransform getCoverageTransform();

	/**
	 * 
	 */
	public DefaultWcsSource(PluginProperties props, LayerTransformer transformer) {
		this.props = props;
		this.transformer = transformer;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.reg.WcsSource#listCoverages()
	 */
	@Override
	public List<CoverageDescription> listCoverages() {
		return getCoverageList(false);
	}

	/**
	 * @param wait
	 * @return
	 */
	protected List<CoverageDescription> getCoverageList(boolean wait) {
		List<CoverageDescription> coverageList = null;
		try {
			coverageList = getCoverageTransform().transform(
					transformer.getLayers(), true);
		} catch (Exception e) {
			log.error("Unable to get plugin dao", e);
			return new ArrayList<CoverageDescription>(0);

		}
		updateIdCache(coverageList, wait);
		return coverageList;
	}

	protected PluginDao getDao() throws PluginException {
		if (_dao == null) {
			_dao = PluginFactory.getInstance().getPluginDao(
					props.getPluginName());
		}
		return _dao;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wcs.reg.WcsSource#describeCoverage(java.lang.String)
	 */
	@Override
	public CoverageDescription describeCoverage(String identifier)
			throws WcsException {
		CoverageDescription rval;
		try {
			SimpleLayer layer = transformer.find(identifier);
			if (layer == null) {
				throw new WcsException(Code.LayerNotDefined);
			}
			rval = getCoverageTransform().transform(layer, false);
		} catch (DataAccessLayerException e) {
			log.error("Problem accessing layers", e);
			throw new WcsException(Code.InternalServerError, e);
		} catch (Exception e) {
			log.error("Unable to get plugin dao", e);
			throw new WcsException(Code.InternalServerError);
		}
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.reg.WcsSource#getCoverage(java.lang.String,
	 * com.raytheon.uf.common.time.DataTime,
	 * org.opengis.referencing.crs.CoordinateReferenceSystem,
	 * com.vividsolutions.jts.geom.Envelope)
	 */
	@Override
	public Coverage getCoverage(String identifier, DataTime time,
			CoordinateReferenceSystem crs, Envelope bbox,
			List<RangeField> rangeFields) throws WcsException {
		PluginDataObject record = getRecord(identifier, time, rangeFields);
		time = record.getDataTime();
		ReferencedDataRecord dataRecord = getDataRecord(record, crs, bbox);
		if (dataRecord == null) {
			return null;
		}
		Coverage c = new Coverage();
		c.setDataRecord(dataRecord.getRecord());
		c.setEnvelope(dataRecord.getEnvelope());
		c.setCrs(crs);
		String timeString = LayerTransformer.format(time.getRefTime());
		c.setTime(timeString);
		GridGeometry2D gridGeom = getGridGeometry(dataRecord.getRecord(),
				dataRecord.getEnvelope());
		c.setGridGeometry(gridGeom);
		return c;
	}

	/**
	 * @param identifier
	 * @return
	 */
	protected Date getDefaultTime(String identifier) throws WcsException {
		try {
			SimpleLayer layer = transformer.find(identifier);
			return layer.getDefaultTime();
		} catch (DataAccessLayerException e) {
			log.error("Problem accessing layers", e);
			throw new WcsException(Code.InternalServerError, e);
		} catch (Exception e) {
			log.error("Unable to get plugin dao", e);
			throw new WcsException(Code.InternalServerError);
		}
	}

	/**
	 * @param record
	 * @param crs
	 * @param bbox
	 * @return
	 * @throws WcsException
	 */
	protected ReferencedDataRecord getDataRecord(PluginDataObject record,
			CoordinateReferenceSystem crs, Envelope bbox) throws WcsException {
		try {
			// get the projected slice from the DAO
			ReferencedDataRecord projectedRecord = getDao().getProjected(
					record, crs, bbox);

			ReferencedDataRecord rval;
			if (projectedRecord != null) {
				rval = projectedRecord;
			} else {
				rval = null;
			}

			// check if we need to pad ( possibly have no data and need to
			// create all "null" response
			if (projectedRecord == null) {
				// Need all "null" response
				Object padValue = getNullPadValue();
				// Make an empty record with the proper data type
				IDataRecord emptyRecord = makeEmptyRecord(padValue, "Data-"
						+ crs.getName().toString(), record.getDataURI());
				// make an envelope for the empty record ( covers no area )
				ReferencedEnvelope emptyEnvelope = new ReferencedEnvelope(0, 0,
						0, 0, crs);

				projectedRecord = new ReferencedDataRecord(emptyRecord,
						emptyEnvelope);

				GridGeometry2D dataGridGeom = getGridGeometry(emptyRecord,
						emptyEnvelope);

				// send through padder to get all nulls
				ReferencedDataRecord resultingRecord = padData(projectedRecord,
						dataGridGeom, bbox);
				rval = resultingRecord;

			} else if (projectedRecord.getEnvelope().equals(bbox)
					|| projectedRecord.getEnvelope().contains(bbox)) {
				// All is well
			} else {
				// need to resize (grow only) and pad with nulls
				GridGeometry2D dataGridGeom = getGridGeometry(
						projectedRecord.getRecord(),
						projectedRecord.getEnvelope());
				ReferencedDataRecord resultingRecord = padData(projectedRecord,
						dataGridGeom, bbox);
				rval = resultingRecord;

			}

			return rval;
		} catch (Exception e) {
			log.error("Unable to get reprojected data for record: " + record, e);
			throw new WcsException(Code.InternalServerError);
		}
	}

	/**
	 * @param padValue
	 * @return
	 */
	private IDataRecord makeEmptyRecord(Object padValue, String name,
			String group) throws WcsException {

		if (padValue instanceof byte[] || padValue instanceof Byte) {
			byte[] data = new byte[] {};
			ByteDataRecord record = new ByteDataRecord(name, group, data, 2,
					new long[] { 0, 0 });
			return record;
		} else if (padValue instanceof float[] || padValue instanceof Float) {
			float[] data = new float[] {};
			FloatDataRecord record = new FloatDataRecord(name, group, data, 2,
					new long[] { 0, 0 });
			return record;
		} else if (padValue instanceof short[] || padValue instanceof Short) {
			short[] data = new short[] {};
			ShortDataRecord record = new ShortDataRecord(name, group, data, 2,
					new long[] { 0, 0 });
			return record;
		} else if (padValue instanceof int[] || padValue instanceof Integer) {
			int[] data = new int[] {};
			IntegerDataRecord record = new IntegerDataRecord(name, group, data,
					2, new long[] { 0, 0 });
			return record;
		} else if (padValue instanceof long[] || padValue instanceof Long) {
			long[] data = new long[] {};
			LongDataRecord record = new LongDataRecord(name, group, data, 2,
					new long[] { 0, 0 });
			return record;
		} else if (padValue instanceof String[] || padValue instanceof String) {
			String[] data = new String[] {};
			StringDataRecord record = new StringDataRecord(name, group, data,
					2, new long[] { 0, 0 });
			return record;
		} else {
			log.error("bad type when making record, "
					+ padValue.getClass().toString(), new Exception());
			throw new WcsException(Code.InternalServerError);
		}
	}

	/**
	 * @param projectedRecord
	 * @return
	 */
	private ReferencedDataRecord padData(
			ReferencedDataRecord projectedRecord, GridGeometry2D gridGeom,
			Envelope targetEnv) throws WcsException {
		if (projectedRecord.getEnvelope().contains(targetEnv)) {
			// no need to pad
			return projectedRecord;
		} else if (projectedRecord.getEnvelope().intersects(targetEnv)) {
			// there is some padding
			try {
				int[][] dataRange = getContainingGridPoints(
						gridGeom.getCRSToGrid2D(PixelOrientation.UPPER_LEFT),
						projectedRecord.getEnvelope());
				int[][] targetRange = getContainingGridPoints(
						gridGeom.getCRSToGrid2D(PixelOrientation.UPPER_LEFT),
						targetEnv);

				int[][] padRange = calculatePadRange(dataRange, targetRange);

				int[] dataSize = new int[2];

				dataSize[0] = (int) projectedRecord.getRecord().getSizes()[0];
				dataSize[1] = (int) projectedRecord.getRecord().getSizes()[1];
				
				IDataRecord paddedDataRecord = padDataInternal(
						projectedRecord.getRecord(), dataSize, padRange);
				
				// make an adjusted grid geometry to account for padding
				// shift old grid corners by minimum pad
				GridGeometry2D adjustedGridGeom = getGridGeometry(
						padRange[0][0], dataSize[0] + padRange[0][0] - 1,
						padRange[0][1], dataSize[1] + padRange[0][1] - 1,
						projectedRecord.getEnvelope());

				ReferencedEnvelope finalEnvelope = makeReferencedEnvelope(
						adjustedGridGeom, paddedDataRecord);

				ReferencedDataRecord record = new ReferencedDataRecord(
						paddedDataRecord, finalEnvelope);

				return record;

			} catch (TransformException e) {
				log.error("Transform Exception while padding partial record", e);
				throw new WcsException(Code.InternalServerError);
			}

		} else {
			// need 100% nulls
			try {
				int[][] targetRange = getContainingGridPoints(
						gridGeom.getCRSToGrid2D(), targetEnv);
				int[] targetDims = new int[] {
						Math.abs(targetRange[0][0])
								+ Math.abs(targetRange[1][0]),
						Math.abs(targetRange[0][1])
								+ Math.abs(targetRange[1][1]) };
				int targetSize = targetDims[0] * targetDims[1];

				IDataRecord record = projectedRecord.getRecord();
				Object sourceData = record.getDataObject();

				Object targetArray = makeArrayTypeFromRecord(record, targetSize);

				fillArray(targetArray, getNullPadValueFromType(sourceData));

				IDataRecord paddedDataRecord = makeDataRecordOfSameType(record,
						targetArray, 2, new long[] { targetDims[0],
								targetDims[1] });

				ReferencedEnvelope finalEnvelope = makeReferencedEnvelope(
						gridGeom, paddedDataRecord);

				ReferencedDataRecord referencedRecord = new ReferencedDataRecord(
						paddedDataRecord, finalEnvelope);

				return referencedRecord;

			} catch (TransformException e) {
				log.error("Transform Exception while making all null pads", e);
				throw new WcsException(Code.InternalServerError);
			}
		}
	}

	/**
	 * makes a geospatial referenced envelope
	 * 
	 * @param gridGeom
	 * @param paddedDataRecord
	 * @return
	 */
	protected ReferencedEnvelope makeReferencedEnvelope(
			GridGeometry2D gridGeom, IDataRecord dataRecord)
			throws WcsException, TransformException {
		MathTransform transform = gridGeom
				.getGridToCRS(PixelOrientation.UPPER_LEFT);

		DirectPosition srcUpperLeft = new DirectPosition2D(0, 0);
		DirectPosition srcLowerRight = new DirectPosition2D(
				dataRecord.getSizes()[0], dataRecord.getSizes()[1]);

		try {
			DirectPosition destUpperLeft = transform.transform(srcUpperLeft,
					null);
			DirectPosition destLowerRight = transform.transform(srcLowerRight,
					null);
			
			ReferencedEnvelope rval = new ReferencedEnvelope(
					destUpperLeft.getOrdinate(0),
					destLowerRight.getOrdinate(0),
					destLowerRight.getOrdinate(1),
					destUpperLeft.getOrdinate(1),
					gridGeom.getCoordinateReferenceSystem());

			return rval;
		} catch (MismatchedDimensionException e) {
			log.error(
					"error while transforming grid coordinates to referenced envelope",
					e);
			throw new WcsException(Code.InternalServerError);
		}
	}

	/**
	 * @param record
	 * @param dataSize
	 * @param padRange
	 * @return
	 * @throws WcsBadTypeException
	 */
	private IDataRecord padDataInternal(IDataRecord record, int[] dataSize,
			int[][] padRange) throws WcsException {

		int[] newSize = new int[] {
				dataSize[0] + padRange[0][0] + padRange[1][0],
				dataSize[1] + padRange[0][1] + padRange[1][1] };
		int newTotalSize = newSize[0] * newSize[1];

		int dataArraySize = dataSize[0] * dataSize[1];
		
		Object sourceData = record.getDataObject();
		
		Object targetArray = makeArrayTypeFromRecord(record, newTotalSize);

		fillArray(targetArray, getNullPadValueFromType(sourceData));

		for (int srcIndex = 0; srcIndex < dataArraySize; ++srcIndex) {

			// get row and column index from data
			int srcRow = srcIndex / dataSize[0];
			int srcColumn = srcIndex % dataSize[0];

			// calculate new row and column
			// add min y pad to source row to get destination row
			int destRow = padRange[0][1] + srcRow;
			// add min x pad to source column for destination column
			int destColumn = padRange[0][0] + srcColumn;

			int targetIndex = (destRow * newSize[0]) + destColumn;

			copyValue(sourceData, srcIndex, targetArray, targetIndex);
		}

		IDataRecord rval = makeDataRecordOfSameType(record, targetArray, 2,
				new long[] { newSize[0], newSize[1] });

		return rval;
	}

	private IDataRecord makeDataRecordOfSameType(IDataRecord record,
			Object newDataArray, int dims, long[] sizes)
 throws WcsException {
		IDataRecord rval = null;

		if (record instanceof ByteDataRecord) {
			rval = new ByteDataRecord(record.getName(), record.getGroup(),
					(byte[]) newDataArray, dims, sizes);
		} else if (record instanceof FloatDataRecord) {
			rval = new FloatDataRecord(record.getName(), record.getGroup(),
					(float[]) newDataArray, dims, sizes);
		} else if (record instanceof IntegerDataRecord) {
			rval = new IntegerDataRecord(record.getName(), record.getGroup(),
					(int[]) newDataArray, dims, sizes);
		} else if (record instanceof LongDataRecord) {
			rval = new LongDataRecord(record.getName(), record.getGroup(),
					(long[]) newDataArray, dims, sizes);
		} else if (record instanceof ShortDataRecord) {
			rval = new ShortDataRecord(record.getName(), record.getGroup(),
					(short[]) newDataArray, dims, sizes);
		} else if (record instanceof StringDataRecord) {
			rval = new StringDataRecord(record.getName(), record.getGroup(),
					(String[]) newDataArray, dims, sizes);
		} else {
			log.error("Unknown IDataRecord type. type: "
					+ record.getClass().toString(), new Exception());
			throw new WcsException(Code.InternalServerError);
		}

		return rval;
	}

	private Object makeArrayTypeFromRecord(IDataRecord record, int size)
			throws WcsException {
		Object targetArray = null;


		if (record instanceof ByteDataRecord) {
			targetArray = new byte[size];
		} else if (record instanceof FloatDataRecord) {
			targetArray = new float[size];
		} else if (record instanceof IntegerDataRecord) {
			targetArray = new int[size];
		} else if (record instanceof LongDataRecord) {
			targetArray = new long[size];
		} else if (record instanceof ShortDataRecord) {
			targetArray = new short[size];
		} else if (record instanceof StringDataRecord) {
			targetArray = new String[size];
		} else {
			log.error("Unknown IDataRecord type when making array. type: "
					+ record.getClass().toString(), new Exception());
			throw new WcsException(Code.InternalServerError);
		}

		return targetArray;
	}

	/**
	 * @param sourceData
	 * @param srcIndex
	 * @param targetArray
	 * @param targetIndex
	 */
	private void copyValue(Object sourceData, int srcIndex, Object targetArray,
			int targetIndex) throws WcsException {

		if (sourceData instanceof byte[] && targetArray instanceof byte[]) {
			((byte[]) targetArray)[targetIndex] = ((byte[]) sourceData)[srcIndex];
		} else if (sourceData instanceof float[]
				&& targetArray instanceof float[]) {
			((float[]) targetArray)[targetIndex] = ((float[]) sourceData)[srcIndex];
		} else if (sourceData instanceof short[]
				&& targetArray instanceof short[]) {
			((short[]) targetArray)[targetIndex] = ((short[]) sourceData)[srcIndex];
		} else if (sourceData instanceof int[] && targetArray instanceof int[]) {
			((int[]) targetArray)[targetIndex] = ((int[]) sourceData)[srcIndex];
		} else if (sourceData instanceof long[]
				&& targetArray instanceof long[]) {
			((long[]) targetArray)[targetIndex] = ((long[]) sourceData)[srcIndex];
		} else if (sourceData instanceof String[]
				&& targetArray instanceof String[]) {
			((String[]) targetArray)[targetIndex] = ((String[]) sourceData)[srcIndex];
		} else {
			log.error("bad type when making record, "
					+ sourceData.getClass().toString() + " and "
					+ targetArray.getClass().toString(), new Exception(
					"bad type when making record"));
			throw new WcsException(Code.InternalServerError);
		}

	}

	/**
	 * @param targetArray
	 * @param nullPadValue
	 */
	private void fillArray(Object targetArray, Object nullPadValue)
			throws WcsException {

		if (targetArray instanceof byte[]) {
			Arrays.fill((byte[]) targetArray, ((Byte) nullPadValue).byteValue());
		} else if (targetArray instanceof float[]) {
			Arrays.fill((float[]) targetArray,
					((Float) nullPadValue).floatValue());
		} else if (targetArray instanceof short[]) {
			Arrays.fill((short[]) targetArray,
					((Short) nullPadValue).shortValue());
		} else if (targetArray instanceof int[]) {
			Arrays.fill((int[]) targetArray,
					((Integer) nullPadValue).intValue());
		} else if (targetArray instanceof long[]) {
			Arrays.fill((long[]) targetArray, ((Long) nullPadValue).longValue());
		} else if (targetArray instanceof String[]) {
			Arrays.fill((String[]) targetArray, (String) nullPadValue);
		} else {
			log.error("Unknown array type when trying to fill array. type: "
					+ targetArray.getClass().toString(), new Exception());
			throw new WcsException(Code.InternalServerError);
		}

	}

	protected abstract Object getNullPadValue();

	/**
	 * @param targetArray
	 * @return
	 */
	protected Object getNullPadValueFromType(Object typeOrArray)
			throws WcsException {

		if (typeOrArray instanceof byte[]) {
			return new Byte((byte) 0);
		} else if (typeOrArray instanceof float[]) {
			return new Float(0.0f);
		} else if (typeOrArray instanceof short[]) {
			return new Short((short) 0);
		} else if (typeOrArray instanceof int[]) {
			return new Integer(0);
		} else if (typeOrArray instanceof long[]) {
			return new Long(0L);
		} else if (typeOrArray instanceof String[]) {
			return new String();
		} else {
			log.error("Unknown type when getting default fill value. type: "
					+ typeOrArray.getClass().toString(), new Exception());
			throw new WcsException(Code.InternalServerError);
		}
	}

	/**
	 * @param dataRange
	 * @param targetRange
	 * @return
	 */
	private int[][] calculatePadRange(int[][] dataRange, int[][] targetRange) {
		// for minimum we expect dataRange to be less than or equal to
		// targetRange, if the difference is negative we need to pad
		int minxDiff = targetRange[0][0] - dataRange[0][0];
		int minyDiff = targetRange[0][1] - dataRange[0][1];
		// for maximum we expect dataRange to be greater than or equal to
		// targetRange, if the difference is positive we need to pad
		// swap min/max order because grid and crs count y differently
		int maxxDiff = targetRange[1][0] - dataRange[1][0];
		int maxyDiff = targetRange[1][1] - dataRange[1][1];

		// only pad, do not trim. so we need to check if the dataRange was
		// bigger
		if (minxDiff > 0) {
			minxDiff = 0;
		}
		if (minyDiff > 0) {
			minyDiff = 0;
		}

		if (maxxDiff < 0) {
			maxxDiff = 0;
		}
		if (maxyDiff < 0) {
			maxyDiff = 0;
		}

		return new int[][] { { Math.abs(minxDiff), Math.abs(minyDiff) },
				{ Math.abs(maxxDiff), Math.abs(maxyDiff) } };
	}

	protected int[][] getContainingGridPoints(MathTransform2D crsToGrid,
			Envelope env) throws TransformException {
		DirectPosition lower = new DirectPosition2D(env.getMinX(),
				env.getMinY());
		DirectPosition upper = new DirectPosition2D(env.getMaxX(),
				env.getMaxY());
		DirectPosition lowerGrid = crsToGrid.transform(lower, null);
		DirectPosition upperGrid = crsToGrid.transform(upper, null);

		// make sure to "grow" in the proper direction

		// x axis "direction" is the same in geo and grid
		double minx = lowerGrid.getOrdinate(0);
		double maxx = upperGrid.getOrdinate(0);

		// y axis "direction" is swapped between geo and grid
		double maxy = lowerGrid.getOrdinate(1);
		double miny = upperGrid.getOrdinate(1);

		minx = Math.floor(minx);
		maxx = Math.ceil(maxx);

		miny = Math.floor(miny);
		maxy = Math.ceil(maxy);

		return new int[][] { { (int) minx, (int) miny },
				{ (int) maxx, (int) maxy } };
	}

	protected GridGeometry2D getGridGeometry(IDataRecord record,
			ReferencedEnvelope env) {
		return getGridGeometry(0, (int) record.getSizes()[0] - 1, 0,
				(int) record.getSizes()[1] - 1, env);
	}

	protected GridGeometry2D getGridGeometry(int gridXmin, int gridXmax,
			int gridYmin, int gridYmax, ReferencedEnvelope env) {
		GeneralEnvelope genEnvelope = convertEnvelopeToGeneralEnvelope(env);
		genEnvelope.setCoordinateReferenceSystem(env
				.getCoordinateReferenceSystem());
		GridGeometry2D gridGeom = new GridGeometry2D(new GeneralGridEnvelope(
				new int[] { gridXmin, gridYmin }, new int[] { gridXmax,
						gridYmax }, true), genEnvelope);

		return gridGeom;
	}

	/**
	 * @param identifier
	 * @param time
	 * @return
	 * @throws WcsException
	 */
	protected PluginDataObject getRecord(String identifier, DataTime time,
			List<RangeField> rangeFields) throws WcsException {
		if (time == null) {
			time = new DataTime(getDefaultTime(identifier));
		}
		String uri = buildUri(identifier, time);
		PluginDataObject rval;
		try {
			PluginDao dao = getDao();
			rval = dao.getMetadata(uri);
		} catch (PluginException e) {
			log.error("Unable to query metdata", e);
			throw new WcsException(Code.InternalServerError);
		}
		if (rval == null) {
			throw new WcsException(Code.LayerNotDefined);
		}
		return rval;
	}

	protected String buildUri(String identifier, DataTime time) {
		return ("/" + getKey() + '/' + time + '/' + identifier).replaceAll(" ",
				"_");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.reg.WcsSource#getKey()
	 */
	@Override
	public String getKey() {
		return props.getPluginName();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wcs.reg.WcsSource#hasCoverageDescription(java.lang
	 * .String)
	 */
	@Override
	public boolean hasCoverageDescription(String identifier) {
		if (idCache.isEmpty()) {
			getCoverageList(true);
		}
		return idCache.contains(identifier);
	}

	/**
	 * @param coverageList
	 *            list of coverages to add to the Cache
	 * @param wait
	 *            set to true to wait for the updateThread to complete
	 */
	protected void updateIdCache(final List<CoverageDescription> coverageList,
			boolean wait) {

		if (idUpdateThread == null
				|| idUpdateThread.getState().equals(State.TERMINATED)) {
			idUpdateThread = new Thread() {
				public void run() {
					for (CoverageDescription cd : coverageList) {
						idCache.add(cd.getIdentifier());
					}
				};
			};
		}
		if (!idUpdateThread.isAlive()) {
			idUpdateThread.start();
			if (wait) {
				try {
					idUpdateThread.join();
				} catch (InterruptedException e) {
					log.warn("cache update interrupted", e);
				}
			}
		}
	}

	protected GeneralEnvelope convertEnvelopeToGeneralEnvelope(
			Envelope env) {
		GeneralEnvelope rval = new GeneralEnvelope(2);
		rval.setRange(0, env.getMinX(), env.getMaxX());
		rval.setRange(1, env.getMinY(), env.getMaxY());
		return rval;
	}

}
