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
 * Jun 3, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.format;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.Unit;

import org.apache.commons.codec.binary.Base64;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform2D;

import ucar.ma2.Array;
import ucar.ma2.DataType;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriteable;
import ucar.nc2.Variable;
import ucar.nc2.iosp.IOServiceProvider;

import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.wcs.reg.Coverage;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class NetCdfFormatter implements WcsDataFormatter {

	private static final String CF_VERSION = "CF-1.0";
	protected String id = "application/netcdf";

	protected class DirectNetcdfFile extends NetcdfFile {
		public DirectNetcdfFile(IOServiceProvider spi, String name)
				throws IOException {
			super(spi, null, name, null);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.wcs.format.WcsDataFormatter#getIdentifier()
	 */
	@Override
	public String getIdentifier() {
		return id;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.wcs.format.WcsDataFormatter#format(com.raytheon.
	 * uf.common.datastorage.records.IDataRecord, java.io.OutputStream, boolean)
	 */
	@Override
	public void format(Coverage coverage, OutputStream out, boolean base64)
			throws Exception {

		IDataRecord record = coverage.getDataRecord();

		File tempFile = File.createTempFile("wcs", ".nc");
		NetcdfFileWriteable ncFile = NetcdfFileWriteable.createNew(
				tempFile.getAbsolutePath(), false);

		List<Dimension> dimList = new ArrayList<Dimension>();
		List<Dimension> dimListForDataBecauseNetcdfIsOpposite = new ArrayList<Dimension>();
		long[] sizes = record.getSizes();
		// int[] shape = new int[sizes.length];
		for (int i = 0; i < sizes.length; i++) {
			Dimension dim = ncFile.addDimension("dim" + i, (int) sizes[i]);
			dimList.add(dim);
			dimListForDataBecauseNetcdfIsOpposite.add(dim);
			// shape[i] = (int) sizes[i];
		}

		Class<?> javaDataType = null;
		Object dataContent = null;
		DataType ncDataType = null;
		if (record instanceof ByteDataRecord) {
			javaDataType = byte.class;
			ncDataType = DataType.BYTE;
			dataContent = ((ByteDataRecord) record).getByteData();
		} else if (record instanceof FloatDataRecord) {
			javaDataType = float.class;
			ncDataType = DataType.FLOAT;
			dataContent = ((FloatDataRecord) record).getFloatData();
		}

		// reverse netcdf dimensions
		Collections.reverse(dimListForDataBecauseNetcdfIsOpposite);


		ncFile.addGlobalAttribute("Conventions", CF_VERSION);
		Variable dataVariable = ncFile.addVariable("data", ncDataType,
				dimListForDataBecauseNetcdfIsOpposite);

		// TODO - (netcdf) what if I'm not using lat/lon?
		dataVariable.addAttribute(new Attribute("coordinates",
				"Time Latitude Longitude lat lon"));



		// add dimention variables

		GridGeometry2D gridGeom = coverage.getGridGeometry();
		MathTransform2D gridToCrs = gridGeom.getGridToCRS2D(PixelOrientation.UPPER_LEFT);

		int dims = sizes.length;

		int minMax[][] = new int[dims][2];

		for ( int dim = 0; dim < dims; ++dim ) {
			minMax[dim][0] = gridGeom.getGridRange().getLow(dim);
			minMax[dim][1] = gridGeom.getGridRange().getHigh(dim);

			List<Dimension> dimAsList = new ArrayList<Dimension>(1);
			dimAsList.add(dimList.get(dim));
			Variable axisVariable = ncFile.addVariable("dim" + dim,
					DataType.DOUBLE, dimAsList);

			// add attributes to the variable

			// units
			Unit<?> unit = coverage.getCrs().getCoordinateSystem().getAxis(dim).getUnit();
			String unitName = unit.toString();
			if (NonSI.DEGREE_ANGLE.toString().equals(unitName)) {
				unitName = "degree_"
						+ coverage.getCrs().getCoordinateSystem().getAxis(dim)
								.getDirection().name().toLowerCase();
			}

			Attribute axisUnit = new Attribute("units", unitName);
			axisVariable.addAttribute(axisUnit);
			// standard_name
			// TODO - (netcdf) make spec
			String name = coverage.getCrs().getCoordinateSystem().getAxis(dim)
					.getName().toString().replaceAll(" ", "_").toLowerCase();
			Attribute axisName = new Attribute("standard_name", name);
			axisVariable.addAttribute(axisName);
		}

		// create file, then dump data into it
		ncFile.create();

		for (int dim = 0; dim < dims; ++dim) {
			double axisValues[] = new double[minMax[dim][1] - minMax[dim][0]
					+ 1];
			for (int coord = minMax[dim][0]; coord <= minMax[dim][1]; ++coord) {
				double[] point = new double[dims];
				for (int i = 0; i < dims; ++i) {
					if (i == dim) {
						point[i] = coord;
					} else {
						point[i] = minMax[i][0];
					}
				}
				double[] crsPoint = new double[dims];
				gridToCrs.transform(point, 0, crsPoint, 0, 1);
				axisValues[coord] = crsPoint[dim];
			}


			 ncFile.write("dim" + dim, new int[] { 0, 0 },
					Array.factory(double.class, new int[] { minMax[dim][1]
							- minMax[dim][0] + 1 }, axisValues));
			

			// ncFile.flush();
		}

		coverage.getCrs().getCoordinateSystem().getAxis(0).getUnit();

		// reverse shape
		int[] shape = new int[sizes.length];
		for (int i = 0; i < sizes.length; ++i) {
			shape[i] = (int) sizes[sizes.length - (i + 1)];
		}
		
		ncFile.write("data", new int[] { 0, 0 },
				Array.factory(javaDataType, shape, dataContent));

		ncFile.flush();
		ncFile.close();

		
		byte[] data = new byte[(int) tempFile.length()];
		DataInputStream dis = new DataInputStream(new FileInputStream(tempFile));
		dis.readFully(data);
		dis.close();

		if (base64) {
			Base64 b64 = new Base64();
			data = b64.encode(data);
		}
		out.write(data);
		out.flush();
		tempFile.delete();
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

}
