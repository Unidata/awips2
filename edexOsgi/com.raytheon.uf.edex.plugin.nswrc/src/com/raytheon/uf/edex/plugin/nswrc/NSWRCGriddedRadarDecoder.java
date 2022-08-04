/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2014 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.edex.plugin.nswrc;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import ucar.ma2.Array;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Attribute;
import ucar.nc2.Dimension;
import ucar.nc2.Group;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.nswrc.NSWRCConstants;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;
import com.raytheon.uf.edex.nswrctools.DecoderTools;
import com.raytheon.uf.edex.nswrctools.NSWRCVariable;

/**
 * NSWRC (NextGen Surveillance and Weather Radar Capability) Gridded file decoder 
 *   - Decodes Wind, Reflectivity & QPE products from gridded NSWRC files
 *   - Stores data in GridRecord objects from the Grid plugin
 *     - Creates one GridRecord object for each product, for each primary data value 
 *       in the file (altitude or time)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2014  3218     tkuster      Initial creation
 * May 28, 2015  4524     bsteffen     Updates for 14.4.1 baseline
 * 
 * </pre>
 * 
 * @author tkuster
 * @version 1.0
 */
public class NSWRCGriddedRadarDecoder extends AbstractDecoder {

    /*
     * Data variables along the pertinent dimensions that shouldn't be displayed
     * independently
     */
    private static List<String> productsToIgnore = Arrays.asList(
            "QualityIndex", "CrossPolCorrelation");

    private static String ALT = "Alt";

    private static String TIME = "Time";

    private static String LAT = "Lat";

    private static String LON = "Lon";

    private static String CORRECTED_REFLECTIVITY = "CorrectedReflectivity";

    private static String UW = "uW";

    private static String VW = "vW";

    private static String RAIN_HOUR = "RainHour";

    private static String RAIN_RATE = "RainRate";

    private static String PRECIP_RATE = "PR";

    private static String TOTAL_PRECIP_1_HR = "TP1hr";

    private String productType = "NONE";

    /**
     * calls decode(File) from the filePath param
     * 
     * @param filePath
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(String filePath) throws DecoderException {
        File file = new File(filePath);
        return decode(file);
    }

    /**
     * attempts to open the file as a netcdf file and pass it to
     * decode(NetcdfFile)
     * 
     * @param file
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(File file) throws DecoderException {
        PluginDataObject[] rval = new PluginDataObject[0];
        NetcdfFile ncFile = null;

        // try to open the file as netcdf
        if (file.exists()) {
            String filePath = file.getAbsolutePath();
            try {
                ncFile = NetcdfFile.open(filePath);

                setProductType(file.getName());
                rval = decode(ncFile);
            } catch (IOException e) {
                throw new DecoderException("Cannot open netcdf file: "
                        + file.getName(), e);
            } finally {
                if (ncFile != null) {
                    try {
                        ncFile.close();
                    } catch (IOException e) {
                        throw new DecoderException("Cannot close netcdf file: "
                                + file.getName(), e);
                    }
                }
            }
        }

        return rval;
    }

    /**
     * This method assumes that the product type will preface the incoming file
     * name. And that the file name will be delimited with '_' characters.
     * 
     * @param file
     *            file name of incoming file
     */
    private void setProductType(String file) {
        productType = file.substring(0, file.indexOf("_")).toLowerCase();
    }

    /**
     * Decodes the data within the passed in NetCDF File
     * 
     * @param file
     * @param fileName
     *            file name of incoming file
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(NetcdfFile file, String fileName)
            throws DecoderException {
        setProductType(fileName);
        return decode(file);
    }

    /**
     * Decodes the data within the passed in NetCDF File. Assumes productType
     * has already been set.
     * 
     * @param file
     * @return
     * @throws DecoderException
     */
    private PluginDataObject[] decode(NetcdfFile file) throws DecoderException {
        PluginDataObject[] rval = new PluginDataObject[0];

        try {
            Group rootGroup = file.getRootGroup();

            /** Dimensions **/
            List<Dimension> rootDimensions = rootGroup.getDimensions();

            Dimension primaryDim = DecoderTools.getDimension(rootDimensions,
                    ALT);
            if (primaryDim == null) {
                primaryDim = DecoderTools.getDimension(rootDimensions, TIME);
            }
            Dimension latDim = DecoderTools.getDimension(rootDimensions, LAT);
            Dimension lonDim = DecoderTools.getDimension(rootDimensions, LON);

            /** Variables **/
            List<Variable> rootVariables = rootGroup.getVariables();

            Variable primaryVar = DecoderTools.getVariable(rootVariables, ALT);
            if (primaryVar == null) {
                primaryVar = DecoderTools.getVariable(rootVariables, TIME);
            }
            Variable latVar = DecoderTools.getVariable(rootVariables, LAT);
            Variable lonVar = DecoderTools.getVariable(rootVariables, LON);

            double[] primaryData = (double[]) primaryVar.read().get1DJavaArray(
                    Double.class);
            double[] lonData = (double[]) lonVar.read().get1DJavaArray(
                    Double.class);
            double[] latData = (double[]) latVar.read().get1DJavaArray(
                    Double.class);

            // Get all of the actual products
            List<NSWRCVariable> dataRecords = getDataVariables(rootVariables,
                    primaryDim, latDim, lonDim, primaryData,
                    primaryVar.getUnitsString());

            /** Attributes **/
            List<Attribute> rootAttributes = rootGroup.getAttributes();

            // Convert from seconds to milliseconds
            long time = DecoderTools.getAttribute(rootAttributes, TIME)
                    .getNumericValue().longValue() * 1000;
            float latSpacing = DecoderTools
                    .getAttribute(rootAttributes, "LatGridSpacing")
                    .getNumericValue().floatValue();
            float lonSpacing = DecoderTools
                    .getAttribute(rootAttributes, "LonGridSpacing")
                    .getNumericValue().floatValue();

            GridCoverage coverage = new LatLonGridCoverage();
            coverage.setNx(lonDim.getLength());
            coverage.setNy(latDim.getLength());
            coverage.setLa1(latData[0]);
            coverage.setLo1(lonData[0]);
            coverage.setSpacingUnit("degree");
            coverage.setDx(lonSpacing);
            coverage.setDy(latSpacing);
            coverage.setFirstGridPointCorner(determineFirstCorner(lonData,
                    latData));
            coverage = getGridFromCache(coverage);

            ArrayList<GridRecord> returnRecords = new ArrayList<GridRecord>(
                    dataRecords.size());

            /*
             * For each product and primary datum, create a GridRecord object
             * with the pertinent data
             */
            for (int i = 0; i < dataRecords.size(); i++) {
                NSWRCVariable dataRecord = dataRecords.get(i);
                Array dataObject = (Array) dataRecord.getDataObject(latDim);
                float[] data = (float[]) dataObject.get1DJavaArray(Float.class);
                // flip array, so lat is in descending order
                if (latData[1] > latData[0]) {
                    data = flipArray(data, lonDim.getLength(),
                            latDim.getLength());
                }
                data = changeToGridFillValue(data);
                Double altObject = (Double) dataRecord
                        .getDataObject(primaryDim);
                String unit = DecoderTools.getVariable(rootVariables,
                        dataRecord.getVariableName()).getUnitsString();

                String name = dataRecord.getVariableName();

                returnRecords.add(createRecord(data, name,
                        altObject.doubleValue(), time, unit, coverage));
            }

            rval = returnRecords.toArray(new PluginDataObject[returnRecords
                    .size()]);

        } catch (Exception e) {
            throw new DecoderException("Unable to process record", e);
        }
        return rval;
    }

    /**
     * Determines the corner of the first coordinate in the data - LL, UL, LR or
     * UR
     */
    private Corner determineFirstCorner(double[] lonData, double[] latData) {
        Corner corner = null;
        if (lonData[0] < lonData[1]) {
            // On the left
            if (latData[0] < latData[1]) {
                // At the bottom
                corner = Corner.LowerLeft;
            } else {
                // At the top
                corner = Corner.UpperLeft;
            }
        } else {
            // On the right
            if (latData[0] < latData[1]) {
                // At the bottom
                corner = Corner.LowerRight;
            } else {
                // At the top
                corner = Corner.UpperRight;
            }
        }
        return corner;
    }

    /**
     * Flips the data array vertically, so the latitude is in descending order -
     * this is needed to render the data correctly
     */
    private float[] flipArray(float[] inData, int nx, int ny) {
        float[] outData = new float[inData.length];

        for (int y = 0; y < ny; ++y) {
            for (int x = 0; x < nx; ++x) {
                outData[(y * nx) + x] = inData[((ny - y - 1) * nx) + x];
            }
        }

        return outData;
    }

    private float[] changeToGridFillValue(float[] data) {
        float[] ret = new float[data.length];
        for (int i = 0; i < data.length; i++) {
            if (data[i] == NSWRCConstants.FILL_VALUE) {
                ret[i] = GridUtil.GRID_FILL_VALUE;
            } else {
                ret[i] = data[i];
            }
        }
        return ret;
    }

    /**
     * Checks the cache for the GridCoverage object.
     */
    private GridCoverage getGridFromCache(GridCoverage coverage) {
        return GridCoverageLookup.getInstance().getCoverage(coverage, true);
    }

    /**
     * Creates a GridRecord for a specified product & primary value
     * 
     * @param data
     *            - The data for the product
     * @param name
     *            - The product name
     * @param primaryDatum
     *            - the primary value (altitude or time)
     * @param time
     *            - the time used for the DataTime range
     * @param units
     *            - the product's units
     * @param coverage
     *            - the GridCoverage associated with the product
     * @return the created GridRecord object
     * @throws DecoderException
     */
    private GridRecord createRecord(float[] data, String name,
            double primaryDatum, long time, String units, GridCoverage coverage)
            throws DecoderException {
        GridRecord record = new GridRecord();

        Level level = new Level();

        // Create or set up the Parameter object
        Parameter param = new Parameter();

        String paramString = null;
        if (CORRECTED_REFLECTIVITY.equals(name)) {
            param = new Parameter(CORRECTED_REFLECTIVITY, name, units);
        } else if ("SignalToNoiseRatio".equals(name)) {
            param = new Parameter("SNR", name, units);
        } else if ("U".equals(name)) {
            paramString = UW;
        } else if ("V".equals(name)) {
            paramString = VW;
        } else if (RAIN_HOUR.equals(name)) {
            paramString = TOTAL_PRECIP_1_HR;
        } else if (RAIN_RATE.equals(name)) {
            paramString = PRECIP_RATE;
            /*
             * The PR param is in mm/s, where the Netcdf file uses mm/h, so
             * convert
             */
            for (int i = 0; i < data.length; i++) {
                data[i] /= (double) TimeUtil.SECONDS_PER_HOUR;
            }
        }

        try {
            level = LevelFactory.getInstance().getLevel("FH", primaryDatum);
            if (paramString != null) {
                // If the parameter already exists, look it up based on
                // abbreviation
                param = ParameterMapper.getInstance().lookupParameter(
                        paramString, "parameters");
            }
        } catch (MultipleMappingException e) {
            throw new DecoderException("Unable to lookup parameter", e);
        }

        // The primary identification (indicates an NSWRC_GRIDDED product)
        record.setDatasetId(NSWRCConstants.NSWRC_GRIDDED + this.productType);
        /*
         * The secondary identification (differentiates between NSWRC_GRIDDED
         * products)
         */
        record.setSecondaryId(name);
        // The time range during which the measurements were taken
        record.setDataTime(new DataTime(new Date(time)));
        // The altitude level
        record.setLevel(level);
        // The parameter object corresponding to the product
        record.setParameter(param);
        // The GridCoverage object corresponding to the product
        record.setLocation(coverage);
        // The actual product data
        record.setMessageData(data);

        return record;
    }

    /**
     * Runs through the variables in the netcdf file and, for each variable relating to the primary dimensions,
     *   adds them to a list that is returned
     *     -This list represents the actual products to be displayed
     *     -Does not include variables identified as products to ignore
     * @param rootVariables - the different variables in the netcdf file
     * @param primaryDim - primary dimension object from the netcdf file
     * @param latDim - the latitude dimension object from the netcdf file
     * @param lonDim - the longitutde dimension object from the netcdf file
     * @param primaryData - the data relating to the primary variable (all of the primary data entries in the netcdf file)
     * @return - a list of all the products from the NetcdfFile
     * @throws IOException
     * @throws InvalidRangeException
     */
    private List<NSWRCVariable> getDataVariables(List<Variable> rootVariables,
            Dimension primaryDim, Dimension latDim, Dimension lonDim,
            double[] primaryData, String altUnits) throws IOException,
            InvalidRangeException {
        List<NSWRCVariable> rval = new ArrayList<NSWRCVariable>();

        for (Variable var : rootVariables) {
            // look for 3-dimensional data along pertinent dimensions
            if (var.getRank() == 3) {
                if (alongDimensions(var)) {
                    if (!productsToIgnore.contains(var.getShortName())) {
                        rval.addAll(splitVariable(var, primaryDim, latDim,
                                lonDim, primaryData, altUnits));
                    }
                }
            }
        }

        return rval;
    }

    /**
     * Determine if variable is along dimensions pertinent to the products
     */
    private boolean alongDimensions(Variable var) {
        String dim0 = var.getDimension(0).getName();
        String dim1 = var.getDimension(1).getName();
        String dim2 = var.getDimension(2).getName();
        return (dim0.equals(ALT) || dim0.equals(TIME))
                && (dim1.equals(LAT) || dim1.equals(LON))
                && (dim2.equals(LAT) || dim2.equals(LON));
    }

    /**
     * Creates a list of objects equal in length to the number of primary dimension entries in the netcdf file
     *   -The objects consist of one primary dimensional datum,
     *    and an array of data for each lat & lon point relating to that datum
     */
    private Collection<? extends NSWRCVariable> splitVariable(Variable var,
            Dimension primaryDim, Dimension latDim, Dimension lonDim,
            double[] primaryData, String altUnits) throws IOException,
            InvalidRangeException {
        List<NSWRCVariable> rval = new ArrayList<NSWRCVariable>();

        for (int i = 0; i < primaryDim.getLength(); ++i) {
            Double primaryDatum = new Double(primaryData[i]);

            Variable subVariable = var.slice(0, i);
            Array slice = subVariable.read();

            NSWRCVariable rec = new NSWRCVariable();
            rec.setVariableName(var.getShortName());
            if (ALT.equals(primaryDim.getName())) {
                // If primary dimension is Altitude, & in km, convert to m
                if ("km".equalsIgnoreCase(altUnits)
                        || "kilometers".equalsIgnoreCase(altUnits)
                        || "kilometers asl".equalsIgnoreCase(altUnits)) {
                    primaryDatum *= 1000;
                }
            }
            rec.addDataObject(primaryDim, primaryDatum);
            /*
             * if the data is in latitude, longitude order, permute to
             * longitude, latitude order
             */
            if (subVariable.getDimension(0).getName().equalsIgnoreCase(LAT)) {
                slice = slice.permute(new int[] { 1, 0 });
            }
            rec.addDataObject(latDim, slice);
            rval.add(rec);
        }
        return rval;
    }
}
