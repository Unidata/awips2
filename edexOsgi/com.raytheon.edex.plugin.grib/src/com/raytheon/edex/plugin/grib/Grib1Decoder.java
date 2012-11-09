/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.edex.plugin.grib;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ucar.grib.NotSupportedException;
import ucar.grib.grib1.Grib1BinaryDataSection;
import ucar.grib.grib1.Grib1BitMapSection;
import ucar.grib.grib1.Grib1Data;
import ucar.grib.grib1.Grib1GDSVariables;
import ucar.grib.grib1.Grib1GridDefinitionSection;
import ucar.grib.grib1.Grib1Input;
import ucar.grib.grib1.Grib1Pds;
import ucar.grib.grib1.Grib1ProductDefinitionSection;
import ucar.grib.grib1.Grib1Record;
import ucar.grib.grib1.GribPDSParamTable;
import ucar.grid.GridParameter;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.plugin.grib.util.GribLevel;
import com.raytheon.edex.plugin.grib.util.GribModelLookup;
import com.raytheon.edex.plugin.grib.util.GribParameter;
import com.raytheon.edex.plugin.grib.util.GridModel;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.grib.Grib1TableMap;
import com.raytheon.edex.util.grib.GribParamTranslator;
import com.raytheon.edex.util.grib.GribTableLookup;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.dataplugin.level.mapping.MultipleLevelMappingException;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

/**
 * Grib decoder implementation for decoding grib version 1 files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/11/10      4758        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Grib1Decoder extends AbstractDecoder {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Grib1Decoder.class);

    /** Missing value string */
    private static final String MISSING = "Missing";

    /** Set of Time range types for accumulations and averages */
    private static final Set<Integer> AVG_ACCUM_LIST = new HashSet<Integer>();

    static {
        AVG_ACCUM_LIST.add(3);
        AVG_ACCUM_LIST.add(4);
        AVG_ACCUM_LIST.add(6);
        AVG_ACCUM_LIST.add(7);

        IPathManager pm = PathManagerFactory.getPathManager();
        String ucarUserFile = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), "/grib/ucar/userTables.lst")
                .getPath();
        try {
            GribPDSParamTable.addParameterUserLookup(ucarUserFile);
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error reading user parameter tables for ucar grib decoder",
                            e);
        }
    }

    /**
     * Creates a new Grib1Decoder
     */
    public Grib1Decoder() {
        super();
    }

    /**
     * Decodes the grib file provided.
     * 
     * @param gribFileName
     *            The name of the file to be decoded
     * @return The decoded GridRecords
     * @throws GribException
     *             If decoding the file fails or encounters problems
     */
    public GridRecord[] decode(String gribFileName) throws GribException {
        File gribFile = new File(gribFileName);
        RandomAccessFile raf = null;
        try {
            try {
                raf = new RandomAccessFile(gribFile.getAbsolutePath(), "r");
            } catch (IOException e) {
                throw new GribException(
                        "Unable to create RandomAccessFile for grib file: ["
                                + gribFile + "]");
            }
            raf.order(RandomAccessFile.BIG_ENDIAN);
            Grib1Input g1i = new Grib1Input(raf);
            try {
                g1i.scan(false, false);
            } catch (Exception e) {
                throw new GribException("Error scanning grib 1 file: ["
                        + gribFile + "]");
            }
            ArrayList<Grib1Record> records = g1i.getRecords();
            List<GridRecord> gribRecords = new ArrayList<GridRecord>();
            for (int i = 0; i < records.size(); i++) {
                GridRecord rec = decodeRecord(records.get(i), raf);
                if (rec != null) {
                    gribRecords.add(rec);
                }
            }
            return gribRecords.toArray(new GridRecord[] {});
        } finally {
            if (raf != null) {
                try {
                    raf.close();
                } catch (IOException e) {
                    throw new GribException(
                            "Failed to close RandomAccessFile for grib file: ["
                                    + gribFile + "]", e);
                }
            }
        }
    }

    /**
     * Decodes a single record from the grib file
     * 
     * @param rec
     *            The record to decode
     * @param raf
     *            The file object
     * @return The decoded GridRecord
     * @throws GribException
     *             If the record cannot be decoded properly
     */
    private GridRecord decodeRecord(Grib1Record rec, RandomAccessFile raf)
            throws GribException {

        GridRecord retVal = new GridRecord();

        // Extract the sections from the grib record
        Grib1ProductDefinitionSection pds = rec.getPDS();
        Grib1GridDefinitionSection gds = rec.getGDS();
        Grib1Pds pdsVars = pds.getPdsVars();
        Grib1GDSVariables gdsVars = gds.getGdsVars();

        // Initialize some common values
        int centerid = pdsVars.getCenter();
        int subcenterid = pdsVars.getSubCenter();
        String parameterAbbreviation = null;
        String parameterName = null;
        String parameterUnit = null;

        // Some centers use other center's parameter tables so we need to check
        // for that
        int[] tableValue = Grib1TableMap.getInstance().getTableAlias(centerid,
                subcenterid, pdsVars.getParameterTableVersion());
        int centerAlias = tableValue[0];
        int subcenterAlias = tableValue[1];
        int tableAlias = tableValue[2];

        /*
         * Decodes the parameter information from the record. An attempt is
         * first made to map the grib 1 parameter to the equivalent grib 2
         * parameter. If this cannot be successfully executed, the grib 1
         * parameter will be used from the parameter tables contained in the
         * unidata decoder.
         */
        GribParameter parameter = GribTableLookup.getInstance()
                .getGrib2Parameter(centerAlias, subcenterAlias, tableAlias,
                        pdsVars.getParameterNumber());

        if (parameter == null || parameter.getName().equals(MISSING)) {
            try {
                logger.warn("Unable to map Grib 1 parameter to equivalent Grib 2 parameter for center ["
                        + centerid
                        + "] subcenter ["
                        + subcenterid
                        + "] table number ["
                        + pdsVars.getParameterTableVersion()
                        + "] parameter number ["
                        + pdsVars.getParameterNumber()
                        + "]  Using grib 1 parameter mapping");
                GridParameter param = GribPDSParamTable.getParameterTable(
                        centerid, subcenterid,
                        pdsVars.getParameterTableVersion()).getParameter(
                        pdsVars.getParameterNumber());
                parameterName = param.getDescription();
                parameterAbbreviation = param.getName();
                parameterUnit = param.getUnit();

            } catch (NotSupportedException e) {
                throw new GribException("Error getting grib 1 parameter", e);
            }
        } else {
            parameterName = parameter.getName();
            parameterUnit = parameter.getUnit();
            if (parameter.getD2dAbbrev() == null) {
                parameterAbbreviation = parameter.getAbbreviation();
            } else {
                parameterAbbreviation = parameter.getD2dAbbrev();
            }
        }

        // Gets the spatial information
        int gridId = pdsVars.getGridId();

        GridCoverage gridCoverage = null;
        if (gdsVars != null) {
            gridCoverage = getGridCoverage(gdsVars, gridId);
        } else {
            gridCoverage = GribSpatialCache.getInstance().getGridByName(
                    String.valueOf(gridId));
        }

        // Set up variables.
        int nx = new Integer(gridCoverage.getNx());
        int ny = new Integer(gridCoverage.getNy());

        retVal.setLocation(gridCoverage);
        int genProcess = pdsVars.getGenProcessId();

        retVal.addExtraAttribute("centerid", centerid);
        retVal.addExtraAttribute("subcenterid", subcenterid);
        retVal.addExtraAttribute("genprocess", pdsVars.getGenProcessId());
        retVal.addExtraAttribute("backGenprocess", 0);

        // unidata does not handle isEnsemble call when
        // octet size is less than 40.

        if (pdsVars.getLength() > 40 && pdsVars.isEnsemble()) {
            // rcg: added code to get perturbation
            int pos42 = pdsVars.getOctet(42);
            int pos43 = pdsVars.getOctet(43);
            switch (pos42) {
            case 1:
                retVal.setEnsembleId("ctl" + pos43);
                break;
            case 2:
                retVal.setEnsembleId("n" + pos43);
                break;
            case 3:
                retVal.setEnsembleId("p" + pos43);
                break;
            case 4:
                retVal.setEnsembleId("cls" + pos43);
                break;
            default:
                retVal.setEnsembleId(pos42 + "." + pos43);
            }
        } else {
            retVal.setEnsembleId(null);
        }

        retVal.addExtraAttribute("gridid", gridCoverage.getName());

        retVal.setDatasetId(createModelName(centerid, subcenterid, genProcess,
                gridCoverage));

        // Get the level information
        float[] levelMetadata = this.convertGrib1LevelInfo(
                pdsVars.getLevelType1(), (float) pdsVars.getLevelValue1(),
                pdsVars.getLevelType2(), (float) pdsVars.getLevelValue2());
        retVal.setLevel(getLevelInfo(centerid, subcenterid, levelMetadata[0],
                levelMetadata[1], levelMetadata[2], levelMetadata[3],
                levelMetadata[4], levelMetadata[5]));

        // Construct the DataTime
        GregorianCalendar refTime = new GregorianCalendar();
        refTime.setTimeInMillis(pdsVars.getReferenceTime());
        int forecastTime = convertToSeconds(pdsVars.getForecastTime(),
                pdsVars.getTimeUnit());
        int p1 = convertToSeconds(pdsVars.getP1(), pdsVars.getTimeUnit());
        int p2 = convertToSeconds(pdsVars.getP2(), pdsVars.getTimeUnit());
        DataTime dataTime = constructDataTime(
                refTime,
                forecastTime,
                getTimeInformation(refTime, pdsVars.getTimeRangeIndicator(),
                        p1, p2));

        /*
         * Extract the data values from the file. The AVG_ACCUM_LIST is checked
         * to see if this is an accumulation or average grid being initialized.
         * A check is also made for thinned grids.
         */
        Grib1Data gd = new Grib1Data(raf);
        float[] data = null;
        try {
            boolean bmsPresent = pdsVars.bmsExists();
            int scanMode;
            if (gdsVars != null) {
                scanMode = gdsVars.getScanMode();
            } else {
                scanMode = gridCoverage.getScanMode();
            }

            int timeRange = pdsVars.getTimeRangeIndicator();

            // Check for initialization of average or accumulation parameters
            if ((AVG_ACCUM_LIST.contains(timeRange) && dataTime
                    .getValidPeriod().getDuration() == 0)) {
                statusHandler.handle(Priority.EVENTA,
                        "Discarding empty accumulation grid");
                return null;
            } else if ((gdsVars != null && gdsVars.isThin())
                    || (gdsVars == null && (gridCoverage instanceof LatLonGridCoverage && ((LatLonGridCoverage) gridCoverage)
                            .isThin()))) {
                // Unfortunately the UCAR Decoder does Cubic Spline
                // interpolation, however we want to do simpler linear
                // interpolation like Awips 1.
                raf.seek(rec.getDataOffset());
                Grib1BitMapSection bms = null;
                if (pdsVars.bmsExists()) {
                    bms = new Grib1BitMapSection(raf);
                }
                if (gdsVars != null) {
                    Grib1BinaryDataSection bds = new Grib1BinaryDataSection(
                            raf, pdsVars.getDecimalScale(), bms,
                            gdsVars.getScanMode(), gdsVars.getNx(),
                            gdsVars.getNy());
                    data = fillThinnedGrid(bds.getValues(),
                            gdsVars.getParallels(), gdsVars.getNx(),
                            gdsVars.getNy());
                } else {

                    Grib1BinaryDataSection bds = new Grib1BinaryDataSection(
                            raf, pdsVars.getDecimalScale(), bms,
                            gridCoverage.getScanMode(), nx, ny);
                    data = fillThinnedGrid(bds.getValues(),
                            ((LatLonGridCoverage) gridCoverage).getParallels(),
                            nx, ny);
                }

            } else {
                if (gdsVars != null) {
                    data = gd.getData(
                            rec.getDataOffset() - gdsVars.getLength(),
                            rec.getDataOffset(), pdsVars.getDecimalScale(),
                            bmsPresent);
                } else {
                    data = gd.getData(rec.getDataOffset(),
                            pdsVars.getDecimalScale(), bmsPresent);

                    // Correct data count for pole data point.
                    if ("row".equals(gridCoverage.getIncludePole())) {
                        int newSize = nx * ny;
                        float[] dataCopy = new float[newSize];
                        for (int i = 0; i < dataCopy.length; i++) {
                            if (i < data.length) {
                                dataCopy[i] = data[i];
                            }
                        }
                        data = dataCopy;
                    } else if ("column".equals(gridCoverage.getIncludePole())) {
                        int newSize = nx * ny;
                        float[] dataCopy = new float[newSize];
                        for (int i = 0; i < dataCopy.length; i++) {
                            if (i < data.length) {
                                dataCopy[i] = data[i];
                            }
                        }
                        data = dataCopy;
                    }
                }

            }
            if (gdsVars != null) {
                correctForScanMode(data, gdsVars.getNx(), gdsVars.getNy(),
                        bmsPresent, scanMode);
                retVal.setMessageData(data);
            } else {
                correctForScanMode(data, nx, ny, bmsPresent, scanMode);
                retVal.setMessageData(data);
            }

        } catch (IOException e) {
            throw new GribException("Error getting data from grib file", e);
        }

        // Check for subgridding
        String modelName = retVal.getDatasetId();
        GridCoverage subCoverage = GribSpatialCache.getInstance()
                .getSubGridCoverage(modelName, gridCoverage);

        if (subCoverage != null) {
            SubGrid subGrid = GribSpatialCache.getInstance().getSubGrid(
                    modelName, gridCoverage);
            // resize the data array
            float[][] dataArray = this.resizeDataTo2D(data,
                    gridCoverage.getNx(), gridCoverage.getNy());
            dataArray = this.subGrid(dataArray, subGrid.getUpperLeftX(),
                    subGrid.getUpperLeftY(), subGrid.getNX(), subGrid.getNY());
            data = this.resizeDataTo1D(dataArray, subGrid.getNY(),
                    subGrid.getNX());
            retVal.setMessageData(data);
            retVal.setLocation(subCoverage);
        }

        String newAbbr = GribParamTranslator.getInstance().translateParameter(
                1, parameterAbbreviation, centerid, subcenterid, genProcess,
                dataTime, gridCoverage);

        if (newAbbr == null) {
            if (!parameterName.equals(MISSING)
                    && dataTime.getValidPeriod().getDuration() > 0) {
                parameterAbbreviation = parameterAbbreviation
                        + String.valueOf(dataTime.getValidPeriod()
                                .getDuration() / 3600000) + "hr";
            }
        } else {
            parameterAbbreviation = newAbbr;
        }
        parameterAbbreviation = parameterAbbreviation.replaceAll("_", "-");
        try {
            parameterAbbreviation = ParameterMapper.getInstance()
                    .lookupBaseName(parameterAbbreviation, "grib");
        } catch (MultipleMappingException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            parameterAbbreviation = e.getArbitraryMapping();
        }

        retVal.setPluginName("grid");
        Parameter param = new Parameter(parameterAbbreviation, parameterName,
                parameterUnit);
        GribParamTranslator.getInstance().getParameterNameAlias(modelName,
                param);
        retVal.setParameter(param);

        retVal.setPersistenceTime(new Date());
        retVal.setDataTime(dataTime);
        // if (gdsVars != null) {
        // retVal.setResCompFlags(gdsVars.getResolution());
        // } else {
        // retVal.setResCompFlags(gridCoverage.getResolution());
        // }

        try {
            retVal.constructDataURI();
        } catch (PluginException e) {
            throw new GribException("Error constructing grib dataURI", e);
        }

        // check if FLAG.FCST_USED needs to be removed
        checkForecastFlag(retVal.getDataTime(), centerid, subcenterid,
                genProcess, gridCoverage);

        return retVal;
    }

    /**
     * Resizes a 1-D data array into a 2-D array based on the provided row and
     * column count
     * 
     * @param data
     *            The 1-D array of data
     * @param columnCount
     *            The number of columns to map the data to
     * @param rowCount
     *            The number of rows to map the data to
     * @return The 2-D array of data
     */
    private float[][] resizeDataTo2D(float[] data, int columnCount, int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row][column] = data[row * columnCount + column];
            }
        }

        return newGrid;
    }

    // Fill thinned grids using interpolation algorithm from awips1, Basically
    // linear interpolation
    private float[] fillThinnedGrid(float[] inData, int[] parallels, int nx,
            int ny) {
        float[] outData = new float[nx * ny];

        int inIndex = 0;
        int outIndex = 0;

        for (int j = 0; j < ny; j++) {
            if (parallels[j] == 1) {
                for (int i = 0; i < nx; i++) {
                    outData[outIndex++] = inData[inIndex];
                }
                inIndex++;
                continue;
            }
            if (parallels[j] == nx) {
                for (int i = 0; i < nx; i++) {
                    outData[outIndex++] = inData[inIndex++];
                }
                continue;
            }
            float trail = inData[inIndex];
            float dx = (parallels[j] - 1.0f) / (nx - 1.0f);
            float total = dx;
            outData[outIndex++] = inData[inIndex++];
            for (int i = 2; i < nx; i++) {
                if (total > 1) {
                    total -= 1;
                    trail = inData[inIndex++];
                }
                outData[outIndex++] = inData[inIndex] * total + trail
                        * (1 - total);
                total += dx;
            }
            outData[outIndex++] = inData[inIndex++];
        }
        return outData;
    }

    /**
     * Resizes a 2-D array of data to a 1-D array
     * 
     * @param data
     *            The 2-D array of data
     * @param rowCount
     *            The number of rows in the 2-D data array
     * @param columnCount
     *            The number of columns in the 2-D data array
     * @return The 1-D array of data
     */
    private float[] resizeDataTo1D(float[][] data, int rowCount, int columnCount) {
        float[] newGrid = new float[rowCount * columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row * columnCount + column] = data[row][column];
            }
        }
        return newGrid;
    }

    /**
     * Extracts a sub-grid of data from a 2-D array of data
     * 
     * @param data
     *            The 2-D array of data
     * @param startColumn
     *            The start column of the sub-grid
     * @param startRow
     *            The start row of the sub-grid
     * @param columnCount
     *            The number of columns in the sub-grid
     * @param rowCount
     *            The number of rows in the sub-grid
     * @return The sub-grid of data
     */
    private float[][] subGrid(float[][] data, int startColumn, int startRow,
            int columnCount, int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        // sub grid might wrap around a world wide grid, double check bounds
        int endRow = startRow + rowCount;
        int endColumn = startColumn + columnCount;
        if (endColumn > data[0].length) {
            endColumn = data[0].length;
        }
        // determine remaining col count for world wide grids
        columnCount -= endColumn - startColumn;

        int newGridRow = 0;
        int newGridColumn = 0;

        for (int row = startRow; row < endRow; row++, newGridRow++) {
            newGridColumn = 0;
            for (int column = startColumn; column < endColumn; column++, newGridColumn++) {
                newGrid[newGridRow][newGridColumn] = data[row][column];
            }

            // handle grid wrap for world wide grids
            for (int column = 0; column < columnCount; column++, newGridColumn++) {
                newGrid[newGridRow][newGridColumn] = data[row][column];
            }
        }

        return newGrid;
    }

    /**
     * Flips the grid according to the scan mode in the GDS
     * 
     * @param data
     *            The data
     * @param nx
     *            Nx value
     * @param ny
     *            Ny value
     * @param bmsExists
     *            If the bitmap need be applied
     * @param scanMode
     *            The scan mode
     */
    private void correctForScanMode(float[] data, int nx, int ny,
            boolean bmsExists, int scanMode) {
        for (int i = 0; i < data.length; i++) {
            if (bmsExists && data[i] == -9999) {
                data[i] = -999999;
            }
        }
        switch (scanMode) {
        case 0:
            break;
        case 64:
            Util.flipHoriz(data, ny, nx);
            break;
        case 128:
            Util.flipVert(data, ny, nx);
            break;
        case 192:
            Util.rotate180(data, ny, nx);
            break;
        }
    }

    /**
     * Gets the time ranges for the data
     * 
     * @param refTime
     *            The reference time
     * @param timeRange
     *            The time range indicator from the PDS section
     * @param p1
     *            The p1 value
     * @param p2
     *            The p2 value
     * @return An array containing the start time and end time
     */
    private Calendar[] getTimeInformation(Calendar refTime, int timeRange,
            int p1, int p2) {

        GregorianCalendar endTime = null;
        GregorianCalendar startTime = null;
        switch (timeRange) {
        case 2:
        case 3:
        case 4:
        case 5:
        case 51:
            startTime = (GregorianCalendar) refTime.clone();
            startTime.add(Calendar.SECOND, p1);
            endTime = (GregorianCalendar) refTime.clone();
            endTime.add(Calendar.SECOND, p2);
            break;
        case 6:
            startTime = (GregorianCalendar) refTime.clone();
            startTime.add(Calendar.SECOND, p1 * -1);
            endTime = (GregorianCalendar) refTime.clone();
            endTime.add(Calendar.SECOND, p2 * -1);
            break;
        case 7:
            startTime = (GregorianCalendar) refTime.clone();
            startTime.add(Calendar.SECOND, p1 * -1);
            endTime = (GregorianCalendar) refTime.clone();
            endTime.add(Calendar.SECOND, p2);
            break;
        default:
            break;
        }
        return new Calendar[] { startTime, endTime };
    }

    /**
     * Gets the spatial information from the grib record
     * 
     * @param gdsVars
     *            The gds values
     * @param gridNumber
     *            The grid number from the PDS
     * @return The spatial information
     * @throws GribException
     *             If the spatial information cannot be determined correctly
     */
    private GridCoverage getGridCoverage(Grib1GDSVariables gdsVars,
            int gridNumber) throws GribException {
        GridCoverage coverage = null;

        int gridType = gdsVars.getGdtn();
        switch (gridType) {
        case 0: {
            LatLonGridCoverage latLonCoverage = new LatLonGridCoverage();
            latLonCoverage.setNx(gdsVars.getNx());
            latLonCoverage.setNy(gdsVars.getNy());
            double la1 = correctLat(gdsVars.getLa1());
            double lo1 = correctLon(gdsVars.getLo1());
            latLonCoverage.setLa1(la1);
            latLonCoverage.setLo1(lo1);
            if (gdsVars.getGridUnits().equals("degrees")) {
                latLonCoverage.setSpacingUnit("degree");
                latLonCoverage.setDx(gdsVars.getDx());
                latLonCoverage.setDy(gdsVars.getDy());
            } else {
                latLonCoverage.setSpacingUnit("km");
                latLonCoverage.setDx(gdsVars.getDx() / 1000);
                latLonCoverage.setDy(gdsVars.getDy() / 1000);
            }
            if (latLonCoverage.getDy() == -9999) {
                latLonCoverage.setDy(latLonCoverage.getDx());
            }
            latLonCoverage.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(latLonCoverage, gridNumber);
            break;
        }
        case 1: {
            MercatorGridCoverage mercator = new MercatorGridCoverage();
            mercator.setMajorAxis(gdsVars.getMajorAxis() * 1000);
            mercator.setMinorAxis(gdsVars.getMinorAxis() * 1000);
            mercator.setNx(gdsVars.getNx());
            mercator.setNy(gdsVars.getNy());
            double la1 = correctLat(gdsVars.getLa1());
            double la2 = correctLat(gdsVars.getLa2());
            double lo1 = correctLon(gdsVars.getLo1());
            double lo2 = correctLon(gdsVars.getLo2());
            mercator.setLa1(la1);
            mercator.setLo1(lo1);
            mercator.setLa2(la2);
            mercator.setLo2(lo2);
            mercator.setLatin(correctLat(gdsVars.getLatin1()));
            if (gdsVars.getGridUnits().equals("degrees")) {
                mercator.setSpacingUnit("degree");
                mercator.setDx(gdsVars.getDx());
                mercator.setDy(gdsVars.getDy());
            } else {
                mercator.setSpacingUnit("km");
                mercator.setDx(gdsVars.getDx() / 1000);
                mercator.setDy(gdsVars.getDy() / 1000);
            }
            mercator.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(mercator, gridNumber);
            break;
        }
        case 3: {
            LambertConformalGridCoverage lambert = new LambertConformalGridCoverage();
            lambert.setMajorAxis(gdsVars.getMajorAxis() * 1000);
            lambert.setMinorAxis(gdsVars.getMinorAxis() * 1000);
            lambert.setNx(gdsVars.getNx());
            lambert.setNy(gdsVars.getNy());
            double la1 = correctLat(gdsVars.getLa1());
            double lo1 = correctLon(gdsVars.getLo1());

            lambert.setLa1(la1);
            lambert.setLo1(lo1);
            lambert.setLatin1(correctLat(gdsVars.getLatin1()));
            lambert.setLatin2(correctLat(gdsVars.getLatin2()));
            lambert.setLov(correctLon(gdsVars.getLoV()));
            if (gdsVars.getGridUnits().equals("degrees")) {
                lambert.setSpacingUnit("degree");
                lambert.setDx(gdsVars.getDx());
                lambert.setDy(gdsVars.getDy());
            } else {
                lambert.setSpacingUnit("km");
                lambert.setDx(gdsVars.getDx() / 1000);
                lambert.setDy(gdsVars.getDy() / 1000);
            }
            lambert.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(lambert, gridNumber);

            break;
        }
        case 5: {
            PolarStereoGridCoverage polar = new PolarStereoGridCoverage();
            polar.setMajorAxis(gdsVars.getMajorAxis() * 1000);
            polar.setMinorAxis(gdsVars.getMinorAxis() * 1000);
            polar.setNx(gdsVars.getNx());
            polar.setNy(gdsVars.getNy());
            double la1 = correctLat(gdsVars.getLa1());
            double lo1 = correctLon(gdsVars.getLo1());
            polar.setLa1(la1);
            polar.setLo1(lo1);
            polar.setLov(correctLon(gdsVars.getLoV()));
            if (gdsVars.getProjectionFlag() == 0) {
                // 0 is north pole
                polar.setLad(60);
            } else {
                // 1 is south pole
                polar.setLad(-60);
            }
            if (gdsVars.getGridUnits().equals("degrees")) {
                polar.setSpacingUnit("degree");
                polar.setDx(gdsVars.getDx());
                polar.setDy(gdsVars.getDy());
            } else {
                polar.setSpacingUnit("km");
                polar.setDx(gdsVars.getDx() / 1000);
                polar.setDy(gdsVars.getDy() / 1000);
            }
            polar.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(polar, gridNumber);
            break;
        }
        default:
            break;
        }

        return coverage;
    }

    /**
     * Checks the cache for the GridCoverage object.
     * 
     * @param coverage
     *            The coverage to check the cache for
     * @param gridNumber
     *            The grid number from the PDS
     * @return The GridCoverage object
     * @throws GribException
     *             If the GridCoverage object cannot be obtained
     */
    private GridCoverage getGridFromCache(GridCoverage coverage, int gridNumber)
            throws GribException {

        GridCoverage grid = GribSpatialCache.getInstance().getGrid(coverage);

        if (grid == null && gridNumber != 255) {
            // 255 is specifically reserved to non-defined grids and should not
            // use the gridNumber as a lookup value

            // This code exists because 3hr probability products contain an
            // invalid lambert conformal grid coverage, but a valid
            // gridNumber(236), so the previous lookup fails but this succeeds.
            grid = GribSpatialCache.getInstance().getGridByName(
                    String.valueOf(gridNumber));
        }
        if (grid == null) {
            grid = GridCoverageLookup.getInstance().getCoverage(coverage, true);
        }
        return grid;
    }

    /**
     * Look up a model name based off the grib numbers
     * 
     * @param centerId
     * @param subcenterId
     * @param process
     * @param gridId
     * @return
     */
    private String createModelName(int centerId, int subcenterId, int process,
            GridCoverage grid) {
        return GribModelLookup.getInstance().getModelName(centerId,
                subcenterId, grid, process);
    }

    /**
     * Check if the forecast flag should be removed
     * 
     * @param time
     *            he datatime to remove forecast flag if needed
     * @param centerId
     * @param subcenterId
     * @param process
     * @param gridId
     */
    private void checkForecastFlag(DataTime time, int centerId,
            int subcenterId, int process, GridCoverage grid) {
        GridModel gridModel = GribModelLookup.getInstance().getModel(centerId,
                subcenterId, grid, process);
        if (gridModel != null && gridModel.getAnalysisOnly()) {
            time.getUtilityFlags().remove(FLAG.FCST_USED);
        }
    }

    /**
     * Constructs a data time object from the time information extracted from
     * the grib record
     * 
     * @param refTime
     *            The reference time
     * @param forecastTime
     *            The forecast time
     * @param model
     *            The GribModel object
     * @return The resulting DataTime object
     */
    private DataTime constructDataTime(Calendar refTime, int forecastTime,
            Calendar[] times) {
        DataTime dataTime = null;
        // Construct the DataTime object
        Calendar startTime = times[0];
        Calendar endTime = times[1];
        if (endTime == null) {
            dataTime = new DataTime(refTime, forecastTime);
        } else {
            TimeRange timeRange = new TimeRange(startTime.getTimeInMillis(),
                    endTime.getTimeInMillis());
            dataTime = new DataTime(refTime, forecastTime, timeRange);
        }
        return dataTime;
    }

    /**
     * Converts a time value to seconds
     * 
     * @param value
     *            The value to convert
     * @param fromUnit
     *            The unit of the value from the PDS section
     * @return The time value in seconds
     */
    private int convertToSeconds(int value, int fromUnit) {

        int retVal = value;

        switch (fromUnit) {
        case 0:
            retVal = value * 60;
            break;
        case 1:
            retVal = value * 3600;
            break;
        case 2:
            retVal = value * 86400;
            break;
        case 3:
            retVal = value * 2678400;
            break;
        case 4:
            retVal = value * 977616000;
            break;
        case 5:
            retVal = value * 10 * 977616000;
            break;
        case 6:
            retVal = value * 30 * 977616000;
            break;
        case 7:
            retVal = value * 100 * 977616000;
            break;
        case 10:
            retVal = value * 3 * 3600;
            break;
        case 11:
            retVal = value * 6 * 3600;
            break;
        case 12:
            retVal = value * 12 * 3600;
            break;
        default:
            break;
        }

        return retVal;
    }

    /**
     * Converts a grib 1 level to the equivalent grib 2 representation
     * 
     * @param ltype1
     *            The type of level
     * @param lval1
     *            The value of the level
     * @return The converted level type information
     */
    private float[] convertGrib1LevelInfo(int ltype1, float lval1, int ltype2,
            float lval2) {
        float level1Type = ltype1;
        float level1Scale = 0;
        float level1Value = 0;

        float level2Type = 255;
        float level2Scale = 0;
        float level2Value = 0;

        switch (ltype1) {
        case 100:
            level1Value = lval1 * 100;
            break;
        case 101:
            level1Type = 100;
            level1Value = lval1 * 1000;
            level2Type = 100;
            level2Value = lval2 * 1000;
            break;
        case 102:
            level1Type = 101;
            break;
        case 103:
            level1Type = 102;
            level1Value = lval1;
            break;
        case 104:
            level1Type = 102;
            level1Value = lval1;
            level2Type = 102;
            level2Value = lval2;
            break;
        case 105:
            level1Type = 103;
            level1Value = lval1;
            break;
        case 106:
            level1Type = 103;
            level1Value = lval1 * 100;
            level2Type = 103;
            level2Value = lval2 * 100;
            break;
        case 107:
            level1Type = 104;
            level1Scale = 4;
            level1Value = lval1;
            break;
        case 108:
            level1Type = 104;
            level1Scale = 2;
            level1Value = lval1;
            level2Type = 104;
            level2Scale = 2;
            level2Value = lval2;
            break;
        case 109:
            level1Type = 105;
            level1Value = lval1;
            break;
        case 110:
            level1Type = 105;
            level1Value = lval1;
            level2Type = 105;
            level2Value = lval2;
            break;
        case 111:
            level1Type = 106;
            level1Scale = 2;
            level1Value = lval1;
            break;
        case 112:
            level1Type = 106;
            level1Scale = 2;
            level1Value = lval1;
            level2Type = 106;
            level2Scale = 2;
            level2Value = lval2;
            break;
        case 113:
            level1Type = 107;
            level1Value = lval1;
            break;
        case 114:
            level1Type = 107;
            level1Value = 475 * lval1;
            level2Type = 107;
            level2Value = 475 * lval2;
            break;
        case 115:
            level1Type = 108;
            level1Value = lval1 * 100;
            break;
        case 116:
            level1Type = 108;
            level1Value = lval1 * 100;
            level2Type = 108;
            level2Value = lval2 * 100;
            break;
        case 117:
            level1Type = 109;
            level1Scale = 9;
            level1Value = lval1;
            if (((int) lval1 & 0x8000) != 0) {
                level1Value = -1 * (lval1 % 32768);
            }
            break;
        case 119:
            level1Type = 111;
            level1Scale = 4;
            level1Value = lval1;
            break;
        case 120:
            level1Type = 111;
            level1Scale = 2;
            level1Value = lval1;
            level2Type = 111;
            level2Scale = 2;
            level2Value = lval2;
            break;
        case 121:
            level1Type = 100;
            level1Value = (1100 * lval1) * 100;
            level2Type = 100;
            level2Value = (1100 * lval2) * 100;
            break;
        case 125:
            level1Type = 103;
            level1Scale = 2;
            level1Value = lval1;
            break;
        case 128:
            level1Type = 104;
            level1Scale = 3;
            level1Value = 1100 * lval1;
            level2Type = 104;
            level2Scale = 3;
            level2Value = 1100 * lval2;
            break;
        case 141:
            level1Type = 100;
            level1Value = lval1 * 100;
            level2Type = 100;
            level2Value = (1100 * lval2) * 100;
            break;
        case 160:
            level1Type = 160;
            level1Value = lval1;
            break;
        default:
            if (ltype1 > 99 && ltype1 < 200) {
                level1Type = 255;
                logger.warn("GRIB1 level " + ltype1 + " not recognized");
            }
            break;
        }

        return new float[] { level1Type, level1Scale, level1Value, level2Type,
                level2Scale, level2Value };
    }

    /**
     * Gets the level information
     * 
     * @param centerID
     *            The center
     * @param subcenterID
     *            The subcenter
     * @param levelOneNumber
     *            The level one type
     * @param scaleFactor1
     *            The level one scale factor
     * @param value1
     *            The level one value
     * @param levelTwoNumber
     *            The level two type
     * @param scaleFactor2
     *            The level two scale factor
     * @param value2
     *            The level two value
     * @throws GribException
     */
    private Level getLevelInfo(int centerID, int subcenterID,
            float levelOneNumber, float scaleFactor1, float value1,
            float levelTwoNumber, float scaleFactor2, float value2)
            throws GribException {
        String levelName = null;
        String levelUnit = null;
        double levelOneValue = Level.getInvalidLevelValue();
        double levelTwoValue = Level.getInvalidLevelValue();

        GribLevel gribLevel = (GribLevel) GribTableLookup.getInstance()
                .getTableValue(centerID, subcenterID, "4.5",
                        (int) levelOneNumber);

        if (gribLevel != null) {
            levelName = gribLevel.getAbbreviation();
            levelUnit = gribLevel.getUnit();
        } else {
            logger.warn("No level information for center[" + centerID
                    + "], subcenter[" + subcenterID
                    + "], tableName[4.5], level value[" + levelOneValue + "]");
        }

        if ((levelName == null) || levelName.isEmpty()) {
            levelName = LevelFactory.UNKNOWN_LEVEL;
        }

        // Scale the level one value if necessary
        if (scaleFactor1 == 0 || value1 == 0) {
            levelOneValue = value1;
        } else {
            levelOneValue = new Double((float) (value1 * Math.pow(10,
                    scaleFactor1 * -1)));
        }

        levelTwoValue = levelOneValue;

        // If second level is present, scale if necessary
        if (levelTwoNumber == 255) {
            levelTwoValue = Level.getInvalidLevelValue();
        } else if (levelTwoNumber == 1) {
            levelTwoValue = Level.getInvalidLevelValue();
        } else {
            if (scaleFactor2 == 0 || value2 == 0) {
                levelTwoValue = value2;
            } else {
                levelTwoValue = (value2 * Math.pow(10, scaleFactor2 * -1));
            }
        }
        if (levelName.equals("SFC") && levelOneValue != 0) {
            levelOneValue = 0.0;
        }
        if (levelName.equals("EATM")) {
            levelOneValue = 0.0;
            levelTwoValue = Level.getInvalidLevelValue();
        }
        try {
            return LevelMapper.getInstance().lookupLevel(levelName, "grib",
                    levelOneValue, levelTwoValue, levelUnit);
        } catch (CommunicationException e) {
            throw new GribException("Error requesting levels", e);
        } catch (MultipleLevelMappingException e) {
            statusHandler.handle(Priority.WARN, e.getLocalizedMessage(), e);
            return e.getArbitraryLevelMapping();
        }
    }

    /**
     * Bounds a longitude to between -180 and 180
     * 
     * @param lon
     *            The longitude to correct
     * @return The corrected longitude
     */
    public static float correctLon(float lon) {

        if (lon < 0) {
            lon = lon % 360;
        } else {
            lon = lon % 360;
        }

        if (lon >= 180) {
            lon = (180 - lon % 180) * -1;
        } else if (lon < -180) {
            lon = (180 - (-lon % 180));
        }

        return lon;
    }

    /**
     * Bounds a latitude to between -90 and 90
     * 
     * @param lat
     *            The latitude to correct
     * @return The corrected latitude
     */
    public static float correctLat(float lat) {

        if (lat < 0) {
            lat = lat % 180;
        } else {
            lat = lat % 180;
        }

        if (lat > 90) {
            lat = 90 - lat % 90;
        } else if (lat < -90) {
            lat = (90 - (-lat % 90)) * -1;
        }
        return lat;
    }
}
