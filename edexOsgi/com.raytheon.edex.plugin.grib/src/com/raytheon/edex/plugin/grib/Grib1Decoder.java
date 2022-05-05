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

import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.edex.plugin.grib.spatial.GribSpatialCache;
import com.raytheon.edex.util.grib.Grib1TableMap;
import com.raytheon.edex.util.grib.GribParamTranslator;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.dataplugin.level.mapping.MultipleLevelMappingException;
import com.raytheon.uf.common.grib.GribModelLookup;
import com.raytheon.uf.common.grib.GridModel;
import com.raytheon.uf.common.grib.tables.GribLevel;
import com.raytheon.uf.common.grib.tables.GribParameter;
import com.raytheon.uf.common.grib.tables.GribTableLookup;
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
import com.raytheon.uf.common.util.ArraysUtil;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.common.util.mapping.MultipleMappingException;

import ucar.nc2.grib.GribData.InterpolationMethod;
import ucar.nc2.grib.grib1.Grib1Gds;
import ucar.nc2.grib.grib1.Grib1Gds.LambertConformal;
import ucar.nc2.grib.grib1.Grib1Gds.LatLon;
import ucar.nc2.grib.grib1.Grib1Gds.Mercator;
import ucar.nc2.grib.grib1.Grib1Gds.PolarStereographic;
import ucar.nc2.grib.grib1.Grib1ParamLevel;
import ucar.nc2.grib.grib1.Grib1ParamTime;
import ucar.nc2.grib.grib1.Grib1Parameter;
import ucar.nc2.grib.grib1.Grib1Record;
import ucar.nc2.grib.grib1.Grib1RecordScanner;
import ucar.nc2.grib.grib1.Grib1SectionGridDefinition;
import ucar.nc2.grib.grib1.Grib1SectionProductDefinition;
import ucar.nc2.grib.grib1.tables.Grib1ParamTableReader;
import ucar.nc2.grib.grib1.tables.Grib1ParamTables;
import ucar.nc2.time.CalendarDate;
import ucar.nc2.time.CalendarPeriod.Field;
import ucar.unidata.geoloc.Earth;
import ucar.unidata.geoloc.EarthEllipsoid;
import ucar.unidata.io.InMemoryRandomAccessFile;
import ucar.unidata.io.RandomAccessFile;

/**
 * Grib decoder implementation for decoding grib version 1 files.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 11, 2010  4758     bphillip  Initial Creation
 * Feb 15, 2013  1638     mschenke  Moved array based utilities from Util into
 *                                  ArraysUtil
 * Aug 30, 2013  2298     rjpeter   Make getPluginName abstract
 * Oct 07, 2013  2402     bsteffen  Decode GribDecodeMessage instead of files.
 * Oct 15, 2013  2473     bsteffen  Removed deprecated and unused code.
 * Jul 30, 2014  3469     bsteffen  Improve logging of invalid files.
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * Mar 05, 2015  3959     rjpeter   Added world wrap check to subGrid.
 * Oct 01, 2015  4868     rjpeter   Discard invalid subgrids.
 * Dec 16, 2015  5182     tjensen   Updated GribModelLookup calls to pass in
 *                                  filepath.
 * Apr 11, 2016  5564     bsteffen  Move localization files to common_static
 * Apr 19, 2016  5572     bsteffen  Move GribModelLookup to common
 * Sep 11, 2017  6406     bsteffen  Upgrade ucar
 * Aug 02, 2018  7397     mapeters  Fix parameter table lookups so standard ucar
 *                                  lookups are used
 * Aug 08, 2018  7397     mapeters  Disable default parameter table, provide
 *                                  default param name for unknown params (like
 *                                  older ucar version)
 * Aug 23, 2018  7397     mapeters  Fix forecast times for intervals
 * Sep 11, 2018  7450     tjensen   Limit decoding to a single grib record
 * Sep 25, 2018  7450     mapeters  Fix extraction of the record to decode from
 *                                  the grib file
 * Jan 29, 2020  8026     bhurley   Remove unit conversion on semi-major and
 *                                  semi-minor axis values
 * 
 * </pre>
 *
 * @author bphillip
 */
public class Grib1Decoder {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Grib1Decoder.class);

    /**
     * http://www.nco.ncep.noaa.gov/pmb/docs/on388/tableb.html#FOS
     *
     * Grib files using these coverages have an entire row of data that is
     * located at the pole. Rather than sending the same value multiple times,
     * they only send a single point. Since we must have a data value for each
     * point, the value must be copied across the entire row.
     */
    private static Set<Integer> ROW_REDUCED_POLE_GIDS = new HashSet<>(
            Arrays.asList(21, 22, 25, 61, 62));

    /**
     * @see ROW_REDUCED_POLE_GIDS
     */
    private static Set<Integer> COLUMN_REDUCED_POLE_GIDS = new HashSet<>(
            Arrays.asList(23, 24, 26, 63, 64));

    /** Missing value string */
    private static final String MISSING = "Missing";

    private static final String UNKNOWN_UNIT = "Unknown";

    /** Format for unknown param, takes paramNumber int and tableName string */
    private static final String UNKNOWN_PARAM_FORMAT = "UnknownParameter_%d_table_%s";

    /**
     * Format for unknown table, takes ints: centerid, subcenterid, tableVersion
     */
    private static final String UNKNOWN_TABLE_FORMAT = "Unknown-%d.%d.%d";

    /** Set of Time range types for accumulations and averages */
    private static final Set<Integer> AVG_ACCUM_LIST = new HashSet<>();

    private static final Grib1ParamTables paramTables;

    static {
        AVG_ACCUM_LIST.add(3);
        AVG_ACCUM_LIST.add(4);
        AVG_ACCUM_LIST.add(6);
        AVG_ACCUM_LIST.add(7);

        IPathManager pm = PathManagerFactory.getPathManager();
        String ucarUserFile = pm
                .getFile(
                        pm.getContext(LocalizationType.COMMON_STATIC,
                                LocalizationLevel.BASE),
                        "/grib/ucar/userTables.lst")
                .getPath();
        try {
            Grib1ParamTables.addParameterTableLookup(ucarUserFile);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading user parameter tables for ucar grib decoder",
                    e);
        }
        /*
         * The default table is not useful, as it doesn't specify the "name"
         * field for any of its parameters, which causes decoding to fail. Also,
         * falling back to it may incorrectly map some parameters. So we clear
         * its parameters so that we just get a null param when failing to find
         * it instead of an invalid default param.
         */
        Grib1ParamTables.getDefaultTable()
                .setParameters(Collections.emptyMap());
        paramTables = new Grib1ParamTables();
    }

    /**
     * Decodes the grib file provided.
     *
     * @param message
     *            message describing the grib file to decode
     * @return The decoded GridRecords
     * @throws GribException
     *             If decoding the file fails or encounters problems
     */
    public GridRecord[] decode(GribDecodeMessage message) throws GribException {
        String fileName = message.getFileName();

        try (RandomAccessFile fileRaf = new RandomAccessFile(fileName, "r")) {
            /*
             * We want to limit decoding to a single record, as a decode message
             * should be generated for each individual record. The
             * Grib1RecordScanner doesn't support this very well, as its
             * constructor seeks back to the start of the random access file, as
             * does the first call of its hasNext(). So, we extract the
             * individual record's bytes from the file and wrap them in an
             * InMemoryRandomAccessFile, so that the start of the "file" is the
             * start of the record, which makes seeking back to the start of it
             * okay.
             */
            fileRaf.order(RandomAccessFile.BIG_ENDIAN);
            fileRaf.seek(message.getStartPosition());
            byte[] recordBytes = new byte[(int) message.getMessageLength()];
            fileRaf.read(recordBytes);
            try (RandomAccessFile recordRaf = new InMemoryRandomAccessFile(
                    fileName, recordBytes)) {
                recordRaf.order(RandomAccessFile.BIG_ENDIAN);
                Grib1RecordScanner g1i = new Grib1RecordScanner(recordRaf);

                if (g1i.hasNext()) {
                    Grib1Record grib1rec = g1i.next();
                    if (grib1rec == null) {
                        /*
                         * If the file is truncated or otherwise corrupt such
                         * that the end section of the grib file is not found
                         * then the scanner returns null despite the fact that
                         * hasNext() returned true.
                         */
                        throw new GribException("Invalid grib1 message in "
                                + fileName + " at position "
                                + message.getStartPosition());
                    }
                    GridRecord gribRecord = decodeRecord(grib1rec, recordRaf,
                            fileName);
                    if (gribRecord != null) {
                        return new GridRecord[] { gribRecord };
                    }
                }

                return new GridRecord[0];
            }
        } catch (IOException e) {
            throw new GribException(
                    "IO failure decoding grib1 file: [" + fileName + "]", e);
        } catch (Exception e) {
            throw new GribException(
                    "Unexpected error in grib1 file: [" + fileName + "]", e);
        }
    }

    /**
     * Decodes a single record from the grib file
     *
     * @param rec
     *            The record to decode
     * @param raf
     *            The file object
     * @param filePath
     *            The path of the file to decode
     * @return The decoded GridRecord
     * @throws GribException
     *             If the record cannot be decoded properly
     */
    private GridRecord decodeRecord(Grib1Record rec, RandomAccessFile raf,
            String filePath) throws GribException {

        GridRecord retVal = new GridRecord();

        // Extract the sections from the grib record
        Grib1SectionProductDefinition pds = rec.getPDSsection();
        Grib1SectionGridDefinition gds = rec.getGDSsection();
        Grib1Gds gdsVars = gds.getGDS();

        // Initialize some common values
        int centerid = pds.getCenter();
        int subcenterid = pds.getSubCenter();
        String parameterAbbreviation = null;
        String parameterName = null;
        String parameterUnit = null;

        // Some centers use other center's parameter tables so we need to check
        // for that
        int[] tableValue = Grib1TableMap.getInstance().getTableAlias(centerid,
                subcenterid, pds.getTableVersion());
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
                        pds.getParameterNumber());

        if ((parameter == null) || parameter.getName().equals(MISSING)) {
            statusHandler
                    .warn("Unable to map Grib 1 parameter to equivalent Grib 2 parameter for center ["
                            + centerid + "] subcenter [" + subcenterid
                            + "] table number [" + pds.getTableVersion()
                            + "] parameter number [" + pds.getParameterNumber()
                            + "]  Using grib 1 parameter mapping");
            Grib1Parameter param = paramTables.getParameter(centerid,
                    subcenterid, pds.getTableVersion(),
                    pds.getParameterNumber());
            if (param == null) {
                param = paramTables.getParameter(centerAlias, subcenterAlias,
                        tableAlias, pds.getParameterNumber());
            }
            if (param != null) {
                parameterName = param.getDescription();
                parameterAbbreviation = param.getName();
                parameterUnit = param.getUnit();
            } else {
                String unknownParam = formatUnknownParamName(centerid,
                        subcenterid, pds.getTableVersion(),
                        pds.getParameterNumber());
                parameterAbbreviation = unknownParam;
                parameterName = unknownParam;
                parameterUnit = UNKNOWN_UNIT;
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
        int gridId = pds.getGridDefinition();

        GridCoverage gridCoverage = null;
        if (gdsVars != null) {
            gridCoverage = getGridCoverage(gdsVars, gridId);
        } else {
            gridCoverage = GribSpatialCache.getInstance()
                    .getGridByName(String.valueOf(gridId));
        }

        // Set up variables.
        int nx = new Integer(gridCoverage.getNx());
        int ny = new Integer(gridCoverage.getNy());

        retVal.setLocation(gridCoverage);
        int genProcess = pds.getGenProcess();

        retVal.addExtraAttribute("centerid", centerid);
        retVal.addExtraAttribute("subcenterid", subcenterid);
        retVal.addExtraAttribute("genprocess", pds.getGenProcess());
        retVal.addExtraAttribute("backGenprocess", 0);

        if (pds.isEnsemble()) {
            /*
             * The pds methods getPerturbationType() and getPerturbationNumber()
             * are not used because they perform additional mappings which do
             * not give us the numbers we need for the GFS ensemble.
             */
            /*
             * The variables are named after the grib positions which are
             * 1-indexed, but retrieved from the array using 0-indexing.
             */
            int pos42 = pds.getRawBytes()[41];
            int pos43 = pds.getRawBytes()[42];
            switch (pos42) {
            case 1:
                retVal.setEnsembleId("ctl" + pos43);
                break;
            case 2:
                retVal.setEnsembleId("n" + pos43);
                break;
            case 3:
                retVal.setEnsembleId("p" + (pos43));
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
                gridCoverage, filePath));

        // Get the level information
        Grib1ParamLevel paramLevel = new Grib1ParamLevel(null, pds);
        float[] levelMetadata = this.convertGrib1LevelInfo(
                paramLevel.getLevelType(), paramLevel.getValue1(),
                paramLevel.getValue2());
        retVal.setLevel(getLevelInfo(centerid, subcenterid, levelMetadata[0],
                levelMetadata[1], levelMetadata[2], levelMetadata[3],
                levelMetadata[4], levelMetadata[5]));

        // Construct the DataTime
        GregorianCalendar refTime = new GregorianCalendar();
        if (pds.getRawBytes()[15] >= 24) {
            /*
             * Byte 15 is the hour of the reference time and the ucar library
             * only supports values 0-23 but we get data with a value of 24 (TP
             * for QPE-RFC-STR). This data is probably invalid, but older
             * versions of the decoder handled it so we must also handle it. The
             * simplest way to handle it is to set the hour to 0 and then add 24
             * hours to the resultant time.
             */
            byte[] rawPDS = pds.getRawBytes();
            rawPDS = Arrays.copyOf(rawPDS, rawPDS.length);
            int hours = rawPDS[15];
            rawPDS[15] = 0;
            CalendarDate refDate = new Grib1SectionProductDefinition(rawPDS)
                    .getReferenceDate();
            refDate = refDate.add(hours, Field.Hour);
            refTime.setTimeInMillis(refDate.getMillis());
        } else {
            refTime.setTimeInMillis(pds.getReferenceDate().getMillis());
        }
        int p1 = convertToSeconds(pds.getTimeValue1(), pds.getTimeUnit());
        int p2 = convertToSeconds(pds.getTimeValue2(), pds.getTimeUnit());
        Grib1ParamTime paramTime = new Grib1ParamTime(null, pds);
        /*
         * getForecastTime() is only valid for non-interval times, otherwise
         * forecastTime is the end of the [start,end] interval
         */
        int forecastTime = paramTime.isInterval() ? paramTime.getInterval()[1]
                : paramTime.getForecastTime();
        forecastTime = convertToSeconds(forecastTime, pds.getTimeUnit());

        DataTime dataTime = constructDataTime(refTime, forecastTime,
                getTimeInformation(refTime, pds.getTimeRangeIndicator(), p1,
                        p2));

        /*
         * Extract the data values from the file. The AVG_ACCUM_LIST is checked
         * to see if this is an accumulation or average grid being initialized.
         * A check is also made for thinned grids.
         */
        float[] data;
        try {
            boolean bmsPresent = pds.bmsExists();
            int scanMode;
            if (gdsVars != null) {
                scanMode = gdsVars.getScanMode();
            } else {
                scanMode = gridCoverage.getScanMode();
            }

            int timeRange = pds.getTimeRangeIndicator();

            // Check for initialization of average or accumulation parameters
            if ((AVG_ACCUM_LIST.contains(timeRange)
                    && (dataTime.getValidPeriod().getDuration() == 0))) {
                statusHandler.handle(Priority.EVENTA,
                        "Discarding empty accumulation grid");
                return null;
            } else if (((gdsVars != null) && gdsVars.getNptsInLine() != null)
                    || ((gdsVars == null)
                            && ((gridCoverage instanceof LatLonGridCoverage)
                                    && ((LatLonGridCoverage) gridCoverage)
                                            .isThin()))) {
                /*
                 * A linear interpolation method is available but it is
                 * different than fillThinnedGrid. The basic problem is
                 * described in this bug:
                 * https://github.com/Unidata/thredds/issues/82. That bug only
                 * identifies the problem with cubicSpline interpolation, but
                 * linear interpolation is making the same assumptions.
                 *
                 * The problem is that the ucar interpolation method assumes the
                 * data is cylindrical and it spaces the points evenly along the
                 * cylinder, while fillThinedGrids only interpolates in the 2D
                 * space of the grid.
                 *
                 * For example if you have a row with only 2 data points then
                 * fillThinnedGrid would place one point on the left and the
                 * other on the right and the grid is filled by interpolating
                 * between them. The ucar interpolation would place one point on
                 * the left edge and the other in the center. The left half of
                 * the data is generated by interpolating between the left edge
                 * and the center. The right half the data is interpolated again
                 * from the same two points. This makes sense if you picture the
                 * grid folded into a cylinder because then the left edge is
                 * right next to the right edge, so you can interpolate between
                 * those points to fill the right half. If you have a worldwide
                 * grid Lat/Lon then this treatment makes sense because the
                 * projection is a cylinder.
                 *
                 * We get thinned grids in "quadrant" form, using grids 37-44.
                 * These grids are definitely not a cylinder so the ucar method
                 * is wrong.
                 */
                data = rec.readDataRaw(raf, InterpolationMethod.none);

                if (gdsVars != null) {
                    data = fillThinnedGrid(data, gdsVars.getNptsInLine(),
                            gdsVars.getNx(), gdsVars.getNy());
                } else {
                    data = fillThinnedGrid(data,
                            ((LatLonGridCoverage) gridCoverage).getParallels(),
                            nx, ny);
                }
            } else {
                data = rec.readData(raf);
                // Correct data count for pole data point.
                if (ROW_REDUCED_POLE_GIDS.contains(gridId)) {
                    int newSize = nx * ny;
                    float[] dataCopy = new float[newSize];
                    for (int i = 0; i < dataCopy.length; i++) {
                        if (i < data.length) {
                            dataCopy[i] = data[i];
                        }
                    }
                    data = dataCopy;
                } else if (COLUMN_REDUCED_POLE_GIDS.contains(gridId)) {
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
            if (data == null) {
                throw new GribException("No valid data found in grib record.");
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
            SubGrid subGrid = GribSpatialCache.getInstance()
                    .getSubGrid(modelName, gridCoverage);
            if ((subGrid.getNX() <= 0) || (subGrid.getNY() <= 0)) {
                // subgrid does not cover enough of CWA
                statusHandler.info("Discarding model [" + modelName
                        + "], sub grid does not meet minimum coverage area");
                return null;
            }

            // resize the data array
            float[][] dataArray = this.resizeDataTo2D(data,
                    gridCoverage.getNx(), gridCoverage.getNy());
            dataArray = this.subGrid(dataArray, subGrid.getUpperLeftX(),
                    subGrid.getUpperLeftY(), subGrid.getNX(), subGrid.getNY(),
                    gridCoverage);
            data = this.resizeDataTo1D(dataArray, subGrid.getNY(),
                    subGrid.getNX());
            retVal.setMessageData(data);
            retVal.setLocation(subCoverage);
        }

        String newAbbr = GribParamTranslator.getInstance().translateParameter(1,
                parameterAbbreviation, centerid, subcenterid, genProcess,
                dataTime, gridCoverage);

        if (newAbbr == null) {
            if (!parameterName.equals(MISSING)
                    && (dataTime.getValidPeriod().getDuration() > 0)) {
                parameterAbbreviation = parameterAbbreviation + String.valueOf(
                        dataTime.getValidPeriod().getDuration() / 3_600_000)
                        + "hr";
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

        Parameter param = new Parameter(parameterAbbreviation, parameterName,
                parameterUnit);
        GribParamTranslator.getInstance().getParameterNameAlias(modelName,
                param);
        retVal.setParameter(param);

        retVal.setPersistenceTime(new Date());
        retVal.setDataTime(dataTime);

        // check if FLAG.FCST_USED needs to be removed
        checkForecastFlag(retVal.getDataTime(), centerid, subcenterid,
                genProcess, gridCoverage, filePath);

        return retVal;
    }

    private static String formatUnknownParamName(int centerid, int subcenterid,
            int tableVersion, int paramNumber) {
        Grib1ParamTableReader table = paramTables.getParameterTable(centerid,
                subcenterid, tableVersion);
        String tableName;
        if (table.getPath()
                .equals(Grib1ParamTables.getDefaultTable().getPath())) {
            // Failed to find in a valid table, format name for unknown table
            tableName = String.format(UNKNOWN_TABLE_FORMAT, centerid,
                    subcenterid, tableVersion);
        } else {
            tableName = table.getName();
        }
        String unknownParam = String.format(UNKNOWN_PARAM_FORMAT, paramNumber,
                tableName);
        return unknownParam;
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
    private float[][] resizeDataTo2D(float[] data, int columnCount,
            int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row][column] = data[(row * columnCount) + column];
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
                    outData[outIndex] = inData[inIndex];
                    outIndex += 1;
                }
                inIndex++;
                continue;
            }
            if (parallels[j] == nx) {
                for (int i = 0; i < nx; i++) {
                    outData[outIndex] = inData[inIndex];
                    outIndex += 1;
                    inIndex += 1;
                }
                continue;
            }
            float trail = inData[inIndex];
            float dx = (parallels[j] - 1.0f) / (nx - 1.0f);
            float total = dx;
            outData[outIndex] = inData[inIndex];
            outIndex += 1;
            inIndex += 1;
            for (int i = 2; i < nx; i++) {
                if (total > 1) {
                    total -= 1;
                    trail = inData[inIndex];
                    inIndex += 1;
                }
                outData[outIndex] = (inData[inIndex] * total)
                        + (trail * (1 - total));
                outIndex += 1;
                total += dx;
            }
            outData[outIndex] = inData[inIndex];
            outIndex += 1;
            inIndex += 1;
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
    private float[] resizeDataTo1D(float[][] data, int rowCount,
            int columnCount) {
        float[] newGrid = new float[rowCount * columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[(row * columnCount) + column] = data[row][column];
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
     * @param coverage
     *            The parent coverage
     * @return The sub-grid of data
     */
    private float[][] subGrid(float[][] data, int startColumn, int startRow,
            int columnCount, int rowCount, GridCoverage coverage) {
        float[][] newGrid = new float[rowCount][columnCount];

        // sub grid might wrap around a world wide grid, double check bounds
        int endRow = startRow + rowCount;
        int endColumn = startColumn + columnCount;
        int wrapCount = -1;
        if (endColumn > data[0].length) {
            wrapCount = coverage.getWorldWrapCount();

            if (wrapCount > 0) {
                /* account for grids that already wrap in the incoming data */
                endColumn = wrapCount;
            } else {
                endColumn = data[0].length;
            }
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
            if ((columnCount > 0) && (wrapCount > 0)) {
                for (int column = 0; column < columnCount; column++, newGridColumn++) {
                    newGrid[newGridRow][newGridColumn] = data[row][column];
                }
            } else {
                for (int column = 0; column < columnCount; column++, newGridColumn++) {
                    newGrid[newGridRow][newGridColumn] = GridUtil.GRID_FILL_VALUE;
                }

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
            if (bmsExists && (data[i] == -9999)) {
                data[i] = GridUtil.GRID_FILL_VALUE;
            }
        }
        switch (scanMode) {
        case 0:
            break;
        case 64:
            ArraysUtil.flipHoriz(data, ny, nx);
            break;
        case 128:
            ArraysUtil.flipVert(data, ny, nx);
            break;
        case 192:
            ArraysUtil.rotate180(data, ny, nx);
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
    private GridCoverage getGridCoverage(Grib1Gds gdsVars, int gridNumber)
            throws GribException {
        GridCoverage coverage = null;

        if (gdsVars instanceof LatLon) {
            LatLon llVars = (LatLon) gdsVars;
            LatLonGridCoverage latLonCoverage = new LatLonGridCoverage();
            latLonCoverage.setNx(gdsVars.getNx());
            latLonCoverage.setNy(gdsVars.getNy());
            double la1 = correctLat(llVars.la1);
            double lo1 = correctLon(llVars.lo1);
            latLonCoverage.setLa1(la1);
            latLonCoverage.setLo1(lo1);
            latLonCoverage.setSpacingUnit("degree");
            latLonCoverage.setDx(gdsVars.getDx());
            latLonCoverage.setDy(gdsVars.getDy());

            if (latLonCoverage.getDy() == -9999) {
                latLonCoverage.setDy(latLonCoverage.getDx());
            } else if (latLonCoverage.getDy() < 0) {
                latLonCoverage.setDy(-1.0 * latLonCoverage.getDy());
            }
            latLonCoverage.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(latLonCoverage, gridNumber);
        } else if (gdsVars instanceof Mercator) {
            Mercator mercVars = (Mercator) gdsVars;
            MercatorGridCoverage mercator = new MercatorGridCoverage();
            Earth earth = null;
            /* http://www.nco.ncep.noaa.gov/pmb/docs/on388/table7.html */
            if (gdsVars.getEarthShape() == 0) {
                earth = new Earth(6367470.0);
            } else {
                earth = EarthEllipsoid.IAU;
            }
            mercator.setMajorAxis(earth.getMajor());
            mercator.setMinorAxis(earth.getMinor());
            mercator.setNx(gdsVars.getNx());
            mercator.setNy(gdsVars.getNy());
            double la1 = correctLat(mercVars.la1);
            double la2 = correctLat(mercVars.la2);
            double lo1 = correctLon(mercVars.lo1);
            double lo2 = correctLon(mercVars.lo2);
            mercator.setLa1(la1);
            mercator.setLo1(lo1);
            mercator.setLa2(la2);
            mercator.setLo2(lo2);
            mercator.setLatin(correctLat(mercVars.latin));
            mercator.setSpacingUnit("km");
            mercator.setDx(gdsVars.getDx());
            mercator.setDy(gdsVars.getDy());
            mercator.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(mercator, gridNumber);
        } else if (gdsVars instanceof LambertConformal) {
            LambertConformal lcVars = (LambertConformal) gdsVars;

            LambertConformalGridCoverage lambert = new LambertConformalGridCoverage();
            Earth earth = null;
            /* http://www.nco.ncep.noaa.gov/pmb/docs/on388/table7.html */
            if (gdsVars.getEarthShape() == 0) {
                earth = new Earth(6367470.0);
            } else {
                earth = EarthEllipsoid.IAU;
            }
            lambert.setMajorAxis(earth.getMajor());
            lambert.setMinorAxis(earth.getMinor());
            lambert.setNx(gdsVars.getNx());
            lambert.setNy(gdsVars.getNy());
            double la1 = correctLat(lcVars.la1);
            double lo1 = correctLon(lcVars.lo1);

            lambert.setLa1(la1);
            lambert.setLo1(lo1);
            lambert.setLatin1(correctLat(lcVars.latin1));
            lambert.setLatin2(correctLat(lcVars.latin2));
            lambert.setLov(correctLon(((LambertConformal) gdsVars).lov));

            lambert.setSpacingUnit("km");
            lambert.setDx(gdsVars.getDx());
            lambert.setDy(gdsVars.getDy());
            lambert.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(lambert, gridNumber);

        } else if (gdsVars instanceof PolarStereographic) {
            PolarStereographic psVars = (PolarStereographic) gdsVars;

            PolarStereoGridCoverage polar = new PolarStereoGridCoverage();
            Earth earth = null;
            /* http://www.nco.ncep.noaa.gov/pmb/docs/on388/table7.html */
            if (gdsVars.getEarthShape() == 0) {
                earth = new Earth(6367470.0);
            } else {
                earth = EarthEllipsoid.IAU;
            }
            polar.setMajorAxis(earth.getMajor());
            polar.setMinorAxis(earth.getMinor());
            polar.setNx(gdsVars.getNx());
            polar.setNy(gdsVars.getNy());
            double la1 = correctLat(psVars.la1);
            double lo1 = correctLon(psVars.lo1);
            polar.setLa1(la1);
            polar.setLo1(lo1);
            polar.setLov(correctLon(psVars.lov));
            if (psVars.projCenterFlag == 0) {
                // 0 is north pole
                polar.setLad(60);
            } else {
                // 1 is south pole
                polar.setLad(-60);
            }
            polar.setSpacingUnit("km");
            polar.setDx(gdsVars.getDx());
            polar.setDy(gdsVars.getDy());
            polar.setFirstGridPointCorner(GribSpatialCache
                    .determineFirstGridPointCorner(gdsVars.getScanMode()));
            coverage = getGridFromCache(polar, gridNumber);
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

        if ((grid == null) && (gridNumber != 255)) {
            // 255 is specifically reserved to non-defined grids and should not
            // use the gridNumber as a lookup value

            // This code exists because 3hr probability products contain an
            // invalid lambert conformal grid coverage, but a valid
            // gridNumber(236), so the previous lookup fails but this succeeds.
            grid = GribSpatialCache.getInstance()
                    .getGridByName(String.valueOf(gridNumber));
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
     * @param filePath
     * @param gridId
     * @return
     */
    private String createModelName(int centerId, int subcenterId, int process,
            GridCoverage grid, String filePath) {
        return GribModelLookup.getInstance().getModelName(centerId, subcenterId,
                grid, process, null, filePath);
    }

    /**
     * Check if the forecast flag should be removed
     *
     * @param time
     *            The datatime to remove forecast flag if needed
     * @param centerId
     * @param subcenterId
     * @param process
     * @param filePath
     * @param gridId
     */
    private void checkForecastFlag(DataTime time, int centerId, int subcenterId,
            int process, GridCoverage grid, String filePath) {
        GridModel gridModel = GribModelLookup.getInstance().getModel(centerId,
                subcenterId, grid, process, null, filePath);
        if ((gridModel != null)
                && Boolean.TRUE.equals(gridModel.getAnalysisOnly())) {
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
            retVal = value * 86_400;
            break;
        case 3:
            retVal = value * 2_678_400;
            break;
        case 4:
            retVal = value * 977_616_000;
            break;
        case 5:
            retVal = value * 10 * 977_616_000;
            break;
        case 6:
            retVal = value * 30 * 977_616_000;
            break;
        case 7:
            retVal = value * 100 * 977_616_000;
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
    private float[] convertGrib1LevelInfo(int ltype1, float lval1,
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
                level1Value = -1 * (lval1 % 32_768);
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
            if ((ltype1 > 99) && (ltype1 < 200)) {
                level1Type = 255;
                statusHandler.warn("GRIB1 level " + ltype1 + " not recognized");
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
            statusHandler.warn("No level information for center[" + centerID
                    + "], subcenter[" + subcenterID
                    + "], tableName[4.5], level value[" + levelOneValue + "]");
        }

        if ((levelName == null) || levelName.isEmpty()) {
            levelName = LevelFactory.UNKNOWN_LEVEL;
        }

        // Scale the level one value if necessary
        if ((scaleFactor1 == 0) || (value1 == 0)) {
            levelOneValue = value1;
        } else {
            levelOneValue = (float) (value1 * Math.pow(10, scaleFactor1 * -1));
        }

        levelTwoValue = levelOneValue;

        // If second level is present, scale if necessary
        if (levelTwoNumber == 255) {
            levelTwoValue = Level.getInvalidLevelValue();
        } else if (levelTwoNumber == 1) {
            levelTwoValue = Level.getInvalidLevelValue();
        } else {
            if ((scaleFactor2 == 0) || (value2 == 0)) {
                levelTwoValue = value2;
            } else {
                levelTwoValue = (value2 * Math.pow(10, scaleFactor2 * -1));
            }
        }
        if ("SFC".equals(levelName) && (levelOneValue != 0)) {
            levelOneValue = 0.0;
        }
        if ("EATM".equals(levelName)) {
            levelOneValue = 0.0;
            levelTwoValue = Level.getInvalidLevelValue();
        }
        try {
            return LevelMapper.getInstance().lookupLevel(levelName, "grib",
                    levelOneValue, levelTwoValue, levelUnit);
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
            lon = (180 - (lon % 180)) * -1;
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
            lat = 90 - (lat % 90);
        } else if (lat < -90) {
            lat = (90 - (-lat % 90)) * -1;
        }
        return lat;
    }
}
