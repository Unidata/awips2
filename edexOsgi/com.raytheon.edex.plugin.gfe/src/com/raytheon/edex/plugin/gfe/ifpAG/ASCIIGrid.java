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
package com.raytheon.edex.plugin.gfe.ifpAG;

import java.awt.Point;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Scanner;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import org.locationtech.jts.geom.Coordinate;

/**
 * Class containing grid data in ASCII format.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 13, 2011  8393     dgilling  Initial creation
 * Feb 19, 0013  1637     randerso  Added exception handling for Discrete and
 *                                  Weather
 * Oct 31, 2013  2508     randerso  Change to use DiscreteGridSlice.getKeys()
 * Apr 22, 2014  3050     randerso  Allow exceptions to propagate to caller from
 *                                  readASCIIGridData
 * Jan 14, 2016  5237     tgurney   Allow outputAsciiGridData to take
 *                                  OutputStream as well as File
 * Apr 04, 2016  5539     randerso  Fixed unsigned byte issues
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author dgilling
 */

public class ASCIIGrid {

    private IFPServerConfig config;

    private List<IGridSlice> gridSlices;

    private GridLocation coordConversion;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ASCIIGrid.class);

    private static final DateFormat validTimeFormat = new SimpleDateFormat(
            "yyyyMMdd_HHmm");

    /**
     * Default constructor for ASCIIGrid.
     */
    public ASCIIGrid() {
        // no-op
    }

    /**
     * Constructor for ASCIIGrid class taking a List<IGridSlice> as argument.
     *
     * @param config
     *            the server configuration for the site
     *
     * @param gridSlices
     *            The List of IGridSlice objects.
     * @param coordConversionString
     *            A custom GridLocation description string for remapping the
     *            gridSlices.
     */
    public ASCIIGrid(IFPServerConfig config, List<IGridSlice> gridSlices,
            String coordConversionString) {
        this.config = config;
        this.gridSlices = gridSlices;
        statusHandler.handle(Priority.EVENTB,
                "Number of Grids: " + gridSlices.size());
        this.coordConversion = decodeConversionString(coordConversionString,
                config.getSiteID());

        if (coordConversion.isValid()) {
            statusHandler.handle(Priority.VERBOSE,
                    "Remapping Output Grids based on -c switch.");
            for (IGridSlice slice : gridSlices) {
                GridLocation sourceGloc = slice.getGridInfo().getGridLoc();
                // AWIPS1 code that appears to be unnecessary
                // GridParmInfo sourceGPI = slice.getGridInfo().clone();
                // slice.setGridInfo(sourceGPI);
                sampleGrid(slice, sourceGloc, coordConversion);
            }
            statusHandler.handle(Priority.VERBOSE, "Remapping completed.");
        }
    }

    /**
     * Translates an existing grid (specified by the index) to a different
     * GridParmInfo. May change the grid domain.
     *
     * @param index
     * @param destGPI
     * @return true if successful
     */
    public boolean translateGrid(int index, GridParmInfo destGPI) {
        if ((index < 0) || (index > (gridSlices.size() - 1))) {
            return false;
        }

        // adjust valid time of grid
        TimeRange expandedTR = adjustToTimeConstraints(
                gridSlices.get(index).getValidTime(),
                destGPI.getTimeConstraints());
        if (!expandedTR.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Attempt to time shift grid " + (index + 1) + " from "
                            + gridSlices.get(index).getValidTime().toString()
                            + " to match time constraints: "
                            + destGPI.getTimeConstraints().toString()
                            + " but unable.");
            return false; // can't get a valid time so fail this grid
        }
        if (!gridSlices.get(index).getValidTime().equals(expandedTR)) {
            statusHandler.handle(Priority.VERBOSE,
                    "Time shifted grid: " + destGPI.getParmID().toString()
                            + " from: "
                            + gridSlices.get(index).getValidTime().toString()
                            + " to: " + expandedTR.toString());
        }
        gridSlices.get(index).setValidTime(expandedTR);

        // replace source attributes with destination attributes into the source
        // grids to increase storage success
        GridParmInfo sourceGPI = gridSlices.get(index).getGridInfo();
        if (!sourceGPI.getGridType().equals(destGPI.getGridType())) {
            return false;
        }
        gridSlices.get(index).setGridInfo(destGPI);

        // do any requested coordinate domain conversions
        if (!sampleGrid(gridSlices.get(index), sourceGPI.getGridLoc(),
                destGPI.getGridLoc())) {
            return false;
        }

        return true;
    }

    /**
     * Output the ASCII grid data to a File
     *
     * @param outputFile
     * @throws IOException
     */
    public void outputAsciiGridData(File outputFile) throws IOException {
        try (FileOutputStream outputStream = new FileOutputStream(outputFile)) {
            outputAsciiGridData(outputFile);
        }
    }

    /**
     * Output the ASCII grid data to an OutputStream
     *
     * @param outputStream
     * @throws IOException
     */
    public void outputAsciiGridData(OutputStream outputStream)
            throws IOException {
        try (PrintWriter printStream = new PrintWriter(
                new OutputStreamWriter(outputStream, "US-ASCII"))) {

            // output for each IGridSlice
            for (IGridSlice gs : gridSlices) {
                // keyword identifying new ASCIIGrid
                printStream.println("ASCIIGRID");

                // data type
                if (gs.getGridInfo().getGridType().equals(GridType.SCALAR)) {
                    printStream.println("SCALAR");
                } else if (gs.getGridInfo().getGridType()
                        .equals(GridType.VECTOR)) {
                    printStream.println("VECTOR");
                } else if (gs.getGridInfo().getGridType()
                        .equals(GridType.WEATHER)) {
                    printStream.println("WEATHER");
                } else if (gs.getGridInfo().getGridType()
                        .equals(GridType.DISCRETE)) {
                    printStream.println("DISCRETE");
                } else {
                    printStream.println("NONE");
                }

                // parameter name and level
                if (!gs.getGridInfo().getParmID().getParmLevel()
                        .equals(ParmID.defaultLevel())) {
                    printStream.println(gs.getGridInfo().getParmID()
                            .getParmName() + "_"
                            + gs.getGridInfo().getParmID().getParmLevel());
                } else {
                    printStream.println(
                            gs.getGridInfo().getParmID().getParmName());
                }

                // database site identifier
                printStream.println(
                        gs.getGridInfo().getParmID().getDbId().getSiteId());

                // database optional type
                if (gs.getGridInfo().getParmID().getDbId().getDbType()
                        .equals("")) {
                    printStream.println("<notype>");
                } else {
                    printStream.println(
                            gs.getGridInfo().getParmID().getDbId().getDbType());
                }

                // database model name
                printStream.println(
                        gs.getGridInfo().getParmID().getDbId().getModelName());

                // database time
                printStream.println(
                        gs.getGridInfo().getParmID().getDbId().getModelTime());

                // projection identifier
                printStream.println(gs.getGridInfo().getGridLoc()
                        .getProjection().getProjectionID());

                // grid size (x y), minimum world coordinates (x y),
                // domain extent (x y)
                printStream.println(gs.getGridInfo().getGridLoc().getNx()
                        .toString() + " "
                        + gs.getGridInfo().getGridLoc().getNy().toString() + " "
                        + gs.getGridInfo().getGridLoc().getOrigin().x + " "
                        + gs.getGridInfo().getGridLoc().getOrigin().y + " "
                        + gs.getGridInfo().getGridLoc().getExtent().x + " "
                        + gs.getGridInfo().getGridLoc().getExtent().y);

                // units
                printStream.println(gs.getGridInfo().getUnitString());

                // descriptive name
                printStream.println(gs.getGridInfo().getDescriptiveName());

                // minimum possible value, maximum possible value, data
                // precision,
                // time independent parameter
                printStream.print(gs.getGridInfo().getMinValue() + " "
                        + gs.getGridInfo().getMaxValue() + " "
                        + gs.getGridInfo().getPrecision() + " ");
                if (gs.getGridInfo().isTimeIndependentParm()) {
                    printStream.print(1);
                } else {
                    printStream.print(0);
                }
                printStream.println();

                // time constraints (startTime, duration, repeatInterval)
                printStream.println(
                        gs.getGridInfo().getTimeConstraints().getStartTime()
                                + " "
                                + gs.getGridInfo().getTimeConstraints()
                                        .getDuration()
                                + " " + gs.getGridInfo().getTimeConstraints()
                                        .getRepeatInterval());

                // valid time range for grid
                printStream.print(
                        validTimeFormat.format(gs.getValidTime().getStart())
                                + " ");
                printStream.println(
                        validTimeFormat.format(gs.getValidTime().getEnd()));

                // output the grid points
                // we loop in reverse order across the y-axis because AWIPS1 has
                // (0,0) in UL, while we use LL for (0, 0) in AWIPS2
                if (gs.getGridInfo().getGridType().equals(GridType.SCALAR)) {
                    ScalarGridSlice scalar = (ScalarGridSlice) gs;
                    for (int i = scalar.getScalarGrid().getYdim()
                            - 1; i >= 0; i--) {
                        for (int j = 0; j < scalar.getScalarGrid()
                                .getXdim(); j++) {
                            printStream.println(
                                    round(scalar.getScalarGrid().get(j, i),
                                            gs.getGridInfo().getPrecision()));
                        }
                    }
                } else if (gs.getGridInfo().getGridType()
                        .equals(GridType.VECTOR)) {
                    VectorGridSlice vector = (VectorGridSlice) gs;
                    for (int i = vector.getMagGrid().getYdim()
                            - 1; i >= 0; i--) {
                        for (int j = 0; j < vector.getMagGrid()
                                .getXdim(); j++) {
                            printStream
                                    .print(round(vector.getMagGrid().get(j, i),
                                            gs.getGridInfo().getPrecision()));
                            printStream.print(' ');
                            printStream
                                    .print(round(vector.getDirGrid().get(j, i),
                                            gs.getGridInfo().getPrecision()));
                            printStream.println();
                        }
                    }
                } else if (gs.getGridInfo().getGridType()
                        .equals(GridType.WEATHER)) {
                    WeatherGridSlice weather = (WeatherGridSlice) gs;
                    for (int i = weather.getWeatherGrid().getYdim()
                            - 1; i >= 0; i--) {
                        for (int j = 0; j < weather.getWeatherGrid()
                                .getXdim(); j++) {
                            String key = weather.getKeys()[0xFF
                                    & weather.getWeatherGrid().get(j, i)]
                                            .toString();
                            printStream.println(key);
                        }
                    }
                } else if (gs.getGridInfo().getGridType()
                        .equals(GridType.DISCRETE)) {
                    DiscreteGridSlice discrete = (DiscreteGridSlice) gs;
                    for (int i = discrete.getDiscreteGrid().getYdim()
                            - 1; i >= 0; i--) {
                        for (int j = 0; j < discrete.getDiscreteGrid()
                                .getXdim(); j++) {
                            String key = discrete.getKeys()[0xFF
                                    & discrete.getDiscreteGrid().get(j, i)]
                                            .toString();
                            printStream.println(key);
                        }
                    }
                }
            }
        }
    }

    /**
     * Read the ASCII grid data from a File
     *
     * @param aGridData
     * @return empty string if successful or error message
     * @throws FileNotFoundException
     * @throws GfeException
     * @throws ParseException
     */
    public String readASCIIGridData(File aGridData)
            throws FileNotFoundException, GfeException, ParseException {
        List<IGridSlice> gridSlices = new ArrayList<IGridSlice>();

        Scanner inputStream = null;
        try {
            inputStream = new Scanner(aGridData, "US-ASCII");

            while (true) {
                // read the ASCIIGRID keyword
                // if we have an ASCIIGRID to read
                if (!inputStream.next().equals("ASCIIGRID")) {
                    String msg = "ASCIIGRID keyword not found. Grid#="
                            + gridSlices.size();
                    statusHandler.handle(Priority.PROBLEM, msg);
                    return msg;
                }

                // read the data type
                String strDataType = inputStream.next();
                GridType dataType = GridType.valueOf(strDataType);

                // read the parm name
                String parmNameAndLevel = inputStream.next();

                // read the database site identifier
                String dbSiteId = inputStream.next();

                // read the database optional type
                String dbType = inputStream.next();
                if (dbType.equals("<notype>")) {
                    dbType = "";
                }

                // read the database model name
                String dbModelName = inputStream.next();

                // read the database time
                String dbTime = inputStream.next();

                // make the DatabaseID
                DatabaseID dbId = new DatabaseID(dbSiteId, DataType.GRID,
                        dbType, dbModelName, dbTime);

                // make the ParmID
                int pos = parmNameAndLevel.indexOf("_");
                ParmID parmId;
                if (pos > 0) {
                    parmId = new ParmID(parmNameAndLevel.substring(0, pos),
                            dbId, parmNameAndLevel.substring(pos));
                } else {
                    parmId = new ParmID(parmNameAndLevel, dbId);
                }

                // read the projection id
                String projId = inputStream.next();
                if (projId.equals("<NoProjection>")) {
                    projId = "";
                }

                // read the grid size
                int xSize = inputStream.nextInt();
                int ySize = inputStream.nextInt();

                // read the grid origin and extent
                float xOrigin = inputStream.nextFloat();
                float yOrigin = inputStream.nextFloat();
                float xExtent = inputStream.nextFloat();
                float yExtent = inputStream.nextFloat();

                // make the GridLocation
                IFPServer ifpServer = IFPServer.getActiveServer(dbSiteId);
                if (ifpServer == null) {
                    throw new GfeException(
                            "No active IFPServer for site: " + dbSiteId);
                }
                IFPServerConfig config = ifpServer.getConfig();
                GridLocation baseGLoc = config.dbDomain();
                ProjectionData projData = config.getProjectionData(projId);
                GridLocation gLocation = new GridLocation(dbSiteId, projData,
                        new Point(xSize, ySize),
                        new Coordinate(xOrigin, yOrigin),
                        new Coordinate(xExtent, yExtent),
                        baseGLoc.getTimeZone());

                // read the units
                String units = inputStream.next();

                // read the descriptive name
                // first nextLine() call will chomp the endline character on the
                // units line from above
                // we specifically need the second nextLine() here in case the
                // descriptive name is more than 1 word and next() would just go
                // to the first space
                inputStream.nextLine();
                String descName = inputStream.nextLine();

                // read the min and max allowed values, precision, and
                // timeIndependentParm
                float minAll = inputStream.nextFloat();
                float maxAll = inputStream.nextFloat();
                int precision = inputStream.nextInt();
                int timeIndFlag = inputStream.nextInt();
                boolean timeIndependentParm = (timeIndFlag == 1);

                // make the time constraints object
                int startTime = inputStream.nextInt();
                int duration = inputStream.nextInt();
                int repeatInterval = inputStream.nextInt();
                TimeConstraints ts = new TimeConstraints(duration,
                        repeatInterval, startTime);

                // make the GridParmInfo object
                GridParmInfo gParmInfo = new GridParmInfo(parmId, gLocation,
                        dataType, units, descName, minAll, maxAll, precision,
                        timeIndependentParm, ts);

                // read the valid time range for the grid
                String strStartGridTime = inputStream.next();
                String strEndGridTime = inputStream.next();
                Date startGridTime = validTimeFormat.parse(strStartGridTime);
                Date endGridTime = validTimeFormat.parse(strEndGridTime);

                // make the valid TimeRange for the grid
                TimeRange validTR = new TimeRange(startGridTime, endGridTime);

                // make a GridDataHistory
                GridDataHistory history;
                if (parmId.getDbId().getModelTime()
                        .equals(DatabaseID.NO_MODEL_TIME)) {
                    history = new GridDataHistory(OriginType.INITIALIZED,
                            parmId, validTR);
                } else {
                    history = new GridDataHistory(OriginType.OTHER, parmId,
                            validTR);
                }

                // read the grid and make the GridSlice for this grid

                // SCALAR grid
                if (dataType.equals(GridType.SCALAR)) {
                    Grid2DFloat scalarGrid = new Grid2DFloat(xSize, ySize);
                    for (int i = ySize - 1; i >= 0; i--) {
                        for (int j = 0; j < xSize; j++) {
                            // read the grid value
                            float gridValue = inputStream.nextFloat();
                            scalarGrid.set(j, i, gridValue);
                        }
                    }

                    // make SCALAR grid slice and append it to the
                    // List<IGridSlice>
                    IGridSlice gs = new ScalarGridSlice(validTR, gParmInfo,
                            new GridDataHistory[] { history }, scalarGrid);
                    gridSlices.add(gs);
                    statusHandler.handle(Priority.VERBOSE,
                            "Input: " + gs.getValidTime().toString() + " "
                                    + gs.getGridInfo().getParmID().toString());
                } else if (dataType.equals(GridType.VECTOR)) {
                    // VECTOR GRID
                    Grid2DFloat vectorMagGrid = new Grid2DFloat(xSize, ySize);
                    Grid2DFloat vectorDirGrid = new Grid2DFloat(xSize, ySize);
                    for (int i = ySize - 1; i >= 0; i--) {
                        for (int j = 0; j < xSize; j++) {
                            // read the grid values
                            float gridMagValue = inputStream.nextFloat();
                            float gridDirValue = inputStream.nextFloat();
                            vectorMagGrid.set(j, i, gridMagValue);
                            vectorDirGrid.set(j, i, gridDirValue);
                        }
                    }

                    // make VECTOR grid slice and append it to the
                    // List<IGridSlice>
                    IGridSlice gs = new VectorGridSlice(validTR, gParmInfo,
                            new GridDataHistory[] { history }, vectorMagGrid,
                            vectorDirGrid);
                    gridSlices.add(gs);
                    statusHandler.handle(Priority.VERBOSE,
                            "Input: " + gs.getValidTime().toString() + " "
                                    + gs.getGridInfo().getParmID().toString());
                } else if (dataType.equals(GridType.WEATHER)) {
                    // WEATHER grid
                    List<WeatherKey> weatherKeys = new ArrayList<WeatherKey>();
                    Grid2DByte weatherGrid = new Grid2DByte(xSize, ySize);

                    for (int i = ySize - 1; i >= 0; i--) {
                        for (int j = 0; j < xSize; j++) {
                            // read the weather key
                            String strWeatherKey = inputStream.next();

                            // make the weather key
                            WeatherKey wk = new WeatherKey(dbSiteId,
                                    strWeatherKey);

                            // check if this WeatherKey exists in the List
                            if (!weatherKeys.contains(wk)) {
                                weatherKeys.add(wk);
                            }

                            // add the weather key to the grid (as its index)
                            weatherGrid.set(j, i, weatherKeys.indexOf(wk));
                        }
                    }

                    // make WEATHER grid slice and append it to the
                    // List<IGridSlice>
                    IGridSlice gs = new WeatherGridSlice(validTR, gParmInfo,
                            new GridDataHistory[] { history }, weatherGrid,
                            weatherKeys.toArray(
                                    new WeatherKey[weatherKeys.size()]));
                    gridSlices.add(gs);
                    statusHandler.handle(Priority.VERBOSE,
                            "Input: " + gs.getValidTime().toString() + " "
                                    + gs.getGridInfo().getParmID().toString());
                } else if (dataType.equals(GridType.DISCRETE)) {
                    // DISCRETE grid
                    List<DiscreteKey> discreteKeys = new ArrayList<DiscreteKey>();
                    Grid2DByte discreteGrid = new Grid2DByte(xSize, ySize);

                    for (int i = ySize - 1; i >= 0; i--) {
                        for (int j = 0; j < xSize; j++) {
                            // read the discrete key
                            String strDiscreteKey = inputStream.next();

                            // make the discrete key
                            DiscreteKey dk = new DiscreteKey(dbSiteId,
                                    strDiscreteKey, parmId);

                            // check if this DiscreteKey exists in the List
                            if (!discreteKeys.contains(dk)) {
                                discreteKeys.add(dk);
                            }

                            // add the discrete key to the grid (as its index)
                            discreteGrid.set(j, i, discreteKeys.indexOf(dk));
                        }
                    }

                    // make DISCRETE grid slice and append it to the
                    // List<IGridSlice>
                    IGridSlice gs = new DiscreteGridSlice(validTR, gParmInfo,
                            new GridDataHistory[] { history }, discreteGrid,
                            discreteKeys.toArray(
                                    new DiscreteKey[discreteKeys.size()]));
                    gridSlices.add(gs);
                    statusHandler.handle(Priority.VERBOSE,
                            "Input: " + gs.getValidTime().toString() + " "
                                    + gs.getGridInfo().getParmID().toString());
                }

                // check for end of data
                if (!inputStream.hasNext()) {
                    break;
                }
            }
        } finally {
            if (inputStream != null) {
                inputStream.close();
            }
        }
        this.gridSlices = gridSlices;

        return "";
    }

    /**
     * Decodes the conversion string to determine the desired grid conversion
     * algorithm. Format of conversion string is:
     * "xsize ysize projID originX originY extentX extentY"
     *
     * @param cString
     * @return
     */
    private GridLocation decodeConversionString(String cString, String siteId) {
        if ((cString == null) || (cString.length() == 0)) {
            return new GridLocation();
        }

        String[] parts = cString.split(" ", 7);

        int xsize = 0;
        int ysize = 0;
        String projection = "";
        float originX = 0.0f;
        float originY = 0.0f;
        float extentX = 0.0f;
        float extentY = 0.0f;
        try {
            xsize = Integer.parseInt(parts[0]);
            ysize = Integer.parseInt(parts[1]);
            projection = parts[2];
            originX = Float.parseFloat(parts[3]);
            originY = Float.parseFloat(parts[4]);
            extentX = Float.parseFloat(parts[5]);
            extentY = Float.parseFloat(parts[6]);
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid conversion string detected", e);
            return new GridLocation();
        }

        GridLocation gloc = new GridLocation(siteId,
                config.getProjectionData(projection), new Point(xsize, ysize),
                new Coordinate(originX, originY),
                new Coordinate(extentX, extentY),
                config.dbDomain().getTimeZone());
        if (!gloc.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Non-compatible conversion string detected");
            return new GridLocation();
        }
        return gloc;
    }

    /**
     * Converts the grid data into the desired domain/resolution. The specified
     * grid is via index.
     *
     * @param gs
     * @param sourceDomain
     * @param outputDomain
     * @return
     */
    private boolean sampleGrid(IGridSlice gs, GridLocation sourceDomain,
            GridLocation outputDomain) {
        if (sourceDomain.equals(outputDomain)) {
            return true; // nothing to do -- already in desired projection
        }

        float maxLimit = gs.getGridInfo().getMaxValue();
        float minLimit = gs.getGridInfo().getMinValue();

        RemapGrid remap = new RemapGrid(sourceDomain, outputDomain);

        // some data overlaps and remapping is possible
        try {
            switch (gs.getGridInfo().getGridType()) {
            case SCALAR:
                ScalarGridSlice scalar = (ScalarGridSlice) gs;
                scalar.setScalarGrid(remap.remap(scalar.getScalarGrid(),
                        -99999.99f, maxLimit, minLimit, minLimit));
                break;
            case VECTOR:
                VectorGridSlice vector = (VectorGridSlice) gs;
                Grid2DFloat mag = new Grid2DFloat(outputDomain.getNx(),
                        outputDomain.getNy());
                Grid2DFloat dir = new Grid2DFloat(outputDomain.getNx(),
                        outputDomain.getNy());
                remap.remap(vector.getMagGrid(), vector.getDirGrid(),
                        -99999.99f, maxLimit, minLimit, minLimit, mag, dir);
                vector.setMagGrid(mag);
                vector.setDirGrid(dir);
                break;
            case WEATHER:
                WeatherGridSlice weather = (WeatherGridSlice) gs;
                weather.setWeatherGrid(
                        remap.remap(weather.getWeatherGrid(), 255, 0));
                break;
            case DISCRETE:
                DiscreteGridSlice discrete = (DiscreteGridSlice) gs;
                discrete.setDiscreteGrid(
                        remap.remap(discrete.getDiscreteGrid(), 255, 0));
                break;
            default:
                statusHandler.handle(Priority.WARN,
                        "Illegal data type detected.");
                break;
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to remap ASCIIGrid",
                    e);
            return false;
        }

        return true;
    }

    /**
     * Adjusts the input time range to match the input time constraints. Returns
     * the expanded or adjusted time range.
     *
     * @param inputTR
     * @param tc
     * @return
     */
    private TimeRange adjustToTimeConstraints(TimeRange inputTR,
            TimeConstraints tc) {
        // Dead code from AWIPS1, left incase it is ever needed
        // boolean anyGaps = false;
        // if (tc.getDuration() != tc.getRepeatInterval()) {
        // anyGaps = true;
        // }
        TimeRange expandedTR = new TimeRange();

        // attempt an exact match
        TimeRange exp2Quantum = tc.expandTRToQuantum(inputTR);
        if (inputTR.equals(exp2Quantum)) {
            expandedTR = inputTR;
        } else {
            // else find the one possible time constraint time match that has
            // the biggest overlap
            TimeRange[] trs = tc.constraintTimes(inputTR);
            long tcDuration = Long.MIN_VALUE;
            for (TimeRange tr : trs) {
                TimeRange intersec = inputTR.intersection(tr);
                if (intersec.isValid()) {
                    if (intersec.getDuration() > tcDuration) {
                        expandedTR = tr;
                        tcDuration = intersec.getDuration();
                    }
                }
            }
        }

        return expandedTR;
    }

    /**
     * Returns the sequence of GridSlices.
     *
     * @return the sequence of IGridSlices.
     */
    public List<IGridSlice> getGridSlices() {
        return gridSlices;
    }

    private float round(float val, int precision) {
        double tmp = Math.rint(val * Math.pow(10.0, precision));
        return (float) ((float) tmp / Math.pow(10.0, precision));
    }
}
