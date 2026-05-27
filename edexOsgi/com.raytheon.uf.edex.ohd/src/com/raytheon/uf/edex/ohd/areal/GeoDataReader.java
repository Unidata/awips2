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
package com.raytheon.uf.edex.ohd.areal;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateList;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.MultiLineString;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKBReader;

import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.hydro.areal.GeoAreaData;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Processes the hydro Areal Definitions file contents for zones, counties,
 * basins, and reservoirs and returns a list of GeoAreaData objects.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2018   6979     mduff       Initial creation
 * Mar 11, 2020 19533   mgamazaychikov Added code to import shapefile data
 * Jul 10, 2020 19533   mgamazaychikov Added code to call importShapeFile.sh over ssh
 *
 * </pre>
 *
 * @author mduff
 */

public class GeoDataReader {

    private static final String NEWLINE = System.getProperty("line.separator");

    private static final int LOC_AREANAME_LEN = 40;

    private static final String IMPORT_SHAPEFILE = "/awips2/database/sqlScripts/share/sql/maps/importShapeFile.sh";

    private static final String MAPS_DB_SCHEMA = "mapdata";

    private static final String MAPS_DB_TABLE = "basins";

    private static final String MAPS_DB = "maps";

    private static final String TABLE_COLUMNS = "id, name, lat, lon";

    private static final String GEOMETRY_COLUMN = "the_geom";

    private static final String FROM = " FROM ";

    private static final String WHERE = " WHERE ";

    private static final String SSH = "ssh";

    private static final String SCP = "scp";

    private static final String REMOTE = "/tmp/";

    private StringBuilder logBuffer = new StringBuilder();

    private List<GeoAreaData> geoDataList = new ArrayList<>();

    /**
     * Import the data from the file.
     *
     * <pre>
     * &lt;id&gt; &lt;name&gt; &lt;feature rank&gt; &lt;numpoints&gt; [center lat] [center lon]
     * &lt;lat&gt; &lt;lon&gt;
     * &lt;lat&gt; &lt;lon&gt;
     * &lt;lat&gt; &lt;lon&gt;
     * &lt;lat&gt; &lt;lon&gt;
     * &lt;lat&gt; &lt;lon&gt;
     * ... ...
     * &lt;lat&gt; &lt;lon&gt;
     *
     * where id is the 1-8 character id of the geoarea or geoline
     * name is the name of the geoarea or geoline. It may be up to
     *      20 characters long for a geoline and up to 40 characters
     *      long for a geoarea.
     * feature rank is the order of the geoarea or geoline. This allows
     *      geographic features to be displayed according to relative
     *      importance. Lower numbers take precedence over higher numbers.
     * numpoints is the number of latitude/longitude pairs defining the
     *      geoarea or geoline.
     * center lat is the centroid latitude.  This applies only to geoarea polygons.
     * center lon is the centroid longitude. This applies only to geoarea polygons.
     * 
     * Code from A1:  process_geoarea.c, load_geodata.c, read_geodata.c, geo_header_file.c
     * </pre>
     */
    public String importGeoArea(List<String> geoDataContents,
            ArealTypeSelection selectedType) {

        boolean saveDataBlock = true;
        int linenum = 0;
        int nPts = -1;
        double intLat = -1;
        double intLon = -1;
        String id = null;
        double[] lonPoints = null;
        double[] latPoints = null;
        String areaName = null;
        String line = null;

        boolean headerLine = true;
        for (int i = 0; i < geoDataContents.size(); i++) {
            if (headerLine) {
                line = geoDataContents.get(i);
                linenum++;
                if (line.length() == 0) {
                    // Skip any blank lines
                    continue;
                }
                saveDataBlock = true;

                /*
                 * Process the header line, extract each of the attributes from
                 * the header block for the subsequent set of points
                 */

                /* allow the line to have the interior lat-lon at the end. */
                String str = line;

                /* Remove any excess whitespace. */
                str = str.trim();
                str = str.replaceAll("\\s{2,}", " ");
                str = str.replaceAll("'", "''");

                String[] parts = str.split(" ");
                int numParts = parts.length;

                /*
                 * Check the last 2 items for a decimal point. If a decimal
                 * point is found then assume this line has two lat/lon values
                 * at the end.
                 */
                if (numParts > 2 && parts[numParts - 2].contains(".")
                        && parts[numParts - 1].contains(".")) {
                    intLat = Double.parseDouble(parts[numParts - 2]);
                    // * -1 for hydro perspective
                    intLon = Double.parseDouble(parts[numParts - 1]) * -1;

                    if ((intLat < 0) || (intLat > 90)) {
                        log("WARNING:  invalid interior " + intLat
                                + " lat in line " + linenum + ":  " + line);
                        saveDataBlock = false;
                    }

                    if ((intLon < -180) || (intLon > 180)) {
                        log("WARNING:  invalid interior " + intLon
                                + " lon in line " + linenum + ":  " + line);
                        saveDataBlock = false;
                    }
                }

                int shiftNum = 0;
                if (numParts == 4) {
                    shiftNum = 2;
                }

                /*
                 * get the number of lat-lon pairs that follow, from the end of
                 * the line
                 */
                nPts = Integer.parseInt(parts[numParts - 3 + shiftNum]);

                lonPoints = new double[nPts];
                latPoints = new double[nPts];

                /*
                 * get the stream order, which is not always specified, from the
                 * field preceding the num of lat-lon pairs
                 */
                int streamOrder = Integer
                        .parseInt(parts[numParts - 4 + shiftNum]);

                if ((streamOrder < -1) || (streamOrder > 50)) {
                    log("WARNING: Error reading stream order in line " + linenum
                            + ": " + line);
                }

                /* now get the identifier at the beginning of the string */
                id = parts[0];

                /* get the identifying name */
                StringBuilder name = new StringBuilder();
                for (int j = 1; j <= numParts - 5 + shiftNum; j++) {
                    name.append(parts[j]).append(" ");
                }

                name.trimToSize();
                if (name.length() > LOC_AREANAME_LEN) {
                    log(String.format(
                            "WARNING: truncated name (use 1-%d chars) in line %d: %s",
                            LOC_AREANAME_LEN, linenum, line));
                    areaName = name.substring(0, 40);
                } else if (name.length() <= 0) {
                    log(String.format(
                            "WARNING: invalid name (use 1-%d chars) in line %d: %s",
                            LOC_AREANAME_LEN, linenum, line));

                    areaName = "UNDEFINED";
                } else {
                    areaName = name.toString();
                }
                areaName = areaName.trim();
            }

            if (!headerLine) {
                for (int index = 0; index < nPts; index++) {
                    line = geoDataContents.get(i);
                    line = line.trim();

                    if (line == null) {
                        log("ERROR: Unexpected end-of-file reached after line "
                                + linenum);
                        return logBuffer.toString();
                    }

                    linenum++;

                    /* Extract the latitude and longitude values. */
                    String[] latlon = line.split("\\s+");
                    if (latlon.length != 2) {
                        log("WARNING: finding a latitude/longitude pair");
                        log("for id " + id + " in line " + linenum + ": "
                                + line);
                        log("(line " + (i + 1) + " of block)");
                        saveDataBlock = false;
                    } else {
                        double lat = Double.parseDouble(latlon[0]);
                        double lon = Double.parseDouble(latlon[1]) * -1;

                        /* Test the bounds of the longitude value. */
                        if ((lon < -180) || (lon > 180)) {
                            log("WARNING: reading or invalid lon for id " + id
                                    + " in line " + (i + 1) + ": " + line);
                            log("(line " + (i + 1) + " of block)");
                            saveDataBlock = false;
                        }

                        lonPoints[index] = lon;

                        /* Test the bounds of the latitude value */
                        if ((lat < 0) || (lat > 90)) {
                            log("WARNING: reading or invalid lat for id " + id
                                    + " in line " + (i + 1) + ": " + line);
                            log("(line " + (i + 1) + " of block)");
                            saveDataBlock = false;
                        }

                        latPoints[index] = lat;
                    }

                    // Increment i as lines are being processed
                    i++;
                }
                i--;

                // Create the data object
                GeoAreaData geoData = new GeoAreaData();
                geoData.setAreaId(id);
                geoData.setName(areaName);
                geoData.setBoundaryType(selectedType.getDataName());
                geoData.setInteriorLat(intLat);
                // Convert back to hydro format of positive longitude values
                geoData.setInteriorLon(intLon * -1);
                geoData.setLon(lonPoints);
                geoData.setLat(latPoints);
                geoData.setNumberPoints(nPts);
                geoData.setSaveDataBlock(saveDataBlock);
                geoDataList.add(geoData);
            }

            // Reset for header line flag
            headerLine = !headerLine;
        }

        if (geoDataList.isEmpty()) {
            log("ERROR: Could not read data for geotype "
                    + selectedType.getDataName());
        }

        return logBuffer.toString();
    }

    private void log(String msg) {
        logBuffer.append(msg).append(NEWLINE);
    }

    /**
     * Returns the list of processed data in GeoAreaData objects.
     * 
     * @return
     */
    public List<GeoAreaData> getGeoDataList() {
        return geoDataList;
    }

    private static String buildGeospatialConstraint(String geomField,
            Envelope env) {
        StringBuilder constraint = new StringBuilder(geomField);
        constraint.append(" && ST_SetSrid(");
        constraint.append(String.format("'BOX3D(%f %f, %f %f)'::box3d",
                env.getMinX(), env.getMinY(), env.getMaxX(), env.getMaxY()));
        constraint.append(", 4326)");
        return constraint.toString();
    }

    public String importGeoSHPArea(String file,
            ArealTypeSelection selectedType) {
        String host = System.getProperty("user.name") + "@" + System.getenv("DB_HOST");
        String localPathToFile = file.substring(0, file.lastIndexOf("/") + 1);
        String baseFileName = file.substring(file.lastIndexOf("/") + 1, file.lastIndexOf(".") + 1);
        String shpFileToIngest = baseFileName + "shp";
        String shxFileToIngest = baseFileName + "shx";
        String dbfFileToIngest = baseFileName + "dbf";
        String prjFileToIngest = baseFileName + "prj";
        File fl = new File(localPathToFile + shpFileToIngest);
        if (!fl.exists()) {
            log("ERROR: File: " + localPathToFile + shpFileToIngest + " does not exist.");
            return logBuffer.toString();
        } else {
            if (!callCopyFileCommand(host, localPathToFile + shpFileToIngest, REMOTE + shpFileToIngest)) {
                return logBuffer.toString();
            }
        }
        fl = new File(localPathToFile + shxFileToIngest);
        if (!fl.exists()) {
            log("ERROR: File: " + localPathToFile + shxFileToIngest + " does not exist.");
            return logBuffer.toString();
        } else {
            if (!callCopyFileCommand(host, localPathToFile + shxFileToIngest, REMOTE + shxFileToIngest)){
                return logBuffer.toString();
            }
        }
        fl = new File(localPathToFile + dbfFileToIngest);
        if (!fl.exists()) {
            log("ERROR: File: " + localPathToFile + dbfFileToIngest + " does not exist.");
            return logBuffer.toString();
        } else {
            if (!callCopyFileCommand(host, localPathToFile + dbfFileToIngest, REMOTE + dbfFileToIngest)){
                return logBuffer.toString();
            }
        }
        fl = new File(localPathToFile + prjFileToIngest);
        if (!fl.exists()) {
            log("WARNING: File: " + localPathToFile + prjFileToIngest + " does not exist: importShapeFile.sh will proceed with WGS84 projection.");
        } else {
            if (!callCopyFileCommand(host, localPathToFile + prjFileToIngest, REMOTE + prjFileToIngest)){
                return logBuffer.toString();
            }
        }

        boolean shapefileIngestSuccess = callImportScriptCommand(host, REMOTE + shpFileToIngest);
        if (!shapefileIngestSuccess) {
            return logBuffer.toString();
        }

        boolean saveDataBlock = true;
        int nPts = -1;
        double intLat = -1;
        double intLon = -1;
        String baseID = null;
        double[] lonPoints = null;
        double[] latPoints = null;
        String areaName = null;
        List<String> areaIDList = new ArrayList<>();
        Rectangle extent = null;
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
        } catch (NumberFormatException e1) {
            log("ERROR:  cannot read coordinates from coord_host.dat file "
                    + e1.getMessage());
            return logBuffer.toString();
        } catch (FileNotFoundException e1) {
            log("ERROR:  coord_host.dat file not found " + e1.getMessage());
            return logBuffer.toString();
        } catch (IOException e1) {
            log("ERROR:  cannot read coordinates from coord_host.dat file "
                    + e1.getMessage());
            return logBuffer.toString();
        }
        HRAPSubGrid newSubGrid = null;

        try {
            newSubGrid = new HRAPSubGrid(extent);
        } catch (HrapConversionException e1) {
            log("ERROR:  cannot convert HRAP coordinates " + e1.getMessage());
            return logBuffer.toString();
        }
        Polygon rect = (Polygon) newSubGrid.getGeometry();

        // create SELECT PSQL query
        StringBuilder query = new StringBuilder("SELECT ");
        // add columns id, nam, lat, lon to the query
        query.append(TABLE_COLUMNS);
        // add the geometry column to the query
        query.append(", ST_AsBinary(").append(GEOMETRY_COLUMN).append(") as ")
                .append(GEOMETRY_COLUMN);
        // specify the table to query
        query.append(FROM).append(MAPS_DB_SCHEMA).append(".")
                .append(MAPS_DB_TABLE);
        // subset query result on the HRAP rectangle by adding geospatial
        // constraint
        query.append(WHERE).append(buildGeospatialConstraint(GEOMETRY_COLUMN,
                rect.getEnvelopeInternal())).append(";");

        WKBReader wkbReader = new WKBReader();

        CoreDao dao = new CoreDao(DaoConfig.forDatabase(MAPS_DB));
        Object[] results = dao.executeSQLQuery(query.toString());
        Map<String, Geometry> map1 = new HashMap<>();

        for (int i = 0; i < results.length; ++i) {

            Object[] row = (Object[]) results[i];
            if (row.length > 0) {
                // parse each row of the results
                String idRow = null;
                String nameRow = null;
                double latRow = 0;
                double lonRow = 0;
                if (row[0] != null) {
                    idRow = (String) row[0];
                }
                if (row[1] != null) {
                    nameRow = (String) row[1];
                }
                if (row[2] != null) {
                    latRow = (Double) row[2];
                }
                if (row[3] != null) {
                    lonRow = (Double) row[3];
                }
                if (row[4] != null) {
                    MultiPolygon boundary = null;
                    try {
                        boundary = (MultiPolygon) wkbReader
                                .read((byte[]) row[4]);
                    } catch (ParseException e) {
                        log("ERROR:  Unable to read geometry."
                                + e.getMessage());
                    }

                    baseID = idRow;
                    if (!areaIDList.contains(baseID)) {
                        map1.put(baseID, boundary);
                        areaIDList.add(baseID);
                        StringBuilder name = new StringBuilder();
                        name.append(nameRow);
                        if (name.length() > LOC_AREANAME_LEN) {
                            log(String.format(
                                    "WARNING: truncated name (use 1-%d chars) in polygon ID: %s",
                                    LOC_AREANAME_LEN, baseID));
                            areaName = name.substring(0, 40);
                        } else if (name.length() <= 0) {
                            log(String.format(
                                    "WARNING: invalid name (use 1-%d chars) in in polygon ID: %s",
                                    LOC_AREANAME_LEN, baseID));
                            areaName = "UNDEFINED";
                        } else {
                            areaName = name.toString();
                        }
                        areaName = areaName.trim();
                        intLat = latRow;
                        intLon = lonRow;
                        if ((intLat < 0) || (intLat > 90)) {
                            log("WARNING:  invalid interior " + intLat
                                    + " lat in polygon ID: " + baseID);
                            saveDataBlock = false;
                        }

                        if ((intLon < -180) || (intLon > 180)) {
                            log("WARNING:  invalid interior " + intLon
                                    + " lon in polygon ID: " + baseID);
                            saveDataBlock = false;
                        }

                        for (int ipol = 0; ipol < boundary
                                .getNumGeometries(); ipol++) {
                            Coordinate[] coords = null;
                            String id = null;
                            if (boundary.getNumGeometries() > 1 && ipol == 0) {
                                log("WARNING: polygon ID " + baseID + " has "
                                        + boundary.getNumGeometries()
                                        + " parts.");
                            }

                            if (boundary.getNumGeometries() > 1) {
                                id = baseID + String.format("%04d", ipol + 1);
                                log("Attempting to read part separately as "
                                        + id);
                                Geometry geom = boundary.getGeometryN(ipol)
                                        .getBoundary();
                                LineString ls = null;
                                MultiLineString mls = null;
                                if (geom instanceof LineString) {
                                    ls = (LineString) geom;
                                } else if (geom instanceof MultiLineString) {
                                    mls = (MultiLineString) geom;
                                    if (mls.getNumGeometries() > 1) {
                                        log("WARNING:  found unclosed multistring in polygon "
                                                + id
                                                + ". Consolidating multistring into linestring.");
                                        Coordinate[] coords2 = mls
                                                .getCoordinates();
                                        GeometryFactory factory = new GeometryFactory();
                                        ls = factory.createLineString(coords2);
                                    } else {
                                        ls = (LineString) mls.getGeometryN(0);
                                    }
                                }

                                if (!ls.isClosed()) {
                                    log("WARNING:  found unclosed linestring in polygon "
                                            + id + ". Attempting to close it.");
                                    CoordinateList list = new CoordinateList(
                                            ls.getCoordinates());
                                    list.closeRing();
                                    GeometryFactory factory = new GeometryFactory();
                                    LinearRing ring = factory.createLinearRing(
                                            list.toCoordinateArray());
                                    coords = ring.getCoordinates();
                                } else {
                                    coords = ls.getCoordinates();
                                }
                            } else {
                                id = baseID;
                                Geometry geom = boundary.getGeometryN(ipol)
                                        .getBoundary();
                                LineString ls = null;
                                MultiLineString mls = null;
                                if (geom instanceof LineString) {
                                    ls = (LineString) geom;
                                } else if (geom instanceof MultiLineString) {
                                    mls = (MultiLineString) geom;
                                    if (mls.getNumGeometries() > 1) {
                                        log("WARNING:  found unclosed multistring in polygon "
                                                + id + " -> skipping it.");
                                        continue;
                                    } else {
                                        ls = (LineString) mls.getGeometryN(0);
                                    }
                                }

                                if (!ls.isClosed()) {
                                    log("WARNING:  found unclosed linestring in polygon "
                                            + id + ". Attempting to close it.");
                                    CoordinateList list = new CoordinateList(
                                            ls.getCoordinates());
                                    list.closeRing();
                                    GeometryFactory factory = new GeometryFactory();
                                    LinearRing ring = factory.createLinearRing(
                                            list.toCoordinateArray());
                                    coords = ring.getCoordinates();
                                } else {
                                    coords = ls.getCoordinates();
                                }
                            }

                            areaIDList.add(id);
                            nPts = coords.length;
                            lonPoints = new double[nPts];
                            latPoints = new double[nPts];
                            for (int ic = 0; ic < coords.length; ic++) {
                                double lat = coords[ic].y;
                                double lon = coords[ic].x;
                                /* Test the bounds of the longitude value. */
                                if ((lon < -180) || (lon > 180)) {
                                    log("WARNING:  reading or invalid lon for polygon ID: id "
                                            + id);
                                    log("lon=" + lon);
                                    saveDataBlock = false;
                                }
                                lonPoints[ic] = lon;

                                /* Test the bounds of the latitude value */
                                if ((lat < 0) || (lat > 90)) {
                                    log("WARNING: reading or invalid lat for polygon ID: id "
                                            + id);
                                    log("lat=" + lat);
                                    saveDataBlock = false;
                                }
                                latPoints[ic] = lat;
                            }
                            GeoAreaData geoData = new GeoAreaData();
                            geoData.setAreaId(id);
                            geoData.setName(areaName);
                            geoData.setBoundaryType(selectedType.getDataName());
                            geoData.setInteriorLat(intLat);
                            // Convert back to hydro format of positive
                            // longitude values
                            geoData.setInteriorLon(intLon * -1);
                            geoData.setLon(lonPoints);
                            geoData.setLat(latPoints);
                            geoData.setNumberPoints(nPts);
                            geoData.setSaveDataBlock(saveDataBlock);
                            geoDataList.add(geoData);
                        }

                    } else {
                        Geometry existingBoundary = map1.get(baseID);
                        Geometry diff = existingBoundary.difference(boundary);
                        if (diff.isEmpty()) {
                            log("WARNING:  found duplicate polygon " + baseID
                                    + " -> skipping it.");
                        } else {
                            log("WARNING:  oops");
                        }
                    }
                }
            }
        }

        return logBuffer.toString();
    }

    private boolean callImportScriptCommand(String host, String file) {
        boolean commandResult = false;
        LinkedList<String> command = getIngestCommandToExecute(host, file);
        StringBuilder commandSB = new StringBuilder();
        for (String str : command) {
            commandSB.append(str).append(" ");
        }
        String commandString = commandSB.toString().trim();
        ProcessBuilder procDesc = new ProcessBuilder(command);
        procDesc.redirectErrorStream(true);
        try {
            Process proc = procDesc.start();
            StringBuilder message = new StringBuilder();
            try (BufferedReader errorReader = new BufferedReader(
                    new InputStreamReader(proc.getErrorStream()))) {
                String outp; 
                while ((outp = errorReader.readLine()) != null) {
                    message.append(outp).append('\n');
                }
                int exitVal = proc.waitFor();
                String stdErr = message.toString();
                if (exitVal != 0 || !stdErr.isEmpty()) {
                    log("ERROR: Unsuccessfully executed import shapefile command: "
                            + commandString + " Error Exit Code: " + exitVal);
                } else {
                    log("INFO: Successfully executed import shapefile command: "
                            + commandString + ": " + message);
                    commandResult = true;
                }
            } finally {
                if (proc != null) {
                    proc.destroy();
                }
            }
        } catch (Throwable e) {
            log("ERROR: Exception encountered during import shapefile command: " + e.getMessage());
        }
        return commandResult;
    }

    private boolean callCopyFileCommand(String host, String fileLocal, String fileRemote) {
        boolean commandResult = false;
        LinkedList<String> command = getCopyCommandToExecute(host, fileLocal, fileRemote);
        StringBuilder commandSB = new StringBuilder();
        for (String str : command) {
            commandSB.append(str).append(" ");
        }
        String commandString = commandSB.toString().trim();
        ProcessBuilder procDesc = new ProcessBuilder(command);
        procDesc.redirectErrorStream(true);
        try {
            Process proc = procDesc.start();
            StringBuilder message = new StringBuilder();
            try (BufferedReader errorReader = new BufferedReader(
                    new InputStreamReader(proc.getErrorStream()))) {
                String outp; 
                while ((outp = errorReader.readLine()) != null) {
                    message.append(outp).append('\n');
                }
                int exitVal = proc.waitFor();
                String stdErr = message.toString();
                if (exitVal != 0 || !stdErr.isEmpty()) {
                    log("ERROR: Unsuccessfully executed copy shapefile command: " + commandString + " Error Exit Code: " + exitVal);
                } else {
                    log("INFO: Successfully executed copy shapefile command: " + commandString + ": " + message);
                    commandResult = true;
                }
            } finally {
                if (proc != null) {
                    proc.destroy();
                }
            }
        } catch (Throwable e) {
            log("ERROR: Exception encountered during copy shapefile command: " + e.getMessage());
        }
        return commandResult;
    }

    private LinkedList<String> getIngestCommandToExecute(String host, String file) {
        LinkedList<String> command = new LinkedList<>();
        command.add(SSH);
        command.add(host);
        command.add(IMPORT_SHAPEFILE);
        command.add(file);
        command.add(MAPS_DB_TABLE);
        return command;
    }

    private LinkedList<String> getCopyCommandToExecute(String host, String local, String remote) {
        LinkedList<String> command = new LinkedList<>();
        command.add(SCP);
        command.add(local);
        command.add(host + ":" + remote);
        return command;
    }
}
