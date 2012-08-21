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

package com.raytheon.uf.common.dataplugin.gfe.util;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;
import com.vividsolutions.jts.operation.polygonize.Polygonizer;

/**
 * Utility class for performing miscellaneous tasks relating to GFE. This class
 * is mostly provided for the client.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GfeUtil {

    /** Date formatter for generating correct path names */
    public static final SimpleDateFormat DateFormatter = new SimpleDateFormat(
            "yyyy_MM_dd_HH");

    public static final String KEY_SEPARATOR = "|";

    public static final String HAZARDS_KEY = "Hazards";

    public static final String NOTIFY = "gfeNotify";

    private static Pattern DISCRETE_PATTERN = Pattern
            .compile("'[\\w<>+/^\\[\\]\\.,:-]*'");

    private static final String TEMP_D2D_DIR = "D2DDataTransfer";

    public static final String ISC_QUEUE_ENDPOINT = "queueISCGrids";

    public static String HISTORY_DELIMITER = "----";

    /**
     * Creates the group for storing data to the HDF5 data store
     * 
     * @param parmId
     *            The parmId of the data to be stored
     * @param time
     *            The time of the data to be stored
     * @return The group name for the data
     */
    public static String getHDF5Group(ParmID parmId, TimeRange time) {
        synchronized (DateFormatter) {
            String groupName = parmId.getParmName() + "/"
                    + parmId.getParmLevel() + "/"
                    + DateFormatter.format(correctDate(time.getStart())) + "--"
                    + DateFormatter.format(correctDate(time.getEnd()));
            return groupName;
        }
    }

    /**
     * Creates the group for storing data to the HDF5 data store
     * 
     * @param parmId
     *            The parmId of the data to be stored
     * @return The group name for the data
     */
    public static String getHDF5Group(ParmID parmId) {
        synchronized (DateFormatter) {
            String groupName = parmId.getParmName() + "/"
                    + parmId.getParmLevel();
            return groupName;
        }
    }

    /**
     * Creates the groups for storing data to the HDF5 data store
     * 
     * @param parmId
     *            The parmId of the data to be stored
     * @param times
     *            The times of the data to be stored
     * @return The group names for the data
     */
    public static String[] getHDF5Groups(ParmID parmId, List<TimeRange> times) {
        String[] rval = new String[times.size()];
        StringBuilder tmp = new StringBuilder(100);
        tmp.append(parmId.getParmName()).append('/')
                .append(parmId.getParmLevel()).append('/');
        String mainString = tmp.toString();
        int i = 0;
        synchronized (DateFormatter) {
            for (TimeRange tr : times) {
                tmp.setLength(0);
                tmp.append(mainString);
                tmp.append(DateFormatter.format(correctDate(tr.getStart())));
                tmp.append("--");
                tmp.append(DateFormatter.format(correctDate(tr.getEnd())));
                rval[i++] = tmp.toString();
            }
        }
        return rval;
    }

    /**
     * Gets the HDF5 file name for singleton databases based on a databaseID and
     * a timeRange
     * 
     * @param baseDir
     *            the base directory
     * @param id
     *            The database ID
     * @return The HDF5 file name
     */
    public static File getHDF5File(String baseDir, DatabaseID id) {

        String hdf5FilePath = getHDF5Dir(baseDir, id).toString() + "/"
                + id.toString() + ".h5";
        return new File(hdf5FilePath);
    }

    public static File getTempHDF5File(String baseDir, ParmID id) {

        String hdf5FilePath = getTempHDF5Dir(baseDir, id).toString() + "/"
                + id.toString() + ".h5";
        return new File(hdf5FilePath);
    }

    public static File getTempHDF5Dir(String baseDir, ParmID id) {
        return new File(baseDir + id.getDbId().getSiteId() + "/" + TEMP_D2D_DIR
                + "/");
    }

    public static File getHDF5Dir(String baseDir, DatabaseID id) {
        String hdf5DirPath = "";

        String dbModelTime = id.getModelTime();
        String gfeDataDir = baseDir;
        gfeDataDir = baseDir + id.getSiteId() + "/" + id.getModelName() + "/";
        /*
         * Creates the appropriate file structure for the data. HDF5 files are
         * created based on the end time of the data
         */
        if (dbModelTime.equals(DatabaseID.NO_MODEL_TIME)) {
            /*
             * Create the file structure for a singleton database.
             */
            hdf5DirPath = gfeDataDir + "/";
        } else {
            /*
             * Create the file structure for a model database.
             */

            hdf5DirPath = gfeDataDir + dbModelTime.substring(0, 4) + "_"
                    + dbModelTime.substring(4, 6) + "_"
                    + dbModelTime.substring(6, 8) + "_"
                    + dbModelTime.substring(9) + "/";

        }
        return new File(hdf5DirPath);
    }

    /**
     * Puts a specified data into GMT
     * 
     * @param time
     *            The date
     * @return The GMT equivalent of the given date
     */
    public static Date correctDate(Date time) {
        int zoneOffset = TimeZone.getDefault().getOffset(time.getTime());
        Calendar cal = new GregorianCalendar();
        cal.setTime(time);
        cal.add(Calendar.MILLISECOND, zoneOffset * -1);
        return cal.getTime();
    }

    /**
     * Transforms a D2D grid coverage object into a GFE grid location object
     * 
     * @param coverage
     *            The D2D grid coverage object
     * @return The GFE grid location object
     */
    public static GridLocation transformGridCoverage(GridCoverage coverage) {
        GridLocation location = new GridLocation();
        location.setCrsObject(coverage.getCrs());
        location.setGeometry(coverage.getGeometry());
        location.setNx(coverage.getNx());
        location.setNy(coverage.getNy());
        return location;
    }

    /**
     * Creates a grid for the specified GridLocation that has all bits set that
     * are inside the provided polygon.
     * 
     * @param polygon
     *            coordinates must be in grid coordinates
     * @param gloc
     *            gridLocation defining the desired grid
     * @return the grid
     */
    public static Grid2DBit filledBitArray(MultiPolygon polygon,
            GridLocation gloc) {
        int rows = gloc.getNy();
        int cols = gloc.getNx();
        Grid2DBit raster = new Grid2DBit(cols, rows);

        GeometryFactory gf = new GeometryFactory();
        PreparedGeometry prep = PreparedGeometryFactory.prepare(polygon);

        Coordinate c = new Coordinate();
        for (int row = 0; row < rows; row++) {
            c.y = row;
            for (int col = 0; col < cols; col++) {
                c.x = col;
                Point p = gf.createPoint(c);
                if (prep.contains(p)) {
                    raster.set(col, row);
                }
            }
        }
        return raster;
    }

    /**
     * Converts a string of coordinates to a MultiPolygon.
     * 
     * The coordinate string must be closed (first and last coordinates equal)
     * and contain a minimum of 4 coordinates.
     * 
     * @param coords
     * @return the MultiPolygon
     */
    @SuppressWarnings("unchecked")
    public static MultiPolygon createPolygon(Coordinate[] coords) {
        if (coords.length < 4) {
            throw new IllegalArgumentException(
                    "You must supply a minimum of 4 coordinates");
        }
        // create a line string
        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(coords);

        // check for closed line string
        if (!ls.isClosed()) {
            throw new IllegalArgumentException(
                    "The coorinate list must be closed (first and last coordinates must be equal)");
        }

        // node the line string (insert vertices where lines cross)
        com.vividsolutions.jts.geom.Point pt = gf.createPoint(ls
                .getCoordinate());
        Geometry nodedLines = ls.union(pt);

        // create the polygon(s) from the noded line
        Polygonizer polygonizer = new Polygonizer();
        polygonizer.add(nodedLines);
        Collection<Polygon> polygons = polygonizer.getPolygons();

        // Collection<?> dangles = polygonizer.getDangles();
        // if (dangles != null && dangles.size() > 0) {
        // StringBuilder s = new StringBuilder(
        // "Edit area contains dangling lines.");
        // for (Object g : dangles) {
        // s.append("\n" + g);
        // }
        // Activator.getDefault().getLog().log(
        // new Status(Status.WARNING, Activator.PLUGIN_ID, s
        // .toString()));
        // }
        //
        // Collection<?> cutEdges = polygonizer.getCutEdges();
        // if (cutEdges != null && cutEdges.size() > 0) {
        // StringBuilder s = new StringBuilder("Edit area contains cut edges.");
        // for (Object g : cutEdges) {
        // s.append("\n" + g);
        // }
        // Activator.getDefault().getLog().log(
        // new Status(Status.WARNING, Activator.PLUGIN_ID, s
        // .toString()));
        // }

        // create a multipolygon from the collection of polygons
        Geometry g = gf.createMultiPolygon(polygons
                .toArray(new Polygon[polygons.size()]));

        // clean up self intersections/overlaps
        g = g.buffer(0.0);

        MultiPolygon mp = null;
        if (g instanceof MultiPolygon) {
            mp = (MultiPolygon) g;
        } else if (g instanceof Polygon) {
            mp = gf.createMultiPolygon(new Polygon[] { (Polygon) g });
        }

        if ((mp == null) || !mp.isValid()) {
            // StringBuilder s = new StringBuilder();
            // s.append("Edit area contains invalid polygons.\n");
            // IsValidOp validOp = new IsValidOp(mp);
            // s.append(validOp.getValidationError());
            // for (int i = 0; i < mp.getNumGeometries(); i++) {
            // Polygon p = (Polygon) mp.getGeometryN(i);
            // if (!p.isValid()) {
            // s.append("\n" + p);
            // }
            // }
            // Activator.getDefault().getLog().log(
            // new Status(Status.WARNING, Activator.PLUGIN_ID, s
            // .toString()));

            // return an empty polygon
            mp = gf.createMultiPolygon(new Polygon[0]);
        }
        return mp;
    }

    /**
     * Transforms a python string representation of a list of discrete keys into
     * a Java List<String>
     * 
     * @param list
     *            a string representation of a python list of discrete keys
     * @return the discrete key strings separated out
     */
    public static List<String> discreteKeyStringToList(String list) {
        Matcher m = DISCRETE_PATTERN.matcher(list);
        ArrayList<String> matchList = new ArrayList<String>();
        while (m.find()) {
            int start = m.start() + 1;
            int end = m.end() - 1;
            String sub = list.substring(start, end);
            matchList.add(sub);
        }
        return matchList;
    }

    public static Set<GridDataHistory> parseHistoryStrings(String history) {
        Set<GridDataHistory> historySet = new HashSet<GridDataHistory>();
        if (history == null) {
            return historySet;
        }
        if (history.isEmpty()) {
            return historySet;
        }
        String[] histories = history.split(HISTORY_DELIMITER);
        for (String historyString : histories) {
            historySet.add(new GridDataHistory(historyString));
        }
        return historySet;
    }

    public static String getHistoryStrings(Collection<GridDataHistory> histories) {
        StringBuffer buffer = new StringBuffer();
        int index = histories.size();
        for (GridDataHistory history : histories) {
            buffer.append(history.getCodedString());
            if (--index > 0) {
                buffer.append(HISTORY_DELIMITER);
            }
        }
        return buffer.toString();
    }
}
