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
package com.raytheon.viz.hydrobase.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.common.xmrg.hrap.HrapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Process the spatial work for loading basins, zones, etc. for hydro.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 18, 2015    5217    mpduff      Initial creation
 * Mar 08, 2016    5217    mpduff      Fixed column values to be full hrap columns rather
 *                                        than relative to the subgrid.
 * Apr 07, 2016    5217    mpduff      Fixed an issue calculating hrap column.
 * Apr 15, 2016    5217    mpduff      Need to reproject the basin polygon into HRAP CRS.
 * Aug 11, 2016    4619    bkowal      {@link HrapUtil} is now common.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class HydroGeoProcessor {
    private final Geometry[][] hrapGeometries;

    /**
     * Constructor. Set up the hrapGeometry cache.
     * 
     * @throws Exception
     */
    public HydroGeoProcessor() throws HrapConversionException {
        HRAP hrap = HRAP.getInstance();
        Coordinate ll = hrap.getLatLonLL();
        Coordinate ur = hrap.getLatLonUR();

        Coordinate hrapLL = HrapUtil.latLonToHrap(ll);
        Coordinate hrapUR = HrapUtil.latLonToHrap(ur);
        int cols = (int) Math.floor(hrapUR.x - hrapLL.x);
        int rows = (int) Math.floor(hrapUR.y - hrapLL.y);
        hrapGeometries = new Geometry[rows][cols];
    }

    /**
     * Get the HrapBinList.
     * 
     * @param GeoAreaData
     *            data for the feature to check
     */
    public HrapBinList getHrapBinList(GeoAreaData geoData) throws Exception {
        List<Coordinate> coords = getPointsFromArea(geoData);

        Polygon poly = MapUtil.getPolygon(coords.toArray(new Coordinate[0]));

        /*
         * Reproject the polygon to the same map space as the HRAP grid.
         */
        HRAP hrap = HRAP.getInstance();
        GridGeometry2D hrapGridGeometry = hrap.getGridGeometry();
        CoordinateReferenceSystem latLonCRS = DefaultGeographicCRS.WGS84;
        CoordinateReferenceSystem hrapCRS = hrap.getGridGeometry()
                .getCoordinateReferenceSystem();
        MathTransform transform = CRS.findMathTransform(latLonCRS, hrapCRS);
        Geometry crsGeometry = JTS.transform(poly, transform);
        Geometry gridSpaceGeometry = JTS.transform(crsGeometry,
                hrapGridGeometry.getCRSToGrid2D());
        Coordinate[] gridSpaceCoords = gridSpaceGeometry.getCoordinates();
        Coordinate[] minMaxXY = getMinMaxXY(Arrays.asList(gridSpaceCoords));
        Coordinate hrapMin = minMaxXY[0];
        Coordinate hrapMax = minMaxXY[1];

        int maxRow = (int) Math.ceil(hrapMax.y);
        int maxCol = (int) Math.ceil(hrapMax.x);
        int minRow = (int) Math.floor(hrapMin.y);
        int minCol = (int) Math.floor(hrapMin.x);

        /* expand the box to make sure polygon has been covered */
        minRow -= 2;
        minCol -= 2;
        maxRow += 2;
        maxCol += 2;

        int rows = maxRow - minRow;
        int cols = maxCol - minCol;

        int rowCtr = 0;
        int rowNum = 0;
        int colNum = 0;
        int startCol = 0;
        int endCol = 0;
        int binCtr = 0;
        double area = 0;

        HrapBinList binList = new HrapBinList();
        for (int r = 0; r < rows; r++) {
            rowNum = r + minRow;
            startCol = -1;
            colNum = 0;
            for (int c = 0; c < cols; c++) {
                colNum = c + minCol;
                Coordinate coord = new Coordinate(colNum, rowNum);
                if (hrapGeometries[rowNum][colNum] == null) {
                    hrapGeometries[rowNum][colNum] = HrapUtil
                            .getGridCellPolygon(coord);
                }
                if (poly.intersects(hrapGeometries[rowNum][colNum])) {
                    endCol = c;
                    binCtr++;
                    if (startCol == -1) {
                        // First cell in the row
                        startCol = c;
                        rowCtr++;
                    }
                    area += HrapUtil.getHrapBinArea(coord);
                }
            }

            if (startCol != -1) {
                // Make the columns be full HRAP columns, not subgrid columns
                startCol += minCol;
                endCol += minCol;
                binList.addData(rowNum, startCol, endCol);
                binList.setNumBins(binCtr);
                binList.setNumRows(rowCtr);
                binList.setArea(area);
            }
        }

        return binList;
    }

    /**
     * Get the min and max corner points.
     * 
     * @param coords
     *            List of all coordinates
     * 
     * @return Array of min and max Coordinates
     */
    private Coordinate[] getMinMaxXY(List<Coordinate> coords) {
        double minX = 9999;
        double minY = 9999;
        double maxX = -9999;
        double maxY = -9999;

        for (Coordinate c : coords) {
            if (c.x > maxX) {
                maxX = c.x;
            }
            if (c.x < minX) {
                minX = c.x;
            }

            if (c.y > maxY) {
                maxY = c.y;
            }
            if (c.y < minY) {
                minY = c.y;
            }
        }

        Coordinate[] minMaxCoords = new Coordinate[2];
        Coordinate min = new Coordinate(minX, minY);
        Coordinate max = new Coordinate(maxX, maxY);
        minMaxCoords[0] = min;
        minMaxCoords[1] = max;

        return minMaxCoords;
    }

    /**
     * Creates an array of points from the information pointed to by the GeoArea
     * pointer. Ensures that (1) the last point is the same as the first and (2)
     * that if n points in a row are the same in the database, only one point is
     * propagated to the points array
     * 
     * @param data
     *            The GeoAreaData object
     */
    private List<Coordinate> getPointsFromArea(GeoAreaData data) {
        List<Coordinate> points = new ArrayList<>();

        /* init the first point */
        Coordinate coord = new Coordinate(data.getLon()[0], data.getLat()[0]);
        points.add(coord);
        double[] lat = data.getLat();
        double[] lon = data.getLon();

        /*
         * for each input point from the database, starting with the second
         * point
         */
        // Add the first point every time.
        points.add(new Coordinate(lon[0], lat[0]));
        for (int i = 1; i < data.getNumberPoints(); i++) {

            /* if input points are different */
            coord = new Coordinate(lon[i], lat[i]);
            points.add(coord);
        }

        /*
         * if the first point and the last point are not the same, add a final
         * point that is the same as the first
         */
        if (!pointsEqual(points.get(0), points.get(points.size() - 1))) {
            coord = new Coordinate(lon[0], lat[0]);
            points.add(coord);
        }

        return points;
    }

    /**
     * Checks two Points for equality.
     * 
     * @param p1
     *            Point 1
     * @param p2
     *            Point 2
     * @return true if points are equal
     */
    private boolean pointsEqual(Coordinate p1, Coordinate p2) {
        if ((p1.x == p2.x) && (p1.y == p2.y)) {
            return true;
        }

        return false;
    }
}
