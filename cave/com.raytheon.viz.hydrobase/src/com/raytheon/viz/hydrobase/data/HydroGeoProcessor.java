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
import java.util.List;

import com.raytheon.viz.hydrocommon.util.HrapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HydroGeoProcessor {
    public static GeometryFactory factory = new GeometryFactory();

    public HydroGeoProcessor() {
    }

    public HrapBinList getHrapBinList(GeoAreaData geoData) {
        List<Coordinate> coords = getPointsFromArea(geoData);
        Coordinate[] minMaxXY = getMinMaxXY(coords);

        LinearRing lr = factory.createLinearRing(coords
                .toArray(new Coordinate[0]));
        Polygon poly = factory.createPolygon(lr, null);

        Coordinate minC = minMaxXY[0];
        Coordinate maxC = minMaxXY[1];

        Coordinate hrapMin = HrapUtil.latLonToHrap(minC);
        Coordinate hrapMax = HrapUtil.latLonToHrap(maxC);

        double wfoMinX = hrapMin.x;
        double wfoMinY = hrapMin.y;
        double wfoMaxX = hrapMax.x;
        double wfoMaxY = hrapMax.y;

        double maxRow = Math.floor(wfoMaxY);
        double maxCol = Math.floor(wfoMaxX);
        double minRow = Math.floor(wfoMinY);
        double minCol = Math.floor(wfoMinX);

        /* expand the box to make sure polygon has been covered */
        minRow -= 2;
        minCol -= 2;
        maxRow += 2;
        maxCol += 2;

        int rowCtr = 0;
        double rowNum = 0;
        double startCol = 0;
        double endCol = 0;
        int binCtr = 0;
        double area = 0;

        HrapBinList binList = new HrapBinList();

        for (double r = minRow + 0.5; r <= maxRow; r++) { // row
            rowNum = r;
            startCol = -1;

            for (double c = minCol + 0.5; c <= maxCol; c++) {
                Coordinate coord = new Coordinate(c, r);
                Coordinate gridCell = HrapUtil.hrapToLatLon(coord);
                Point p = factory.createPoint(gridCell);
                if (poly.intersects(p)) { // inside
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
            } else if (c.x < minX) {
                minX = c.x;
            }

            if (c.y > maxY) {
                maxY = c.y;
            } else if (c.y < minY) {
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
        ArrayList<Coordinate> points = new ArrayList<Coordinate>();

        /* init the first point */
        Coordinate coord = new Coordinate(data.getLon()[0], data.getLat()[0]);
        points.add(coord);
        double[] lat = data.getLat();
        double[] lon = data.getLon();

        /*
         * for each input point from the database, starting with the second
         * point
         */
        for (int i = 1; i < data.getNumberPoints(); i++) {

            /* if input points are different */
            if ((lat[i] != lat[i - 1]) || (lon[i] != lon[i - 1])) {
                coord = new Coordinate(lon[i], lat[i]);
                points.add(coord);
            }
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
