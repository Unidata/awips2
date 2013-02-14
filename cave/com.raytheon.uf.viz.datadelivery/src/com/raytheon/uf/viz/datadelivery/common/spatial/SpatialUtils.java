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
package com.raytheon.uf.viz.datadelivery.common.spatial;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.Utils;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Data Delivery UI LatLon Projection Spatial Utilities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2012   1278     mpduff      Initial creation.
 * Dec 07, 2012 1278       bgonzale    Added initialization ctor, NaN checks
 *                                     in isValid methods, and getters for corners.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SpatialUtils {
    /**
     * A longitudinal shift amount if a global model is not from 0-360. OFS
     * models run 74-434.
     */
    private double longitudinalShift = 0;

    /**
     * Uppler Left Coordinate.
     */
    private Coordinate ul;

    /**
     * Lower Right Coordinate.
     */
    private Coordinate lr;

    /**
     * Are the coordinates East/West or Easting?
     */
    private boolean eastWestCoordinates = false;

    /**
     * Default Constructor. Should not be used with methods requiring ul or lr
     * coordinates.
     */
    public SpatialUtils() {

    }

    /**
     * Constructor.
     *
     * @param ul
     *            Uppler Left Coordinate
     * @param lr
     *            Lower Right Coordinate
     */
    public SpatialUtils(Coordinate ul, Coordinate lr) {
        this.ul = ul;
        this.lr = lr;

        determineEastWest();

        calcLongitudinalShift();
    }

    /**
     * Initialization Constructor.
     * 
     * @param dataSet
     *            DataSet that defines the spatial area.
     */
    public SpatialUtils(DataSet dataSet) {
        if (dataSet == null) {
            // default to world coverage
            this.ul = new Coordinate(0.0, 90.0);
            this.lr = new Coordinate(359.0, -90.0);
        } else {
            this.ul = dataSet.getCoverage().getUpperLeft();
            this.lr = dataSet.getCoverage().getLowerRight();
        }

        determineEastWest();

        calcLongitudinalShift();
    }

    /**
     * Convert the lat/lon value to Easting.
     *
     * @param value
     *            The lat/lon value to convert
     *
     * @return The converted value
     */
    @VisibleForTesting
    public double convertToEasting(double value) {
        if (value < 0) {
            return value + 360;
        }

        return value;
    }

    /**
     * Convert the lat/lon value to East/West.
     *
     * @param value
     *            The lat/lon value to convert
     * @return The converted value
     */
    @VisibleForTesting
    public double convertToEastWest(double value) {
        if (value > 180) {
            value = value - 360;
        }

        return value;
    }

    /**
     * Determine if this dataset should be displayed in East/West coordinates or
     * in Easting.
     */
    private void determineEastWest() {
        if (ul.x < 0 || lr.x < 0) {
            eastWestCoordinates = true;
        }
    }

    /**
     * Check if the area set by the coordinates intersects with the given
     * geometry.
     *
     * @param coords
     *            Upper left and lower right corners
     * @param geom
     *            The provided geometry
     * @return true if the area intersects the Geometry
     */
    @VisibleForTesting
    public boolean validateAreasIntersect(Coordinate[] coords, Geometry geom) {
        Geometry geom1 = Utils.getGeometry(coords[0], coords[1]);

        return geom1.intersects(geom);
    }

    /**
     * Verify the latitude value falls within the bounds.
     *
     * @param lat
     *            The latitude value
     * @return true if the latitude value falls between the bounds
     */
    @VisibleForTesting
    public boolean isValidLat(double lat) {
        if (Double.isNaN(lat)) {
            return false;
        }
        lat = Math.round(lat * 10000) / 10000.0;
        double lowerLat = Math.round(lr.y * 10000) / 10000.0;
        double upperLat = Math.round(ul.y * 10000) / 10000.0;

        return (lat >= lowerLat && lat <= upperLat);
    }

    /**
     * Verify the longitude value falls within the bounds.
     *
     * @param lon
     *            The longitude value
     * @return true if the longitude value falls between the bounds
     */
    @VisibleForTesting
    public boolean isValidLon(double lon) {
        if (Double.isNaN(lon)) {
            return false;
        }
        lon = Math.round(lon * 10000) / 10000.0;
        double x = Math.round(ul.x * 10000) / 10000.0;
        double x2 = Math.round(lr.x * 10000) / 10000.0;

        return (lon >= x && lon <= x2);
    }

    /**
     * Determine the longitudinal shift. This is only for global models
     */
    private void calcLongitudinalShift() {
        if (lr.x - ul.x > 357) {
            longitudinalShift = ul.x;
        }
    }

    /**
     * @return the longitudinalShift
     */
    public double getLongitudinalShift() {
        return longitudinalShift;
    }

    /**
     * Adjust the corner points so we have upper left and lower right
     * coordinates
     *
     * @param c1
     * @param c2
     * @return Coordinate[] of upper left and lower right coordinates
     */
    public Coordinate[] adjustCorners(Coordinate c1, Coordinate c2) {
        Coordinate ul = new Coordinate();
        Coordinate lr = new Coordinate();

        ul.x = c1.x < c2.x ? c1.x : c2.x;
        ul.y = c1.y < c2.y ? c2.y : c1.y;

        lr.x = c1.x > c2.x ? c1.x : c2.x;
        lr.y = c1.y > c2.y ? c2.y : c1.y;

        return new Coordinate[] { ul, lr };
    }

    /**
     * @return the eastWestCoordinates
     */
    public boolean isEastWestCoordinates() {
        return eastWestCoordinates;
    }

    /**
     * Constrain the coordinate's longitude (x) by the East spatial bound.
     * 
     * @param coordinate
     *            Lower right coordinate.
     */
    public void constrainByEastLongitude(Coordinate coordinate) {
        if (coordinate.x > lr.x) {
            coordinate.x = lr.x;
        }
    }

    /**
     * Get the Upper left Coordinate value.
     * 
     * @return Upper left Coordinate
     */
    public Coordinate getUpperLeft() {
        return ul;
    }

    /**
     * Get the Lower right Coordinate value.
     * 
     * @return Lower right Coordinate.
     */
    public Coordinate getLowerRight() {
        return lr;
    }
}
