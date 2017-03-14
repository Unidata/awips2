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
package com.raytheon.viz.volumebrowser.util;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Utilities for cross sections
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CrossSectionUtil {

    // TODO these should probably come from a config file

    private static final double LAT_WEST = -135.0;

    private static final double LAT_EAST = -60.3;

    private static final double LAT_NORTH = 50.0;

    private static final double LAT_SOUTH = 22.5;

    private static final double LAT_INCREMENT = 2.5;

    private static final double LON_WEST = -128.0;

    private static final double LON_EAST = -84.0;

    private static final double LON_NORTH = 59.8;

    private static final double LON_SOUTH = 20.6;

    private static final double LON_INCREMENT = 4.0;

    /**
     * Returns a set of points, where each set of two points forms a latitude
     * line
     * 
     * @return the latitude lines
     */
    public static List<LineString> getAllLats() {
        ArrayList<LineString> lines = new ArrayList<LineString>();

        GeometryFactory gf = new GeometryFactory();

        double currentLat = LAT_NORTH;
        while (currentLat > LAT_SOUTH) {
            Coordinate[] coords = new Coordinate[2];
            coords[0] = new Coordinate(LAT_WEST, currentLat);
            coords[1] = new Coordinate(LAT_EAST, currentLat);
            lines.add(gf.createLineString(coords));
            currentLat -= LAT_INCREMENT;
        }

        return lines;
    }

    /**
     * Returns a set of points, where each set of two points forms a longitude
     * line
     * 
     * @return the longitude lines
     */
    public static List<LineString> getAllLons() {
        ArrayList<LineString> lines = new ArrayList<LineString>();

        GeometryFactory gf = new GeometryFactory();

        double currentLon = LON_WEST;
        while (currentLon < LON_EAST) {
            Coordinate[] coords = new Coordinate[2];
            coords[0] = new Coordinate(currentLon, LON_NORTH);
            coords[1] = new Coordinate(currentLon, LON_SOUTH);
            lines.add(gf.createLineString(coords));

            currentLon += LON_INCREMENT;
        }

        return lines;
    }
}