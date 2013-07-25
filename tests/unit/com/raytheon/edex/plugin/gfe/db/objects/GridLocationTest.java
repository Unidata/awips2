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
package com.raytheon.edex.plugin.gfe.db.objects;

import java.awt.Point;

import junit.framework.Assert;

import org.junit.Test;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;

public class GridLocationTest {
    private static final double LATLON_TOLERANCE = 1e-3;

    private static final double GRIDCELL_TOLERANCE = 2e-3;

    private static boolean withinTolerance(double x, double expected,
            double tolerance) {
        return (Math.abs(x - expected) < tolerance);
    }

    private static final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                    -133.459, 12.190), new Coordinate(-49.385, 57.290),
            new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
            new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private static final ProjectionData grid204 = new ProjectionData("Grid204",
            ProjectionType.MERCATOR.ordinal(), new Coordinate(-250.0, -25.0),
            new Coordinate(-109.129, 60.644), new Coordinate(0.0, 0.0), 0.0f,
            0.0f, new Point(1, 1), new Point(93, 68), 0.0f, -179.564f, 0.0f);

    private static final ProjectionData grid208 = new ProjectionData("Grid208",
            ProjectionType.MERCATOR.ordinal(),
            new Coordinate(-166.219, 10.656), new Coordinate(-147.844, 27.917),
            new Coordinate(0.0, 0.0), 0.0f, 0.0f, new Point(1, 1), new Point(
                    25, 25), 0.0f, -157.082f, 0.0f);

    private static final ProjectionData grid210 = new ProjectionData("Grid210",
            ProjectionType.MERCATOR.ordinal(), new Coordinate(-77.000, 9.000),
            new Coordinate(-58.625, 26.422), new Coordinate(0.0, 0.0), 0.0f,
            0.0f, new Point(1, 1), new Point(25, 25), 0.0f, -67.812f, 0.0f);

    private static final ProjectionData grid214AK = new ProjectionData(
            "Grid214AK", ProjectionType.POLAR_STEREOGRAPHIC.ordinal(),
            new Coordinate(-178.571, 40.5301), new Coordinate(-93.689, 63.975),
            new Coordinate(0.0, 0.0), 0.0f, 0.0f, new Point(1, 1), new Point(
                    104, 70), 0.0f, 0.0f, -150.0f);

    static class TestCase {
        GridLocation gloc;

        Coordinate[][] testInOut;

        TestCase(GridLocation gloc, Coordinate[][] testInOut) {
            this.gloc = gloc;
            this.testInOut = testInOut;
        }
    }

    private static TestCase[] testCases = new TestCase[] { //
            new TestCase(
            //
                    new GridLocation("OAX", grid211, new Point(145, 145),
                            new Coordinate(45.0, 30.0), new Coordinate(9, 9),
                            "CST6CDT"),

                    new Coordinate[][] {
                            { new Coordinate(0, 144),
                                    new Coordinate(-102.2547, 38.4222) },
                            { new Coordinate(0, 0),
                                    new Coordinate(-102.7189, 44.6994) },
                            { new Coordinate(144, 0),
                                    new Coordinate(-94.0345, 44.8531) },
                            { new Coordinate(144, 144),
                                    new Coordinate(-94.0926, 38.5718) } }//
            ),

            new TestCase(
            //
                    new GridLocation("GUM", grid204, new Point(433, 225),
                            new Coordinate(15.00, 20.00), new Coordinate(27.0,
                                    14.0), "Pacific/Guam"), new Coordinate[][] {
                            { new Coordinate(0, 224),
                                    new Coordinate(131.4369, 3.25771) },
                            { new Coordinate(0, 0),
                                    new Coordinate(131.4369, 23.9652) },
                            { new Coordinate(432, 0),
                                    new Coordinate(172.7795, 23.9652) },
                            { new Coordinate(432, 224),
                                    new Coordinate(172.7795, 3.25771) }, }//

            ),

            new TestCase(
                    //
                    new GridLocation("HFO", grid208, new Point(321, 225),
                            new Coordinate(7.00, 11.00), new Coordinate(10.0,
                                    7.0), "Pacific/Honolulu"),
                    new Coordinate[][] {
                            { new Coordinate(0, 224),
                                    new Coordinate(-161.62525, 18.06678) },
                            { new Coordinate(0, 0),
                                    new Coordinate(-161.62525, 23.082) },
                            { new Coordinate(320, 0),
                                    new Coordinate(-153.969, 23.082) },
                            { new Coordinate(320, 224),
                                    new Coordinate(-153.969, 18.06678) }, }//

            ),

            new TestCase(
                    //
                    new GridLocation("SJU", grid210, new Point(32, 28),
                            new Coordinate(10.0, 10.0),
                            new Coordinate(8.0, 7.0), "America/Puerto_Rico"),
                    new Coordinate[][] {
                            { new Coordinate(0, 27),
                                    new Coordinate(-70.1094, 15.7263) },
                            { new Coordinate(0, 0),
                                    new Coordinate(-70.1094, 20.8133) },
                            { new Coordinate(31, 0),
                                    new Coordinate(-63.9844, 20.8133) },
                            { new Coordinate(31, 27),
                                    new Coordinate(-63.9844, 15.7263) } }//

            ),

            new TestCase(
                    //
                    new GridLocation("AJK", grid214AK, new Point(337, 241),
                            new Coordinate(62.0, 23.0), new Coordinate(21.0,
                                    15.0), "America/Juneau"),
                    new Coordinate[][] {
                            { new Coordinate(0, 240),
                                    new Coordinate(-145.6561, 54.7841) },
                            { new Coordinate(0, 0),
                                    new Coordinate(-144.6433, 61.1204) },
                            { new Coordinate(336, 0),
                                    new Coordinate(-127.1252, 58.9010) },
                            { new Coordinate(336, 240),
                                    new Coordinate(-131.1300, 53.0166) }, }//
            ) //
    };

    @Test
    public void testGridCoordinateToLatLon() {
        PixelOrientation orientation = PixelOrientation.CENTER;

        for (TestCase testCase : testCases) {
            System.out.println("GridLocation: " + testCase.gloc.getSiteId());
            for (Coordinate[] coords : testCase.testInOut) {
                Coordinate gridCoord = coords[0];
                Coordinate expected = coords[1];
                Coordinate latLon = MapUtil.gridCoordinateToLatLon(gridCoord,
                        orientation, testCase.gloc);
                System.out.println(gridCoord + "  " + latLon);
                Assert.assertTrue("expected: " + expected.x + ", got: "
                        + latLon.x, withinTolerance(latLon.x, expected.x,
                        LATLON_TOLERANCE));
                Assert.assertTrue("expected: " + expected.y + ", got: "
                        + latLon.y, withinTolerance(latLon.y, expected.y,
                        LATLON_TOLERANCE));
            }
        }
    }

    @Test
    public void testLatLonToGridCoordinate() {
        PixelOrientation orientation = PixelOrientation.CENTER;

        for (TestCase testCase : testCases) {
            System.out.println("GridLocation: " + testCase.gloc.getSiteId());
            for (Coordinate[] coords : testCase.testInOut) {
                Coordinate expected = coords[0];
                Coordinate latLon = coords[1];
                Coordinate gridCoord = MapUtil.latLonToGridCoordinate(latLon,
                        orientation, testCase.gloc);
                System.out.println(String.format("(%.3f,%.3f)  (%.4f,%.4f)",
                        gridCoord.x, gridCoord.y, latLon.x, latLon.y));
                Assert.assertTrue("expected: " + expected.x + ", got: "
                        + gridCoord.x, withinTolerance(gridCoord.x, expected.x,
                        GRIDCELL_TOLERANCE));
                Assert.assertTrue("expected: " + expected.y + ", got: "
                        + gridCoord.y, withinTolerance(gridCoord.y, expected.y,
                        GRIDCELL_TOLERANCE));
            }
        }
    }
}
