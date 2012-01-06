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
package com.raytheon.edex.plugin.gfe.config;

import java.awt.Point;

import junit.framework.Assert;

import org.junit.Test;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.vividsolutions.jts.geom.Coordinate;

public class ProjectionDataTest {
    private static final double TOLERANCE = 1e-3;

    private static boolean withinTolerance(double x, double expected,
            double tolerance) {
        return (Math.abs(x - expected) < tolerance);
    }

    static class TestCase {
        ProjectionData proj;

        Coordinate[][] testInOut;

        TestCase(ProjectionData proj, Coordinate[][] testInOut) {
            this.proj = proj;
            this.testInOut = testInOut;
        }
    }

    private static TestCase[] testCases = new TestCase[] { //
            new TestCase(
            //
                    new ProjectionData("Grid211",
                            ProjectionType.LAMBERT_CONFORMAL.ordinal(),
                            new Coordinate(-133.459, 12.190), new Coordinate(
                                    -49.385, 57.290), new Coordinate(-95.0,
                                    25.0), 25.0f, 25.0f, new Point(1, 1),
                            new Point(93, 65), 0.0f, 0.0f, 0.0f),

                    new Coordinate[][] {
                            { new Coordinate(1, 1),
                                    new Coordinate(-133.459, 12.190) },
                            { new Coordinate(1, 65),
                                    new Coordinate(-152.856, 54.536) },
                            { new Coordinate(93, 65),
                                    new Coordinate(-49.385, 57.290) },
                            { new Coordinate(93, 1),
                                    new Coordinate(-65.091, 14.335) } }//
            ), //
            new TestCase(
            //
                    new ProjectionData("Grid203",
                            ProjectionType.POLAR_STEREOGRAPHIC.ordinal(),
                            new Coordinate(-185.837, 19.132), new Coordinate(
                                    -53.660, 57.634), new Coordinate(0.0, 0.0),
                            0.0f, 0.0f, new Point(1, 1), new Point(45, 39),
                            0.0f, 0.0f, -150.0f),

                    new Coordinate[][] {
                            { new Coordinate(1, 1),
                                    new Coordinate(174.163, 19.132) },
                            { new Coordinate(1, 39),
                                    new Coordinate(115.601, 44.646) },
                            { new Coordinate(45, 39),
                                    new Coordinate(-53.660, 57.634) },
                            { new Coordinate(45, 1),
                                    new Coordinate(-123.434, 24.361) } }//
            ), //
            new TestCase(
            //
                    new ProjectionData("Grid204", ProjectionType.MERCATOR
                            .ordinal(), new Coordinate(-250.0, -25.0),
                            new Coordinate(-109.129, 60.644), new Coordinate(
                                    0.0, 0.0), 0.0f, 0.0f, new Point(1, 1),
                            new Point(93, 68), 0.0f, -179.564f, 0.0f),

                    new Coordinate[][] {
                            { new Coordinate(1, 1),
                                    new Coordinate(110.0, -25.0) },
                            { new Coordinate(1, 68),
                                    new Coordinate(110.0, 60.644) },
                            { new Coordinate(93, 68),
                                    new Coordinate(-109.129, 60.644) },
                            { new Coordinate(93, 1),
                                    new Coordinate(-109.129, -25.0) } }//
            ),

            new TestCase(
            //
                    new ProjectionData("Grid208", ProjectionType.MERCATOR
                            .ordinal(), new Coordinate(-167.315, 9.343),
                            new Coordinate(-145.878, 28.092), new Coordinate(
                                    0.0, 0.0), 0.0f, 0.0f, new Point(1, 1),
                            new Point(29, 27), 0.0f, -157.082f, 0.0f),

                    new Coordinate[][] {
                            { new Coordinate(1, 1),
                                    new Coordinate(-167.315, 9.343) },
                            { new Coordinate(1, 27),
                                    new Coordinate(-167.315, 28.092) },
                            { new Coordinate(29, 27),
                                    new Coordinate(-145.878, 28.092) },
                            { new Coordinate(29, 1),
                                    new Coordinate(-145.878, 9.343) } }//
            ),

            new TestCase(
            //
                    new ProjectionData("Grid210", ProjectionType.MERCATOR
                            .ordinal(), new Coordinate(-77.000, 9.000),
                            new Coordinate(-58.625, 26.422), new Coordinate(
                                    0.0, 0.0), 0.0f, 0.0f, new Point(1, 1),
                            new Point(25, 25), 0.0f, -67.812f, 0.0f),

                    new Coordinate[][] {
                            { new Coordinate(1, 1),
                                    new Coordinate(-77.000, 9.000) },
                            { new Coordinate(1, 25),
                                    new Coordinate(-77.000, 26.422) },
                            { new Coordinate(25, 25),
                                    new Coordinate(-58.625, 26.422) },
                            { new Coordinate(25, 1),
                                    new Coordinate(-58.625, 9.000) } }//
            ),

            new TestCase(
            //
                    new ProjectionData("Grid214AK",
                            ProjectionType.POLAR_STEREOGRAPHIC.ordinal(),
                            new Coordinate(-178.571, 40.5301), new Coordinate(
                                    -93.689, 63.975), new Coordinate(0.0, 0.0),
                            0.0f, 0.0f, new Point(1, 1), new Point(104, 70),
                            0.0f, 0.0f, -150.0f),

                    new Coordinate[][] {
                            { new Coordinate(1, 1),
                                    new Coordinate(-178.571, 40.5301) },
                            { new Coordinate(1, 70),
                                    new Coordinate(150.191, 61.400) },
                            { new Coordinate(104, 70),
                                    new Coordinate(-93.689, 63.975) },
                            { new Coordinate(104, 1),
                                    new Coordinate(-124.580, 41.739) } }//
            )

    };

    @Test
    public void testGridCoordinateToLatLon() {
        PixelOrientation orientation = PixelOrientation.CENTER;

        for (TestCase testCase : testCases) {
            System.out
                    .println("Projection: " + testCase.proj.getProjectionID());
            for (Coordinate[] coords : testCase.testInOut) {
                Coordinate gridCoord = coords[0];
                Coordinate expected = coords[1];
                Coordinate latLon = testCase.proj.gridCoordinateToLatLon(
                        gridCoord, orientation);
                System.out.println(gridCoord + "  " + latLon);
                Assert.assertTrue("expected: " + expected.x + ", got: "
                        + latLon.x, withinTolerance(latLon.x, expected.x,
                        TOLERANCE));
                Assert.assertTrue("expected: " + expected.y + ", got: "
                        + latLon.y, withinTolerance(latLon.y, expected.y,
                        TOLERANCE));
            }
        }
    }

    @Test
    public void testLatLonToGridCoordinate() {
        PixelOrientation orientation = PixelOrientation.CENTER;

        for (TestCase testCase : testCases) {
            System.out
                    .println("Projection: " + testCase.proj.getProjectionID());
            for (Coordinate[] coords : testCase.testInOut) {
                Coordinate expected = coords[0];
                Coordinate latLon = coords[1];
                Coordinate gridCoord = testCase.proj.latLonToGridCoordinate(
                        latLon, orientation);
                System.out.println(gridCoord + "  " + latLon);
                Assert.assertTrue("expected: " + expected.x + ", got: "
                        + gridCoord.x, withinTolerance(gridCoord.x, expected.x,
                        TOLERANCE));
                Assert.assertTrue("expected: " + expected.y + ", got: "
                        + gridCoord.y, withinTolerance(gridCoord.y, expected.y,
                        TOLERANCE));
            }
        }
    }

}
