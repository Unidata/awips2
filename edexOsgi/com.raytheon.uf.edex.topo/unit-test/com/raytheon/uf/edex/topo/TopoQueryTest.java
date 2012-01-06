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
package com.raytheon.uf.edex.topo;

import java.awt.Point;
import java.io.File;

import junit.framework.Assert;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.topo.GridLocation;
import com.raytheon.edex.topo.ProjectionData;
import com.raytheon.edex.topo.ProjectionData.ProjectionType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.topo.TopoQuery;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 18, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TopoQueryTest {

    TopoQuery topoQuery;

    Coordinate c1 = new Coordinate(-100, 40);

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        topoQuery = new TopoQuery(new File("/common/randerso/topo/srtm30.hdf"));
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        topoQuery = null;
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(com.vividsolutions.jts.geom.Coordinate)}
     * .
     */
    @Test
    public void testGetHeightCoordinate() {
        long t0 = System.currentTimeMillis();
        double height = topoQuery.getHeight(c1);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for a single point.");

        Assert.assertEquals(691.0, height);
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(com.vividsolutions.jts.geom.Coordinate[])}
     * .
     */
    @Test
    public void testGetHeightCoordinateArray() {
        Coordinate[] coords = new Coordinate[25];

        coords[0] = new Coordinate(c1.x - 0.1, c1.y);
        for (int i = 1; i < coords.length; i++) {
            coords[i] = new Coordinate(coords[i - 1].x + 1.0 / 120.0,
                    coords[i - 1].y);
        }

        long t0 = System.currentTimeMillis();
        double[] heights = topoQuery.getHeight(coords);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for " + heights.length
                + " points.");

        double[] expected = new double[] { 707.0, 706.0, 706.0, 717.0, 711.0,
                714.0, 719.0, 705.0, 698.0, 696.0, 695.0, 693.0, 691.0, 700.0,
                712.0, 717.0, 715.0, 719.0, 717.0, 705.0, 716.0, 726.0, 729.0,
                719.0, 708.0 };

        Assert.assertEquals(expected.length, heights.length);
        for (int i = 0; i < expected.length; i++) {
            Assert.assertEquals(expected[i], heights[i]);
        }
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(org.geotools.coverage.grid.GridGeometry2D)}
     * .
     */
    @Test
    public void testGetHeightGridGeometry2D() {
        CoordinateReferenceSystem crs = MapUtil.LATLON_PROJECTION;

        double[] input = new double[] { c1.x - 0.1, c1.y - 0.1, c1.x + 0.1,
                c1.y + 0.1 };
        double[] output = new double[input.length];
        try {
            MathTransform transform = MapUtil.getTransformFromLatLon(crs);
            transform.transform(input, 0, output, 0, input.length / 2);
        } catch (FactoryException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (TransformException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        GeneralEnvelope extent = new GeneralEnvelope(2);
        extent.setRange(0, output[0], output[2]);
        extent.setRange(1, output[1], output[3]);
        extent.setCoordinateReferenceSystem(crs);

        int gridWidth = 24;
        int gridHeight = 24;
        GridGeometry2D gridGeometry = new GridGeometry2D(
                new GeneralGridEnvelope(new int[] { 0, 0 }, new int[] {
                        gridWidth, gridHeight }, false), extent);

        long t0 = System.currentTimeMillis();
        float[] heights = topoQuery.getHeight(gridGeometry);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for " + gridWidth + "x"
                + gridHeight + " grid.");

        float[] expected = new float[] { 712.0f, 711.0f, 707.0f, 703.0f,
                705.0f, 706.0f, 703.0f, 702.0f, 710.0f, 717.0f, 706.0f, 721.0f,
                708.0f, 715.0f, 724.0f, 718.0f, 721.0f, 711.0f, 723.0f, 721.0f,
                708.0f, 708.0f, 705.0f, 710.0f, 730.0f, 726.0f, 728.0f, 717.0f,
                724.0f, 727.0f, 717.0f, 717.0f, 717.0f, 730.0f, 713.0f, 727.0f,
                720.0f, 713.0f, 728.0f, 724.0f, 733.0f, 718.0f, 729.0f, 727.0f,
                715.0f, 720.0f, 715.0f, 721.0f, 737.0f, 729.0f, 737.0f, 719.0f,
                733.0f, 735.0f, 717.0f, 727.0f, 730.0f, 729.0f, 728.0f, 723.0f,
                733.0f, 718.0f, 725.0f, 729.0f, 727.0f, 726.0f, 723.0f, 719.0f,
                726.0f, 721.0f, 717.0f, 722.0f, 743.0f, 735.0f, 740.0f, 725.0f,
                739.0f, 743.0f, 725.0f, 728.0f, 738.0f, 732.0f, 738.0f, 729.0f,
                733.0f, 726.0f, 727.0f, 729.0f, 716.0f, 717.0f, 720.0f, 710.0f,
                716.0f, 717.0f, 708.0f, 709.0f, 743.0f, 747.0f, 743.0f, 732.0f,
                742.0f, 745.0f, 732.0f, 733.0f, 742.0f, 743.0f, 738.0f, 733.0f,
                738.0f, 734.0f, 720.0f, 720.0f, 716.0f, 707.0f, 715.0f, 705.0f,
                709.0f, 710.0f, 701.0f, 698.0f, 747.0f, 756.0f, 748.0f, 741.0f,
                749.0f, 751.0f, 745.0f, 744.0f, 744.0f, 734.0f, 735.0f, 724.0f,
                729.0f, 726.0f, 725.0f, 708.0f, 714.0f, 704.0f, 710.0f, 708.0f,
                699.0f, 702.0f, 691.0f, 683.0f, 753.0f, 760.0f, 757.0f, 753.0f,
                754.0f, 747.0f, 746.0f, 750.0f, 741.0f, 731.0f, 727.0f, 723.0f,
                720.0f, 718.0f, 717.0f, 708.0f, 701.0f, 704.0f, 700.0f, 705.0f,
                693.0f, 691.0f, 675.0f, 671.0f, 746.0f, 751.0f, 752.0f, 756.0f,
                751.0f, 740.0f, 738.0f, 741.0f, 738.0f, 735.0f, 725.0f, 721.0f,
                713.0f, 709.0f, 708.0f, 704.0f, 690.0f, 693.0f, 688.0f, 700.0f,
                690.0f, 681.0f, 673.0f, 676.0f, 742.0f, 739.0f, 744.0f, 744.0f,
                744.0f, 743.0f, 730.0f, 727.0f, 732.0f, 727.0f, 729.0f, 718.0f,
                709.0f, 705.0f, 696.0f, 684.0f, 682.0f, 682.0f, 678.0f, 682.0f,
                681.0f, 674.0f, 685.0f, 693.0f, 737.0f, 729.0f, 738.0f, 732.0f,
                738.0f, 738.0f, 731.0f, 718.0f, 728.0f, 719.0f, 721.0f, 713.0f,
                712.0f, 699.0f, 698.0f, 686.0f, 685.0f, 684.0f, 685.0f, 677.0f,
                677.0f, 683.0f, 700.0f, 706.0f, 725.0f, 725.0f, 732.0f, 729.0f,
                731.0f, 729.0f, 730.0f, 717.0f, 717.0f, 716.0f, 717.0f, 709.0f,
                694.0f, 689.0f, 688.0f, 687.0f, 685.0f, 690.0f, 702.0f, 690.0f,
                704.0f, 701.0f, 708.0f, 714.0f, 712.0f, 713.0f, 721.0f, 730.0f,
                721.0f, 723.0f, 722.0f, 712.0f, 705.0f, 704.0f, 702.0f, 701.0f,
                690.0f, 691.0f, 699.0f, 695.0f, 704.0f, 708.0f, 710.0f, 699.0f,
                716.0f, 716.0f, 715.0f, 720.0f, 707.0f, 706.0f, 706.0f, 717.0f,
                711.0f, 714.0f, 719.0f, 705.0f, 698.0f, 696.0f, 695.0f, 693.0f,
                691.0f, 700.0f, 712.0f, 717.0f, 715.0f, 719.0f, 717.0f, 705.0f,
                716.0f, 726.0f, 729.0f, 719.0f, 712.0f, 707.0f, 704.0f, 709.0f,
                706.0f, 703.0f, 704.0f, 700.0f, 697.0f, 697.0f, 695.0f, 693.0f,
                695.0f, 712.0f, 709.0f, 723.0f, 730.0f, 726.0f, 717.0f, 712.0f,
                723.0f, 732.0f, 727.0f, 721.0f, 728.0f, 718.0f, 718.0f, 726.0f,
                718.0f, 707.0f, 702.0f, 704.0f, 702.0f, 714.0f, 722.0f, 712.0f,
                708.0f, 725.0f, 712.0f, 723.0f, 738.0f, 725.0f, 728.0f, 722.0f,
                722.0f, 739.0f, 735.0f, 720.0f, 737.0f, 720.0f, 729.0f, 738.0f,
                725.0f, 721.0f, 732.0f, 724.0f, 711.0f, 726.0f, 733.0f, 723.0f,
                709.0f, 726.0f, 725.0f, 721.0f, 736.0f, 736.0f, 736.0f, 730.0f,
                724.0f, 736.0f, 733.0f, 722.0f, 740.0f, 727.0f, 734.0f, 742.0f,
                735.0f, 730.0f, 737.0f, 728.0f, 715.0f, 732.0f, 743.0f, 732.0f,
                716.0f, 729.0f, 737.0f, 726.0f, 730.0f, 741.0f, 738.0f, 737.0f,
                727.0f, 737.0f, 737.0f, 726.0f, 742.0f, 733.0f, 739.0f, 751.0f,
                749.0f, 736.0f, 739.0f, 739.0f, 721.0f, 728.0f, 742.0f, 734.0f,
                727.0f, 726.0f, 740.0f, 737.0f, 729.0f, 743.0f, 747.0f, 746.0f,
                735.0f, 741.0f, 734.0f, 735.0f, 745.0f, 736.0f, 745.0f, 753.0f,
                755.0f, 742.0f, 749.0f, 741.0f, 728.0f, 728.0f, 741.0f, 745.0f,
                730.0f, 731.0f, 738.0f, 744.0f, 733.0f, 741.0f, 749.0f, 751.0f,
                739.0f, 745.0f, 739.0f, 742.0f, 751.0f, 738.0f, 752.0f, 757.0f,
                762.0f, 753.0f, 745.0f, 736.0f, 733.0f, 734.0f, 741.0f, 748.0f,
                736.0f, 741.0f, 736.0f, 749.0f, 742.0f, 743.0f, 746.0f, 753.0f,
                748.0f, 747.0f, 746.0f, 749.0f, 753.0f, 744.0f, 748.0f, 761.0f,
                768.0f, 758.0f, 745.0f, 743.0f, 742.0f, 740.0f, 744.0f, 753.0f,
                744.0f, 740.0f, 744.0f, 742.0f, 750.0f, 747.0f, 754.0f, 755.0f,
                750.0f, 753.0f, 755.0f, 749.0f, 754.0f, 748.0f, 757.0f, 755.0f,
                765.0f, 756.0f, 752.0f, 746.0f, 746.0f, 749.0f, 742.0f, 752.0f,
                753.0f, 743.0f, 751.0f, 747.0f, 753.0f, 755.0f, 756.0f, 754.0f,
                745.0f, 751.0f, 753.0f, 744.0f, 763.0f, 751.0f, 762.0f, 758.0f,
                766.0f, 762.0f, 758.0f, 748.0f, 749.0f, 755.0f, 745.0f, 755.0f,
                760.0f, 749.0f, 758.0f, 757.0f, 757.0f, 759.0f, 754.0f, 751.0f,
                742.0f, 744.0f, 749.0f, 738.0f, 769.0f, 760.0f, 761.0f, 770.0f,
                767.0f, 768.0f, 763.0f, 755.0f, 756.0f, 761.0f, 752.0f, 756.0f,
                765.0f, 757.0f, 763.0f, 759.0f, 755.0f, 751.0f, 750.0f, 743.0f,
                740.0f, 739.0f, 746.0f, 739.0f, };

        Assert.assertEquals(expected.length, heights.length);

        for (int i = 0; i < expected.length; i++) {
            Assert.assertEquals(expected[i], heights[i]);
        }
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(org.geotools.coverage.grid.GridGeometry2D)}
     * .
     */
    @Test
    public void testGetHeightGridGeometry2DGFE() {
        ProjectionData grid211 = new ProjectionData("Grid211",
                ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                        -133.459, 12.190), new Coordinate(-49.385, 57.290),
                new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
                new Point(93, 65), 0.0f, 0.0f, 0.0f);

        GridLocation gloc = new GridLocation(grid211, new Point(145, 145),
                new Coordinate(45, 30), new Coordinate(9, 9));

        GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gloc);

        long t0 = System.currentTimeMillis();
        float[] heights = topoQuery.getHeight(gridGeometry);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for " + gloc.getNx() + "x"
                + gloc.getNy() + " grid.");

        Assert.assertEquals(gloc.getNx() * gloc.getNy(), heights.length);

        for (int y = 0; y < gloc.getNy(); y++) {
            for (int x = 0; x < gloc.getNx(); x++) {
                // System.out
                // .print(Float.isNaN(heights[y * gloc.getNx() + x]) ? "N"
                // : ".");
                Assert.assertFalse("Got a NaN", Float.isNaN(heights[y
                        * gloc.getNx() + x]));
            }
            // System.out.println();
        }
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(org.geotools.coverage.grid.GridGeometry2D)}
     * .
     */
    @Test
    public void testGetHeightGridGeometry2DGrid211() {
        ProjectionData grid211 = new ProjectionData("Grid211",
                ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                        -133.459, 12.190), new Coordinate(-49.385, 57.290),
                new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
                new Point(93, 65), 0.0f, 0.0f, 0.0f);

        GridLocation gloc = new GridLocation(grid211, new Point(93, 65),
                new Coordinate(1, 1), new Coordinate(93, 65));

        GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gloc);

        long t0 = System.currentTimeMillis();
        float[] heights = topoQuery.getHeight(gridGeometry);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for " + gloc.getNx() + "x"
                + gloc.getNy() + " grid.");

        Assert.assertEquals(gloc.getNx() * gloc.getNy(), heights.length);

        for (int y = 0; y < gloc.getNy(); y++) {
            for (int x = 0; x < gloc.getNx(); x++) {
                // System.out
                // .print(Float.isNaN(heights[y * gloc.getNx() + x]) ? "N"
                // : ".");
                Assert.assertFalse("Got a NaN", Float.isNaN(heights[y
                        * gloc.getNx() + x]));
            }
            // System.out.println();
        }
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(org.geotools.coverage.grid.GridGeometry2D)}
     * .
     */
    @Test
    public void testGetHeightGridGeometry2DDateLine() {
        ProjectionData grid204 = new ProjectionData("Grid204",
                ProjectionType.MERCATOR.ordinal(), new Coordinate(110, -25),
                new Coordinate(-109.129, 60.644), new Coordinate(0.0, 0.0),
                0.0f, 0.0f, new Point(1, 1), new Point(93, 68), 0.0f,
                -179.564f, 0.0f);

        GridLocation gloc = new GridLocation(grid204, new Point(145, 145),
                new Coordinate(42, 20), new Coordinate(9, 9));

        GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gloc);

        long t0 = System.currentTimeMillis();
        float[] heights = topoQuery.getHeight(gridGeometry);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for " + gloc.getNx() + "x"
                + gloc.getNy() + " grid.");

        Assert.assertEquals(gloc.getNx() * gloc.getNy(), heights.length);

        for (int y = 0; y < gloc.getNy(); y++) {
            for (int x = 0; x < gloc.getNx(); x++) {
                // System.out
                // .print(Float.isNaN(heights[y * gloc.getNx() + x]) ? "N"
                // : ".");
                Assert.assertFalse("Got a NaN", Float.isNaN(heights[y
                        * gloc.getNx() + x]));
            }
            // System.out.println();
        }
    }

    /**
     * Test method for
     * {@link com.com.raytheon.uf.edex.topo.TopoQuery#getHeight(org.geotools.coverage.grid.GridGeometry2D)}
     * .
     */
    @Test
    public void testGetHeightGridGeometry2DNorthPole() {
        ProjectionData grid201 = new ProjectionData("Grid201",
                ProjectionType.POLAR_STEREOGRAPHIC.ordinal(), new Coordinate(
                        -150, -20), new Coordinate(30, -20), new Coordinate(
                        0.0, 0.0), 0.0f, 0.0f, new Point(1, 1), new Point(65,
                        65), 0.0f, 0.0f, -105.0f);

        GridLocation gloc = new GridLocation(grid201, new Point(145, 145),
                new Coordinate(29, 29), new Coordinate(9, 9));

        GridGeometry2D gridGeometry = MapUtil.getGridGeometry(gloc);

        long t0 = System.currentTimeMillis();
        float[] heights = topoQuery.getHeight(gridGeometry);
        long t = System.currentTimeMillis() - t0;

        System.out.println("Took " + t + " ms for " + gloc.getNx() + "x"
                + gloc.getNy() + " grid.");

        Assert.assertEquals(gloc.getNx() * gloc.getNy(), heights.length);

        for (int y = 0; y < gloc.getNy(); y++) {
            for (int x = 0; x < gloc.getNx(); x++) {
                // System.out
                // .print(Float.isNaN(heights[y * gloc.getNx() + x]) ? "N"
                // : ".");
                Assert.assertFalse("Got a NaN", Float.isNaN(heights[y
                        * gloc.getNx() + x]));
            }
            // System.out.println();
        }
    }
}
