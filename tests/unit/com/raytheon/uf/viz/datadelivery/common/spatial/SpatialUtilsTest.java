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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Test {@link SpatialUtils}.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2012   1278     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class SpatialUtilsTest {

    @Before
    public void setUp() {

    }

    @Test
    public void testConvertToEasting() {
        SpatialUtils su = new SpatialUtils();
        double result = su.convertToEasting(-90);
        assertEquals("Converting -90", 270, result, 0);

        result = su.convertToEasting(-180);
        assertEquals("converting -180", 180, result, 0);

        result = su.convertToEasting(90);
        assertEquals("converting 90", 90, result, 0);
    }

    @Test
    public void testConvertToEastWest() {
        SpatialUtils su = new SpatialUtils();
        double result = su.convertToEastWest(270);
        assertEquals("Converting 270", -90, result, 0);

        result = su.convertToEastWest(180);
        assertEquals("converting 180", 180, result, 0);

        result = su.convertToEastWest(90);
        assertEquals("converting 90", 90, result, 0);
    }

    @Test
    public void testValidateAreasIntersect() {
        // Known geometry
        double ulx = -100;
        double lrx = -90;
        double uly = 45;
        double lry = 35;

        Coordinate ul = new Coordinate(ulx, uly);
        Coordinate lr = new Coordinate(lrx, lry);

        // Create the geometry - from Utils.getGeometry();
        GeometryFactory factory = new GeometryFactory();
        Coordinate[] coors = new Coordinate[5];

        coors[0] = ul;
        coors[1] = new Coordinate(ul.x, lr.y);
        coors[2] = lr;
        coors[3] = new Coordinate(lr.x, ul.y);
        // complete the square
        coors[4] = coors[0];

        LinearRing linearRing = factory.createLinearRing(coors);
        Polygon poly = factory.createPolygon(linearRing, null);
        Geometry geom = poly;

        // Comparable geometry
        ulx = -120;
        lrx = -80;
        uly = 40;
        lry = 30;

        Coordinate ul2 = new Coordinate(ulx, uly);
        Coordinate lr2 = new Coordinate(lrx, lry);
        Coordinate[] coords = new Coordinate[] { ul2, lr2 };

        SpatialUtils su = new SpatialUtils(ul, lr);
        boolean intersects = su.validateAreasIntersect(coords, geom);
        assertTrue("Areas should intersect", intersects);

        ulx = 120;
        lrx = 80;
        uly = 40;
        lry = 30;

        Coordinate ul3 = new Coordinate(ulx, uly);
        Coordinate lr3 = new Coordinate(lrx, lry);
        coords = new Coordinate[] { ul3, lr3 };
        intersects = su.validateAreasIntersect(coords, geom);
        assertFalse("Areas should NOT intersect", intersects);
    }

    @Test
    public void testIsValidLat() {
        Coordinate ul = new Coordinate(0, 40);
        Coordinate lr = new Coordinate(360, 10);
        SpatialUtils su = new SpatialUtils(ul, lr);
        boolean valid = su.isValidLat(35);
        assertTrue("Latitude should be valid", valid);

        valid = su.isValidLat(750);
        assertFalse("Latitude should be invalid", valid);
    }

    @Test
    public void testIsValidLon() {
        Coordinate ul = new Coordinate(0, 90);
        Coordinate lr = new Coordinate(220, 0);
        SpatialUtils su = new SpatialUtils(ul, lr);
        boolean valid = su.isValidLon(200);
        assertTrue("Longitude should be valid", valid);

        valid = su.isValidLon(270);
        assertFalse("Longitude should be invalid", valid);
    }

    @Test
    public void testAdjustCorners() {
        double urx = 20;
        double ury = 20;
        double llx = 10;
        double lly = 10;

        Coordinate ur = new Coordinate(urx, ury);
        Coordinate ll = new Coordinate(llx, lly);

        SpatialUtils su = new SpatialUtils();
        Coordinate[] adjustedCorners = su.adjustCorners(ur, ll);

        assertEquals("1. UL.X should be 10", 10, adjustedCorners[0].x, 0);
        assertEquals("1. UL.Y should be 20", 20, adjustedCorners[0].y, 0);
        assertEquals("1. LR.X should be 20", 20, adjustedCorners[1].x, 0);
        assertEquals("1. LR.Y should be 10", 10, adjustedCorners[1].y, 0);

        double lrx = 20;
        double lry = 10;
        double ulx = 10;
        double uly = 20;

        Coordinate lr = new Coordinate(lrx, lry);
        Coordinate ul = new Coordinate(ulx, uly);

        adjustedCorners = su.adjustCorners(lr, ul);
        assertEquals("2. UL.X should be 10", 10, adjustedCorners[0].x, 0);
        assertEquals("2. UL.Y should be 20", 20, adjustedCorners[0].y, 0);
        assertEquals("2. LR.X should be 20", 20, adjustedCorners[1].x, 0);
        assertEquals("2. LR.Y should be 10", 10, adjustedCorners[1].y, 0);

        // Test Flipped Corners
        lrx = 10;
        lry = 20;
        ulx = 20;
        uly = 10;

        lr = new Coordinate(lrx, lry);
        ul = new Coordinate(ulx, uly);

        adjustedCorners = su.adjustCorners(lr, ul);
        assertEquals("3. UL.X should be 10", 10, adjustedCorners[0].x, 0);
        assertEquals("3. UL.Y should be 20", 20, adjustedCorners[0].y, 0);
        assertEquals("3. LR.X should be 20", 20, adjustedCorners[1].x, 0);
        assertEquals("3. LR.Y should be 10", 10, adjustedCorners[1].y, 0);
    }

    @Test
    public void testCalcLongitudinalShift() {
        Coordinate ul = new Coordinate(74.16, 89.9095);
        Coordinate lr = new Coordinate(434.0623, -90);
        SpatialUtils su = new SpatialUtils(ul, lr);
        double shift = su.getLongitudinalShift();
        assertEquals(74.16, shift, 0);
    }
}
