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
package com.raytheon.viz.gfe.contours.util;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.viz.gfe.contours.util.CartCoord2D;
import com.raytheon.viz.gfe.contours.util.PolyLineFloat;

/**
 * JUnit test for selected methods in {@link PolyLineFloat}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Mar2008    968        MW Fegan    Initial implementation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class TestPolyLineFloat {
    /* poly-line objects for testing */
    private PolyLineFloat ple = null;
    private PolyLineFloat pl1 = null;
    /* test points */
    private Point[] points = {new Point(-5,7),  new Point(-2,6),
                              new Point(-3,3),  new Point(0,2),
                              new Point(2,-1),  new Point(5,1),
                              new Point(6,-4),  new Point(2,-3),
                              new Point(-2,-4), new Point(-5,-2)};
    private Point pt1 = new Point(-3, 3,2);
    private Point pt2 = new Point(-1,-1,4);
    private Point pt3 = new Point( 4,-2,4);
    /* test data as Cartesian Objects */
    private CartCoord2D ccd1 = new CartCoord2D(pt1.x, pt1.y);
    private CartCoord2D ccd2 = new CartCoord2D(pt2.x, pt2.y);
    private CartCoord2D ccd3 = new CartCoord2D(pt3.x, pt3.y);
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        ple = new PolyLineFloat();
        pl1 = new PolyLineFloat();
        for (Point pt : points) {
            CartCoord2D ccd = new CartCoord2D(pt.x,pt.y);
            pl1.add(ccd);
        }
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        pl1 = null;
    }

    /**
     * Test method for {@link PolyLineFloat#findClosest(CartCoord2D)}.
     */
    @Test
    public void testFindClosest() {
        System.out.println("Testing findClosest(...) method");
        int closest;
        closest = ple.findClosest(ccd1);
        assertEquals("Checking empty poly-line",-1,closest);
        closest = pl1.findClosest(ccd1);
        assertEquals("Checking a point on the poly-line", pt1.closest, closest);
        closest = pl1.findClosest(ccd2);
        assertEquals("Checking a point not on the poly-line",pt2.closest,closest);
        closest = pl1.findClosest(ccd3);
        assertEquals("Checking a point with multiple possible results",pt3.closest,closest);
    }
    /**
     * 
     * Helper class, contains an x/y pair.
     */
    private class Point {
        public float x;
        public float y;
        public int closest = -1;
        Point(float x, float y) {
            this.x = x;
            this.y = y;
        }
        Point(float x, float y, int closest) {
            this(x,y);
            this.closest = closest;
        }
    }
}
