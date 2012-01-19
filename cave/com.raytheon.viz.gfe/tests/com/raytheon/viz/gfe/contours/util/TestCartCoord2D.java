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

/**
 * JUnit test for selected methods in {@link CartCoord2D}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29Feb2008    968        MW Fegan    Initial Creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class TestCartCoord2D {
    /* test values for setting up the objects */
    private Point pt1= new Point((float)5.0,(float)6.0);
    private Point pt2= new Point((float)5.0,(float)7.0);
    private Point pt3= new Point((float)4.0,(float)8.0);
    
    /* scalars for testing vector ops */
    private float TWO = (float)2.0;
    private float oneDotTwo =(float)67;
    private float oneCrossTwo = (float)5;
    private float oneDistThree = (float)Math.sqrt(5);
    
    /* objects for the tests */
    private CartCoord2D ccd1;
    private CartCoord2D ccd2;
    private CartCoord2D ccd3;
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        ccd1 = new CartCoord2D(pt1.x,pt1.y);
        ccd2 = new CartCoord2D(pt2.x,pt2.y);
        ccd3 = new CartCoord2D(pt3.x,pt3.y);
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        ccd1 = null;
        ccd2 = null;
        ccd3 = null;
    }
    /**
     * Test method for {@link CartCoord2D#CartCoord2D()}.
     */
    @Test
    public void testCartCoord2D() {
        System.out.println("Testing basic object creation");
        assertEquals("Object creation - validating abscissa:",pt1.x, ccd1.x);
        assertEquals("Object creation - validating ordinate:", pt1.y, ccd1.y);
    }

    /**
     * Test method for {@link CartCoord2D#compareTo(java.lang.Object)}.
     */
    @Test
    public void testCompareTo() {
        System.out.println("Testing object comparisons");
        assertEquals("Checking compareTo (equals) ",0,ccd1.compareTo(ccd1));
        assertEquals("Checking compareTo (less than)",-1,ccd1.compareTo(ccd2));
        assertEquals("Checking compareTo (greater than)",1,ccd1.compareTo(ccd3));
    }
    /**
     * Test method for {@link CartCoord2D#scalarProduct(float)} and
     * {@link CartCoord2D#multiply(CartCoord2D, float)} methods.
     */
    @Test
    public void testScalarProduct() {
        System.out.println("Testing scalar product methods");
        /* test instance method */
        ccd1.scalarProduct(TWO);
        assertEquals("Checking instance method, abscissa", TWO * pt1.x,ccd1.x);
        assertEquals("Checking instance method, ordinate", TWO * pt1.y,ccd1.y);
        assertNotSame("Checking object changed abscissa", pt1.x, ccd1.x);
        assertNotSame("Checking object changed ordinate", pt1.y, ccd1.y);
        /* reset the objects */
        resetCCDs(true, false, false);
        /* reset the objects */
        CartCoord2D temp = CartCoord2D.multiply(ccd1, TWO);
        assertEquals("Checking static method, abscissa", TWO * pt1.x,temp.x);
        assertEquals("Checking static method, ordinate", TWO * pt1.y,temp.y);
        assertEquals("Checking object not changed, abscissa",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate",pt1.y,ccd1.y);        
    }
    /**
     * Test method for {@link CartCoord2D#dotProduct(CartCoord2D)} and
     * {@link CartCoord2D#dotProduct(CartCoord2D, CartCoord2D)} methods.
     */
    @Test
    public void testVectorDotProduct() {
        System.out.println("Testing vector dot product");
        /* test instance method */
        float dp = ccd1.dotProduct(ccd2);
        assertEquals("Checking instance method",oneDotTwo,dp);
        assertEquals("Checking object not changed, abscissa 1",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate 1",pt1.y,ccd1.y);
        assertEquals("Checking object not changed, abscissa 2",pt2.x,ccd2.x);
        assertEquals("Checking object not changed, ordinate 2",pt2.y,ccd2.y);
        /* reset the objects */
        resetCCDs(true, true, false);
        dp = CartCoord2D.dotProduct(ccd1, ccd2);
        /* test static method */
        assertEquals("Checking static method",oneDotTwo,dp);
        assertEquals("Checking object not changed, abscissa 1",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate 1",pt1.y,ccd1.y);
        assertEquals("Checking object not changed, abscissa 2",pt2.x,ccd2.x);
        assertEquals("Checking object not changed, ordinate 2",pt2.y,ccd2.y);
    }
    /**
     * Test method for {@link CartCoord2D#crossProduct(CartCoord2D)} and
     * {@link CartCoord2D#crossProduct(CartCoord2D, CartCoord2D)} methods.
     */
    @Test
    public void testVectorCrossProduct() {
        System.out.println("Testing vector cross product");
        /* test instance method */
        float cp = ccd1.crossProduct(ccd2);
        assertEquals("Checking instance method",oneCrossTwo,cp);
        assertEquals("Checking object not changed, abscissa 1",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate 1",pt1.y,ccd1.y);
        assertEquals("Checking object not changed, abscissa 2",pt2.x,ccd2.x);
        assertEquals("Checking object not changed, ordinate 2",pt2.y,ccd2.y);
        /* reset the objects */
        resetCCDs(true, true, false);
        /* test static method */
        cp = CartCoord2D.crossProduct(ccd1, ccd2);
        assertEquals("Checking static method",oneCrossTwo,cp);
        assertEquals("Checking object not changed, abscissa 1",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate 1",pt1.y,ccd1.y);
        assertEquals("Checking object not changed, abscissa 2",pt2.x,ccd2.x);
        assertEquals("Checking object not changed, ordinate 2",pt2.y,ccd2.y);        
    }
    /**
     * Test method for {@link CartCoord2D#distance(CartCoord2D)} and
     * {@link CartCoord2D#distance(CartCoord2D, CartCoord2D)} methods.
     */
    @Test
    public void testVectorDistance() {
        System.out.println("Testing distance calculations");
        /* test instance method */
        float dist = ccd1.distance(ccd3);
        assertEquals("Checking instance method",oneDistThree,dist);
        assertEquals("Checking object not changed, abscissa 1",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate 1",pt1.y,ccd1.y);
        assertEquals("Checking object not changed, abscissa 2",pt3.x,ccd3.x);
        assertEquals("Checking object not changed, ordinate 2",pt3.y,ccd3.y);
        /* reset objects */
        resetCCDs(true, false, true);
        /* test static method */ 
        dist = CartCoord2D.distance(ccd1, ccd3);
        assertEquals("Checking static method",oneDistThree,dist);
        assertEquals("Checking object not changed, abscissa 1",pt1.x,ccd1.x);
        assertEquals("Checking object not changed, ordinate 1",pt1.y,ccd1.y);
        assertEquals("Checking object not changed, abscissa 2",pt3.x,ccd3.x);
        assertEquals("Checking object not changed, ordinate 2",pt3.y,ccd3.y);
    }
    /**
     * Helper method, allows resetting of selected {@link CartCoord2D} 
     * test values to original states.
     * 
     * @param first indicates if the first test point is to be reset
     * @param second indicates if the second test point is to be reset
     * @param third indicates if the third test point is to be reset
     */
    private void resetCCDs(boolean first, boolean second, boolean third) {
        if (first) {
            ccd1 = null;
            ccd1 = new CartCoord2D(pt1.x,pt1.y);
        }
        if (second) {
            ccd2 = null;
            ccd2 = new CartCoord2D(pt2.x,pt2.y);
        }
        if (third) {
            ccd3 = null;
            ccd3 = new CartCoord2D(pt3.x,pt3.y);
        }
    }
    /**
     * 
     * Helper class, contains an x/y pair.
     */
    private class Point {
        public float x;
        public float y;
        Point(float x, float y) {
            this.x = x;
            this.y = y;
        }
    }
}
