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
package com.raytheon.uf.common.dataplugin.gfe.grid;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DInteger;
import com.raytheon.uf.common.dataplugin.gfe.grid.IGrid2D;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * JUnit test cases for selected methods in {@link Grid2DInteger}.
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

public class Grid2DIntegerTest {
    private Grid2DInteger gdi1 = null;  // initial grid
    private Grid2DInteger gdiC = null;  // constant valued grid for "set all"
    private Grid2DInteger gdiM = null;  // modified grid for "set all of"
    private Grid2DInteger gdiT = null;  // translated grid
    private Grid2DBit     gdbM = null;  // mask grid
    private Grid2DInteger gdiS = null;  // sub grid
    /*
     * standard initial grid
     */
    private int xDim = 4;
    private int yDim = 5;
    private int[] initial = {4, 8, 6, 1, 
                             3, 4, 6, 5, 
                             7, 6, 1, 1, 
                             2, 5, 9, 1, 
                             6, 4, 7, 9};
    /*
     * grid created by replacing each occurrence of mVal with rVal.
     */
    private int mVal = 6;
    private int rVal = 0;
    private int[] mVals = {4, 8, 0, 1, 
                           3, 4, 0, 5, 
                           7, 0, 1, 1, 
                           2, 5, 9, 1, 
                           0, 4, 7, 9};
    /*
     * constant value grid.
     */
    private int cVal = 5;
    private int[] cVals = {cVal, cVal, cVal, cVal,
                           cVal, cVal, cVal, cVal,
                           cVal, cVal, cVal, cVal,
                           cVal, cVal, cVal, cVal,
                           cVal, cVal, cVal, cVal};
    /*
     * Data for translation test. This grid should match the grid
     * formed by translating the original grid.
     */
    private int xTran = 1;
    private int yTran = 2;
    Coordinate trans = new Coordinate(xTran,yTran);
    private int[] tVals = {0, 0, 0, 0, 
                           0, 0, 0, 0, 
                           0, 4, 8, 6, 
                           0, 3, 4, 6, 
                           0, 7, 6, 1};
    /*
     * bit mask for testing CopyWithMask". Resulting grid
     * should be the same as the grid created with mVals. 
     */
    private byte[] mask = {1, 1, 0, 1, 
                           1, 1, 0, 1, 
                           1, 0, 1, 1, 
                           1, 1, 1, 1, 
                           0, 1, 1, 1};
    /*
     * subgrid for testing SubGrid.
     */
    private int subMinX = 1;
    private int subMaxX = 3;
    private int xDimSub = subMaxX - subMinX + 1;
    private int subMinY = 2;
    private int subMaxY = 4;
    private int yDimSub = subMaxY - subMinY + 1;
    private int[] sVals = {6, 1, 1, 
                           5, 9, 1, 
                           4, 7, 9};
    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        gdi1 = new Grid2DInteger(xDim,yDim,initial);
        gdiC = new Grid2DInteger(xDim,yDim,cVals);
        gdiM = new Grid2DInteger(xDim,yDim,mVals);
        gdiT = new Grid2DInteger(xDim,yDim,tVals);
        gdbM = new Grid2DBit(xDim,yDim,mask);
        gdiS = new Grid2DInteger(xDimSub,yDimSub,sVals);
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
        gdi1 = null;
        gdiC = null;
        gdiM = null;
        gdiT = null;
        gdbM = null;
        gdiS = null;
    }

    /**
     * Test method for {@link Grid2DInteger#equals(java.lang.Object)}.
     */
    @Test
    public void testEqualsObject() {
        System.out.println("Testing object equality");
        Object temp = new Object();
        
        /* run verifications */
        assertFalse("Testing object must be the same type",gdi1.equals(temp));
        assertTrue("Test equality itself",gdi1.equals(gdi1));
        assertFalse("Test must have same shape",
                    gdi1.equals(new Grid2DInteger(5,10)));
        assertFalse("Testing must have same values",
                    gdi1.equals(new Grid2DInteger(xDim,yDim,mVals)));
        assertTrue("Test same values => equality",
                   gdi1.equals(new Grid2DInteger(xDim,yDim,initial)));
    }
   /**
     * Test method for {@link Grid2DInteger#Grid2DInteger(Grid2DInteger)}.
     */
    @Test
    public void testGrid2DIntegerCopyConstructor() {
        System.out.println("Testing copy constructor");
        Grid2DInteger temp = new Grid2DInteger(gdi1);
        assertTrue("Testing copy constructor - new object has same values",temp.equals(gdi1));
        assertNotSame("Testing copy constructor - new object not same as old object", gdi1, temp);
        gdi1.set(0, 0, -1);
        assertFalse("Testing copy constructor - independent objects", temp.equals(gdi1));
    }
    /**
     * Test method for {@link Grid2DInteger#clone()}.
     */
    @Test
    public void testCloneObject() {
        System.out.println("Testing the clone() method");
        Grid2DInteger temp = gdi1.clone();
        assertNotSame("Testing that clone is different object",gdi1,temp);
        assertTrue("Testing that clone equals original",gdi1.equals(temp));
        
    }

    /**
     * Test method for {@link Grid2DInteger#setAllValues(int)}.
     */
    @Test
    public void testSetAllValues() {
        System.out.println("Testing constant initialization/replacement");
        assertFalse("Checking for different grids",gdi1.equals(gdiC));
        gdi1.setAllValues(cVal);
        assertTrue("Checking modified grid",gdi1.equals(gdiC));
    }

    /**
     * Test method for {@link Grid2DInteger#setAllOfValue(int, int)}.
     */
    @Test
    public void testSetAllOfValue() {
        System.out.println("Testing selective replacement");
        assertFalse("Checking for different grids",gdi1.equals(gdiM));
        gdi1.setAllOfValue(mVal, rVal);
        assertTrue("Checking modified grid",gdi1.equals(gdiM));
    }

    /**
     * Test method for {@link Grid2DInteger#translate(com.vividsolutions.jts.geom.Coordinate)}.
     */
    @Test
    public void testTranslate() {
        System.out.println("Testing translation constructor");
        /* save the original to verify no change */
        Grid2DInteger sve = new Grid2DInteger(gdi1);
        /* translate the original */
        Grid2DInteger temp = gdi1.translate(trans);

        /* run verifications */
        assertTrue("Checking original not changed",gdi1.equals(sve));
        assertFalse("Checking original not equal target",gdi1.equals(gdiT));
        assertTrue("Translated equals target",temp.equals(gdiT));
    }

    /**
     * Test method for {@link Grid2DInteger#translateMe(com.vividsolutions.jts.geom.Coordinate)}.
     */
    @Test
    public void testTranslateMe() {
        System.out.println("Testing translation operation");
        /* save the original to verify no change */
        Grid2DInteger sve = new Grid2DInteger(gdi1);
        /* translate the original */
        Grid2DInteger temp = gdi1.translateMe(trans);
        /* run verifications */
        assertFalse("Checking that original changed",sve.equals(gdi1));
        assertTrue("Checking original equals translated",temp.equals(gdi1));
        assertTrue("Checking translated equals target",temp.equals(gdiT));
    }

    /**
     * Test method for {@link Grid2DInteger#copyWithMask(IGrid2D, Grid2DBit)}.
     */
    @Test
    public void testCopyWithMask() {
        System.out.println("Testing copy with mask");
        /* save the original to verify no change */
        Grid2DInteger sve = new Grid2DInteger(gdi1);
        /* create an empty grid */
        Grid2DInteger temp = new Grid2DInteger(xDim,yDim);
        temp.setAllValues(0);
        
        /* copy gdi1 into temp using the mask */
        temp.copyWithMask(gdi1, gdbM);
        
        /* verify results */
        assertTrue("Checking that original grid not changed",sve.equals(gdi1));
        assertTrue("Checking that new grid matched pattern",temp.equals(gdiM));
    }

    /**
     * Test method for {@link Grid2DInteger#subGrid(int, int, int, int)}.
     */
    @Test
    public void testSubGrid() {
        System.out.println("Testing sub grid extraction");
        
        /* save the original to verify no change */
        Grid2DInteger sve = new Grid2DInteger(gdi1);

        /* extract sub grid */
        IGrid2D obj = gdi1.subGrid(subMinX, subMinY, subMaxX, subMaxY);

        /* tests the appropriate return and prevents a type case exception */
        assertTrue("Checking sub grid type",(obj instanceof Grid2DInteger));

        Grid2DInteger conv = (Grid2DInteger)obj;
        
        /* verify results */
        assertTrue("Checking that original grid not changed",sve.equals(gdi1));
        assertTrue("Checking that sub grid is correct",gdiS.equals(conv));
    }


}
