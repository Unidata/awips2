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

import java.awt.Point;

import junit.framework.Assert;

import org.junit.Test;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class Grid2DTest {
    private String s1[] = { "A0", "A1", "A2", "A3", "B0", "B1", "B2", "B3",
            "C0", "C1", "C2", "C3", "D0", "D1", "D2", "D3" };

    private String s2[][] = { { "W0", "W1", "W2", "W3" },
            { "X0", "X1", "X2", "X3" }, { "Y0", "Y1", "Y2", "Y3" },
            { "Z0", "Z1", "Z2", "Z3" } };

    // copy mask
    private boolean mask[] = { false, false, false, false, false, true, true,
            false, false, true, true, false, false, false, false, false, };

    // copy with mask result
    private String s3[][] = { { "A0", "A1", "A2", "A3" },
            { "B0", "X1", "X2", "B3" }, { "C0", "Y1", "Y2", "C3" },
            { "D0", "D1", "D2", "D3" } };

    private String s4[][] = { { "B2", "B3" }, { "C2", "C3" } };

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#Grid2D(int, int)}
     * .
     */
    @Test
    public void testGrid2DIntInt() {
        Grid2D<String> test = new Grid2D<String>(4, 4);
        Assert.assertTrue(test instanceof Grid2D);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#Grid2D(int, int, java.lang.Object)}
     * .
     */
    @Test
    public void testGrid2DIntIntE() {
        Grid2D<String> test = new Grid2D<String>(4, 4, "A");
        Assert.assertTrue(test instanceof Grid2D);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#Grid2D(int, int, E[])}
     * .
     */
    @Test
    public void testGrid2DIntIntEArray() {
        Grid2D<String> test = new Grid2D<String>(4, 4, s1);
        Assert.assertTrue(test instanceof Grid2D);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#Grid2D(int, int, E[][])}
     * .
     */
    @Test
    public void testGrid2DIntIntEArrayArray() {
        Grid2D<String> test = new Grid2D<String>(4, 4, s2);
        Assert.assertTrue(test instanceof Grid2D);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#Grid2D(com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D)}
     * .
     */
    @Test
    public void testGrid2DGrid2DOfE() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4, s1);
        Assert.assertEquals(test1, new Grid2D<String>(test1));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#copyWithMask(com.raytheon.uf.common.dataplugin.gfe.grid.IGrid2D, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)}
     * .
     */
    @Test
    public void testCopyWithMask() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4, s1);
        Grid2D<String> test2 = new Grid2D<String>(4, 4, s2);
        Grid2DBit maskGrid = new Grid2DBit(4, 4, mask);
        Grid2D<String> expected = new Grid2D<String>(4, 4, s3);
        test1.copyWithMask(test2, maskGrid);
        Assert.assertEquals(expected, test1);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#get(int, int)}.
     */
    @Test
    public void testGet() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4, s1);
        int i = 0;
        for (int y = 0; y < 4; y++) {
            for (int x = 0; x < 4; x++) {
                Assert.assertEquals(s1[i++], test1.get(x, y));
            }
        }

        Grid2D<String> test2 = new Grid2D<String>(4, 4, s2);
        for (int y = 0; y < 4; y++) {
            for (int x = 0; x < 4; x++) {
                Assert.assertEquals(s2[y][x], test2.get(x, y));
            }
        }
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#set(int, int, java.lang.Object)}
     * .
     */
    @Test
    public void testSet() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        for (int y = 0; y < 4; y++) {
            for (int x = 0; x < 4; x++) {
                test1.set(x, y, s2[y][x]);
            }
        }
        Grid2D<String> expected = new Grid2D<String>(4, 4, s2);
        Assert.assertEquals(expected, test1);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#getXDim()}.
     */
    @Test
    public void testGetXDim() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Assert.assertEquals(4, test1.getXdim());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#getYDim()}.
     */
    @Test
    public void testGetYDim() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Assert.assertEquals(4, test1.getYdim());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#isValid()}.
     */
    @Test
    public void testIsValid() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Assert.assertTrue(test1.isValid());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#isValid(int, int)}
     * .
     */
    @Test
    public void testIsValidIntInt() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Assert.assertTrue(test1.isValid(2, 2));
        Assert.assertFalse(test1.isValid(4, 0));
        Assert.assertFalse(test1.isValid(0, 4));
        Assert.assertFalse(test1.isValid(-1, 0));
        Assert.assertFalse(test1.isValid(0, -1));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#subGrid(int, int, int, int)}
     * .
     */
    @Test
    public void testSubGrid() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4, s1);
        Grid2D<String> expected = new Grid2D<String>(2, 2, s4);

        Grid2D<String> result = test1.subGrid(2, 1, 3, 2);
        Assert.assertEquals(expected, result);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#clone()}.
     */
    @Test
    public void testClone() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4, s1);
        Grid2D<String> result = test1.clone();
        Assert.assertEquals(test1, result);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#getXdim()}.
     */
    @Test
    public void testGetXdim() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Assert.assertEquals(4, test1.getXdim());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#getYdim()}.
     */
    @Test
    public void testGetYdim() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Assert.assertEquals(4, test1.getYdim());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D#getGridSize()}.
     */
    @Test
    public void testGetGridSize() {
        Grid2D<String> test1 = new Grid2D<String>(4, 4);
        Point result = test1.getGridSize();
        Assert.assertEquals(new Point(4, 4), result);
    }

}
