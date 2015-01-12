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
import java.nio.FloatBuffer;

import junit.framework.Assert;

import org.junit.Test;

public class Grid2DFloatTest {

    private float bArray[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
            14, 15 };

    private float bArrayTranslated[] = { 0, 0, 0, 0, 0, 0, 1, 2, 0, 4, 5, 6, 0,
            8, 9, 10 };

    private float bArraySubGrid[] = { 5, 6, 7, 9, 10, 11, 13, 14, 15 };

    private float bArrayCopyWithMask[] = { 0, 0, 2, 3, 0, 0, 6, 7, 0, 0, 10,
            11, 0, 0, 14, 15 };

    private byte bitArray[] = { 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1 };

    private String bArrayToString = "4X4\n[\n0.0,1.0,2.0,3.0\n4.0,5.0,6.0,7.0\n8.0,9.0,10.0,11.0\n12.0,13.0,14.0,15.0\n]";

    @Test
    public void testGrid2DFloatIntInt() {
        Grid2DFloat test = new Grid2DFloat(4, 4);
        Assert.assertTrue(test instanceof Grid2DFloat);
    }

    @Test
    public void testGrid2DFloatIntIntFloatArray() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertTrue(test instanceof Grid2DFloat);
    }

    // @Test
    // public void testGrid2DFloatIntIntFloatBuffer() {
    // FloatBuffer bBuffer = FloatBuffer.allocate(16);
    // Grid2DFloat test = new Grid2DFloat(4, 4, bBuffer);
    // Assert.assertTrue(test instanceof Grid2DFloat);
    // }

    @Test
    public void testGrid2DFloatGrid2DFloat() {
        Grid2DFloat test1 = new Grid2DFloat(4, 4, bArray);
        Assert.assertEquals(test1, new Grid2DFloat(test1));
    }

    @Test
    public void testGet() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertEquals(test.get(2, 2), 10.0f);
    }

    @Test
    public void testSet() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        test.set(2, 2, 0);
        Assert.assertEquals(test.get(2, 2), 0.0f);
    }

    @Test
    public void testSetAllValues() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        test.setAllValues(0);
        Assert.assertEquals(test, new Grid2DFloat(4, 4));
    }

    @Test
    public void testIsValid() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertTrue(test.isValid());
    }

    @Test
    public void testIsValidIntInt() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertTrue(test.isValid(2, 2));
        Assert.assertFalse(test.isValid(4, 4));
    }

    @Test
    public void testClear() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        test.clear();
        Assert.assertEquals(test, new Grid2DFloat(4, 4));
    }

    @Test
    public void testTranslate() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        test = test.translate(new Point(1, 1));
        Assert.assertEquals(test, new Grid2DFloat(4, 4, bArrayTranslated));
    }

    @Test
    public void testTranslateMe() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        test.translateMe(new Point(1, 1));
        Assert.assertEquals(test, new Grid2DFloat(4, 4, bArrayTranslated));
    }

    @Test
    public void testGetBuffer() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertTrue(test.getBuffer() instanceof FloatBuffer);
    }

    @Test
    public void testGetXDim() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertEquals(test.getXdim(), 4);
    }

    @Test
    public void testGetYDim() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertEquals(test.getYdim(), 4);
    }

    @Test
    public void testSubGrid() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        test = test.subGrid(1, 1, 3, 3);
        Assert.assertEquals(test, new Grid2DFloat(3, 3, bArraySubGrid));
    }

    @Test
    public void testEqualsGrid2DFloat() throws CloneNotSupportedException {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Grid2DFloat test2 = test.clone();
        Assert.assertTrue(test.equals(test2));
        test2.set(0, 0, 1);
        Assert.assertFalse(test.equals(test2));
    }

    @Test
    public void testClone() throws CloneNotSupportedException {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Grid2DFloat test2 = test.clone();
        Assert.assertEquals(test, test2);
    }

    @Test
    public void testCopyWithMask() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Grid2DBit mask = new Grid2DBit(4, 4, bitArray);
        Grid2DFloat test2 = new Grid2DFloat(4, 4);
        test2.copyWithMask(test, mask);
        Assert.assertEquals(test2, new Grid2DFloat(4, 4, bArrayCopyWithMask));
    }

    @Test
    public void testToString() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Assert.assertEquals(test.toString(), bArrayToString);
    }

    @Test
    public void testClearIntInt() {
        Grid2DFloat test = new Grid2DFloat(4, 4, bArray);
        Grid2DFloat test2 = new Grid2DFloat(4, 4, bArray);
        test.clear(1, 0);
        test2.set(1, 0, 0f);
        Assert.assertEquals(test, test2);
    }

    private float testSetAllOfValue1[] = { 0, 1.5f, 0, 2, 0, 1.5f, 0, 2, 0,
            1.5f, 0, 2, 0, 1.5f, 0, 2 };

    private float testSetAllOfValue2[] = { 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
            0, 2, 0, 2 };

    @Test
    public void testSetAllOfValue() {
        Grid2DFloat test1 = new Grid2DFloat(4, 4, testSetAllOfValue1);
        Grid2DFloat test2 = new Grid2DFloat(4, 4, testSetAllOfValue2);
        test1.setAllOfValue((float) 1.5, 2);
        Assert.assertEquals(test1, test2);
    }

}
