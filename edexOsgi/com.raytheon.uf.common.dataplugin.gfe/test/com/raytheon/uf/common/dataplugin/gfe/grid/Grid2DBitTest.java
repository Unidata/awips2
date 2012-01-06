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
import java.nio.ByteBuffer;

import junit.framework.Assert;

import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;

public class Grid2DBitTest {

	private byte bitArray1[] = { 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 };

	private byte bitArray2[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

	private byte bitArray3[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

	private byte bitArray4[] = { 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0 };

	private byte bitArray5[] = { 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1 };

	private byte bitArray6[] = { 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1 };

	private byte bitArray8[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };

	private boolean booleanArray1[] = { true, true, true, true, true, true,
			true, true, true, true, true, true, true, true, true, true };

	@Test
	public void testGrid2DBitIntInt() {
		Grid2DBit test = new Grid2DBit(4, 4);
		Assert.assertNotNull(test);
		Assert.assertEquals(test, new Grid2DBit(4, 4, bitArray2));
	}

	@Test
	public void testGrid2DBitIntIntByteArray() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Assert.assertNotNull(test);
	}

	@Test
	public void testGrid2DBitIntIntByteBuffer() {
		Grid2DBit test = new Grid2DBit(4, 4, ByteBuffer.allocate(16));
		Assert.assertNotNull(test);
		Assert.assertEquals(test, new Grid2DBit(4, 4));
	}

	@Test
	public void testGrid2DBitIntIntBooleanArray() {
		Grid2DBit test = new Grid2DBit(4, 4, booleanArray1);
		Assert.assertNotNull(test);
		Assert.assertEquals(test, new Grid2DBit(4, 4, bitArray3));
	}

	@Test
	public void testGrid2DBitGrid2DBit() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Assert.assertEquals(test, new Grid2DBit(test));
	}

	@Test
	public void testSetIntIntByte() {
		Grid2DBit test1 = new Grid2DBit(4, 4, bitArray1);
		test1.set(0, 0, (byte) 1);
		Assert.assertEquals(test1.get(0, 0), 1);
	}

	@Test
	public void testSetAllValues() {
		Grid2DBit test1 = new Grid2DBit(4, 4, bitArray1);
		test1.setAllValues((byte) 1);
		Assert.assertEquals(test1, new Grid2DBit(4, 4, bitArray3));
	}

	@Test
	public void testCopyWithMask() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray2);
		Grid2DBit mask = new Grid2DBit(4, 4, bitArray5);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray3);
		test.copyWithMask(test2, mask);
		Assert.assertEquals(test, mask);
	}

	@Test
	public void testSetIntInt() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray2);
		test.set(0, 0);
		Assert.assertEquals(test.getAsBoolean(0, 0), true);
	}

	@Test
	public void testOrEquals() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray2);
		test.orEquals(test2);
		Grid2DBit test3 = new Grid2DBit(4, 4, bitArray1);
		Assert.assertEquals(test, test3);
	}

	@Test
	public void testNegate() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray2);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray3);
		test.negate();
		Assert.assertEquals(test, test2);
	}

	@Test
	public void testAndEquals() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray3);
		test.andEquals(test2);
		Grid2DBit test3 = new Grid2DBit(4, 4, bitArray1);
		Assert.assertEquals(test, test3);
	}

	@Test
	public void testXorEquals() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray3);
		Assert.assertEquals(test.xorEquals(test2), new Grid2DBit(4, 4,
				bitArray4));
	}

	@Test
	public void testXor() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray3);
		Assert.assertEquals(test.xor(test2), new Grid2DBit(4, 4, bitArray4));
	}

	@Test
	public void testOr() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray2);
		Assert.assertEquals(test, test.or(test2));
	}

	@Test
	public void testAnd() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray3);
		Assert.assertEquals(test, test.and(test2));
	}

	@Test
	public void testIsAnyBitsSet() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray2);
		Assert.assertTrue(test.isAnyBitsSet());
		Assert.assertFalse(test2.isAnyBitsSet());
	}

	@Test
	public void testNumberOfBitsSet() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Grid2DBit test2 = new Grid2DBit(4, 4, bitArray2);
		Assert.assertEquals(test.numberOfBitsSet(), 8);
		Assert.assertEquals(test2.numberOfBitsSet(), 0);
	}

	@Test
	public void testContiguousBitArray() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray6);
		Grid2DBit test2 = test.contiguousBitArray(new Point(3, 0));
		Assert.assertEquals(test2, new Grid2DBit(4, 4, bitArray5));
	}

	@Test
	public void testBoundedRegion() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray6);
		Grid2DBit test2 = test.boundedRegion(new Point(3, 0));
		Assert.assertEquals(test2, new Grid2DBit(4, 4, bitArray5));
	}

	@Test
	public void testExtremaOfSetBits() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray5);
		Point c1 = new Point();
		Point c2 = new Point();
		test.extremaOfSetBits(c1, c2);
		Assert.assertEquals(c1, new Point(2, 0));
		Assert.assertEquals(c2, new Point(3, 3));
	}

	@Test
	public void testClearContiguous() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray5);
		Grid2DBit.clearContiguous(test, 3, 3, 8);
		Assert.assertEquals(test, new Grid2DBit(4, 4, bitArray2));
	}

	@Test
	public void testFindNearestSet() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray8);
		Point c1 = new Point(0, 0);
		Point c2 = new Point();
		test.findNearestSet(c1, c2);
		Assert.assertEquals(c2, new Point(3, 3));
	}

	@Test
	public void testGetContiguousBitArrayLocations() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray6);
		Point coords[] = test.getContiguousBitArrayLocations();
		Point c1 = new Point(0, 0);
		Point c2 = new Point(2, 0);
		Point result[] = { c1, c2 };
		for (int i = 0; i < coords.length; i++) {
			Assert.assertEquals(coords[i], result[i]);
		}
	}

	@Test
	public void testBoundedRegionLocations() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray6);
		Point coords[] = test.boundedRegionLocations();
		Point c1 = new Point(0, 0);
		Point c2 = new Point(2, 0);
		Point result[] = { c1, c2 };
		for (int i = 0; i < coords.length; i++) {
			Assert.assertEquals(coords[i], result[i]);
		}
	}

	@Test
	public void testGetAsBoolean() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Assert.assertTrue(test.getAsBoolean(1, 0));
		Assert.assertFalse(test.getAsBoolean(0, 0));
	}

	@Test
	public void testClone() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Assert.assertEquals(test, test.clone());
	}

	@Test
	public void testSetAllOfValue() {
		Grid2DBit test1 = new Grid2DBit(4, 4, bitArray1);
		boolean pass = false;
		try {
			test1.setAllOfValue((byte) 0, (byte) 1);
		} catch (Exception e) {
			if (e instanceof UnsupportedOperationException) {
				pass = true;
			}
		}
		if (!pass) {
			Assert.fail();
		}
	}

	@Test
	public void testTranslateNegativeX() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Point point = new Point(-1, 0);
		Grid2DBit test2 = test.translate(point);
		Assert.assertEquals(test2, new Grid2DBit(4, 4, bitArray4));
	}

	@Test
	public void testTranslatePositiveX() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray3);
		Point point = new Point(2, 0);
		Grid2DBit test2 = test.translate(point);
		Assert.assertEquals(test2, new Grid2DBit(4, 4, bitArray5));
	}

	@Test
	public void testTranslateYetXandY() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray1);
		Point point = new Point(2, 3);
		Grid2DBit test2 = test.translate(point);
		Assert.assertEquals(test2, new Grid2DBit(4, 4, bitArray8));
	}

	@Test
	public void testTranslateByBadDelta() {
		Grid2DBit test = new Grid2DBit(4, 4, bitArray3);
		Point point = new Point(0, -4);
		Grid2DBit test2 = test.translate(point);
		Assert.assertEquals(test2, new Grid2DBit(4, 4, bitArray2));
	}
}
