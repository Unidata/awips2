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
package com.raytheon.uf.common.util;

import java.awt.Point;
import java.util.ArrayList;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.util.GridUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 21, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SliceUtilTest {
    private static final int TEST_SIZE = 10;

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.util.GridUtil#bresenham(java.awt.Point, java.awt.Point)}.
     */
    @Test
    public void testBresenhamPointPoint() {
        Point p1 = new Point(1, 1);
        Point p2 = new Point(TEST_SIZE, TEST_SIZE);
        ArrayList<Point> pts = GridUtil.bresenham(p1, p2);

        Assert.assertEquals(TEST_SIZE, pts.size());
        int i = 1;
        for (Point p : pts) {
            Assert.assertEquals(i, p.x);
            Assert.assertEquals(i, p.y);
            i++;
        }
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.util.GridUtil#bresenham(java.awt.Point, java.awt.Point, java.util.ArrayList)}.
     */
    @Test
    public void testBresenhamPointPointArrayListOfPoint() {
        Point p1 = new Point(1, 1);
        Point p2 = new Point(TEST_SIZE, TEST_SIZE * 2);
        ArrayList<Point> pts = new ArrayList<Point>();
        pts.add(p1);
        GridUtil.bresenham(p1, p2, pts);

        Assert.assertEquals(TEST_SIZE * 2, pts.size());
        int i = 1;
        for (Point p : pts) {
            Assert.assertEquals((i + 1) / 2, p.x);
            Assert.assertEquals(i, p.y);
            i++;
        }
    }

}
