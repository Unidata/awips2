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
package com.raytheon.uf.common.dataplugin.gfe.slice;

import java.awt.Point;
import java.util.Arrays;
import java.util.Date;

import junit.framework.Assert;

import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;

public class VectorGridSliceTest {

    private final float testFA1a[] = { 0.0f, 1.1f, 2.2f, 3.3f, 4.4f, 5.5f,
            6.6f, 7.7f, 8.8f, 9.9f, 10.1f, 11.11f, 12.12f, 13.13f, 14.14f,
            15.15f };

    private final float testFA1b[] = { 120, 120, 120, 120, 120, 120, 120, 120,
            120, 120, 120, 120, 120, 120, 120, 120 };

    private final Grid2DFloat testG2DF1a = new Grid2DFloat(4, 4, this.testFA1a);

    private final Grid2DFloat testG2DF1b = new Grid2DFloat(4, 4, this.testFA1b);

    private final TimeRange testTR1 = new TimeRange(new Date(), 1000);

    private final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                    -133.459, 12.190), new Coordinate(-49.385, 57.290),
            new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
            new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private final GridLocation gloc = new GridLocation("BOU", grid211,
            new Point(4, 4), new Coordinate(38, 27), new Coordinate(9, 9),
            "CST6CDT");

    private final GridParmInfo testGPI1 = new GridParmInfo(new ParmID(
            "Wind_SFC:BOU_GRID__Official_00000000_0000"), gloc,
            GridType.VECTOR, "kts", "Surface Wind", 0f, 16f, 3, false,
            new TimeConstraints(10, 20, 30), false);

    private final GridDataHistory testGDHA1[] = new GridDataHistory[1];

    private final GFERecord testGFER1 = new GFERecord();
    {
        this.testGFER1.setGridInfo(this.testGPI1);
        this.testGFER1.setGridHistory(Arrays.asList(testGDHA1[0]));
    }

    private final float testFA2a[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };

    private final float testFA2b[] = { 240, 240, 240, 240, 240, 240, 240, 240,
            240, 240, 240, 240, 240, 240, 240, 240 };

    private final Grid2DFloat testG2DF2a = new Grid2DFloat(4, 4, this.testFA2a);

    private final Grid2DFloat testG2DF2b = new Grid2DFloat(4, 4, this.testFA2b);

    private final TimeRange testTR2 = new TimeRange(new Date(), 2000);

    private final GridParmInfo testGPI2 = new GridParmInfo(new ParmID(
            "Swell_SFC:BOU_GRID__Official_00000000_0000"), gloc,
            GridType.VECTOR, "ft", "Primary Swell", 1f, 10f, 3, false,
            new TimeConstraints(10, 20, 30), false);

    private final GridDataHistory testGDHA2[] = new GridDataHistory[2];

    private final float testFA3a[] = { 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f,
            1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };

    private final float testFA3b[] = { 360, 360, 360, 360, 360, 360, 360, 360,
            360, 360, 360, 360, 360, 360, 360, 360 };

    private final Grid2DFloat testG2DF3a = new Grid2DFloat(4, 4, this.testFA3a);

    private final Grid2DFloat testG2DF3b = new Grid2DFloat(4, 4, this.testFA3b);

    private final TimeRange testTR3 = new TimeRange(new Date(), 3000);

    private final GridParmInfo testGPI3 = new GridParmInfo(new ParmID(
            "Swell2_SFC:BOU_GRID__Official_00000000_0000"), gloc,
            GridType.VECTOR, "ft", "Secondary Swell", 0f, 10f, 3, false,
            new TimeConstraints(10, 20, 30), false);

    private final GridDataHistory testGDHA3[] = new GridDataHistory[3];

    private final float testFA4a[] = { 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f,
            0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f };

    private final float testFA4b[] = { 0 };

    private final Grid2DFloat testG2DF4a = new Grid2DFloat(4, 4, this.testFA4a);

    private final Grid2DFloat testG2DF4b = new Grid2DFloat(1, 1, this.testFA4b);

    private final TimeRange testTR4 = new TimeRange(new Date(), 4000);

    private final GridParmInfo testGPI4 = new GridParmInfo(new ParmID(
            "Wind20ft_SFC:BOU_GRID__Official_00000000_0000"), gloc,
            GridType.VECTOR, "kts", "20ft. Wind", 0f, 10f, 3, false,
            new TimeConstraints(10, 20, 30), false);

    private final GridDataHistory testGDHA4[] = new GridDataHistory[4];

    private final byte testBA1[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0 };

    private final Grid2DBit testG2DB1 = new Grid2DBit(4, 4, this.testBA1);

    private final byte testBA2[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1 };

    private final Grid2DBit testG2DB2 = new Grid2DBit(4, 4, this.testBA2);

    private final byte testBA3[] = { 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1 };

    private final Grid2DBit testG2DB3 = new Grid2DBit(4, 4, this.testBA3);

    // private final byte testBA4[] = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    // 0,
    // 0, 0 };

    // private final Grid2DBit testG2DB4 = new Grid2DBit(4, 4, this.testBA4);

    private final byte testBA5[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 0 };

    private final Grid2DBit testG2DB5 = new Grid2DBit(4, 4, this.testBA5);

    @Test
    public void testEqualsObject() throws CloneNotSupportedException {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        VectorGridSlice test3 = null;
        test3 = test1.clone();
        Assert.assertTrue(test1.equals(test3));
        Assert.assertFalse(test1.equals(test2));
    }

    @Test
    public void testIsValid() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test4 = new VectorGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4a, this.testG2DF4b);
        Assert.assertNull(test1.isValid());
        Assert.assertNotNull(test4.isValid());
    }

    @Test
    public void testToString() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        Assert.assertNotNull(test1.toString());
    }

    @Test
    public void testComparisonOperateOpFloat() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        VectorGridSlice test3 = new VectorGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3a, this.testG2DF3b);

        Assert.assertEquals(this.testG2DB2, test2.comparisonOperate(Op.EQ, 0));
        Assert.assertEquals(this.testG2DB2,
                test3.comparisonOperate(Op.NOT_EQ, 0));
        Assert.assertEquals(this.testG2DB2, test3.comparisonOperate(Op.GT, 0));
        Assert.assertEquals(this.testG2DB2,
                test2.comparisonOperate(Op.GT_EQ, 0));
        Assert.assertEquals(this.testG2DB2, test2.comparisonOperate(Op.LT, 1));
        Assert.assertEquals(this.testG2DB2,
                test2.comparisonOperate(Op.LT_EQ, 0));

        Assert.assertEquals(this.testG2DB2,
                test1.comparisonOperate(Op.EQ, 1200));
        Assert.assertEquals(this.testG2DB2,
                test1.comparisonOperate(Op.NOT_EQ, 2400));
        Assert.assertEquals(this.testG2DB2,
                test1.comparisonOperate(Op.GT, 1100));
        Assert.assertEquals(this.testG2DB2,
                test2.comparisonOperate(Op.GT_EQ, 2400));
        Assert.assertEquals(this.testG2DB2,
                test2.comparisonOperate(Op.LT, 2500));
        Assert.assertEquals(this.testG2DB2,
                test3.comparisonOperate(Op.LT_EQ, 3600));
    }

    @Test
    public void testMax() {
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        VectorGridSlice test3 = new VectorGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3a, this.testG2DF3b);
        IContinuousSlice test4 = test2.max(test3);
        Assert.assertEquals(test3, test4);
    }

    @Test
    public void testMin() {
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        VectorGridSlice test3 = new VectorGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3a, this.testG2DF3b);
        IContinuousSlice test4 = test3.min(test2);
        Assert.assertEquals(test2, test4);
    }

    @Test
    public void testCheckDims() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test4 = new VectorGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4a, this.testG2DF4b);
        Assert.assertNull(test1.checkDims());
        Assert.assertNotNull(test4.checkDims());
    }

    @Test
    public void testCheckDataLimits() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        Assert.assertNull(test1.checkDataLimits());
        Assert.assertNotNull(test2.checkDataLimits());
    }

    @Test
    public void testAlmostFloatFloat() {
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        Assert.assertEquals(this.testG2DB1, test2.almost(1, .9f));
        Assert.assertEquals(this.testG2DB2, test2.almost(1, 1f));
        Assert.assertEquals(this.testG2DB1, test2.almost(2500, 0));
        Assert.assertEquals(this.testG2DB2, test2.almost(2400, 0));
    }

    @Test
    public void testVectorGridSliceTimeRangeGFERecordGrid2DFloatGrid2DFloat() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        Assert.assertNotNull(test1);
    }

    @Test
    public void testVectorGridSliceTimeRangeGridParmInfoGridDataHistoryArrayGrid2DFloatGrid2DFloat() {
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        Assert.assertNotNull(test2);
    }

    @Test
    public void testVectorGridSliceVectorGridSlice() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test2 = new VectorGridSlice(test1);
        Assert.assertEquals(test1, test2);
    }

    @Test
    public void testGetVectorMagGrid() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        Assert.assertNotNull(test1.getMagGrid());
        Assert.assertEquals(test1.getMagGrid(), test1.getScalarGrid());
    }

    @Test
    public void testGetVectorDirGrid() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        Assert.assertNotNull(test1.getDirGrid());
    }

    @Test
    public void testAssignVectorGridSliceGrid2DFloat() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);

        Assert.assertFalse(test1.equals(test2));
        test2.assign(test1);
        Assert.assertEquals(test1, test2);
    }

    @Test
    public void testAssignVectorGridSliceGrid2DBitGrid2DFloat() {
        VectorGridSlice test2 = new VectorGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2a, this.testG2DF2b);
        VectorGridSlice test3 = new VectorGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3a, this.testG2DF3b);

        test2.assign(test3, this.testG2DB3, this.testG2DF1b);
        Assert.assertEquals(this.testG2DB3,
                test2.comparisonOperate(Op.EQ, test3));
    }

    @Test
    public void testMakeGridSliceFromUVGrid2DFloatGrid2DFloatTimeRangeGFERecord() {
        Assert.assertNotNull(VectorGridSlice.makeGridSliceFromUV(
                this.testG2DF1a, this.testG2DF4a, this.testTR1, this.testGFER1));
    }

    @Test
    public void testMakeGridSliceFromUVGrid2DFloatGrid2DFloatTimeRangeGridParmInfoGridDataHistoryArray() {
        Assert.assertNotNull(VectorGridSlice.makeGridSliceFromUV(
                this.testG2DF1a, this.testG2DF4a, this.testTR1, this.testGPI1,
                this.testGDHA1));
    }

    @Test
    public void testVectorUGrid() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);

        Assert.assertNotNull(test1.vectorUGrid());
    }

    @Test
    public void testVectorVGrid() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);

        Assert.assertNotNull(test1.vectorVGrid());
    }

    @Test
    public void testVerticalMotion() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test3 = new VectorGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3a, this.testG2DF3b);

        Assert.assertNotNull(test1.verticalMotion(test3, this.testG2DB5));
    }

    @Test
    public void testFindTerrainVerticalMotion() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test3 = new VectorGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3a, this.testG2DF3b);

        test3.findTerrainVerticalMotion(1, 1, test1);
    }

    @Test
    public void testFindSlopeVector() {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);

        test1.findSlopeVector(1, 1, this.testG2DF1a);
    }

    @Test
    public void testClone() throws CloneNotSupportedException {
        VectorGridSlice test1 = new VectorGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1a, this.testG2DF1b);
        VectorGridSlice test2 = null;

        test2 = test1.clone();

        Assert.assertEquals(test1, test2);
    }

}
