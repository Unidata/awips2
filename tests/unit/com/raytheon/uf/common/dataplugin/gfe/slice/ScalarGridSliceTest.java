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

import org.junit.Ignore;
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

/**
 * Tests the ScalarGridSlice class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/03/2008   879        rbell       Initial Creation.
 * Jul 25, 2013 2208       njensen     Moved to tests project
 * Aug 14, 2013 1571       randerso    Changed to use ProjectionType enum
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

// TODO fix?
@Ignore
public class ScalarGridSliceTest {

    private static final int HOUR = 3600;

    private final float testFA1[] = { 0.0f, 1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f,
            7.7f, 8.8f, 9.9f, 10.1f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f };

    private final Grid2DFloat testG2DF1 = new Grid2DFloat(4, 4, this.testFA1);

    private final TimeRange testTR1 = new TimeRange(new Date(), 1000);

    private final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL, new Coordinate(-133.459, 12.190),
            new Coordinate(-49.385, 57.290), new Coordinate(-95.0, 25.0),
            25.0f, 25.0f, new Point(1, 1), new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private final GridLocation gloc = new GridLocation("BOU", grid211,
            new Point(4, 4), new Coordinate(38, 27), new Coordinate(9, 9),
            "MST7MDT");

    private final GridLocation gloc2 = new GridLocation("BOU", grid211,
            new Point(1, 1), new Coordinate(38, 27), new Coordinate(9, 9),
            "MST7MDT");

    private final TimeConstraints TC1 = new TimeConstraints(HOUR, HOUR, 0);

    private final TimeConstraints TC3 = new TimeConstraints(HOUR, HOUR, 0);

    private final TimeConstraints TC6 = new TimeConstraints(HOUR, HOUR, 0);

    private final GridParmInfo testGPI1 = new GridParmInfo(new ParmID(
            "T_SFC:BOU_GRID__Official_00000000_0000"), gloc, GridType.SCALAR,
            "F", "Surface Temperature", -80f, 120f, 0, false, TC1, false);

    private final GridDataHistory testGDHA1[] = new GridDataHistory[1];

    private final GFERecord testGFER1 = new GFERecord();
    {
        this.testGFER1.setGridInfo(this.testGPI1);
        this.testGFER1.setGridHistory(Arrays.asList(testGDHA1[0]));
    }

    private final float testFA2[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };

    private final Grid2DFloat testG2DF2 = new Grid2DFloat(4, 4, this.testFA2);

    private final TimeRange testTR2 = new TimeRange(new Date(), 2000);

    private final GridParmInfo testGPI2 = new GridParmInfo(new ParmID(
            "HeatIndex_SFC:BOU_GRID__Official_00000000_0000"), gloc2,
            GridType.SCALAR, "F", "Heat Index", 20f, 130f, 0, false, TC3, false);

    private final GridDataHistory testGDHA2[] = new GridDataHistory[2];

    private final float testFA3[] = { 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f,
            1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };

    private final Grid2DFloat testG2DF3 = new Grid2DFloat(4, 4, this.testFA3);

    private final TimeRange testTR3 = new TimeRange(new Date(), 3000);

    private final GridParmInfo testGPI3 = new GridParmInfo(new ParmID(
            "QPF_SFC:BOU_GRID__Official_00000000_0000"), gloc, GridType.SCALAR,
            "in", "QPF", 0f, 5f, 2, false, TC6, false);

    private final GridDataHistory testGDHA3[] = new GridDataHistory[3];

    private final float testFA4[] = { 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f,
            0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f };

    private final Grid2DFloat testG2DF4 = new Grid2DFloat(4, 4, this.testFA4);

    private final TimeRange testTR4 = new TimeRange(new Date(), 4000);

    private final GridParmInfo testGPI4 = new GridParmInfo(new ParmID(
            "PoP_SFC:BOU_GRID__Official_00000000_0000"), gloc, GridType.SCALAR,
            "%", "Prob of Precip", 0f, 100f, 0, false, TC6, false);

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

    private final byte testBA4[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1 };

    private final Grid2DBit testG2DB4 = new Grid2DBit(4, 4, this.testBA4);

    private final byte testBA5[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 0 };

    private final Grid2DBit testG2DB5 = new Grid2DBit(4, 4, this.testBA5);

    @Test
    public void testEqualsObject() throws CloneNotSupportedException {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR1,
                this.testGFER1, this.testG2DF1.clone());
        Assert.assertTrue(test1.equals(test2));
    }

    @Test
    public void testIsValid() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        Assert.assertNull(test1.isValid());
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        Assert.assertNotNull(test2.isValid());
    }

    @Test
    public void testToString() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        Assert.assertNotNull(test1.toString());
    }

    @Test
    public void testScalarGridSliceTimeRangeGFERecordGrid2DFloat() {
        Assert.assertNotNull(new ScalarGridSlice(this.testTR1, this.testGFER1,
                this.testG2DF1));
    }

    @Test
    public void testScalarGridSliceTimeRangeGridParmInfoGridDataHistoryArrayGrid2DFloat() {
        Assert.assertNotNull(new ScalarGridSlice(this.testTR1, this.testGPI1,
                this.testGDHA1, this.testG2DF1));
    }

    @Test
    public void testScalarGridSliceScalarGridSlice() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(test1);
        Assert.assertEquals(test1, test2);
    }

    @Test
    public void testGetScalarGrid() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        Assert.assertEquals(test1.getScalarGrid(), this.testG2DF1);
    }

    @Test
    public void testComparisonOperateOpFloat() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);

        Assert.assertEquals(this.testG2DB2, test2.comparisonOperate(Op.EQ, 0f));
        Assert.assertEquals(this.testG2DB1,
                test2.comparisonOperate(Op.NOT_EQ, 0f));
        Assert.assertEquals(this.testG2DB3, test1.comparisonOperate(Op.GT, 0f));
        Assert.assertEquals(this.testG2DB4,
                test1.comparisonOperate(Op.GT_EQ, 15.15f));
        Assert.assertEquals(this.testG2DB5,
                test1.comparisonOperate(Op.LT, 15.15f));
        Assert.assertEquals(this.testG2DB2,
                test1.comparisonOperate(Op.LT_EQ, 15.15f));
    }

    @Test
    public void testComparisonOperateOpIContinuousSlice()
            throws CloneNotSupportedException {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        Assert.assertEquals(this.testG2DB2,
                test1.comparisonOperate(Op.EQ, test1.clone()));

        Assert.assertEquals(this.testG2DB2,
                test2.comparisonOperate(Op.NOT_EQ, test3));
        Assert.assertEquals(this.testG2DB3,
                test1.comparisonOperate(Op.GT, test2));

        Assert.assertEquals(this.testG2DB2,
                test1.comparisonOperate(Op.GT_EQ, test1.clone()));

        Assert.assertEquals(this.testG2DB3,
                test2.comparisonOperate(Op.LT, test1));
        Assert.assertEquals(this.testG2DB2,
                test2.comparisonOperate(Op.LT_EQ, test1));
    }

    @Test
    public void testLimitValueFloatFloatGrid2DBit() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        test1.limitValue(0, 0, this.testG2DB4);
        Assert.assertEquals(this.testG2DB4,
                test1.comparisonOperate(Op.EQ, test2));

    }

    @Test
    public void testLimitValueFloatFloat() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        test1.limitValue(-1, -1, this.testG2DB4);
        Assert.assertEquals(this.testG2DB4, test1.comparisonOperate(Op.EQ, -1f));
    }

    @Test
    public void testMax() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        IContinuousSlice test3 = test1.max(test2);
        Assert.assertEquals(test2, test3);
    }

    @Test
    public void testMin() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        IContinuousSlice test3 = test2.min(test1);
        Assert.assertEquals(test3, test1);
    }

    @Test
    public void testOperateEqualsOpFloatGrid2DBit() {
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        ScalarGridSlice test4 = new ScalarGridSlice(test2);
        test4.operateEquals(Op.ADD, 1.0f, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test4.comparisonOperate(Op.EQ, 0f));

        ScalarGridSlice test5 = new ScalarGridSlice(test3);
        test5.operateEquals(Op.SUBTRACT, 1.0f, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test5.comparisonOperate(Op.EQ, 1f));

        ScalarGridSlice test6 = new ScalarGridSlice(test3);
        test6.operateEquals(Op.MULTIPLY, 0f, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test6.comparisonOperate(Op.EQ, 1f));

        ScalarGridSlice test7 = new ScalarGridSlice(test3);
        test7.operateEquals(Op.DIVIDE, 2.0f, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test7.comparisonOperate(Op.EQ, 1f));

        ScalarGridSlice test8 = new ScalarGridSlice(test2);
        test8.operateEquals(Op.ASSIGN, 1.0f, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test8.comparisonOperate(Op.EQ, 0f));
    }

    @Test
    public void testOperateEqualsOpFloat() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        ScalarGridSlice test4 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);

        ScalarGridSlice test5 = new ScalarGridSlice(test2);
        test5.operateEquals(Op.ADD, 1.0f);
        Assert.assertEquals(this.testG2DB2,
                test5.comparisonOperate(Op.EQ, test3));

        ScalarGridSlice test6 = new ScalarGridSlice(test3);
        test6.operateEquals(Op.SUBTRACT, 1.0f);
        Assert.assertEquals(this.testG2DB2,
                test6.comparisonOperate(Op.EQ, test2));

        ScalarGridSlice test7 = new ScalarGridSlice(test1);
        test7.operateEquals(Op.MULTIPLY, 0f);
        Assert.assertEquals(this.testG2DB2,
                test7.comparisonOperate(Op.EQ, test2));

        ScalarGridSlice test8 = new ScalarGridSlice(test3);
        test8.operateEquals(Op.DIVIDE, 2.0f);
        Assert.assertEquals(this.testG2DB2,
                test8.comparisonOperate(Op.EQ, test4));

        ScalarGridSlice test9 = new ScalarGridSlice(test1);
        test9.operateEquals(Op.ASSIGN, 1.0f);
        Assert.assertEquals(this.testG2DB2,
                test9.comparisonOperate(Op.EQ, test3));
    }

    @Test
    public void testOperateEqualsOpIContinuousSliceGrid2DBit() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        ScalarGridSlice test4 = new ScalarGridSlice(test2);
        test4.operateEquals(Op.ADD, test3, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test4.comparisonOperate(Op.EQ, 0f));

        ScalarGridSlice test5 = new ScalarGridSlice(test3);
        test5.operateEquals(Op.SUBTRACT, test3, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test5.comparisonOperate(Op.EQ, 1f));

        ScalarGridSlice test6 = new ScalarGridSlice(test3);
        test6.operateEquals(Op.MULTIPLY, test2, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test6.comparisonOperate(Op.EQ, 1f));

        ScalarGridSlice test7 = new ScalarGridSlice(test3);
        test7.operateEquals(Op.DIVIDE, test1, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test7.comparisonOperate(Op.EQ, 1f));

        ScalarGridSlice test8 = new ScalarGridSlice(test2);
        test8.operateEquals(Op.ASSIGN, test3, this.testG2DB4);
        Assert.assertEquals(this.testG2DB5, test8.comparisonOperate(Op.EQ, 0f));
    }

    @Test
    public void testOperateEqualsOpIContinuousSlice() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        ScalarGridSlice test4 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);

        ScalarGridSlice test5 = new ScalarGridSlice(test2);
        test5.operateEquals(Op.ADD, test3);
        Assert.assertEquals(this.testG2DB2,
                test5.comparisonOperate(Op.EQ, test3));

        ScalarGridSlice test6 = new ScalarGridSlice(test3);
        test6.operateEquals(Op.SUBTRACT, test3);
        Assert.assertEquals(this.testG2DB2,
                test6.comparisonOperate(Op.EQ, test2));

        ScalarGridSlice test7 = new ScalarGridSlice(test1);
        test7.operateEquals(Op.MULTIPLY, test2);
        Assert.assertEquals(this.testG2DB2,
                test7.comparisonOperate(Op.EQ, test2));

        ScalarGridSlice test8 = new ScalarGridSlice(test4);
        test8.operateEquals(Op.DIVIDE, test4);
        Assert.assertEquals(this.testG2DB2,
                test8.comparisonOperate(Op.EQ, test3));

        ScalarGridSlice test9 = new ScalarGridSlice(test2);
        test9.operateEquals(Op.ASSIGN, test1);
        Assert.assertEquals(this.testG2DB2,
                test9.comparisonOperate(Op.EQ, test1));
    }

    @Test
    public void testOperateOpFloatGrid2DBit() {
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        Assert.assertEquals(
                this.testG2DB5,
                test2.operate(Op.ADD, 1.0f, this.testG2DB4).comparisonOperate(
                        Op.EQ, 0f));
        Assert.assertEquals(this.testG2DB5,
                test3.operate(Op.SUBTRACT, 1.0f, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 1f));
        Assert.assertEquals(this.testG2DB5,
                test3.operate(Op.MULTIPLY, 0f, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 1f));
        Assert.assertEquals(this.testG2DB5,
                test3.operate(Op.DIVIDE, 2.0f, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 1f));
        Assert.assertEquals(this.testG2DB5,
                test2.operate(Op.ASSIGN, 1.0f, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 0f));
    }

    @Test
    public void testOperateOpIContinuousSliceGrid2DBit() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        Assert.assertEquals(
                this.testG2DB5,
                test2.operate(Op.ADD, test3, this.testG2DB4).comparisonOperate(
                        Op.EQ, 0f));
        Assert.assertEquals(this.testG2DB5,
                test3.operate(Op.SUBTRACT, test3, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 1f));
        Assert.assertEquals(this.testG2DB5,
                test3.operate(Op.MULTIPLY, test2, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 1f));
        Assert.assertEquals(this.testG2DB5,
                test3.operate(Op.DIVIDE, test1, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 1f));
        Assert.assertEquals(this.testG2DB5,
                test2.operate(Op.ASSIGN, test3, this.testG2DB4)
                        .comparisonOperate(Op.EQ, 0f));
    }

    @Test
    public void testOperateOpFloat() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        ScalarGridSlice test4 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);

        Assert.assertEquals(this.testG2DB2, test2.operate(Op.ADD, 1.0f)
                .comparisonOperate(Op.EQ, test3));
        Assert.assertEquals(this.testG2DB2, test3.operate(Op.SUBTRACT, 1.0f)
                .comparisonOperate(Op.EQ, test2));
        Assert.assertEquals(this.testG2DB2, test1.operate(Op.MULTIPLY, 0f)
                .comparisonOperate(Op.EQ, test2));
        Assert.assertEquals(this.testG2DB2, test3.operate(Op.DIVIDE, 2.0f)
                .comparisonOperate(Op.EQ, test4));
        Assert.assertEquals(this.testG2DB2, test1.operate(Op.ASSIGN, 1.0f)
                .comparisonOperate(Op.EQ, test3));
    }

    @Test
    public void testOperateOpIContinuousSlice() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);
        ScalarGridSlice test4 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);

        Assert.assertEquals(this.testG2DB2, test2.operate(Op.ADD, test3)
                .comparisonOperate(Op.EQ, test3));
        Assert.assertEquals(this.testG2DB2, test3.operate(Op.SUBTRACT, test3)
                .comparisonOperate(Op.EQ, test2));
        Assert.assertEquals(this.testG2DB2, test1.operate(Op.MULTIPLY, test2)
                .comparisonOperate(Op.EQ, test2));
        Assert.assertEquals(this.testG2DB2, test4.operate(Op.DIVIDE, test4)
                .comparisonOperate(Op.EQ, test3));
        Assert.assertEquals(this.testG2DB2, test2.operate(Op.ASSIGN, test1)
                .comparisonOperate(Op.EQ, test1));
    }

    @Test
    public void testSum() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR4,
                this.testGPI4, this.testGDHA4, this.testG2DF4);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        Assert.assertEquals(this.testG2DB2,
                test1.sum(test1).comparisonOperate(Op.EQ, test2));
    }

    @Test
    public void testCheckDims() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);

        Assert.assertNull(test1.checkDims());
        Assert.assertNotNull(test2.checkDims());
    }

    @Test
    public void testCheckDataLimits() {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);

        Assert.assertNull(test1.checkDataLimits());
        Assert.assertNotNull(test2.checkDataLimits());
    }

    @Test
    public void testAlmostFloatFloat() {
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);

        Assert.assertEquals(this.testG2DB2, test2.almost(1, 1));
        Assert.assertEquals(this.testG2DB1, test2.almost(1, .9999f));
    }

    @Test
    public void testAlmostScalarGridSliceFloat() {
        ScalarGridSlice test2 = new ScalarGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DF2);
        ScalarGridSlice test3 = new ScalarGridSlice(this.testTR3,
                this.testGPI3, this.testGDHA3, this.testG2DF3);

        Assert.assertEquals(this.testG2DB2, test2.almost(test3, 1));
        Assert.assertEquals(this.testG2DB1, test2.almost(test3, .9999f));
    }

    @Test
    public void testClone() throws CloneNotSupportedException {
        ScalarGridSlice test1 = new ScalarGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DF1);
        ScalarGridSlice test2 = null;

        test2 = test1.clone();

        Assert.assertEquals(test1, test2);

        test1.operateEquals(Op.ADD, 1);
        Assert.assertFalse(test1.equals(test2));
    }

}
