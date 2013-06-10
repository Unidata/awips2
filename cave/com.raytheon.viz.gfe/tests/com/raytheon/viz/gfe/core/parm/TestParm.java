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
package com.raytheon.viz.gfe.core.parm;

import java.awt.Point;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm.CreateFromScratchMode;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Tests for Parm
 * 
 * 
 * NOTE: This test is designed to be run as a regular JUnit (not plugin) unit
 * test
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	02/21/2008             chammack    Initial Creation
 *  03/20/2013    #1774    randerso    Use TimeUtil constants
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class TestParm {

    private static final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                    -133.459, 12.190), new Coordinate(-49.385, 57.290),
            new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
            new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private static final GridLocation gloc = new GridLocation("OAX", grid211,
            new Point(145, 145), new Coordinate(45, 30), new Coordinate(9, 9),
            "CST6CDT");

    private static final TimeConstraints TC1 = new TimeConstraints(
            TimeUtil.SECONDS_PER_HOUR, TimeUtil.SECONDS_PER_HOUR, 0);

    private static GridParmInfo gpi;

    private static ParmID pid;

    private static Date d1;

    private static Date d2;

    private static Date d3;

    private static Date d4;
    static {
        pid = new ParmID("T", "OAX_GRID__Practice_00000000_0000",
                ParmID.defaultLevel());
        gpi = new GridParmInfo(pid, gloc, GridType.SCALAR, "F", "Temperature",
                -20f, 80f, 2, false, TC1, false);
        // gpi.setId("T");
        // gpi.setMaxValue(80);
        // gpi.setMinValue(-20);
        // gpi.setPrecision(2);
        // gpi.setTimeConstraints(new TimeConstraints());
        // gpi.setUnit(NonSI.FAHRENHEIT);
        // gpi.setGridType(GridType.SCALAR);
        // gpi.setDescriptiveName("Temperature");
        // gpi.setX(128);
        // gpi.setY(128);

        Calendar c = Calendar.getInstance();
        c.set(Calendar.MINUTE, 00);
        c.set(Calendar.SECOND, 00);
        c.set(Calendar.MILLISECOND, 00);
        d1 = c.getTime();
        c.add(Calendar.MINUTE, 60);
        d2 = c.getTime();
        c.add(Calendar.MINUTE, 60);
        d3 = c.getTime();
        c.add(Calendar.MINUTE, 60);
        d4 = c.getTime();
    }

    private Parm parm;

    private final ParmID testPID1 = new ParmID("T",
            "OAX_GRID__Practice_00000000_0000", ParmID.defaultLevel());

    private final GridLocation testGL1 = new GridLocation("OAX", grid211,
            new Point(145, 145), new Coordinate(45, 30), new Coordinate(9, 9),
            "CST6CDT");

    private final TimeConstraints testTC1 = new TimeConstraints(
            TimeUtil.SECONDS_PER_HOUR, TimeUtil.SECONDS_PER_HOUR, 0);

    private final GridParmInfo testGPI1 = new GridParmInfo(this.testPID1,
            this.testGL1, GridType.SCALAR, "F", "Temperature", -20f, 80f, 2,
            false, this.testTC1, false);

    private final MockParm testDP1 = new MockParm(this.testPID1, this.testGPI1,
            true, true, null);

    private final ScalarWxValue testSWV1 = new ScalarWxValue(1.5f, this.testDP1);

    @Before
    public void setUp() {
        parm = new MockParm(pid, gpi, true, true, DataManager.getInstance(null));
    }

    /**
     * Test method for {@link com.raytheon.viz.gfe.core.parm.Parm#getParmID()}.
     */
    @Test
    public void testGetParmID() {
        ParmID pid = parm.getParmID();
        Assert.assertEquals(pid, parm.getParmID());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.parm.Parm#getInventorySpan()}.
     */
    @Test
    public void testGetInventorySpan() {

        TimeRange tr = new TimeRange(d1, d2);

        try {
            parm.insertNewGrid(new TimeRange[] { tr },
                    CreateFromScratchMode.DEFAULT);

            tr = new TimeRange(d2, d3);
            parm.insertNewGrid(new TimeRange[] { tr },
                    CreateFromScratchMode.DEFAULT);
        } catch (GFEOperationFailedException e) {
            Assert.fail(e.getMessage());
        }

        TimeRange result = parm.getInventorySpan();
        Assert.assertEquals(result, new TimeRange(d1, d3));
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.parm.Parm#getParmTimeRange()}.
     */
    @Test
    public void testGetParmTimeRange() {
        TimeRange tr = this.parm.getParmTimeRange();
        Assert.assertTrue(!tr.isValid());
        TimeRange newTr = new TimeRange(d1, d2);

        try {
            parm.insertNewGrid(new TimeRange[] { newTr },
                    CreateFromScratchMode.DEFAULT);
            tr = this.parm.getParmTimeRange();
            Assert.assertTrue(tr.isValid());
            Assert.assertEquals(tr, newTr);
            newTr = new TimeRange(d1, d3);
            parm.insertNewGrid(new TimeRange[] { newTr },
                    CreateFromScratchMode.DEFAULT);
        } catch (GFEOperationFailedException e) {
            Assert.fail(e.getMessage());
        }

    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.parm.Parm#getGridInventory(com.raytheon.uf.common.time.TimeRange)}
     * .
     */
    @Test
    public void testGetGridInventoryTimeRange() {

        TimeRange tr = new TimeRange(d1, d2);

        try {
            parm.insertNewGrid(new TimeRange[] { tr },
                    CreateFromScratchMode.DEFAULT);

            tr = new TimeRange(d2, d3);
            parm.insertNewGrid(new TimeRange[] { tr },
                    CreateFromScratchMode.DEFAULT);

            IGridData[] gd = parm.getGridInventory(tr);
            Assert.assertEquals(gd.length, 1);

            tr = new TimeRange(d1, d4);
            gd = parm.getGridInventory(tr);
            Assert.assertEquals(gd.length, 2);

            tr = new TimeRange(d3, d4);
            gd = parm.getGridInventory(tr);
            Assert.assertEquals(gd.length, 0);

        } catch (GFEOperationFailedException e) {
            Assert.fail(e.getMessage());
        }

    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.parm.Parm#insertNewGrid(com.raytheon.uf.common.time.TimeRange[], com.raytheon.viz.gfe.core.parm.Parm.CreateFromScratchMode)}
     * .
     */
    @Test
    public void testInsertNewGrid() {

        TimeRange tr = new TimeRange(d1, d2);

        IGridData[] gd = parm.getGridInventory();
        Assert.assertEquals(gd.length, 0);

        try {
            parm.insertNewGrid(new TimeRange[] { tr },
                    CreateFromScratchMode.DEFAULT);

            gd = parm.getGridInventory();
            Assert.assertEquals(gd.length, 1);

            tr = new TimeRange(d2, d3);
            parm.insertNewGrid(new TimeRange[] { tr },
                    CreateFromScratchMode.DEFAULT);

            gd = parm.getGridInventory();
            Assert.assertEquals(gd.length, 2);

        } catch (GFEOperationFailedException e) {
            Assert.fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.parm.Parm#getOverlappingGrid(com.raytheon.viz.gfe.core.griddata.IGridData[], java.util.Date)}
     * .
     */
    @Test
    public void testGetOverlappingGrid() {

        TimeRange tr = new TimeRange(d1, d3);

        IGridSlice gs = new ScalarGridSlice(tr, testGPI1,
                new GridDataHistory[0], null);
        IGridData gd = AbstractGridData.makeGridData(parm, gs);
        parm.setGrids(Arrays.asList(gd));

        IGridData result = parm.overlappingGrid(d2);
        Assert.assertEquals(result, gd);
        result = parm.overlappingGrid(d4);
        Assert.assertNull(result);
    }

    @Test
    public void testTWAvg() {

        final TimeRange tr1 = new TimeRange(d1, d2);
        final TimeRange tr2 = new TimeRange(d2, d3);
        final TimeRange tr3 = new TimeRange(d3, d4);
        IGridData original = null;
        try {
            parm.insertNewGrid(new TimeRange[] { tr1 },
                    CreateFromScratchMode.DEFAULT);
            parm.insertNewGrid(new TimeRange[] { tr2 },
                    CreateFromScratchMode.DEFAULT);
            parm.insertNewGrid(new TimeRange[] { tr3 },
                    CreateFromScratchMode.DEFAULT);

            IGridData[] gd = parm.getGridInventory();
            ((ScalarGridSlice) gd[0].getGridSlice()).getScalarGrid()
                    .setAllValues(20);
            ((ScalarGridSlice) gd[1].getGridSlice()).getScalarGrid()
                    .setAllValues(30);
            ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid()
                    .setAllValues(40);

            original = gd[0];
        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Parm newParm = parm.twavg(tr1);

        IGridData[] gd = newParm.getGridInventory();
        Assert.assertTrue(((ScalarGridSlice) gd[0].getGridSlice())
                .getScalarGrid().equals(
                        ((ScalarGridSlice) original.getGridSlice())
                                .getScalarGrid()));

        TimeRange trTest = new TimeRange(d1, d3);
        newParm = parm.twavg(trTest);
        gd = newParm.getGridInventory();
        Grid2DFloat g2dFloat = ((ScalarGridSlice) gd[0].getGridSlice())
                .getScalarGrid();

        for (int i = 0; i < g2dFloat.getXdim(); i++) {
            for (int j = 0; j < g2dFloat.getYdim(); j++) {
                float val = g2dFloat.get(i, j);
                Assert.assertEquals(25.0f, val);
            }
        }

    }

    @Test
    public void testModifyingGrid() {

        final boolean[] result = new boolean[] { false };

        final TimeRange tr1 = new TimeRange(d1, d2);
        try {
            parm.insertNewGrid(new TimeRange[] { tr1 },
                    CreateFromScratchMode.DEFAULT);
        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        parm.getListeners().addGridChangedListener(
                new IGridDataChangedListener() {

                    @Override
                    public void gridDataChanged(ParmID parm, TimeRange validTime) {
                        if (validTime.equals(tr1)) {
                            result[0] = true;
                        }
                    }
                });

        IGridData[] gd = null;
        try {
            gd = parm.startParmEdit(new Date[] { d1 });
        } catch (GFEOperationFailedException e) {
            Assert.fail(e.getMessage());
        }

        System.out.println(gd.length);

        Assert.assertEquals(gd.length, 1);
        Assert.assertTrue(gd[0] instanceof ScalarGridData);
        ScalarGridSlice gs = (ScalarGridSlice) ((ScalarGridData) gd[0])
                .getGridSlice();
        gs.getScalarGrid().setAllValues(0.0f);

        Grid2DFloat grid2DF = new Grid2DFloat(parm.getGridInfo().getGridLoc()
                .getNx(), parm.getGridInfo().getGridLoc().getNy());
        grid2DF.setAllValues(80.0f);

        Grid2DBit grid2DBit = new Grid2DBit(parm.getGridInfo().getGridLoc()
                .getNx(), parm.getGridInfo().getGridLoc().getNy());

        for (int i = 0; i < grid2DBit.getYdim(); i++) {
            for (int j = 0; j < grid2DBit.getXdim(); j++) {
                if (i == j) {
                    grid2DBit.set(j, i);
                }
            }
        }

        ((ScalarGridData) gd[0]).set(grid2DF, grid2DBit);

        Grid2DFloat resultGrid = gs.getScalarGrid();
        for (int i = 0; i < resultGrid.getYdim(); i++) {
            for (int j = 0; j < resultGrid.getXdim(); j++) {
                float value = resultGrid.get(j, i);
                if (i == j) {
                    Assert.assertEquals(80.0f, value);
                } else {
                    Assert.assertEquals(0.0f, value);
                }
            }
        }

        parm.endParmEdit();

        Assert.assertTrue(result[0]);

    }

    @Test
    public void testMin() {
        final TimeRange tr1 = new TimeRange(d1, d2);
        final TimeRange tr2 = new TimeRange(d2, d3);
        final TimeRange tr3 = new TimeRange(d3, d4);
        final TimeRange tr4 = new TimeRange(d1, d4);

        IGridData[] gd = null;

        try {
            this.parm.insertNewGrid(new TimeRange[] { tr1 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr2 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr3 },
                    CreateFromScratchMode.DEFAULT);

            gd = this.parm.getGridInventory();
            ((ScalarGridSlice) gd[0].getGridSlice()).getScalarGrid()
                    .setAllValues(20);
            ((ScalarGridSlice) gd[1].getGridSlice()).getScalarGrid()
                    .setAllValues(30);
            ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid()
                    .setAllValues(40);

        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Assert.assertEquals(((ScalarGridSlice) this.parm.min(tr4)
                .getGridInventory()[0].getGridSlice()).getScalarGrid(),
                ((ScalarGridSlice) gd[0].getGridSlice()).getScalarGrid());
    }

    @Test
    public void testMax() {
        final TimeRange tr1 = new TimeRange(d1, d2);
        final TimeRange tr2 = new TimeRange(d2, d3);
        final TimeRange tr3 = new TimeRange(d3, d4);
        final TimeRange tr4 = new TimeRange(d1, d4);

        IGridData[] gd = null;

        try {
            this.parm.insertNewGrid(new TimeRange[] { tr1 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr2 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr3 },
                    CreateFromScratchMode.DEFAULT);

            gd = this.parm.getGridInventory();
            ((ScalarGridSlice) gd[0].getGridSlice()).getScalarGrid()
                    .setAllValues(20);
            ((ScalarGridSlice) gd[1].getGridSlice()).getScalarGrid()
                    .setAllValues(30);
            ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid()
                    .setAllValues(40);

        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Assert.assertEquals(((ScalarGridSlice) this.parm.max(tr4)
                .getGridInventory()[0].getGridSlice()).getScalarGrid(),
                ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid());
    }

    @Test
    public void testSum() {
        final TimeRange tr1 = new TimeRange(d1, d2);
        final TimeRange tr2 = new TimeRange(d2, d3);
        final TimeRange tr3 = new TimeRange(d3, d4);
        final TimeRange tr4 = new TimeRange(d1, d3);

        IGridData[] gd = null;

        try {
            this.parm.insertNewGrid(new TimeRange[] { tr1 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr2 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr3 },
                    CreateFromScratchMode.DEFAULT);

            gd = this.parm.getGridInventory();
            ((ScalarGridSlice) gd[0].getGridSlice()).getScalarGrid()
                    .setAllValues(20);
            ((ScalarGridSlice) gd[1].getGridSlice()).getScalarGrid()
                    .setAllValues(30);
            ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid()
                    .setAllValues(50);

        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Assert.assertEquals(((ScalarGridSlice) this.parm.sum(tr4)
                .getGridInventory()[0].getGridSlice()).getScalarGrid(),
                ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid());
    }

    @Test
    public void testAvg() {
        final TimeRange tr1 = new TimeRange(d1, d2);
        final TimeRange tr2 = new TimeRange(d2, d3);
        final TimeRange tr3 = new TimeRange(d3, d4);
        final TimeRange tr4 = new TimeRange(d1, d4);

        IGridData[] gd = null;

        try {
            this.parm.insertNewGrid(new TimeRange[] { tr1 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr2 },
                    CreateFromScratchMode.DEFAULT);
            this.parm.insertNewGrid(new TimeRange[] { tr3 },
                    CreateFromScratchMode.DEFAULT);

            gd = this.parm.getGridInventory();
            ((ScalarGridSlice) gd[0].getGridSlice()).getScalarGrid()
                    .setAllValues(20);
            ((ScalarGridSlice) gd[1].getGridSlice()).getScalarGrid()
                    .setAllValues(30);
            ((ScalarGridSlice) gd[2].getGridSlice()).getScalarGrid()
                    .setAllValues(40);

        } catch (GFEOperationFailedException e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }

        Assert.assertEquals(((ScalarGridSlice) this.parm.avg(tr4)
                .getGridInventory()[0].getGridSlice()).getScalarGrid(),
                ((ScalarGridSlice) gd[1].getGridSlice()).getScalarGrid());
    }

    @Test
    public void testPencilStretch() {
        Coordinate testPoint1 = new Coordinate(0, 0);
        Coordinate testPoint2 = new Coordinate(1, 1);
        Coordinate testPoint3 = new Coordinate(2, 2);
        this.parm.pencilStretch(d1, this.testSWV1, new Coordinate[] {
                testPoint1, testPoint2, testPoint3 }, true);
        this.parm.pencilStretch(d1, this.testSWV1, new Coordinate[] {
                testPoint1, testPoint2, testPoint3 }, false);
    }

}
