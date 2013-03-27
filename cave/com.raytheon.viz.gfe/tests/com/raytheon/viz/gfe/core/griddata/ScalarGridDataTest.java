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
package com.raytheon.viz.gfe.core.griddata;

import java.awt.Point;
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
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.core.parm.MockParm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Test for the ScalarGridData class
 * 
 * 
 * NOTE: This test is designed to be run as a regular JUnit (not plugin) unit
 * test
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 26, 2008				rbell	Initial creation
 * Mar 20, 2013    #1774    randerso    Use TimeUtil constants
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

public class ScalarGridDataTest {

    private MockParm testDP1;

    private ScalarGridSlice testSGS1;

    private ScalarWxValue testSWV1;

    private Point testC1;

    private Grid2DFloat testG2DF1;

    private Grid2DBit testG2DB2;

    private Grid2DFloat testG2DF2;

    private TimeRange testTR1;

    @Before
    public void setUp() {
        float testFA1[] = { 0.0f, 1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.6f, 7.7f,
                8.8f, 9.9f, 10.1f, 11.11f, 12.12f, 13.13f, 14.14f, 15.15f };

        this.testG2DF1 = new Grid2DFloat(4, 4, testFA1);

        testTR1 = new TimeRange(new Date(), 1000);

        GridDataHistory testGDHA1[] = new GridDataHistory[1];

        this.testC1 = new Point(1, 1);

        ParmID testPID1 = new ParmID("T", "OAX_GRID__Practice_00000000_0000",
                ParmID.defaultLevel());

        ProjectionData grid211 = new ProjectionData("Grid211",
                ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                        -133.459, 12.190), new Coordinate(-49.385, 57.290),
                new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
                new Point(93, 65), 0.0f, 0.0f, 0.0f);

        GridLocation testGL1 = new GridLocation("OAX", grid211,
                new Point(4, 4), new Coordinate(45, 30), new Coordinate(9, 9),
                "CST6CDT");

        TimeConstraints testTC1 = new TimeConstraints(
                TimeUtil.SECONDS_PER_HOUR, TimeUtil.SECONDS_PER_HOUR, 0);

        GridParmInfo testGPI1 = new GridParmInfo(testPID1, testGL1,
                GridType.SCALAR, "F", "Temperature", -20f, 80f, 2, false,
                testTC1, false);

        this.testSGS1 = new ScalarGridSlice(testTR1, testGPI1, testGDHA1,
                this.testG2DF1);

        this.testDP1 = new MockParm(testPID1, testGPI1, true, true, null);

        this.testSWV1 = new ScalarWxValue(1.5f, this.testDP1);

        byte testBA2[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };

        this.testG2DB2 = new Grid2DBit(4, 4, testBA2);

        float testFA2[] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
                0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };

        this.testG2DF2 = new Grid2DFloat(4, 4, testFA2);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.ScalarGridData#set(com.vividsolutions.jts.geom.Coordinate, float)}
     * .
     */
    @Test
    public void testSetCoordinateFloat() {
        ScalarGridData test1 = new ScalarGridData(this.testDP1, this.testSGS1);
        test1.set(this.testC1, 2.5f);
        Assert.assertEquals(2.5f, test1.getValue(1, 1), 0f);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.ScalarGridData#gridSet(com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)}
     * .
     */
    @Test
    public void testGridSet() {
        ScalarGridData test1 = new ScalarGridData(this.testDP1, this.testSGS1);
        test1.gridSet(this.testG2DF2, this.testG2DB2);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.ScalarGridData#set(com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)}
     * .
     */
    @Test
    public void testSetGrid2DFloatGrid2DBit() {
        ScalarGridData test1 = new ScalarGridData(this.testDP1, this.testSGS1);
        test1.set(this.testG2DF2, this.testG2DB2);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.ScalarGridData#pointSet(float, com.vividsolutions.jts.geom.Coordinate)}
     * .
     */
    @Test
    public void testPointSet() {
        ScalarGridData test1 = new ScalarGridData(this.testDP1, this.testSGS1);
        test1.pointSet(1.5f, this.testC1);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.ScalarGridData#clone()}.
     * 
     * @throws CloneNotSupportedException
     */
    @Test
    public void testClone() throws CloneNotSupportedException {
        ScalarGridData test1 = new ScalarGridData(this.testDP1, this.testSGS1);
        test1.clone();
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.ScalarGridData#calculatePencilInfluence()}
     * .
     */
    @Test
    public void testCalculatePencilInfluence() {
        ScalarGridData test1 = new ScalarGridData(this.testDP1, this.testSGS1);
        Point testPoint1 = new Point(0, 0);
        Point testPoint2 = new Point(1, 1);
        Point testPoint3 = new Point(2, 2);
        test1.calculatePencilInfluence(new Point[] { testPoint1, testPoint2,
                testPoint3 }, this.testSGS1.getScalarGrid());
    }

}
