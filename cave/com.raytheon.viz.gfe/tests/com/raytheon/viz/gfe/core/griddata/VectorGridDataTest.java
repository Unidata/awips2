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
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.core.parm.MockParm;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Vector Grid Unit Test
 * 
 * 
 * NOTE: This test is designed to be run as a regular JUnit (not plugin) unit
 * test
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 26, 2008				rbell	    Initial creation
 * Mar 20, 2013    #1774    randerso    Use TimeUtil constants
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

public class VectorGridDataTest {

    private final float testFA1[] = new float[145 * 145];
    {
        for (int i = 0; i < 145 * 145; i++) {
            this.testFA1[i] = (float) (i + (i / (Math.pow(10.0,
                    (i + "").length()))));
        }
    }

    private final Grid2DFloat testG2DF1 = new Grid2DFloat(145, 145,
            this.testFA1);

    private final float testFA1b[] = new float[145 * 145];
    {
        for (int i = 0; i < 145 * 145; i++) {
            this.testFA1b[i] = i % 365;
        }
    }

    private final Grid2DFloat testG2DF1b = new Grid2DFloat(145, 145,
            this.testFA1b);

    private final TimeRange testTR1 = new TimeRange(new Date(), 1000);

    private final GridDataHistory testGDHA1[] = new GridDataHistory[1];

    private final Point testC1 = new Point(1, 1);

    private final ParmID testPID1 = new ParmID("T",
            "OAX_GRID__Practice_00000000_0000", ParmID.defaultLevel());

    private final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                    -133.459, 12.190), new Coordinate(-49.385, 57.290),
            new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
            new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private final GridLocation testGL1 = new GridLocation("OAX", this.grid211,
            new Point(145, 145), new Coordinate(45, 30), new Coordinate(9, 9),
            "CST6CDT");

    private final TimeConstraints testTC1 = new TimeConstraints(
            TimeUtil.SECONDS_PER_HOUR, TimeUtil.SECONDS_PER_HOUR, 0);

    private final GridParmInfo testGPI1 = new GridParmInfo(this.testPID1,
            this.testGL1, GridType.VECTOR, "F", "Temperature", -20f, 80f, 2,
            false, this.testTC1, false);

    private final MockParm testDP1 = new MockParm(this.testPID1, this.testGPI1,
            true, true, null);

    private final VectorGridSlice testVGS1 = new VectorGridSlice(this.testTR1,
            this.testGPI1, this.testGDHA1, this.testG2DF1, this.testG2DF1b);

    private final VectorWxValue testVWV1 = new VectorWxValue(1.5f, 2.5f,
            this.testDP1);

    private final byte testBA2[] = new byte[145 * 145];
    {
        for (int i = 0; i < 145 * 145; i++) {
            this.testFA1[i] = 0;
        }
        this.testFA1[0] = 1;
    }

    private final Grid2DBit testG2DB2 = new Grid2DBit(145, 145, this.testBA2);

    private final float testFA2[] = new float[145 * 145];
    {
        for (int i = 0; i < 145 * 145; i++) {
            this.testFA1[i] = -1;
        }
    }

    private final Grid2DFloat testG2DF2 = new Grid2DFloat(145, 145,
            this.testFA2);

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.VectorGridData#set(java.awt.Point, com.raytheon.viz.gfe.core.wxvalue.WxValue)}
     * .
     */
    @Test
    public void testSetCoordinateWxValue() {
        VectorGridData test1 = new VectorGridData(this.testDP1, this.testVGS1);
        Assert.assertEquals(146.146f, test1.getMagValue(1, 1), 0f);
        test1.set(this.testC1, this.testVWV1);
        Assert.assertEquals(this.testVWV1.getValue(), test1.getMagValue(1, 1),
                0f);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.VectorGridData#pointSet(float, java.awt.Point)}
     * .
     */
    @Test
    public void testPointSet() {
        VectorGridData test1 = new VectorGridData(this.testDP1, this.testVGS1);
        test1.pointSet(1.5f, this.testC1);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.VectorGridData#getGridSlice()}.
     */
    @Test
    public void testGetGridSlice() {
        VectorGridData test1 = new VectorGridData(this.testDP1, this.testVGS1);
        test1.getGridSlice();
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.VectorGridData#set(com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)}
     * .
     */
    @Test
    public void testSetGrid2DFloatGrid2DFloatGrid2DBit() {
        VectorGridData test1 = new VectorGridData(this.testDP1, this.testVGS1);
        test1.set(this.testG2DF2, this.testG2DF1b, this.testG2DB2);
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.VectorGridData#clone()}.
     * 
     * @throws CloneNotSupportedException
     */
    @Test
    public void testClone() throws CloneNotSupportedException {
        VectorGridData test1 = new VectorGridData(this.testDP1, this.testVGS1);
        Assert.assertEquals(test1, test1.clone());
    }

    /**
     * Test method for
     * {@link com.raytheon.viz.gfe.core.griddata.VectorGridData#calculatePencilInfluence()}
     * .
     */
    @Test
    public void testCalculatePencilInfluence() {
        VectorGridData test1 = new VectorGridData(this.testDP1, this.testVGS1);
        Point testPoint1 = new Point(0, 0);
        Point testPoint2 = new Point(1, 1);
        Point testPoint3 = new Point(2, 2);
        test1.calculatePencilInfluence(new Point[] { testPoint1, testPoint2,
                testPoint3 }, this.testVGS1.getScalarGrid());
    }

}
