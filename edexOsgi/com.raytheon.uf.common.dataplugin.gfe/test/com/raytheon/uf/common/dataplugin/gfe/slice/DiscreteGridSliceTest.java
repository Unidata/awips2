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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.Assert;
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
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKeyDef;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Tests the DiscreteGridSlice class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/27/2008   879        rbell       Initial Creation.
 * 01/30/2013   15719      jdynina     Allowed more than 128 chars in wx string 
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

public class DiscreteGridSliceTest {

    List<DiscreteKeyDef> dkd = new ArrayList<DiscreteKeyDef>();
    {
        dkd.add(new DiscreteKeyDef("<NoData>", "No Data"));
        dkd.add(new DiscreteKeyDef("A", "Alfa"));
        dkd.add(new DiscreteKeyDef("B", "Bravo"));
        dkd.add(new DiscreteKeyDef("C", "Charlie"));
        dkd.add(new DiscreteKeyDef("D", "Delta"));
        dkd.add(new DiscreteKeyDef("E", "Echo"));
        dkd.add(new DiscreteKeyDef("F", "Foxtrot"));
        dkd.add(new DiscreteKeyDef("G", "Golf"));
        dkd.add(new DiscreteKeyDef("H", "Hotel"));
        dkd.add(new DiscreteKeyDef("I", "India"));
        dkd.add(new DiscreteKeyDef("J", "Juliet"));
        dkd.add(new DiscreteKeyDef("K", "Kilo"));
        dkd.add(new DiscreteKeyDef("L", "Lima"));
        dkd.add(new DiscreteKeyDef("M", "Mike"));
        dkd.add(new DiscreteKeyDef("N", "November"));
        dkd.add(new DiscreteKeyDef("O", "Oscar"));
        dkd.add(new DiscreteKeyDef("P", "Papa"));
        dkd.add(new DiscreteKeyDef("Q", "Quebec"));
        dkd.add(new DiscreteKeyDef("R", "Romeo"));
        dkd.add(new DiscreteKeyDef("S", "Sierra"));
        dkd.add(new DiscreteKeyDef("T", "Tango"));
        dkd.add(new DiscreteKeyDef("U", "Uniform"));
        dkd.add(new DiscreteKeyDef("V", "Victor"));
        dkd.add(new DiscreteKeyDef("W", "Whiskey"));
        dkd.add(new DiscreteKeyDef("X", "Xray"));
        dkd.add(new DiscreteKeyDef("Y", "Yankee"));
        dkd.add(new DiscreteKeyDef("Z", "Zulu"));
        dkd.add(new DiscreteKeyDef("AA", "Alfa Alfa"));
        dkd.add(new DiscreteKeyDef("AB", "Alfa Bravo"));
        dkd.add(new DiscreteKeyDef("AC", "Alfa Charlie"));
        dkd.add(new DiscreteKeyDef("AD", "Alfa Delta"));
        dkd.add(new DiscreteKeyDef("AE", "Alfa Echo"));
        dkd.add(new DiscreteKeyDef("AF", "Alfa Foxtrot"));
        dkd.add(new DiscreteKeyDef("AG", "Alfa Golf"));
        dkd.add(new DiscreteKeyDef("AH", "Alfa Hotel"));
        dkd.add(new DiscreteKeyDef("AI", "Alfa India"));
        dkd.add(new DiscreteKeyDef("AJ", "Alfa Juliet"));
        dkd.add(new DiscreteKeyDef("AK", "Alfa Kilo"));
        dkd.add(new DiscreteKeyDef("AL", "Alfa Lima"));
        dkd.add(new DiscreteKeyDef("AM", "Alfa Mike"));
        dkd.add(new DiscreteKeyDef("AN", "Alfa November"));
        dkd.add(new DiscreteKeyDef("AO", "Alfa Oscar"));
        dkd.add(new DiscreteKeyDef("AP", "Alfa Papa"));
        dkd.add(new DiscreteKeyDef("AQ", "Alfa Quebec"));
        dkd.add(new DiscreteKeyDef("AR", "Alfa Romeo"));
        dkd.add(new DiscreteKeyDef("AS", "Alfa Sierra"));
        dkd.add(new DiscreteKeyDef("AT", "Alfa Tango"));
        dkd.add(new DiscreteKeyDef("AU", "Alfa Uniform"));
        dkd.add(new DiscreteKeyDef("AV", "Alfa Victor"));
        dkd.add(new DiscreteKeyDef("AW", "Alfa Whiskey"));
        dkd.add(new DiscreteKeyDef("AX", "Alfa Xray"));
        dkd.add(new DiscreteKeyDef("AY", "Alfa Yankee"));
        dkd.add(new DiscreteKeyDef("AZ", "Alfa Zulu"));
        dkd.add(new DiscreteKeyDef("BA", "Bravo Alfa"));
        dkd.add(new DiscreteKeyDef("BB", "Bravo Bravo"));
        dkd.add(new DiscreteKeyDef("BC", "Bravo Charlie"));
        dkd.add(new DiscreteKeyDef("BD", "Bravo Delta"));
        dkd.add(new DiscreteKeyDef("BE", "Bravo Echo"));
        dkd.add(new DiscreteKeyDef("BF", "Bravo Foxtrot"));
        dkd.add(new DiscreteKeyDef("BG", "Bravo Golf"));
        dkd.add(new DiscreteKeyDef("BH", "Bravo Hotel"));
        dkd.add(new DiscreteKeyDef("BI", "Bravo India"));
        dkd.add(new DiscreteKeyDef("BJ", "Bravo Juliet"));
        dkd.add(new DiscreteKeyDef("BK", "Bravo Kilo"));
        dkd.add(new DiscreteKeyDef("BL", "Bravo Lima"));
        dkd.add(new DiscreteKeyDef("BM", "Bravo Mike"));
        dkd.add(new DiscreteKeyDef("BN", "Bravo November"));
        dkd.add(new DiscreteKeyDef("BO", "Bravo Oscar"));
        dkd.add(new DiscreteKeyDef("BP", "Bravo Papa"));
        dkd.add(new DiscreteKeyDef("BQ", "Bravo Quebec"));
        dkd.add(new DiscreteKeyDef("BR", "Bravo Romeo"));
        dkd.add(new DiscreteKeyDef("BS", "Bravo Sierra"));
        dkd.add(new DiscreteKeyDef("BT", "Bravo Tango"));
        dkd.add(new DiscreteKeyDef("BU", "Bravo Uniform"));
        dkd.add(new DiscreteKeyDef("BV", "Bravo Victor"));
        dkd.add(new DiscreteKeyDef("BW", "Bravo Whiskey"));
        dkd.add(new DiscreteKeyDef("BX", "Bravo Xray"));
        dkd.add(new DiscreteKeyDef("BY", "Bravo Yankee"));
        dkd.add(new DiscreteKeyDef("BZ", "Bravo Zulu"));
        dkd.add(new DiscreteKeyDef("CA", "Charlie Alfa"));
        dkd.add(new DiscreteKeyDef("CB", "Charlie Bravo"));
        dkd.add(new DiscreteKeyDef("CC", "Charlie Charlie"));
        dkd.add(new DiscreteKeyDef("CD", "Charlie Delta"));
        dkd.add(new DiscreteKeyDef("CE", "Charlie Echo"));
        dkd.add(new DiscreteKeyDef("CF", "Charlie Foxtrot"));
        dkd.add(new DiscreteKeyDef("CG", "Charlie Golf"));
        dkd.add(new DiscreteKeyDef("CH", "Charlie Hotel"));
        dkd.add(new DiscreteKeyDef("CI", "Charlie India"));
        dkd.add(new DiscreteKeyDef("CJ", "Charlie Juliet"));
        dkd.add(new DiscreteKeyDef("CK", "Charlie Kilo"));
        dkd.add(new DiscreteKeyDef("CL", "Charlie Lima"));
        dkd.add(new DiscreteKeyDef("CM", "Charlie Mike"));
        dkd.add(new DiscreteKeyDef("CN", "Charlie November"));
        dkd.add(new DiscreteKeyDef("CO", "Charlie Oscar"));
        dkd.add(new DiscreteKeyDef("CP", "Charlie Papa"));
        dkd.add(new DiscreteKeyDef("CQ", "Charlie Quebec"));
        dkd.add(new DiscreteKeyDef("CR", "Charlie Romeo"));
        dkd.add(new DiscreteKeyDef("CS", "Charlie Sierra"));
        dkd.add(new DiscreteKeyDef("CT", "Charlie Tango"));
        dkd.add(new DiscreteKeyDef("CU", "Charlie Uniform"));
        dkd.add(new DiscreteKeyDef("CV", "Charlie Victor"));
        dkd.add(new DiscreteKeyDef("CW", "Charlie Whiskey"));
        dkd.add(new DiscreteKeyDef("CX", "Charlie Xray"));
        dkd.add(new DiscreteKeyDef("CY", "Charlie Yankee"));
        dkd.add(new DiscreteKeyDef("CZ", "Charlie Zulu"));
        dkd.add(new DiscreteKeyDef("DA", "Delta Alfa"));
        dkd.add(new DiscreteKeyDef("DB", "Delta Bravo"));
        dkd.add(new DiscreteKeyDef("DC", "Delta Charlie"));
        dkd.add(new DiscreteKeyDef("DD", "Delta Delta"));
        dkd.add(new DiscreteKeyDef("DE", "Delta Echo"));
        dkd.add(new DiscreteKeyDef("DF", "Delta Foxtrot"));
        dkd.add(new DiscreteKeyDef("DG", "Delta Golf"));
        dkd.add(new DiscreteKeyDef("DH", "Delta Hotel"));
        dkd.add(new DiscreteKeyDef("DI", "Delta India"));
        dkd.add(new DiscreteKeyDef("DJ", "Delta Juliet"));
        dkd.add(new DiscreteKeyDef("DK", "Delta Kilo"));
        dkd.add(new DiscreteKeyDef("DL", "Delta Lima"));
        dkd.add(new DiscreteKeyDef("DM", "Delta Mike"));
        dkd.add(new DiscreteKeyDef("DN", "Delta November"));
        dkd.add(new DiscreteKeyDef("DO", "Delta Oscar"));
        dkd.add(new DiscreteKeyDef("DP", "Delta Papa"));
        dkd.add(new DiscreteKeyDef("DQ", "Delta Quebec"));
        dkd.add(new DiscreteKeyDef("DR", "Delta Romeo"));
        dkd.add(new DiscreteKeyDef("DS", "Delta Sierra"));
        dkd.add(new DiscreteKeyDef("DT", "Delta Tango"));
        dkd.add(new DiscreteKeyDef("DU", "Delta Uniform"));
        dkd.add(new DiscreteKeyDef("DV", "Delta Victor"));
        dkd.add(new DiscreteKeyDef("DW", "Delta Whiskey"));
        dkd.add(new DiscreteKeyDef("DX", "Delta Xray"));
        dkd.add(new DiscreteKeyDef("DY", "Delta Yankee"));
        dkd.add(new DiscreteKeyDef("DZ", "Delta Zulu"));
        dkd.add(new DiscreteKeyDef("EA", "Echo Alfa"));
        dkd.add(new DiscreteKeyDef("EB", "Echo Bravo"));
        dkd.add(new DiscreteKeyDef("EC", "Echo Charlie"));
        dkd.add(new DiscreteKeyDef("ED", "Echo Delta"));
        dkd.add(new DiscreteKeyDef("EE", "Echo Echo"));
        dkd.add(new DiscreteKeyDef("EF", "Echo Foxtrot"));
        dkd.add(new DiscreteKeyDef("EG", "Echo Golf"));
        dkd.add(new DiscreteKeyDef("EH", "Echo Hotel"));
        dkd.add(new DiscreteKeyDef("EI", "Echo India"));
        dkd.add(new DiscreteKeyDef("EJ", "Echo Juliet"));
        dkd.add(new DiscreteKeyDef("EK", "Echo Kilo"));
        dkd.add(new DiscreteKeyDef("EL", "Echo Lima"));
        dkd.add(new DiscreteKeyDef("EM", "Echo Mike"));
    }

    String siteId = "XXX";

    ParmID pid = new ParmID("test_SFC:BOU_GRID__Fcst_00000000_0000");

    ParmID pidOut = new ParmID("testoverlap_SFC:BOU_GRID__Fcst_00000000_0000");

    DiscreteDefinition dxDef = new DiscreteDefinition();

    DiscreteKey testDK1 = null;

    DiscreteKey testDK2 = null;

    DiscreteKey testDK3 = null;
    {
        this.dxDef.addDefinition(this.pid.getCompositeName(), false, 0,
                this.dkd);
        this.dxDef.addDefinition(this.pidOut.getCompositeName(), true, 0,
                this.dkd);
        DiscreteKey.setDiscreteDefinition(siteId, dxDef);
        try {
            this.testDK1 = new DiscreteKey(siteId, "C^A^B", pidOut);
            this.testDK2 = new DiscreteKey(siteId, "B^A", pidOut);
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.testDK3 = new DiscreteKey(this.testDK2);
        this.testDK3.addAll(this.testDK1);
    }

    private final Grid2DByte testG2DB1 = new Grid2DByte(4, 4);

    private final TimeRange testTR1 = new TimeRange(new Date(), 1000);

    private final ProjectionData grid211 = new ProjectionData("Grid211",
            ProjectionType.LAMBERT_CONFORMAL.ordinal(), new Coordinate(
                    -133.459, 12.190), new Coordinate(-49.385, 57.290),
            new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
            new Point(93, 65), 0.0f, 0.0f, 0.0f);

    private final GridLocation gloc = new GridLocation("BOU", grid211,
            new Point(4, 4), new Coordinate(38, 27), new Coordinate(9, 9),
            "MST7MDT");

    private final GridParmInfo testGPI1 = new GridParmInfo(new ParmID(
            "Hazards1_SFC:BOU_GRID__Official_00000000_0000"), gloc,
            GridType.DISCRETE, "wwa", "Hazards 1", 0f, 0f, 0, false,
            new TimeConstraints(10, 20, 30), false);

    private final GridDataHistory testGDHA1[] = new GridDataHistory[1];

    private final GFERecord testGFER1 = new GFERecord();

    // private final DiscreteKey testDK1 = new DiscreteKey(this.testDD1);
    private final DiscreteKey testDKA1[] = { this.testDK2 };

    // /////////////////////////////////////////////////////////////////////////
    private final byte testBA2[] = { -128, -127, -126, -125, -124, -123, -122,
            -121, -120, -119, -118, -117, -116, -115, -114, -113 };

    private final Grid2DByte testG2DB2 = new Grid2DByte(4, 4, this.testBA2);

    private final TimeRange testTR2 = new TimeRange(new Date(), 5000);

    private final GridParmInfo testGPI2 = new GridParmInfo(new ParmID(
            "Hazards2_SFC:BOU_GRID__Official_00000000_0000"), gloc,
            GridType.DISCRETE, "wwa", "Hazards 2", 0f, 0f, 0, false,
            new TimeConstraints(5, 10, 20), false);

    private final GridDataHistory testGDHA2[] = new GridDataHistory[1];

    private final DiscreteKey testDKA2[] = { this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2, this.testDK2, this.testDK2,
            this.testDK2, this.testDK2 };

    // /////////////////////////////////////////////////////////////////////////
    private final Grid2DBit testG2DB3 = new Grid2DBit(4, 4);
    {
        this.testG2DB3.set(0, 0);
    }

    // /////////////////////////////////////////////////////////////////////////
    private final byte testBA6[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1 };

    private final Grid2DByte testG2DB6 = new Grid2DByte(4, 4, this.testBA6);

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#DiscreteGridSlice(com.raytheon.uf.common.time.TimeRange, com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte, com.raytheon.uf.common.dataplugin.gfe.DiscreteKey[])}
     * .
     */
    @Test
    public final void testDiscreteGridSliceTimeRangeGFERecordGrid2DByteDiscreteKeyArray() {
        new DiscreteGridSlice(this.testTR1, this.testGFER1, this.testG2DB1,
                this.testDKA1);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#DiscreteGridSlice(com.raytheon.uf.common.time.TimeRange, com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte, com.raytheon.uf.common.dataplugin.gfe.DiscreteKey[])}
     * .
     */
    @Test
    public final void testDiscreteGridSliceTimeRangeGridParmInfoGridDataHistoryArrayGrid2DByteDiscreteKeyArray() {
        new DiscreteGridSlice(this.testTR1, this.testGPI1, this.testGDHA1,
                this.testG2DB1, this.testDKA1);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#DiscreteGridSlice(com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice)}
     * .
     */
    @Test
    public final void testDiscreteGridSliceDiscreteGridSlice() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DB1, this.testDKA1);
        DiscreteGridSlice test2 = new DiscreteGridSlice(test1);
        Assert.assertEquals(test1, test2);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#assign(com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice)}
     * .
     */
    @Test
    public final void testAssignIGridSlice() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DB1, this.testDKA1);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DB2, this.testDKA2);
        test1.assign(test2);
        Assert.assertEquals(test1, test2);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#equals(java.lang.Object)}
     * .
     */
    @Test
    public final void testEqualsObject() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        Assert.assertTrue(test1.equals(test2));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#isValid()}
     * .
     */
    @Test
    public final void testIsValid() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DB2, this.testDKA1);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        Assert.assertNotNull(test1.isValid());
        Assert.assertNull(test2.isValid());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#toString()}
     * .
     */
    @Test
    public final void testToString() {
        new DiscreteGridSlice(this.testTR2, this.testGPI2, this.testGDHA2,
                this.testG2DB2, this.testDKA2);
        // System.out.println(test1.toString());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#checkKeyAndData()}
     * .
     */
    @Test
    public final void testCheckKeyAndData() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        String result = test1.checkKeyAndData();
        Assert.assertNull(result);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2,
                new DiscreteKey[15]);
        result = test2.checkKeyAndData();
        Assert.assertNotNull(result);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#checkKey()}
     * .
     */
    @Test
    public final void testCheckKey() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        Assert.assertNull(test1.checkKey());
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#assign(com.raytheon.uf.common.dataplugin.gfe.DiscreteKey, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)}
     * .
     */
    @Test
    public final void testAssignDiscreteKeyGrid2DBit() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(test1);
        Assert.assertEquals(test1, test2);
        test2.assign(this.testDK3, this.testG2DB3);
        Assert.assertFalse(test1.equals(test2));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#assign(com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice, com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit)}
     * .
     */
    @Test
    public final void testAssignDiscreteGridSliceGrid2DBit() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(test1);
        Assert.assertEquals(test1, test2);
        test2.assign(this.testDK3, this.testG2DB3);
        Assert.assertFalse(test1.equals(test2));
        test1.assign(test2, this.testG2DB3);
        Assert.assertEquals(test1, test2);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#assign(com.raytheon.uf.common.dataplugin.gfe.DiscreteKey)}
     * .
     */
    @Test
    public final void testAssignDiscreteKey() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB1,
                new DiscreteKey[] { this.testDK3 });
        Assert.assertFalse(test1.equals(test2));
        test1.assign(this.testDK3);
        Assert.assertEquals(test1, test2);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#assign(com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice)}
     * .
     */
    @Test
    public final void testAssignDiscreteGridSlice() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DB1, this.testDKA1);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        Assert.assertFalse(test1.equals(test2));
        test1.assign(test2);
        Assert.assertEquals(test1, test2);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#eq(com.raytheon.uf.common.dataplugin.gfe.DiscreteKey)}
     * .
     */
    @Test
    public final void testEqDiscreteKey() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        test1.assign(this.testDK3, this.testG2DB3);
        Assert.assertEquals(this.testG2DB3, test1.eq(this.testDK3));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#notEq(com.raytheon.uf.common.dataplugin.gfe.DiscreteKey)}
     * .
     */
    @Test
    public final void testNotEqDiscreteKey() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        test1.assign(this.testDK3, this.testG2DB3);
        Grid2DBit test2 = new Grid2DBit(this.testG2DB3);
        test2.negate();
        Assert.assertEquals(test2, test1.notEq(this.testDK3));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#almost(java.lang.String)}
     * .
     */
    @Test
    public final void testAlmost() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR1,
                this.testGPI1, this.testGDHA1, this.testG2DB1, this.testDKA1);
        Assert.assertEquals(this.testG2DB6, test1.almost("A"));
        Assert.assertEquals(this.testG2DB1, test1.almost("C"));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#eq(com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice)}
     * .
     */
    @Test
    public final void testEqDiscreteGridSlice() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB1,
                new DiscreteKey[] { this.testDK3 });
        test1.assign(this.testDK3, this.testG2DB3);
        Assert.assertEquals(this.testG2DB3, test1.eq(test2));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#notEq(com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice)}
     * .
     */
    @Test
    public final void testNotEqDiscreteGridSlice() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB1,
                new DiscreteKey[] { this.testDK3 });
        test1.assign(this.testDK3, this.testG2DB3);
        Grid2DBit test3 = new Grid2DBit(this.testG2DB3);
        test3.negate();
        Assert.assertEquals(test3, test1.notEq(test2));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice#collapse()}
     * .
     */
    @Test
    public final void testCollapse() {
        DiscreteGridSlice test1 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB2, this.testDKA2);
        DiscreteGridSlice test2 = new DiscreteGridSlice(this.testTR2,
                this.testGPI2, this.testGDHA2, this.testG2DB1, this.testDKA1);
        test1.collapse();
        Assert.assertEquals(test1, test2);
    }

}
