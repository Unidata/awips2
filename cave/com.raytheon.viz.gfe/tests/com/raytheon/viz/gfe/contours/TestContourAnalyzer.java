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
/**
 * 
 */
package com.raytheon.viz.gfe.contours;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.viz.gfe.contours.util.CLine;
import com.raytheon.viz.gfe.contours.util.ContourValueDistance;
import com.raytheon.viz.gfe.contours.util.SearchDir;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author wdougherty
 * 
 */
public class TestContourAnalyzer {

    private static final int SUBGRID_FACTOR = 4;

    private static final int XDIM = 10;

    private static final int YDIM = 10;

    private static final int XDIMBIG = 143;

    private static final int YDIMBIG = 143;

    private static final float[] ORIGINAL_VALUES;
    static {
        float[] tmp = new float[XDIM * YDIM];
        for (int i = 0; i < XDIM * YDIM; i++) {
            tmp[i] = (i - XDIM * YDIM);
        }
        ORIGINAL_VALUES = tmp;
    }

    public ContourAnalyzer analyzer;

    private Grid2DFloat originalGrid;

    private List<CLine> contours;

    private float xOrigin;

    private float yOrigin;

    private float xGridRatio;

    private float yGridRatio;

    private int xMin;

    private int xMax;

    private int yMin;

    private int yMax;

    private float valMin;

    private float valMax;

    private boolean clampOn;

    // private GeometryFactory factory;

    // private CoordinateSequenceFactory seqFactory;

    // private CoordinateSequence cseq;

    private CLine contour;

    private float contourLevel;

    private Grid2DFloat result;

    // testDoAnalysis(), testGridValueFromGradient(), and
    // testComputeShadowAreas() work on the output of successive stages of
    // analysis on the same grid.
    // This is output from the legacy system for that grid.
    private static final float[] REGRESSIONDATA;
    static {
        REGRESSIONDATA = new float[] { 7.84327f, 5.96378f, 6.0503f, 1.50418f,
                4.36618f, 3.66293f, 6f, 6f, 4.04976f, 3.88061f, 8.01783f,
                7.77779f, 5.46911f, 5.57095f, 3.00279f, 5.31989f, 5.42566f,
                6.22717f, 4.58707f, 5.87048f, 8.26714f, 8.06572f, 7.78835f,
                5.39567f, 5.55349f, 4.50139f, 5.65495f, 6.38618f, 6.5898f,
                6.03285f, 2.02754f, 8.46209f, 8.28246f, 8.01465f, 4.67585f,
                5.33792f, 6f, 7.02801f, 7.32415f, 7.98623f, 8.8943f, 7.99071f,
                9.78866f, 8.8779f, 8.81802f, 8.42469f, 8.53871f, 8.69406f,
                8.81388f, 8.87169f, 8.98692f, 10.1995f, 8.47962f, 9.77953f,
                10.1211f, 10.1349f, 11.0184f, 10.6321f, 10.1308f, 9.79553f,
                15.0084f, 9.83671f, 10.0956f, 10.4576f, 13.0028f, 12.5014f,
                12f, 11.5288f, 10.9972f, 10.4958f, 9.92034f, 10.1534f,
                10.4598f, 12.5776f, 12.4059f, 13.3379f, 12.2844f, 11.7405f,
                11.6913f, 12.1395f, 10.1143f, 10.3609f, 12.4444f, 12.4115f,
                14.6758f, 12.6393f, 12.4409f, 12f, 13.3856f, 12.3722f,
                10.2388f, 11.9461f, 11.9149f, 16.0138f, 13.5024f, 14.1094f,
                12f, 12f, 13.8694f, 14.0784f };
    }

    /**
     * @throws java.lang.Exception
     */
    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        // Configure log4j
        // BasicConfigurator.configure();
    }

    /**
     * @throws java.lang.Exception
     */
    @AfterClass
    public static void tearDownAfterClass() throws Exception {
    }

    /**
     * This initializes fields to default values for the ContourAnalyzer ctor,
     * so our test cases can just override the defaults we need.
     * 
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        // Set the most common values to pass to the ContourAnalyzer ctor.
        originalGrid = new Grid2DFloat(XDIM, YDIM, ORIGINAL_VALUES);
        contours = new ArrayList<CLine>(); // most tests will add points
        xOrigin = 0;
        yOrigin = 0;
        xGridRatio = 1.0f;
        yGridRatio = 1.0f;
        xMin = 0;
        xMax = XDIM;
        yMin = 0;
        yMax = YDIM;
        clampOn = false;
        valMax = 1e37f;
        valMin = -valMax;
        // factory = new GeometryFactory();
        // seqFactory = factory.getCoordinateSequenceFactory();
        contourLevel = 100.0f;
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
    }

    /**
     * Helper method for comparing the expected output grid to the actual output
     * grid. We'll do this a lot. The label gives us something to make it easier
     * to determine which test failed.
     * 
     * @param expected
     *            The Grid2DFloat we expected to get out of the analyzer.
     * @param result
     *            The grid we actually got from the analyzer.
     * @param label
     *            A string to help us distinguish between test cases
     */
    public void checkResult(Grid2DFloat expected, Grid2DFloat result,
            String label) {
        assertEquals(label + ":getXdim()", expected.getXdim(), result.getXdim());
        assertEquals(label + ":getYdim()", expected.getYdim(), result.getYdim());
        assertArrayEquals(label + ":values", expected.getNumPy(),
                result.getNumPy());
    }

    /**
     * @param label
     * @param expected
     */
    private void invokeAnalyzer(String label, Grid2DFloat expected) {
        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);
        analyzer.recomputeGrid();
        result = analyzer.getFinalResultData();
        checkResult(expected, result, label);
    }

    @Test
    public void testSimplestContour() throws Exception {
        Coordinate[] outerRing = new Coordinate[5];
        outerRing[0] = new Coordinate(-1.0, -1.0);
        outerRing[1] = new Coordinate((XDIM + 1), -1.0);
        outerRing[2] = new Coordinate((XDIM + 1), (YDIM + 1));
        outerRing[3] = new Coordinate(-1.0, (YDIM + 1));
        outerRing[4] = new Coordinate(-1.0, -1.0);
        // cseq = seqFactory.create(outerRing);
        contour = new CLine(outerRing, contourLevel, true);
        contours.add(contour);
        Grid2DFloat expected = new Grid2DFloat(XDIM, YDIM, contourLevel);

        invokeAnalyzer("testSimplestContour", expected);
    }

    @Test
    public void testDiagonalBars() throws Exception {
        Coordinate[] firstBar = new Coordinate[2];
        firstBar[0] = new Coordinate(0.0, 2.0);
        firstBar[1] = new Coordinate(2.0, 0.0);
        // cseq = seqFactory.create(firstBar);
        contourLevel = 1.0f;
        contour = new CLine(firstBar, contourLevel, true);
        contours.add(contour);

        Coordinate[] secondBar = new Coordinate[2];
        secondBar[0] = new Coordinate(0.0, 5.0);
        secondBar[1] = new Coordinate(5.0, 0.0);
        // cseq = seqFactory.create(secondBar);
        contourLevel = 4.0f;
        contour = new CLine(secondBar, contourLevel, true);
        contours.add(contour);

        Coordinate[] thirdBar = new Coordinate[2];
        thirdBar[0] = new Coordinate(0.0, 8.0);
        thirdBar[1] = new Coordinate(8.0, 0.0);
        // cseq = seqFactory.create(thirdBar);
        contourLevel = 9.0f;
        contour = new CLine(thirdBar, contourLevel, true);
        contours.add(contour);
        float[] EA = new float[] { -1f, 8.39801e-18f, 1f, 3.33244f, 4f, 4f,
                7.77489f, 9f, 9f, 10.6667f, 8.39801e-18f, 1f, 2.9833f, 4f, 4f,
                7.18292f, 9f, 9f, 10.6667f, 12.3333f, 1f, 2.9833f, 4f, 4f,
                7.03335f, 9f, 9f, 10.6667f, 11.0984f, 14f, 3.33244f, 4f, 4f,
                7.00384f, 9f, 9f, 10.3579f, 12.3333f, 14f, 13.1967f, 4f, 4f,
                7.03335f, 9f, 9f, 10.6667f, 11.7159f, 14f, 15.6667f, 17.3333f,
                4f, 7.18292f, 9f, 9f, 10.6667f, 12.3333f, 13.0738f, 15.6667f,
                17.3333f, 19f, 7.77488f, 9f, 9f, 10.3579f, 11.7159f, 13.0738f,
                13.1967f, 15.1502f, 16.3802f, 17.6103f, 9f, 9f, 10.6667f,
                12.3333f, 14f, 15.6667f, 15.1502f, 19f, 20.6667f, 22.3333f, 9f,
                10.6667f, 11.0984f, 14f, 15.6667f, 17.3333f, 16.3802f,
                20.6667f, 22.3333f, 24f, 10.6667f, 12.3333f, 14f, 13.1967f,
                17.3333f, 19f, 17.6103f, 22.3333f, 24f, 25.6667f };

        Grid2DFloat expected = new Grid2DFloat(XDIM, YDIM, EA);
        invokeAnalyzer("Diagonal Bars", expected);
    }

    @Ignore
    @Test
    public void testConcentricCircles() throws Exception {
        final int CIRC_PTS = 12;
        Coordinate[] bigCircle = new Coordinate[CIRC_PTS + 1];
        Coordinate[] midCircle = new Coordinate[CIRC_PTS + 1];
        Coordinate[] littleCircle = new Coordinate[CIRC_PTS + 1];
        double angle;
        double xmult;
        double ymult;
        for (int i = 0; i < CIRC_PTS; i++) {
            angle = i * 2 * Math.PI / CIRC_PTS;
            xmult = Math.cos(angle);
            ymult = Math.sin(angle);
            bigCircle[i] = new Coordinate();
            bigCircle[i].x = XDIM / 2 + ((XDIM - 1) / 2) * xmult;
            bigCircle[i].y = YDIM / 2 + ((YDIM - 1) / 2) * ymult;
            midCircle[i] = new Coordinate();
            midCircle[i].x = XDIM / 2 + (XDIM / 3) * xmult;
            midCircle[i].y = YDIM / 2 + (YDIM / 3) * ymult;

            littleCircle[i] = new Coordinate();
            littleCircle[i].x = XDIM / 2 + (XDIM / 5) * xmult;
            littleCircle[i].y = YDIM / 2 + (YDIM / 5) * ymult;
        }
        bigCircle[CIRC_PTS] = new Coordinate(bigCircle[0]);
        midCircle[CIRC_PTS] = new Coordinate(midCircle[0]);
        littleCircle[CIRC_PTS] = new Coordinate(littleCircle[0]);

        // cseq = this.seqFactory.create(bigCircle);
        contourLevel = 4.0f;
        contour = new CLine(bigCircle, contourLevel, true);
        contours.add(contour);

        // cseq = this.seqFactory.create(midCircle);
        contourLevel = 4.0f; // on purpose
        contour = new CLine(midCircle, contourLevel, true);
        contours.add(contour);

        // cseq = this.seqFactory.create(littleCircle);
        contourLevel = 2.0f;
        contour = new CLine(littleCircle, contourLevel, true);
        contours.add(contour);

        Grid2DFloat expected = new Grid2DFloat(XDIM, YDIM, 1.0f);
        invokeAnalyzer("Concentric circles", expected);
    }

    @Test
    public void testFindNearestNorth() throws Exception {
        testFindNearestMajor(SearchDir.N);
    }

    @Test
    public void testFindNearestSouth() throws Exception {
        testFindNearestMajor(SearchDir.S);
    }

    @Test
    public void testFindNearestEast() throws Exception {
        testFindNearestMajor(SearchDir.E);
    }

    @Test
    public void testFindNearestWest() throws Exception {
        testFindNearestMajor(SearchDir.W);
    }

    private void testFindNearestMajor(SearchDir dir) throws Exception {
        Coordinate[] line = new Coordinate[2];

        double lineOriginX = Double.NaN;
        double lineOriginY = Double.NaN;
        double lineLimitX = Double.NaN;
        double lineLimitY = Double.NaN;

        switch (dir) {
        case N:
        case S:
            lineOriginX = 0;
            lineOriginY = YDIM / 2;
            lineLimitX = XDIM - 1;
            lineLimitY = YDIM / 2;
            break;
        case E:
        case W:
            lineOriginX = XDIM / 2;
            lineOriginY = 0;
            lineLimitX = XDIM / 2;
            lineLimitY = YDIM - 1;
            break;
        default:
            fail("testFindNearestMajor: cannot work with direction " + dir);
        }

        line[0] = new Coordinate(lineOriginX, lineOriginY);
        line[1] = new Coordinate(lineLimitX, lineLimitY);
        // cseq = this.seqFactory.create(line);
        contourLevel = 10.0f;
        contour = new CLine(line, contourLevel, true);
        contours.add(contour);

        // ContourAnalyzer ctor won't set up for findNearestContour()
        // properly if we don't have two contours. However, the
        // contours don't have to be confined to the grid, so create
        // a second contour that should never be seen.
        Coordinate[] brLine = new Coordinate[2];
        brLine[0] = new Coordinate(XDIM + 1, 0);
        brLine[1] = new Coordinate(XDIM + 2, 1);
        // cseq = this.seqFactory.create(brLine);
        contourLevel = 999;
        contour = new CLine(brLine, contourLevel, true);
        contours.add(contour);

        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);
        ContourValueDistance cvd = new ContourValueDistance();

        int i = Integer.MIN_VALUE;
        int j = Integer.MIN_VALUE;

        switch (dir) {
        case N:
            i = 1;
            j = YDIM / 2 - 1;
            break;
        case S:
            i = 1;
            j = YDIM / 2 + 1;
            break;
        case E:
            i = XDIM / 2 - 1;
            j = 1;
            break;
        case W:
            i = XDIM / 2 + 1;
            j = 1;
            break;
        }

        boolean found;
        found = analyzer.findNearestContour(i, j, dir, cvd);
        String testID = "testFindNearest(" + dir + "):" + i + "," + j;
        assertEquals(testID, true, found);
        assertEquals(testID, 10.0, cvd.value);
        assertEquals(testID, SUBGRID_FACTOR, cvd.distance);

        switch (dir) {
        case N:
            j = YDIM / 2 + 1;
            break;
        case S:
            j = YDIM / 2 - 1;
            break;
        case E:
            i = XDIM / 2 + 1;
            j = 2; // otherwise, we see brLine: it was snapped to the grid edge.
            break;
        case W:
            i = XDIM / 2 - 1;
            break;
        }

        found = analyzer.findNearestContour(i, j, dir, cvd);
        testID = "testFindNearest(" + dir + "):" + i + "," + j;
        assertEquals(testID, false, found);

        // Try with a contour on the edge of the grid
        contours.clear();
        // put brLine back in
        contours.add(contour);

        switch (dir) {
        case N:
            lineOriginX = 0;
            lineOriginY = YDIM - 1;
            lineLimitX = XDIM - 1;
            lineLimitY = YDIM - 1;
            break;
        case S:
            lineOriginX = 0;
            lineOriginY = 0;
            lineLimitX = XDIM - 1;
            lineLimitY = 0;
            break;
        case E:
            lineOriginX = XDIM - 1;
            lineOriginY = 0;
            lineLimitX = XDIM - 1;
            lineLimitY = YDIM - 1;
            break;
        case W:
            lineOriginX = 0;
            lineOriginY = 0;
            lineLimitX = 0;
            lineLimitY = YDIM - 1;
            break;
        }

        line[0] = new Coordinate(lineOriginX, lineOriginY);
        line[1] = new Coordinate(lineLimitX, lineLimitY);
        // cseq = seqFactory.create(line);
        contourLevel = 10.0f;
        contour = new CLine(line, contourLevel, true);
        contours.add(contour);

        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);

        switch (dir) {
        case N:
            i = 1;
            j = YDIM - 2;
            break;
        case S:
            i = 1;
            j = 1;
            break;
        case E:
            i = XDIM - 2;
            j = 1;
            break;
        case W:
            i = 1;
            j = 1;
            break;
        }

        found = analyzer.findNearestContour(i, j, dir, cvd);
        testID = "testFindNearest(" + dir + "):" + i + "," + j;
        assertEquals(testID, true, found);
        assertEquals(testID, 10.0, cvd.value);
        assertEquals(testID, SUBGRID_FACTOR, cvd.distance);

        // Shouldn't find it to dir if we're on the contour
        switch (dir) {
        case N:
            j = YDIM - 1;
            break;
        case S:
            j = 0;
            break;
        case E:
            i = XDIM - 1;
            break;
        case W:
            i = 0;
            break;
        }
        found = analyzer.findNearestContour(i, j, dir, cvd);
        testID = "testFindNearest(" + dir + "):" + i + "," + j;
        assertEquals(testID, false, found);

    }

    @Test
    public void testFindNearestNE() throws Exception {
        testFindNearestMinor(SearchDir.NE);
    }

    @Test
    public void testFindNearestNW() throws Exception {
        testFindNearestMinor(SearchDir.NW);
    }

    @Test
    public void testFindNearestSE() throws Exception {
        testFindNearestMinor(SearchDir.SE);
    }

    @Test
    public void testFindNearestSW() throws Exception {
        testFindNearestMinor(SearchDir.SW);
    }

    private void testFindNearestMinor(SearchDir dir) {

        // Set up 2 lines to watch for diagonal leakage
        double lineAOriginX = Double.NaN;
        double lineAOriginY = Double.NaN;
        double lineBOriginX = Double.NaN;
        double lineBOriginY = Double.NaN;
        double lineALimitX = Double.NaN;
        double lineALimitY = Double.NaN;
        double lineBLimitX = Double.NaN;
        double lineBLimitY = Double.NaN;

        switch (dir) {
        case NE:
            lineAOriginX = 0.0;
            lineAOriginY = YDIM - 1;
            lineALimitX = XDIM - 1;
            lineALimitY = 0.0;
            lineBOriginX = XDIM / 2;
            lineBOriginY = YDIM - 1;
            lineBLimitX = XDIM - 1;
            lineBLimitY = YDIM / 2;
            break;
        case NW:
            lineAOriginX = 0.0;
            lineAOriginY = 0.0;
            lineALimitX = XDIM - 1;
            lineALimitY = YDIM - 1;
            lineBOriginX = 0.0;
            lineBOriginY = YDIM / 2;
            lineBLimitX = XDIM / 2;
            lineBLimitY = YDIM - 1;
            break;
        case SE:
            lineAOriginX = 0.0;
            lineAOriginY = 0.0;
            lineALimitX = XDIM - 1;
            lineALimitY = YDIM - 1;
            lineBOriginX = XDIM / 2;
            lineBOriginY = 0.0;
            lineBLimitX = XDIM - 1;
            lineBLimitY = YDIM / 2;
            break;
        case SW:
            lineAOriginX = 0.0;
            lineAOriginY = YDIM - 1;
            lineALimitX = XDIM - 1;
            lineALimitY = 0.0;
            lineBOriginX = 0.0;
            lineBOriginY = YDIM / 2;
            lineBLimitX = XDIM / 2;
            lineBLimitY = 0.0;
            break;
        default:
            fail("testFindNearestMinor: cannot work with direction " + dir);
        }

        // This is the contour we want to find
        Coordinate[] lineA = new Coordinate[2];
        lineA[0] = new Coordinate(lineAOriginX, lineAOriginY);
        lineA[1] = new Coordinate(lineALimitX, lineALimitY);
        // cseq = seqFactory.create(lineA);
        contourLevel = 10.0f;
        contour = new CLine(lineA, contourLevel, true);
        contours.add(contour);

        // This is the one we don't want to find
        Coordinate[] lineB = new Coordinate[2];
        lineB[0] = new Coordinate(lineBOriginX, lineBOriginY);
        lineB[1] = new Coordinate(lineBLimitX, lineBLimitY);
        // cseq = seqFactory.create(lineB);
        contourLevel = 999;
        contour = new CLine(lineB, contourLevel, true);
        contours.add(contour);

        // Create the analyzer and prime it for findNearestContour() calls.
        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);

        int i = Integer.MIN_VALUE;
        int j = Integer.MIN_VALUE;
        int deltai = Integer.MIN_VALUE;
        int deltaj = Integer.MIN_VALUE;

        switch (dir) {
        case NE:
            i = XDIM / 2 - 2;
            j = YDIM / 2 - 1;
            deltai = -1;
            deltaj = 0;
            break;
        case NW:
            i = XDIM / 2 + 1;
            j = YDIM / 2 - 1;
            deltai = 1;
            deltaj = 0;
            break;
        case SE:
            i = XDIM / 2 - 1;
            j = YDIM / 2 + 1;
            deltai = -1;
            deltaj = 0;
            break;
        case SW:
            i = XDIM / 2 + 1;
            j = XDIM / 2;
            deltai = 1;
            deltaj = 0;
            break;
        }

        String testID;
        testID = "testFindNearest(" + dir + "):" + i + "," + j;

        boolean found;
        ContourValueDistance cvd = new ContourValueDistance();
        found = analyzer.findNearestContour(i, j, dir, cvd);

        assertEquals(testID, true, found);
        assertEquals(testID, 10.0, cvd.value);
        // findNearestContour() uses 2 approximations for diagonal distance.
        // The first is about sqrt(2.0) times n steps.
        // The second is the same minus 0.414 when it avoids diagonal leakage.
        assertTrue(testID, (SUBGRID_FACTOR * 1.414 == cvd.distance)
                || (SUBGRID_FACTOR * 1.414 - 0.414 == cvd.distance));

        i += deltai;
        j += deltaj;
        testID = "testFindNearest(" + dir + "):" + i + "," + j;
        found = analyzer.findNearestContour(i, j, dir, cvd);
        assertEquals(testID, true, found);
        assertEquals(testID, 10.0, cvd.value);
        assertEquals(testID, SUBGRID_FACTOR * 1.5 * 1.414, cvd.distance);
        assertTrue(testID, (SUBGRID_FACTOR * 1.5 * 1.414 == cvd.distance)
                || (SUBGRID_FACTOR * 1.5 * 1.414 - 0.414 == cvd.distance));

        i += deltai;
        j += deltaj;
        testID = "testFindNearest(" + dir + "):" + i + "," + j;
        found = analyzer.findNearestContour(i, j, dir, cvd);
        assertEquals(testID, true, found);
        assertEquals(testID, 10.0, cvd.value);
        assertEquals(testID, SUBGRID_FACTOR * 2 * 1.414, cvd.distance);
        assertTrue(testID, (SUBGRID_FACTOR * 2 * 1.414 == cvd.distance)
                || (SUBGRID_FACTOR * 2 * 1.414 - 0.414 == cvd.distance));

    }

    @Test
    public void testFindDistantN() throws Exception {
        testFindDistantMajor(SearchDir.N);
    }

    @Test
    public void testFindDistantS() throws Exception {
        testFindDistantMajor(SearchDir.S);
    }

    @Test
    public void testFindDistantE() throws Exception {
        testFindDistantMajor(SearchDir.E);
    }

    @Test
    public void testFindDistantW() throws Exception {
        testFindDistantMajor(SearchDir.W);
    }

    private void testFindDistantMajor(SearchDir dir) throws Exception {

        double lineAOriginX = Double.NaN;
        double lineAOriginY = Double.NaN;
        double lineBOriginX = Double.NaN;
        double lineBOriginY = Double.NaN;
        double lineCOriginX = Double.NaN;
        double lineCOriginY = Double.NaN;
        double lineALimitX = Double.NaN;
        double lineALimitY = Double.NaN;
        double lineBLimitX = Double.NaN;
        double lineBLimitY = Double.NaN;
        double lineCLimitX = Double.NaN;
        double lineCLimitY = Double.NaN;

        switch (dir) {
        case N:
            lineAOriginX = 0.0;
            lineAOriginY = YDIM / 4;
            lineALimitX = XDIM - 1;
            lineALimitY = YDIM / 4;
            lineBOriginX = 0.0;
            lineBOriginY = YDIM / 2;
            lineBLimitX = XDIM - 1;
            lineBLimitY = YDIM / 2;
            lineCOriginX = 0.0;
            lineCOriginY = YDIM * 3 / 4;
            lineCLimitX = XDIM - 1;
            lineCLimitY = YDIM * 3 / 4;
            break;
        case S:
            lineAOriginX = 0.0;
            lineAOriginY = (YDIM - 1) - YDIM / 4;
            lineALimitX = XDIM - 1;
            lineALimitY = (YDIM - 1) - YDIM / 4;
            lineBOriginX = 0.0;
            lineBOriginY = (YDIM - 1) - YDIM / 2;
            lineBLimitX = XDIM - 1;
            lineBLimitY = (YDIM - 1) - YDIM / 2;
            lineCOriginX = 0.0;
            lineCOriginY = (YDIM - 1) - YDIM * 3 / 4;
            lineCLimitX = XDIM - 1;
            lineCLimitY = (YDIM - 1) - YDIM * 3 / 4;
            break;
        case E:
            lineAOriginX = XDIM / 4;
            lineAOriginY = 0.0;
            lineALimitX = XDIM / 4;
            lineALimitY = YDIM - 1;
            lineBOriginX = XDIM / 2;
            lineBOriginY = 0.0;
            lineBLimitX = XDIM / 2;
            lineBLimitY = YDIM - 1;
            lineCOriginX = 3 * XDIM / 4;
            lineCOriginY = 0.0;
            lineCLimitX = 3 * XDIM / 4;
            lineCLimitY = YDIM - 1;
            break;
        case W:
            lineAOriginX = (XDIM - 1) - XDIM / 4;
            lineAOriginY = 0.0;
            lineALimitX = (XDIM - 1) - XDIM / 4;
            lineALimitY = YDIM - 1;
            lineBOriginX = (XDIM - 1) - XDIM / 2;
            lineBOriginY = 0;
            lineBLimitX = (XDIM - 1) - XDIM / 2;
            lineBLimitY = YDIM - 1;
            lineCOriginX = (XDIM - 1) - XDIM * 3 / 4;
            lineCOriginY = 0.0;
            lineCLimitX = (XDIM - 1) - XDIM * 3 / 4;
            lineCLimitY = YDIM - 1;
            break;
        default:
            fail("testFindDistantMajor: cannot work with direction " + dir);
        }

        Coordinate[] lineA = new Coordinate[2];
        lineA[0] = new Coordinate(lineAOriginX, lineAOriginY);
        lineA[1] = new Coordinate(lineALimitX, lineALimitY);
        // cseq = seqFactory.create(lineA);
        contourLevel = 10.0f;
        contour = new CLine(lineA, contourLevel, true);
        contours.add(contour);

        Coordinate[] lineB = new Coordinate[2];
        lineB[0] = new Coordinate(lineBOriginX, lineBOriginY);
        lineB[1] = new Coordinate(lineBLimitX, lineBLimitY);
        // cseq = seqFactory.create(lineB);
        contourLevel = 15.0f;
        contour = new CLine(lineB, contourLevel, true);
        contours.add(contour);

        Coordinate[] lineC = new Coordinate[2];
        lineC[0] = new Coordinate(lineCOriginX, lineCOriginY);
        lineC[1] = new Coordinate(lineCLimitX, lineCLimitY);
        // cseq = seqFactory.create(lineC);
        contourLevel = 999.0f;
        contour = new CLine(lineC, contourLevel, true);
        contours.add(contour);

        // Create the analyzer and prime it for findDistantContour() calls.
        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);

        int i = Integer.MIN_VALUE;
        int j = Integer.MIN_VALUE;

        switch (dir) {
        case N:
            i = 0;
            j = 0;
            break;
        case S:
            i = 0;
            j = YDIM - 1;
            break;
        case E:
            i = 0;
            j = 0;
            break;
        case W:
            i = XDIM - 1;
            j = 0;
            break;
        }

        ContourValueDistance cvd1 = new ContourValueDistance();
        ContourValueDistance cvd2 = new ContourValueDistance();
        int numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);

        String testID = "testFindDistantMajor(" + dir + "):" + i + "," + j;
        assertEquals(testID + "<num>", 2, numcFound);
        assertEquals(testID + "<v1>", 10.0, cvd1.value);
        assertEquals(testID + "<v2>", 15.0, cvd2.value);
        assertEquals(testID + "<d1>", SUBGRID_FACTOR * (YDIM / 4),
                cvd1.distance);
        assertEquals(testID + "<d2>", SUBGRID_FACTOR * (YDIM / 2),
                cvd2.distance);

        switch (dir) {
        case N:
            i = 0;
            j = YDIM / 4 + 1;
            break;
        case S:
            i = 0;
            j = (YDIM - 1) - (YDIM / 4 + 1);
            break;
        case E:
            i = XDIM / 4 + 1;
            j = 0;
            break;
        case W:
            i = (YDIM - 1) - (XDIM / 4 + 1);
            j = 0;
            break;
        }

        numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);
        testID = "testFindDistantMajor(" + dir + "):" + i + "," + j;

        assertEquals(testID + "<num>", 2, numcFound);
        assertEquals(testID + "<v1>", 15.0, cvd1.value);
        assertEquals(testID + "<v2>", 999.0, cvd2.value);
        assertEquals(testID + "<d1>", SUBGRID_FACTOR
                * (YDIM / 2 - (YDIM / 4 + 1)), cvd1.distance);
        assertEquals(testID + "<d2>", SUBGRID_FACTOR
                * (3 * YDIM / 4 - (YDIM / 4 + 1)), cvd2.distance);

        switch (dir) {
        case N:
            i = 0;
            j = YDIM / 2 + 1;
            break;
        case S:
            i = 0;
            j = (YDIM - 1) - (YDIM / 2 + 1);
            break;
        case E:
            i = XDIM / 2 + 1;
            j = 0;
            break;
        case W:
            i = (XDIM - 1) - (XDIM / 2 + 1);
            j = 0;
            break;
        }

        numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);
        testID = "testFindDistantMajor(" + dir + "):" + i + "," + j;

        // Even though there's a grid between i,j and the edge,
        // the routine should return 0 because no edge points have values set.
        assertEquals(testID + "<num>", 0, numcFound);

        // TODO: set edge points (how?), confirm rtnval, cvd1 and cvd2

        switch (dir) {
        case N:
            i = 0;
            j = YDIM * 3 / 4 + 1;
            break;
        case S:
            i = 0;
            j = (YDIM - 1) - (YDIM * 3 / 4 + 1);
            break;
        case E:
            i = XDIM * 3 / 4 + 1;
            j = 0;
            break;
        case W:
            i = (XDIM - 1) - (XDIM * 3 / 4 + 1);
            j = 0;
            break;
        }

        numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);
        testID = "testFindDistantMajor(" + dir + "):" + i + "," + j;

        assertEquals(testID + "<num>", 0, numcFound);
    }

    @Test
    public void testFindDistantNE() throws Exception {
        testFindDistantMinor(SearchDir.NE);
    }

    @Test
    public void testFindDistantSE() throws Exception {
        testFindDistantMinor(SearchDir.SE);
    }

    @Test
    public void testFindDistantSW() throws Exception {
        testFindDistantMinor(SearchDir.SW);
    }

    @Test
    public void testFindDistantNW() throws Exception {
        testFindDistantMinor(SearchDir.NW);
    }

    /**
     * Distance from point to a line segment in 2 dimensions.
     * 
     * @param x1
     * @param y1
     * @param x2
     * @param y2
     * @param px
     * @param py
     * @return distance
     */
    public double linePtDistance(double x1, double y1, double x2, double y2,
            double px, double py) {
        double lineXdiff = x2 - x1;
        double lineYdiff = y2 - y1;
        double term1 = lineXdiff * (y1 - py);
        double term2 = lineYdiff * (x1 - px);
        double top = Math.abs(term1 - term2);
        double lineLength = lineXdiff * lineXdiff + lineYdiff * lineYdiff;
        lineLength = Math.sqrt(lineLength);
        double distance = top / lineLength;
        return distance;
    }

    public void testFindDistantMinor(SearchDir dir) throws Exception {

        double lineAOriginX = Double.NaN;
        double lineAOriginY = Double.NaN;
        double lineBOriginX = Double.NaN;
        double lineBOriginY = Double.NaN;
        double lineCOriginX = Double.NaN;
        double lineCOriginY = Double.NaN;
        double lineALimitX = Double.NaN;
        double lineALimitY = Double.NaN;
        double lineBLimitX = Double.NaN;
        double lineBLimitY = Double.NaN;
        double lineCLimitX = Double.NaN;
        double lineCLimitY = Double.NaN;

        switch (dir) {
        case NE:
            lineAOriginX = 0;
            lineAOriginY = YDIM / 4;
            lineALimitX = XDIM / 4;
            lineALimitY = 0;
            lineBOriginX = 0;
            lineBOriginY = YDIM / 2;
            lineBLimitX = XDIM / 2;
            lineBLimitY = 0;
            lineCOriginX = 0;
            lineCOriginY = YDIM * 3 / 4;
            lineCLimitX = XDIM * 3 / 4;
            lineCLimitY = 0;
            break;
        case SE:
            lineAOriginX = 0;
            lineAOriginY = (YDIM - 1) - (YDIM / 4);
            lineALimitX = XDIM / 4;
            lineALimitY = YDIM - 1;
            lineBOriginX = 0;
            lineBOriginY = (YDIM - 1) - (YDIM / 2);
            lineBLimitX = XDIM / 2;
            lineBLimitY = YDIM - 1;
            lineCOriginX = 0;
            lineCOriginY = (YDIM - 1) - (YDIM * 3 / 4);
            lineCLimitX = XDIM * 3 / 4;
            lineCLimitY = YDIM - 1;
            break;
        case SW:
            lineAOriginX = (XDIM - 1) - (XDIM / 4);
            lineAOriginY = YDIM - 1;
            lineALimitX = XDIM - 1;
            lineALimitY = (YDIM - 1) - (YDIM / 4);
            lineBOriginX = (XDIM - 1) - (XDIM / 2);
            lineBOriginY = YDIM - 1;
            lineBLimitX = XDIM - 1;
            lineBLimitY = (YDIM - 1) - (YDIM / 2);
            lineCOriginX = (XDIM - 1) - (XDIM * 3 / 4);
            lineCOriginY = YDIM - 1;
            lineCLimitX = XDIM - 1;
            lineCLimitY = (YDIM - 1) - (YDIM * 3 / 4);
            break;
        case NW:
            lineAOriginX = (XDIM - 1) - (XDIM / 4);
            lineAOriginY = 0;
            lineALimitX = XDIM - 1;
            lineALimitY = YDIM / 4;
            lineBOriginX = (XDIM - 1) - (XDIM / 2);
            lineBOriginY = 0;
            lineBLimitX = XDIM - 1;
            lineBLimitY = YDIM / 2;
            lineCOriginX = (XDIM - 1) - (XDIM * 3 / 4);
            lineCOriginY = 0;
            lineCLimitX = XDIM - 1;
            lineCLimitY = YDIM * 3 / 4;
            break;

        default:
            fail("testFindDistantMinor: cannot work with direction " + dir);
        }

        Coordinate[] lineA = new Coordinate[2];
        lineA[0] = new Coordinate(lineAOriginX, lineAOriginY);
        lineA[1] = new Coordinate(lineALimitX, lineALimitY);
        // cseq = seqFactory.create(lineA);
        contourLevel = 10.0f;
        contour = new CLine(lineA, contourLevel, true);
        contours.add(contour);

        Coordinate[] lineB = new Coordinate[2];
        lineB[0] = new Coordinate(lineBOriginX, lineBOriginY);
        lineB[1] = new Coordinate(lineBLimitX, lineBLimitY);
        // cseq = seqFactory.create(lineB);
        contourLevel = 15.0f;
        contour = new CLine(lineB, contourLevel, true);
        contours.add(contour);

        Coordinate[] lineC = new Coordinate[2];
        lineC[0] = new Coordinate(lineCOriginX, lineCOriginY);
        lineC[1] = new Coordinate(lineCLimitX, lineCLimitY);
        // cseq = seqFactory.create(lineC);
        contourLevel = 999.0f;
        contour = new CLine(lineC, contourLevel, true);
        contours.add(contour);

        // Create the analyzer and prime it for findDistantContour() calls.
        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);

        int i = Integer.MIN_VALUE;
        int j = Integer.MIN_VALUE;

        switch (dir) {
        case NE:
            i = 0;
            j = 0;
            break;
        case SE:
            i = 0;
            j = YDIM - 1;
            break;
        case SW:
            i = XDIM - 1;
            j = XDIM - 1;
            break;
        case NW:
            i = XDIM - 1;
            j = 0;
            break;
        }

        ContourValueDistance cvd1 = new ContourValueDistance();
        ContourValueDistance cvd2 = new ContourValueDistance();
        int numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);

        String testID = "testFindDistantMinor(" + dir + "):" + i + "," + j;
        assertEquals(testID + "<num>", 2, numcFound);
        assertEquals(testID + "<v1>", 10.0, cvd1.value);
        assertEquals(testID + "<v2>", 15.0, cvd2.value);

        double expectedD1 = linePtDistance(lineAOriginX, lineAOriginY,
                lineALimitX, lineALimitY, i, j);
        expectedD1 *= SUBGRID_FACTOR;
        assertEquals(testID + "<d1>", expectedD1, cvd1.distance);
        double expectedD2 = linePtDistance(lineBOriginX, lineBOriginY,
                lineBLimitX, lineBLimitY, i, j);
        expectedD2 *= SUBGRID_FACTOR;
        assertEquals(testID + "<d2>", expectedD2, cvd2.distance);

        switch (dir) {
        case NE:
            i = 1;
            j = 0;
            break;
        case SE:
            i = 1;
            j = YDIM - 1;
            break;
        case SW:
            i = XDIM - 2;
            j = YDIM - 1;
            break;
        case NW:
            i = XDIM - 2;
            j = 0;
            break;
        }

        numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);
        testID = "testFindDistantMinor(" + dir + "):" + i + "," + j;
        assertEquals(testID + "<num>", 2, numcFound);
        assertEquals(testID + "<v1>", 10.0, cvd1.value);
        assertEquals(testID + "<v2>", 15.0, cvd2.value);

        expectedD1 = linePtDistance(lineAOriginX, lineAOriginY, lineALimitX,
                lineALimitY, i, j);
        expectedD1 *= SUBGRID_FACTOR;
        assertEquals(testID + "<d1>", expectedD1, cvd1.distance);
        expectedD2 = linePtDistance(lineBOriginX, lineBOriginY, lineBLimitX,
                lineBLimitY, i, j);
        expectedD2 *= SUBGRID_FACTOR;
        assertEquals(testID + "<d2>", expectedD2, cvd2.distance);

        switch (dir) {
        case NE:
            i = XDIM / 4 + 1;
            j = 0;
            break;
        case SE:
            i = XDIM / 4 + 1;
            j = YDIM - 1;
            break;
        case SW:
            i = (XDIM - 1) - (XDIM / 4 + 1);
            j = YDIM - 1;
            break;
        case NW:
            i = (XDIM - 1) - (XDIM / 4 + 1);
            j = 0;
            break;
        }

        numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);
        testID = "testFindDistantMinor(" + dir + "):" + i + "," + j;
        assertEquals(testID + "<num>", 2, numcFound);
        assertEquals(testID + "<v1>", 15.0, cvd1.value);
        assertEquals(testID + "<v2>", 999.0, cvd2.value);

        expectedD1 = linePtDistance(lineBOriginX, lineBOriginY, lineBLimitX,
                lineBLimitY, i, j);
        expectedD1 *= SUBGRID_FACTOR;
        assertEquals(testID + "<d1>", expectedD1, cvd1.distance);
        expectedD2 = linePtDistance(lineCOriginX, lineCOriginY, lineCLimitX,
                lineCLimitY, i, j);
        expectedD2 *= SUBGRID_FACTOR;
        assertEquals(testID + "<d2>", expectedD2, cvd2.distance);

        switch (dir) {
        case NE:
            i = XDIM / 2 + 1;
            j = 0;
            break;
        case SE:
            i = XDIM / 2 + 1;
            j = YDIM - 1;
            break;
        case SW:
            i = (XDIM - 1) - (XDIM / 2 + 1);
            j = YDIM - 1;
            break;
        case NW:
            i = (XDIM - 1) - (XDIM / 2 + 1);
            j = 0;
            break;
        }

        numcFound = analyzer.findDistantContours(i, j, dir, cvd1, cvd2);
        testID = "testFindDistantMinor(" + dir + "):" + i + "," + j;
        assertEquals(testID + "<num>", 0, numcFound);

    }

    @Test
    public void testFindAdjacentN() throws Exception {
        testFindAdjacent(SearchDir.N);
    }

    @Test
    public void testFindAdjacentS() throws Exception {
        testFindAdjacent(SearchDir.S);
    }

    @Test
    public void testFindAdjacentE() throws Exception {
        testFindAdjacent(SearchDir.E);
    }

    @Test
    public void testFindAdjacentW() throws Exception {
        testFindAdjacent(SearchDir.W);
    }

    @Test
    public void testFindAdjacentNE() throws Exception {
        testFindAdjacent(SearchDir.NE);
    }

    @Test
    public void testFindAdjacentSE() throws Exception {
        testFindAdjacent(SearchDir.SE);
    }

    @Test
    public void testFindAdjacentSW() throws Exception {
        testFindAdjacent(SearchDir.SW);
    }

    @Test
    public void testFindAdjacentNW() throws Exception {
        testFindAdjacent(SearchDir.NW);
    }

    public void testFindAdjacent(SearchDir dir) throws Exception {

        int i = Integer.MIN_VALUE;
        int j = Integer.MIN_VALUE;

        switch (dir) {
        case N:
            xMin = 0;
            xMax = 0;
            yMin = 0;
            yMax = YDIM - 2;
            i = 0;
            j = 0;
            break;
        case S:
            xMin = 0;
            xMax = 0;
            yMin = 1;
            yMax = YDIM - 1;
            i = 0;
            j = YDIM - 1;
            break;
        case E:
            xMin = 0;
            xMax = XDIM - 2;
            yMin = 0;
            yMax = 0;
            i = 0;
            j = 0;
            break;
        case W:
            xMin = 1;
            xMax = XDIM - 1;
            yMin = 0;
            yMax = 0;
            i = XDIM - 1;
            j = 0;
            break;
        case NE:
            xMin = 0;
            yMin = 0;
            xMax = XDIM - 2;
            yMax = YDIM - 1;
            i = 0;
            j = 0;
            break;
        case SE:
            xMin = 0;
            xMax = XDIM - 2;
            yMin = 0;
            yMax = YDIM - 1;
            i = 0;
            j = YDIM - 1;
            break;
        case SW:
            xMin = 1;
            xMax = XDIM - 1;
            yMin = 0;
            yMax = YDIM - 1;
            i = XDIM - 1;
            j = YDIM - 1;
            break;
        case NW:
            xMin = 1;
            xMax = XDIM - 1;
            yMin = 0;
            yMax = YDIM - 1;
            i = XDIM - 1;
            j = 0;
            break;
        default:
            fail("Invalid search direction: " + dir);
        }

        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);
        analyzer.setSubGridFactor(SUBGRID_FACTOR);

        double expected = Double.MIN_VALUE;
        switch (dir) {
        case N:
            analyzer.howFound.set(0, YDIM - 1, 2);
            expected = -XDIM;
            break;
        case S:
            analyzer.howFound.set(0, 0, 2);
            expected = -(XDIM * YDIM);
            break;
        case E:
            analyzer.howFound.set(XDIM - 1, 0, 2);
            expected = -(XDIM * YDIM) + XDIM - 1;
            break;
        case W:
            analyzer.howFound.set(0, 0, 2);
            expected = -(XDIM * YDIM);
            break;
        case NE:
            analyzer.howFound.set(XDIM - 1, YDIM - 1, 2);
            expected = -1;
            break;
        case SE:
            analyzer.howFound.set(XDIM - 1, 0, 2);
            expected = -(XDIM * YDIM) + XDIM - 1;
            break;
        case SW:
            analyzer.howFound.set(0, 0, 2);
            expected = -(XDIM * YDIM);
            break;
        case NW:
            analyzer.howFound.set(0, YDIM - 1, 2);
            expected = -XDIM;
            break;
        }

        ContourValueDistance cvd = new ContourValueDistance();
        boolean found = analyzer.findAdjacentValue(i, j, dir, cvd);

        assertEquals("", true, found);
        assertEquals("", expected, cvd.value);
    }

    @Test
    public void testDoAnalysis() throws Exception {

        // Create a pair of narrow contours.
        // These will exercise
        Coordinate[] lineA = new Coordinate[3];
        lineA[0] = new Coordinate(XDIM / 2 + 1, 0);
        lineA[1] = new Coordinate(XDIM / 2 + 1, YDIM / 2 - 2);
        lineA[2] = new Coordinate(XDIM / 2 + 2, 0);
        // cseq = seqFactory.create(lineA);
        contourLevel = 6.0f;
        contour = new CLine(lineA, contourLevel, true);
        contours.add(contour);

        Coordinate[] lineB = new Coordinate[3];
        lineB[0] = new Coordinate(XDIM / 2 + 1, YDIM - 1);
        lineB[1] = new Coordinate(XDIM / 2 + 1, (YDIM - 1) - (YDIM / 2 - 2));
        lineB[2] = new Coordinate(XDIM / 2 + 2, YDIM - 1);
        // cseq = seqFactory.create(lineB);
        contourLevel = 12.0f;
        contour = new CLine(lineB, contourLevel, true);
        contours.add(contour);

        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);

        analyzer.doAnalysis();

        // confirm that they were set in the right way
        int[] howFound = analyzer.howFound.getBuffer().array();

        // Default for points that weren't copied from the contours or analyzed.
        int[] expectedHF = new int[100];
        Arrays.fill(expectedHF, -9);
        // These points should have been copied from the contours we supplied
        int[] contouredIndices = new int[] { 6, 7, 36, 66, 96, 97 };
        for (int i = 0; i < contouredIndices.length; i++) {
            expectedHF[contouredIndices[i]] = 9;
        }
        // These points should have been analyzed.
        int[] analyzedIndices = new int[] { 0, 10, 11, 17, 20, 21, 22, 27, 31,
                32, 33, 37, 39, 43, 44, 46, 47, 48, 49, 53, 54, 56, 57, 58, 59,
                61, 62, 63, 67, 69, 70, 71, 72, 77, 80, 81, 87, 90 };
        for (int i = 0; i < analyzedIndices.length; i++) {
            expectedHF[analyzedIndices[i]] = 1;
        }
        // Confirm that operations were carried out on the right set of points.
        assertArrayEquals(expectedHF, howFound);

        float[] analyzedValues = analyzer.finalResultData.getFloats();
        for (int i = 0; i < analyzedIndices.length; i++) {
            assertEquals("REGRESSIONDATA[" + analyzedIndices[i] + "]",
                    REGRESSIONDATA[analyzedIndices[i]],
                    analyzedValues[analyzedIndices[i]]);
        }

    }

    @Test
    public void testGridValueFromGradient() throws Exception {

        testDoAnalysis();
        analyzer.gridValueFromGradient();

        // confirm that they were set in the right way
        int[] howFound = analyzer.howFound.getBuffer().array();

        // Default for points that weren't copied from the contours or analyzed.
        int[] expectedHF = new int[100];
        Arrays.fill(expectedHF, -9);
        // These points should have been copied from the contours we supplied
        int[] contouredIndices = new int[] { 6, 7, 36, 66, 96, 97 };
        for (int i = 0; i < contouredIndices.length; i++) {
            expectedHF[contouredIndices[i]] = 9;
        }
        // These points should have been analyzed.
        int[] analyzedIndices = new int[] { 0, 10, 11, 17, 20, 21, 22, 27, 31,
                32, 33, 37, 39, 43, 44, 46, 47, 48, 49, 53, 54, 56, 57, 58, 59,
                61, 62, 63, 67, 69, 70, 71, 72, 77, 80, 81, 87, 90 };
        for (int i = 0; i < analyzedIndices.length; i++) {
            expectedHF[analyzedIndices[i]] = 1;
        }

        // These points should have been generated from a gradient.
        int[] gradientIndices = new int[] { 3, 9, 14, 16, 18, 25, 26, 30, 34,
                35, 38, 60, 64, 65, 68, 75, 76, 84, 86, 88, 93, 99 };
        for (int i = 0; i < gradientIndices.length; i++) {
            expectedHF[gradientIndices[i]] = 2;
        }

        // Confirm that operations were carried out on the right set of points.
        assertArrayEquals(expectedHF, howFound);

        // Confirm that points that were calculated by gridValueFromGradient()
        // match the output of the legacy system.
        float[] gradientValues = analyzer.finalResultData.getFloats();
        for (int i = 0; i < gradientIndices.length; i++) {
            assertEquals("expectedValue[" + gradientIndices[i] + "]",
                    REGRESSIONDATA[gradientIndices[i]],
                    gradientValues[gradientIndices[i]]);
        }

    }

    @Test
    public void testComputeShadowAreas() throws Exception {
        testGridValueFromGradient();
        analyzer.computeShadowAreas();

        // All points should have been computed by now.
        // All the -9s in testGridValueFromGradient should be 3s in this step.
        int[] expectedHF = new int[100];
        Arrays.fill(expectedHF, 3);

        // Now override the same values we did in testGridValueFromGradient.
        // These points should have been copied from the contours we supplied
        int[] contouredIndices = new int[] { 6, 7, 36, 66, 96, 97 };
        for (int i = 0; i < contouredIndices.length; i++) {
            expectedHF[contouredIndices[i]] = 9;
        }
        // These points should have been analyzed.
        int[] analyzedIndices = new int[] { 0, 10, 11, 17, 20, 21, 22, 27, 31,
                32, 33, 37, 39, 43, 44, 46, 47, 48, 49, 53, 54, 56, 57, 58, 59,
                61, 62, 63, 67, 69, 70, 71, 72, 77, 80, 81, 87, 90 };
        for (int i = 0; i < analyzedIndices.length; i++) {
            expectedHF[analyzedIndices[i]] = 1;
        }

        // These points should have been generated from a gradient.
        int[] gradientIndices = new int[] { 3, 9, 14, 16, 18, 25, 26, 30, 34,
                35, 38, 60, 64, 65, 68, 75, 76, 84, 86, 88, 93, 99 };
        for (int i = 0; i < gradientIndices.length; i++) {
            expectedHF[gradientIndices[i]] = 2;
        }

        // Confirm that operations were carried out on the right set of points.
        int[] howFound = analyzer.howFound.getBuffer().array();
        assertArrayEquals("how-found", expectedHF, howFound);

        // since everything has been computed, finalResultData should
        // completely match the regression data from the legacy routine.
        float[] finalValues = analyzer.finalResultData.getFloats();
        for (int i = 0; i < REGRESSIONDATA.length; i++) {
            assertEquals("finalValues[" + i + "]", REGRESSIONDATA[i],
                    finalValues[i]);
        }
    }

    @Test
    public void testFullSize() throws Exception {

        originalGrid = new Grid2DFloat(XDIMBIG, YDIMBIG, -1.0f);

        // Writing the contours as code didn't work (64k limit), so parse jtext
        // to create the contours
        contours = ContourTextfileReader
                .readCoordinates("testdata/contours/jtext");

        contours.get(0).setModified(true);
        // contours are parsed!

        // Likewise, the expected output is too big to create inline in a static
        // block, so parse it, too.
        float[] expectedA = FloatArrayReader.read("testdata/contours/output",
                XDIMBIG * YDIMBIG);
        Grid2DFloat expected = new Grid2DFloat(XDIMBIG, YDIMBIG, expectedA);

        xMax = XDIMBIG;
        yMax = YDIMBIG;
        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);
        analyzer.recomputeGrid();
        result = analyzer.getFinalResultData();

        final float threshold = 0.01f;
        float expval;
        float actual;
        boolean failed = false;
        for (int i = 0; i < XDIMBIG; i++) {
            for (int j = 0; j < YDIMBIG; j++) {
                actual = result.get(i, j);
                expval = expected.get(i, j);
                if (Math.abs(actual - expval) > threshold) {
                    System.out.println("Grid discrepancy at (" + i + "," + j
                            + "): expected " + expval + " but value was "
                            + actual);
                    failed = true;
                }
            }
        }
        if (failed) {
            fail("Grid discrepancies were encountered. Consult output for details.");
        }
    }

    @Test
    public void testFullSize2() throws Exception {

        System.out.println("testFullSize2:");
        originalGrid = new Grid2DFloat(XDIMBIG, YDIMBIG, -1.0f);

        // Writing the contours as code didn't work (64k limit), so parse jtext
        // to create the contours
        contours = ContourTextfileReader
                .readCoordinates("testdata/contours/jtext2");

        contours.get(0).setModified(true);
        // contours are parsed!

        // Likewise, the expected output is too big to create inline in a static
        // block, so parse it, too.
        float[] expectedA = FloatArrayReader.read("testdata/contours/output2",
                XDIMBIG * YDIMBIG);
        Grid2DFloat expected = new Grid2DFloat(XDIMBIG, YDIMBIG, expectedA);

        xMax = XDIMBIG;
        yMax = YDIMBIG;
        analyzer = new ContourAnalyzer(originalGrid, contours, xOrigin,
                yOrigin, xGridRatio, yGridRatio, xMin, xMax, yMin, yMax,
                clampOn, valMax, valMin);
        analyzer.recomputeGrid();
        result = analyzer.getFinalResultData();

        final float threshold = 0.01f;
        float expval;
        float actual;
        boolean failed = false;
        for (int i = 0; i < XDIMBIG; i++) {
            for (int j = 0; j < YDIMBIG; j++) {
                actual = result.get(i, j);
                expval = expected.get(i, j);
                if (Math.abs(actual - expval) > threshold) {
                    System.out.println("Grid discrepancy at (" + i + "," + j
                            + "): expected " + expval + " but value was "
                            + actual);
                    failed = true;
                }
            }
        }
        if (failed) {
            fail("Grid discrepancies were encountered. Consult output for details.");
        }
    }
}
