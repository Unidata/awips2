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
package com.raytheon.viz.gfe.contours;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DInteger;
import com.raytheon.viz.gfe.contours.util.CLine;
import com.raytheon.viz.gfe.contours.util.ContourValueDistance;
import com.raytheon.viz.gfe.contours.util.SearchDir;
import com.raytheon.viz.gfe.contours.util.StopWatch;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * 
 * <P>
 * Legacy Documentation:
 * 
 * <PRE>
 * Recomputes grid point values in the vicinity of modified contours.
 * If all contours are modified (or new) all grid point values are computed
 * from the contour values.
 * This is used to create a new grid of data values after the user has
 * edited a set of contour lines created from some data grid.
 * (The method to edit contour lines is not given here.)
 * The grid values in the vicinity of the modified
 * contour lines are recomputed, making a new data grid.
 * There must be no &quot;gaps&quot; in the input contours for SIRS to work; that is,
 * every contour input to SIRS must have as many points as
 * possible along the lines(without duplication), even where straight.
 * If this is not true searches for contours may &quot;see through&quot; &quot;gaps&quot;
 * in contours, which can make poor results.
 * 1999 revision to use new support classes since this was last used in 1993.
 * constructor called with
 *  SIRSGrid(const Grid2D&lt;float&gt;&amp; dataGrid, int xDim, int yDim,
 *          const SeqOf&lt;CLine&gt; &amp; contourLines,
 *          const float xOrigin, const float yOrigin,
 *          const float xGridRatio, const float yGridRatio)
 * input is
 *   dataGrid - an original grid of float values. Contours were made
 *              from this data. They were then modified by some
 *              method not part of SIRS.
 *   contourLines - a collection of contour lines stored in CLine objects.
 *              These usually are the orginal contour lines made from
 *              &quot;dataGrid&quot; and some new contour lines; or some of the
 *              original contour lines modified in value or
 *              position or both. The CLine class provides an indicator
 *              if the line is modified. New lines also noted as &quot;modified.&quot;
 *             All new contour lines means a completely new grid is computed.
 *              contourLine coordinates are in &quot;world coordinates&quot; which
 *              may or may not exactly match the grid indices of &quot;dataGrid.&quot;
 *              They might be drawing coordinates for example.
 *              (For usual cases the contour lines in some
 *               degree match the &quot;dataGrid&quot;, though as far as the computer
 *               code goes they need not be closely related.)
 *   xOrigin, yOrigin, xGridRatio, yGridRatio - used to convert dataGrid
 *              coordinates into &quot;world coordinates.&quot;  As, if you are
 *              using the output in XWindows directly. If xOrigin,yOrigin
 *              are 0.0,0.0 and xGridRatio,yGridRatio are 1.0,1.0
 *              the &quot;world&quot; coordinates are == grid indices or coordinates.
 *              This must match the coordinate system the contour line
 *              point positions are in.
 *   xmin, xmax, ymin, ymax - Main grid point coordinates outlining the
 *              &quot;Change Area.&quot;  The limits of area to be recomputed with SIRS;
 *               or boundary of area where contours were changed.  The
 *               limits should actually exceed the positions of changed
 *               contour positions by one or 2 main grid points so that
 *               there is room for a reasonable transition zone.
 *   bool smoothing - whether to invoke smooting over recomputed area
 *               (this can make noticible shifts in contour line positions)
 *               RECOMMENDED OFF.
 *   bool clampOn - if on, prevents recomputed grid values from reaching
 *               next contour level up or down from nearest contour level.
 *               Prevents new contout lines popping up in areas of
 *               large gradient. Allows user to remove max and min contours.
 *               RECOMMENDED ON.
 *   float max, min  Upper and lower limits that grid values made here by
 *               SIRS are not allowed to exceed. Shoud match ifpServer
 *               database limits per Parm, and be derived from a Parm.
 * Calling method to do SIRS.
 *    1. construct a SIRSGrid object called &quot;grid&quot;
 *    SIRSGrid grid(dataGrid, xDim, yDim,
 *      allContours, _xOrigin, _yOrigin, _xGridRatio, _yGridRatio);
 *    2. call member function grid.makeGridBySIRS()
 *       that returns the new modified grid.
 *    grid.makeGridBySIRS() does this:
 *    // do step 1: computes grid points with method of weighed
 *    // contour values (see TDL Note); no changes to unmodified areas.
 *    grid.computeWeighedContourValues();
 *    // step 2:  computes grid points as needed with method of directional
 *    // gradients (see TDL Note).
 *    grid.computeDirectionalGradients();
 *    // step 3: computes remaining grid points in &quot;shadow areas&quot;(see TDL Note)
 *    grid.computeShadowAreas();
 *    // step 4 (&quot;smoothing&quot;; not essential; see function notes for effect)
 *    if (Smoothing)
 *      grid.smoothSIRSData();
 *    // for testing only: comparing errors from different versions of SIRS
 *    // grid.makeAssesment();
 *    return grid._finalResultData; // returning data array; causes needed copy
 * This is an FSL implementation in C++ of the SIRS algorithm described in
 * &quot;The Systematic Radial Interpolated Search&quot; by David P. Ruth,
 * TDL Office Note  92-14, December 1992.
 * This implementation follows the steps in the TDL
 * article closely. The TDL note will be referred to here, rather than
 * repeating it. See that article for basic ideas and terms. Terms such as
 * &quot;step 1&quot; and &quot;directional gradient&quot; are explained there.
 * No TDL code was seen or used in creating this AFPS SIRS code.
 * This an 8-direction version of SIRS; for how to set
 * the directions see SIRSGrid.H:
 * enum searchDir {NE, E, SE, S, SW, W, NW, N, };
 * see also code with [8] below.
 * Step 2 has been improved for some special cases. See
 * computeDirectionalGradients().
 * Works if the user draws a new contour segment
 * which crosses one or more existing contours, and contour fragments.
 * The TDL SIRS concept does not handle this case.
 * Here we look for the second contour value which is closest to the
 * inner most contour's value, and use only those directions and values.
 * This  does not do any smart analysis of the data;
 * such as untangling crossing contours;
 * it just takes the contours whatever they are and makes a data grid from them
 * Of course if contours cross or are in fragments the resulting grid may
 * have some unusual data values, but they won't be too extreme,
 * nor will the process crash.
 * This implementation only recomputes grid points in sight of a contour
 * which has been modified by the editor (or new). Grid points surrounded by
 * unmodified contours are not recomputed. New version (1999) also restricts
 * grid point computation to a &quot;change area&quot; input to the constructor.
 * Data points are recomputed only if a line of sight from each grid point can
 * be drawn to a modified contour without crossing an unmodified contour.
 * The grid point must &quot;see&quot; a modified contour. In many cases very little of
 * the data grid is recomputed.
 * This implemantation design follows the TDL algorithm steps, without
 * a serious attempt at performance optimization, for example by perhaps
 * compressing more than one SIRS step into one code process.
 * This makes code easier to understand but may not the fastest possible.
 * This is not the  TDL implementation of SIRS in any way.
 * The class uses the _dataGrid, the xDim,
 * the yDim. The _dataGrid is the initial data grid which
 * was used to make the set of contours.
 * This implementation works best with &quot;subgrid&quot; contours, contours composed of
 * a sequence of positions, each of which is exactly on some vertex of the
 * subgrid, a grid overlaying the main grid (on which you wish to create the
 * data grid), but denser than it by the integer&quot;_subGridFactor&quot; (used below.)
 * This SIRS function will work on any contours, whether the contour point
 * positions are at integral subgrid points or not.
 * The positions are rounded to the nearest subgrid point (integer position in
 * the subgrid coordinate system). This is not true of the TDL version.
 * Tests show that SIRS gives best control of the kinds of grids
 * contoured in meteorology when the grids have about 20 (or more)
 * different contours.  This usually is regarded as a little too cluttered
 * for display. SIRS will work with fewer contours but with some gradual
 * degradation in SIRS' ability to exactly create the equivalent grid.
 * SIRS will work well with 15 contours across a grid. Of course if a grid
 * is fairly smooth fewer contours than 15 will do quite well. Usually
 * the kind of contour displays used in meteorology will work well
 * in support of SIRS.  If the grid is contoured well enough for the
 * human user to understand the grid details, SIRS will also be satisfactory.
 * If the contours do not show important grid details, SIRS will
 * likewise fail to make a similarly detailed grid from the contours.
 * Where contours are unmodified then the new grid
 * is exactly like the old one including any details which contours
 * do or do not show, so complex dtails in grid data will be retained
 * in areas that are not recomputed due to new contours nearby.
 * </PRE>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18Mar2008    968        MW Fegan    Initial implementation. Rehosted
 *                                      from legacy C++ SIRSGrid.H
 *                                      and SIRSGrid.C
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SIRSGrid extends AbstractGfeAnalyzer {

    /* various flags to control processing */
    boolean doSmoothing;

    /**
     * Constructor. Creates {@code SIRSGrid} object and loads available
     * information for processing.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Load all data about grid &amp; contours into the &quot;SIRSGrid&quot; data structure
     * used to support SIRS.
     * Implementation:
     * Shift input contour point positions to subgrid coordinates.
     * Truncate positions beyond grid limits to lie on edge.
     * Find and fill any gaps in contours.
     * Remove crossed-over contours.
     * Requires about 0.05 sec on the HP 750 for typical 73x73 grid and contours.
     * </PRE>
     * 
     * @param dataGrid
     *            an original grid of float values.
     * @param contourLines
     *            the contour lines to use to modify the grid
     * @param xOrigin
     *            indicates x position of grid origin
     * @param yOrigin
     *            indicates y position of grid origin
     * @param xGridRatio
     *            indicates horizontal scaling of grid
     * @param yGridRatio
     *            indicates vertical scaling of grid
     * @param xMin
     *            minimum x index of area to modify
     * @param xMax
     *            maximum x index of area to modify
     * @param yMin
     *            minimum y index of area to modify
     * @param yMax
     *            maximum y index of area to modify
     * @param smoothing
     *            whether to invoke smoothing over recomputed area.
     * @param clampOn
     *            if true, keeps grid point values bounded by neighboring
     *            contours
     * @param max
     *            maximum allowable grid value
     * @param min
     *            minimum allowable grid value
     */
    public SIRSGrid(Grid2DFloat dataGrid, List<CLine> contourLines,
            float xOrigin, float yOrigin, float xGridRatio, float yGridRatio,
            int xMin, int xMax, int yMin, int yMax, boolean smoothing,
            boolean clampOn, float max, float min) {
        super(dataGrid, xMin, xMax, yMin, yMax, clampOn, max, min);
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        this.xDim = dataGrid.getXdim();
        this.yDim = dataGrid.getYdim();

        /* setup the results grid */
        this.finalResultData = new Grid2DFloat(this.xDim, this.yDim);
        this.finalResultData.setAllValues((float) 0.0);

        /* check for trivial case of one contour */
        if (contourLines.size() == 1) {
            this.oneLine = true;
            this.oneValue = contourLines.get(0).getContourLevel();
        } else {
            this.oneLine = false;

            this.valueFound = new Grid2DInteger(this.xDim, this.yDim);
            this.valueFound.setAllValues(0);
            this.howFound = new Grid2DInteger(this.xDim, this.yDim);
            this.howFound.setAllValues(-9);

            /*
             * Note area in grid that does not need to be recomputed by SIRS,
             * and set grid point values there from "oldData" values (no
             * change).
             */
            setChangeArea();

            logger.info("limits of area to recompute are x " + this.xMin
                    + " to " + this.xMax + " , y " + this.yMin + " to "
                    + this.yMax);

            /*
             * set how many sub-grid points there are per one main grid cell
             * side
             */
            setSubGridFactor(4);

            /* read in data from modified or new contours */
            readNewContours(contourLines, xOrigin, yOrigin, xGridRatio,
                    yGridRatio);

            /* read in data from UN-modified contours */
            readOldContours(contourLines, xOrigin, yOrigin, xGridRatio,
                    yGridRatio);
        }
        stopWatch.stop();
        this.timeUsed = stopWatch.getWallClockTime();
        logger.info("    SIRSGrid cstr         " + stopWatch.getWallClockTime()
                + " seconds");
    }

    /**
     * Updates contours by integrating data from new or modified lines.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * read in data supplied by all new or modified contour lines.
     * implementation:&lt;BR&gt;
     * also fills gaps or holes in contours - very important
     * to advoid the SIRS search routine finding the wrong neighbor contour.
     * </PRE>
     * 
     * @param contourLines
     *            collection of contours to apply to grid
     * @param xOrigin
     *            indicates x position of grid origin
     * @param yOrigin
     *            indicates y position of grid origin
     * @param xGridRatio
     *            indicates horizontal scaling of grid
     * @param yGridRatio
     *            indicates vertical scaling of grid
     */
    private void readNewContours(List<CLine> contourLines, float xOrigin,
            float yOrigin, float xGridRatio, float yGridRatio) {
        /*
         * x and y width of grid in screen coordinates (often pixels) (distance
         * across xDim steps across the grid, not how many points) (result is
         * basically pixels across the width, then xDim-1 is number of cells and
         * 1/xGridRatio is pixels/cell.)
         */
        float xWrldWidth = ((this.xDim - 1)) / xGridRatio;
        float yWrldWidth = ((this.yDim - 1)) / yGridRatio;

        /*
         * conversion factors, screen to subgrid coordinates (how many subgrid
         * cells per pixel (= cells or world coordinate( )
         */
        float xTerm = xGridRatio * this.subGridFactor;
        float yTerm = yGridRatio * this.subGridFactor;

        /*
         * Now add in contour information. Trace along all contours, loading
         * information needed for SIRS. truncate positions outside grid to lie
         * in grid. find and fill gaps in contours, if there are any. load
         * arrays needed. if contour lies exactly on a grid point, set values
         * accordingly.
         * 
         * do not use old contours that have points at same position as new or
         * modified contours
         */
        int i;
        int j;
        int oldi = 0;
        int oldj = 0;

        /* loop over every input contour line */
        for (int lineIndex = 0; lineIndex < contourLines.size(); lineIndex++) {
            /* get complete information for this contour line */
            CLine cline = contourLines.get(lineIndex);

            /*
             * construct list of input contour levels
             * 
             * load first one
             */
            int clSize = this.contourLevels.size();
            /* this contour's level */
            float contourLevel = cline.getContourLevel();

            if (clSize == 0) {
                this.contourLevels.add(contourLevel);
            } else if (this.contourLevels.get(clSize - 1) != contourLevel) {
                this.contourLevels.add(contourLevel);
            }

            /*
             * use only a MODIFIED or NEW contour line if not a modified contour
             */
            if (!cline.isModified()) {
                continue;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("new line, index " + lineIndex + "  level "
                        + contourLevel);
            }

            /*
             * check for gaps in contours.
             * 
             * loop over every point on this contour line; compute subgrid point
             * indices; check for gaps in contours and set proper values in gap;
             * set proper values in subgrids at contour points.
             */
            LineString line = cline.getLineString();
            for (int ptIndex = 0; ptIndex < line.getNumPoints(); ptIndex++) {
                /* get position is world coordinates */
                Coordinate z = new Coordinate(line.getCoordinateN(ptIndex));

                /*
                 * check for input points beyond grid limits set to nearest grid
                 * limit if so. world coordinates
                 */
                if (z.x < xOrigin) {
                    z.x = xOrigin;
                } else if (z.x > xOrigin + xWrldWidth) {
                    z.x = xOrigin + xWrldWidth;
                }
                if (z.y < yOrigin) {
                    z.y = yOrigin;
                } else if (z.y > yOrigin + yWrldWidth) {
                    z.y = yOrigin + yWrldWidth;
                }

                /*
                 * compute equivalent subgrid indices (use nearest integer, not
                 * truncation)
                 */
                i = (int) Math.round(xTerm * (z.x - xOrigin));
                j = (int) Math.round(yTerm * (z.y - yOrigin));

                if (ptIndex == 0) {
                    oldi = i;
                    oldj = j;
                }
                /* Check for gaps in the contour line, and set values there. */
                /* how big is the gap in x and y directions */
                int numx = Math.abs(i - oldi); // typically 1 or 2
                int numy = Math.abs(j - oldj); // typically 1 or 2
                if (numx > 1 || numy > 1) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("     gap from " + oldi + "," + oldj
                                + " to " + i + "," + j);
                    }

                    int maxStep = Math.max(numx, numy);
                    if (logger.isDebugEnabled()) {
                        logger.debug("     gap size " + maxStep);
                    }
                    int signdi = (i - oldi > 0) ? 1 : -1;
                    int signdj = (j - oldj > 0) ? 1 : -1;

                    if (maxStep > 0) { // should ALWAYS be true if you reach
                        // here
                        for (int nn = 1; nn < maxStep; nn++) {
                            /* convert to x,y position in gap (nearest integer) */
                            int m = oldi + signdi
                                    * Math.round(nn * (float) numx / maxStep);
                            int n = oldj + signdj
                                    * Math.round(nn * (float) numy / maxStep);
                            /* set SIRS search control values in gaps */
                            this.onContour.set(m, n, lineIndex + 1);
                            this.contourValue.set(m, n, contourLevel);
                            this.contourNew.set(m, n, (byte) 1);

                        }
                    }
                }/* end if checking for gaps in contour */
                oldi = i;
                oldj = j;

                /*
                 * These next three lines set the key values SIRS works with:
                 * which subgrid points are on contour lines; what is the
                 * contour level there, and is it a new or modified contour.
                 */
                this.onContour.set(i, j, lineIndex + 1);
                /* which contour level */
                this.contourValue.set(i, j, contourLevel);
                /* all contour lines that reach here are modified */
                this.contourNew.set(i, j, (byte) 1);

                /*
                 * Set point values where contours happen to be already defined
                 * on the main grid points. This prevents some odd values
                 * computed right on contours, and also speeds things, by a very
                 * small amount, roughly 5%.
                 */
                if (i % this.subGridFactor == 0 && j % this.subGridFactor == 0) {
                    i = i / this.subGridFactor; // main grid index i
                    j = j / this.subGridFactor; // main grid index j
                    this.finalResultData.set(i, j, contourLevel);
                    this.valueFound.set(i, j, 1);
                    this.howFound.set(i, j, 9); // 9 = value copied new contour
                    // level
                }
            } /* end loop over one contour line's points */
            logger.debug("   modified cont line " + lineIndex + " at level "
                    + contourLevel);
        } /* end loop over all contour lines */
        if (logger.isDebugEnabled()) {
            logger.debug("  contour levels input = " + this.contourLevels);
        }
    }

    /**
     * Read in data from unchanged contours.
     * 
     * @param contourLines
     *            collection of contours to apply to grid
     * @param xOrigin
     *            indicates x position of grid origin
     * @param yOrigin
     *            indicates y position of grid origin
     * @param xGridRatio
     *            indicates horizontal scaling of grid
     * @param yGridRatio
     *            indicates vertical scaling of grid
     */
    private void readOldContours(List<CLine> contourLines, float xOrigin,
            float yOrigin, float xGridRatio, float yGridRatio) {
        int i;
        int j;
        int oldi = 0;
        int oldj = 0;

        /* keep list of added positions for each line (temporary) */
        ArrayList<Integer> iList = new ArrayList<Integer>();
        ArrayList<Integer> jList = new ArrayList<Integer>();
        /*
         * x and y width in world coordinates (often pixels) (distance across
         * xDim steps across the grid, not how many points) (result is basically
         * pixels across the width, when xDim-1 is number of cells and
         * 1/xGridRtion is pixels/cell
         */
        float xWrldWidth = ((this.xDim - 1)) / xGridRatio;
        float yWrldWidth = ((this.yDim - 1)) / yGridRatio;

        /*
         * conversion factors, world to subgrid coordinates (how many subgrid
         * cells per pixel (= cells per world coordinate) )
         */
        float xTerm = xGridRatio * this.subGridFactor;
        float yTerm = yGridRatio * this.subGridFactor;

        /* loop over every contour line for this data grid */
        for (int lineIndex = 0; lineIndex < contourLines.size(); lineIndex++) {
            /* reset temporary list */
            iList.clear();
            jList.clear();

            /* get complete information for this contour line */
            CLine cline = contourLines.get(lineIndex);

            /* if this line is modified go try the next contour line in for loop */
            if (cline.isModified()) {
                continue;
            }

            /* this contour's level */
            float contourLevel = cline.getContourLevel();

            if (logger.isDebugEnabled()) {
                logger.debug("   load old contour line, index " + lineIndex
                        + "  level " + contourLevel);
            }

            /*
             * Check for crossing contours, and for gaps in contours. loop over
             * every point on this contour line; compute subgrid point indices;
             * check for gaps in contours and set proper values in gap; set
             * proper values in subgrids at contour points.
             */
            LineString line = cline.getLineString();
            for (int ptIndex = 0; ptIndex < line.getNumPoints(); ptIndex++) {
                /* get position in world coordinates */
                Coordinate z = new Coordinate(line.getCoordinateN(ptIndex));

                /*
                 * check for input points beyond grid limits; set to nearest
                 * grid limit if so. world coordinates (pixels)
                 */
                if (z.x < xOrigin) {
                    z.x = xOrigin;
                } else if (z.x > xOrigin + xWrldWidth) {
                    z.x = xOrigin + xWrldWidth;
                }
                if (z.y < yOrigin) {
                    z.y = yOrigin;
                } else if (z.y > yOrigin + yWrldWidth) {
                    z.y = yOrigin + yWrldWidth;
                }

                /*
                 * compute equivalent subgrid indices (use nearest integer, not
                 * truncation
                 */
                i = (int) Math.round(xTerm * (z.x - xOrigin));
                j = (int) Math.round(yTerm * (z.y - yOrigin));

                /*
                 * Crossing Contours Test. If this point on an old contour line
                 * is also the same as a point on a new or modified contour
                 * line, quit adding in this old contour line; remove data
                 * already set from this line , and go try next line.
                 */
                if (this.contourNew.get(i, j) == 1) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("  crossing contour found, index"
                                + lineIndex + ",  new level "
                                + this.contourValue.get(i, j) + " position "
                                + i + ", " + j);
                    }
                    iList.add(i);
                    jList.add(j);
                    /*
                     * remove points from this crossed old line already added to
                     * SIRS data set
                     */
                    removePoints(iList, jList);
                    break; /* break off point and go to next line. */
                }

                if (ptIndex == 0) {
                    oldi = i;
                    oldj = j;
                }

                /* Check for gaps in the contour line, and set values there. */
                /* how big is the gap is x and y directions */
                int numx = Math.abs(i - oldi); // typically 1 or 2
                int numy = Math.abs(j - oldj); // typically 1 or 2
                if (numx > 1 || numy > 1) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("     gap from " + oldi + ", " + oldj
                                + " to " + i + ", " + j);
                    }
                    int maxStep = Math.max(numx, numy);
                    if (logger.isDebugEnabled()) {
                        logger.debug("     gap size " + maxStep);
                    }

                    /* set the signs of i & j */
                    int signdi = (i - oldi > 0) ? 1 : -1;
                    int signdj = (j - oldj > 0) ? 1 : -1;

                    if (maxStep > 0) { /*
                                        * should ALWAYS be true if you reach
                                        * here
                                        */
                        for (int nn = 1; nn < maxStep; nn++) {
                            /* convert to x,y position in gap */
                            int m = oldi + signdi
                                    * Math.round(nn * (float) numx / maxStep);
                            int n = oldj + signdj
                                    * Math.round(nn * (float) numy / maxStep);

                            if (logger.isDebugEnabled()) {
                                logger
                                        .debug("       fill in gap with new pt at "
                                                + m + ", " + n);
                            }
                            /* check is this crosses another contour */
                            if (this.contourNew.get(m, n) == 1) {
                                iList.add(m);
                                jList.add(n);
                                /*
                                 * remove data from this crossed old line
                                 * already added in
                                 */
                                removePoints(iList, jList);
                                /* go to next line */
                                break;
                            }
                            /*
                             * set SIRS control values where contours should be
                             * in gaps
                             */
                            this.onContour.set(m, n, lineIndex + 1);
                            this.contourValue.set(m, n, contourLevel);
                            this.contourNew.set(m, n, (byte) 0);
                            iList.add(m);
                            jList.add(n);

                        }
                    }
                } /* end if checking for gaps in contour */
                oldi = i;
                oldj = j;

                /*
                 * these three lines set the key values SIRS works with. which
                 * subgrid points are on contour lines; what is the contour
                 * level there, and is it a nes of modified contour.
                 */
                this.onContour.set(i, j, lineIndex + 1);
                this.contourValue.set(i, j, contourLevel);
                this.contourNew.set(i, j, (byte) 0);
                iList.add(i);
                jList.add(j);

            } /* end loop over one contour line's points */
            if (logger.isDebugEnabled()) {
                logger.debug("   done with old cont line, index " + lineIndex
                        + ", level " + contourLevel);
            }
        } /* end loop over all unmodified contour lines */
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.gfe.IGfeAnalyzer#recomputeGrid()
     */
    @Override
    public Grid2DFloat recomputeGrid() {
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        /*
         * if have trivial case of only one contour line input, set all grid
         * points to its value.
         */
        if (this.oneLine) {
            this.finalResultData.setAllValues(this.oneValue);
            try {
                return this.finalResultData.clone();
            } catch (CloneNotSupportedException e) {
                return new Grid2DFloat();
            }
        }
        /* normal processing */
        /*
         * step 1: computes grid points with method of weighted contour values
         * (see TDL Note); no changes to unmodified areas.
         */
        computeWeighedContourValues();

        /*
         * step 2: computes grid points as needed with method of directional
         * gradients (see TDL Note).
         */
        computeDirectionalGradients();

        /*
         * step 3: computes remaining grid points in "shadow areas" (see TDL
         * Note)
         */
        computeShadowAreas();

        /*
         * step 4: "smoothing" (NOT generally necessary)
         */
        if (this.doSmoothing) {
            smoothSIRSData();
        }

        /*
         * make sure computed grid values do not exceed limits given in input
         */
        trimGridValues();

        /*
         * for testing only: comparing errors from different versions of SIRS,
         * valid arguments are 1, 2 or 3.
         */
        // makeAssessment(1);
        stopWatch.stop();

        /* add recompute grid time used to cstr time used */
        this.timeUsed += stopWatch.getWallClockTime();

        logger.info("    FSL SIRSGrid recomputing grid used " + this.timeUsed
                + " seconds");

        try {
            return this.finalResultData.clone();
        } catch (CloneNotSupportedException e) {
            return new Grid2DFloat();
        }
    }

    /**
     * Computes grid values using distance-weighted average of nearest contour
     * values.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Step 1 in SIRS; see TDL Note for details.
     * For points on grid between contours of differing values and in sight of
     * a modified contour, compute a new grid point value by the distance-weighed
     * average of the nearest contour values, as seen
     * looking from the grid point in the eight search directions.
     * For typical real data, this is where most grid point values are computed.
     * Implementation:
     * Check each unassigned point on the main grid.
     * If in unmodifed area, copy data from existing data grid.
     * If in sight of modified contours,
     * see if its value can be computed with weighed contour value averaging
     * (TDL Note, equation (1)), and do so.
     * It may not be possible to determine some grid point values here,
     * and steps 2 and 3 may apply.
     * The function is slightly complicated.   Two actions
     * are combined here - copying old data for
     * main grid points which are not in sight of a modified contour, and
     * computing a new grid value if in sight of a modified contour.
     * </PRE>
     */
    public void computeWeighedContourValues() {
        boolean modContourFound; // was a modified contour seen in this
        // direction
        boolean modContourPoint; // was any modified contour seen in any dir
        int numDiffValues;

        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        /* used for averaging all values seen in search: */
        double cPrevious;
        double top;
        double bottom;

        int numStep1 = 0; /* counter of how many points solved here */

        /* check all main grid points */
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                if (this.valueFound.get(i, j) != 1) { /*
                                                       * no value set for this
                                                       * point yet
                                                       */
                    modContourPoint = false;
                    numDiffValues = 0;
                    top = 0.0;
                    bottom = 0.0;
                    cPrevious = -9999.0;

                    /* search in all eight directions from this main grid point */
                    for (SearchDir dir : SearchDir.values()) {

                        /*
                         * Find nearest contour value and distance in this
                         * direction
                         */
                        ContourValueDistance cvd = new ContourValueDistance();
                        modContourFound = findNearestContour(i, j, dir, cvd);

                        /* if a modified contour was seen */
                        if (modContourFound && cvd.distance != 0.0) {
                            modContourPoint = true;
                        }

                        /* compile data about contours seen */

                        /* compute average value using ALL "seen" values */
                        if (cvd.distance != 0.0) {
                            /* count how many different contour values seen */
                            if (numDiffValues == 0 || cvd.value != cPrevious) {
                                numDiffValues++;
                            }
                            top += cvd.value / cvd.distance;
                            bottom += 1.0 / cvd.distance;
                            cPrevious = cvd.value;

                        }
                    } /* end of search in all 8 directions from this point */
                    /*
                     * possible cases now: IF a modified contour seen, and 2 or
                     * more different contours were seen, step 1 applies, so
                     * compute result.
                     */

                    /* compute average using all values seen */
                    if (modContourPoint && numDiffValues > 1 && bottom != 0.0) {
                        /*
                         * compute the distance-weighted average of all contour
                         * values.
                         */
                        this.finalResultData.set(i, j, (float) (top / bottom));
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, 1); /* found by step 1 */
                        /* add up how many data points determined by step 1; */
                        numStep1++;
                    } else if (!modContourPoint) {
                        /*
                         * if this point is not in sight of a modified contour
                         * copy data from old grid (no change)
                         */
                        this.finalResultData.set(i, j, this.oldData.get(i, j));
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(1, j, -1); /* means old data copied */
                    }
                } /* end if no value set for this point yet */
            } /* end loop on all grid (i,j) points - j loop */
        } /* end loop on all grid (i,j) points - i loop */
        if (logger.isDebugEnabled()) {
            logger
                    .debug("SIRS: Number of grid point values found by step 1 is "
                            + numStep1);
        }
        /*
         * By now the only points in the grid which do not have values should be
         * done by step 2 or 3.
         */
        stopWatch.stop();
        if (logger.isDebugEnabled()) {
            logger.debug("    SIRS step 1 " + stopWatch.getWallClockTime()
                    + " seconds");
        }
    }

    /**
     * Computes grid values based on gradients made from contour values in each
     * direction from the value.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Step 2 in SIRS; see TDL Note for details.
     * Implementation:
     * Case of grid points that can only &quot;see&quot; contours of the
     * same value, or see one contour with its single value value.
     * Computing new grid point values in such cases from average seen contour
     * values will of course only give exactly the result exactly
     * the same as the single contour value.
     * Rather use gradients made from contour values in each direction.
     * This is the most complex step in SIRS, with lots of checking against
     * possible limits.
     * Check each unassigned point on the main grid.
     * See if its value can be computed with directional gradients
     * (TDL Note, step 2), and try to do so if possible.
     * For typical real data this is used for some 10% of all data points,
     * if the contour interval is about 1/20 of the data range.
     * This impementation of step 2 has two features not seen in Dave Ruth's
     * concept paper:
     * 1. Every value estimate of grid data value is subject to range limit
     * restriction; see comment &quot;NEW&quot;. This reduces errors significantly.
     * 2. More elaborate restriction checking is applied to final result. This
     * does all that Ruth's code does, and more in rare cases of range limits
     * bracketing the local contour value. Yes this can happen with real valid data
     * </PRE>
     */
    public void computeDirectionalGradients() {
        double[] c1 = new double[8]; // nearest contour values found in each
        // direction
        double[] d1 = new double[8]; // distance to a contour found in each
        // direction
        double[] grad = new double[8]; // gradient in each direction
        double[] v = new double[8]; // first estimate of value, from TDL Note
        // eq. 2b.
        double[] r = new double[8]; // "range limit"
        double[] vp = new double[8]; // "constrained value" for some v[]

        int numcavg;
        int numStep2 = 0;

        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        if (logger.isDebugEnabled()) {
            logger.debug(" do step 2 "
                    + ((!this.clampOn) ? "" : " clamp is on"));
        }

        /*
         * first extend the upper and lower contour level limits by one step to
         * provide contour level limiting value at the ends, if desired
         */
        if (this.clampOn && this.contourLevels.size() > 1) {
            if (logger.isDebugEnabled()) {
                logger.debug(" clamp is on ; extend level");
            }
            float vala = this.contourLevels.get(0);
            float valb = this.contourLevels.get(1);
            float delta = Math.abs(valb - vala);
            if (valb > vala) {
                this.contourLevels.add(0, vala - delta);
            }
            if (valb < vala) {
                this.contourLevels.add(0, vala + delta);
            }

            int size = this.contourLevels.size();
            vala = this.contourLevels.get(size - 2);
            valb = this.contourLevels.get(size - 1);
            delta = Math.abs(valb - vala);
            if (valb > vala) {
                this.contourLevels.add(valb + delta);
            }
            if (valb < vala) {
                this.contourLevels.add(valb - delta);
            }
        }

        /* check all the main grid points */
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                /* if value not yet found for this point */
                if (this.valueFound.get(i, j) != 1) {
                    double cavg = 0.0;
                    numcavg = 0;
                    for (int kk = 0; kk < 8; kk++) {
                        grad[kk] = 0.0;
                        r[kk] = 0.0;
                    }
                    /*
                     * search in all directions from this main grid point do
                     * calculations through TDL eq. 2d.
                     */
                    for (SearchDir dir : SearchDir.values()) {
                        int k = dir.ord;

                        /*
                         * Find first and second contours in direction (2 ret.)
                         * or contour plus grid edge (1 returned).
                         */
                        ContourValueDistance cvda = new ContourValueDistance();
                        ContourValueDistance cvdb = new ContourValueDistance();
                        int numFound = findDistantContours(i, j, dir, cvda,
                                cvdb);
                        /* if contours were found */
                        if (numFound == 1 || numFound == 2) {
                            double ca = cvda.value;
                            double da = cvda.distance;
                            c1[k] = ca;
                            d1[k] = da;

                            /*
                             * compose average of surrounding contour values
                             * (every "ca" should be the same for a valid step2
                             * point)
                             */
                            cavg += ca;
                            numcavg++;

                            /*
                             * if ca = cb then in a shadow zone (do step 3); (da
                             * == db makes zero divide but should never occur -
                             * that would mean two contours at the same point.)
                             */

                            /* if saw TWO DIFFERENT contours in this direction */
                            if (cvdb.value != cvda.value
                                    && cvda.distance != cvdb.distance) {
                                grad[k] = cvdb.gradient(cvda); // TDL eq 2a.
                                v[k] = ca - grad[k] * da; // TDL eq 2b.
                                r[k] = (2.0 * ca) - cvdb.value; // TDL eq 2c.

                                /*
                                 * create the "constrained value" vp (eq. 2d)
                                 * used for "adjusted directional values": (eq.
                                 * 2e)
                                 */
                                if (Math.abs(ca - v[k]) > Math.abs(ca - r[k])) {
                                    vp[k] = r[k]; // TDL eq. 2d.
                                    /*
                                     * Copied from the C++: Dave Ruth says this
                                     * next line is not used in SIRS as he does
                                     * it; but in all my tests it reduces error
                                     * of fit to initial grid by about one
                                     * third.
                                     */
                                    v[k] = r[k];
                                } else {
                                    vp[k] = v[k]; // TDL eq. 2d.
                                }
                            }
                        } /* end if contours were found */
                    } /* end of search in all directions - first search */
                    double top = 0.0;
                    double bottom = 0.0;

                    /* loop in all directions for eq. 2e, 2f, and terms for 2g. */
                    for (SearchDir dir : SearchDir.values()) {
                        int k = dir.ord;
                        /*
                         * if have a step 2 value in this direction, compute
                         * "adjusted directional values" where needed:
                         */
                        if (grad[k] != 0.0) {
                            /*
                             * check for case of no contours in opposite
                             * direction:
                             */
                            ContourValueDistance cvda = new ContourValueDistance();
                            ContourValueDistance cvdb = new ContourValueDistance();
                            int numFound = findDistantContours(i, j, dir
                                    .reverse(), cvda, cvdb);
                            if (numFound == 0) {
                                /*
                                 * no contour seen in opposite direction, so do
                                 * 2e
                                 */
                                double vpp = (0.5 * vp[k])
                                        + 0.5
                                        * (c1[k] - (d1[k] / (d1[k] + cvda.distance))
                                                * (c1[k] - r[k]));
                                if (Math.abs(c1[k] - vpp) < Math.abs(c1[k]
                                        - v[k])) {
                                    v[k] = vpp; // TDL eq. 2f.
                                }
                            }
                        }
                        /* compute terms for final weighed average */
                        if (grad[k] != 0.0 && d1[k] != 0.0) {
                            top += v[k] / d1[k];
                            bottom += 1.0 / d1[k];
                        }
                    } /* end of second loop in all directions */
                    /*
                     * compute weighed directional average; restrict by range
                     * limits as needed;
                     */
                    cavg /= numcavg; // contour value near this point
                    if (bottom != 0.0) {
                        /* make the average of weighed directional gradients */
                        double avgVal = top / bottom;

                        /*
                         * TDL Note: "The average value of the gridpoint is
                         * constrained so as not to exceed the LEAST restrictive
                         * range limit in any direction computed in Equation
                         * 2c."
                         * 
                         * In some rare cases have range limits above and below
                         * cavg. Have to handle all possibilities.
                         */

                        /* find high and low range limits */
                        double rMax = cavg;
                        double rMin = cavg;
                        for (SearchDir dir : SearchDir.values()) {
                            int k = dir.ord;
                            if (grad[k] != 0.0) {
                                if (r[k] > rMax) {
                                    rMax = r[k];
                                }
                                if (r[k] < rMin) {
                                    rMin = r[k];
                                }
                            }
                        }

                        /*
                         * If avgVal in between range limits it is ok. or if
                         * avgVal is between rMin and cavg it is ok; else
                         * restrict to appropriate range limit.
                         */
                        if (avgVal <= rMax && avgVal >= rMin) {
                            this.finalResultData.set(i, j, (float) avgVal);
                        } else if (cavg < rMin
                                && (avgVal <= rMin && avgVal >= cavg)) {
                            this.finalResultData.set(i, j, (float) avgVal);
                        } else if (cavg > rMax
                                && (avgVal <= cavg && avgVal >= rMax)) {
                            this.finalResultData.set(i, j, (float) avgVal);
                        } else if (avgVal < rMin) {
                            this.finalResultData.set(i, j, (float) rMin);
                        } else if (avgVal > rMax) {
                            this.finalResultData.set(i, j, (float) rMax);
                        }

                        /*
                         * is desire clamp between adjacent contour values make
                         * sure the finalResult is inside the same pair of
                         * contour levels that limit cavg; cavg should be the
                         * value of the surrounding level
                         */
                        if (this.clampOn && this.contourLevels.size() > 1) {
                            /* loop through the contour levels */
                            for (int ii = 1; ii < this.contourLevels.size() - 1; ii++) {
                                if (cavg > this.contourLevels.get(ii - 1)
                                        && cavg < this.contourLevels
                                                .get(ii + 1)) {
                                    /* case of increasing levels */
                                    if (this.finalResultData.get(i, j) <= this.contourLevels
                                            .get(ii - 1)) {
                                        this.finalResultData
                                                .set(i, j, this.contourLevels
                                                        .get(ii - 1) + 0.001f);
                                    } else if (this.finalResultData.get(i, j) >= this.contourLevels
                                            .get(ii + 1)) {
                                        this.finalResultData
                                                .set(i, j, this.contourLevels
                                                        .get(ii + 1) - 0.001f);
                                    }
                                } else if (cavg < this.contourLevels
                                        .get(ii - 1)
                                        && cavg >= this.contourLevels
                                                .get(ii + 1)) {
                                    /* case of decreasing levels */
                                    if (this.finalResultData.get(i, j) >= this.contourLevels
                                            .get(ii - 1)) {
                                        this.finalResultData
                                                .set(i, j, this.contourLevels
                                                        .get(ii - 1) - 0.001f);
                                    } else if (this.finalResultData.get(i, j) <= contourLevels
                                            .get(ii + 1)) {
                                        this.finalResultData
                                                .set(i, j, this.contourLevels
                                                        .get(ii + 1) + 0.001f);
                                    }
                                }
                            }
                            if (logger.isDebugEnabled()) {
                                logger.debug("     end clamp with fr = "
                                        + this.finalResultData.get(i, j));
                            }
                        } /* end of clamp */
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, 2); // the value was found by
                        // step 2
                        numStep2++;
                    }/* end if bottom != 0 */
                } /* end if value not yet found for this point */

            } /* end check all the main grid points - j loop */

        } /* end check all the main grid points - i loop */
        if (logger.isDebugEnabled()) {
            logger
                    .debug("SIRS: Number of grid point values found by step w is "
                            + numStep2);
            logger.debug("  extended contour levels "
                    + this.contourLevels.toString());
        }
        stopWatch.stop();
        if (logger.isDebugEnabled()) {
            logger.debug("    SIRS step 2 " + stopWatch.getWallClockTime()
                    + " seconds");
        }

    }

    /**
     * Computes grid values not found by {@link #computeDirectionalGradients()}
     * and {@link #computeWeighedContourValues()}.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * SIRS step 3. See TDL Note.
     * Fills in spots not done by steps 1 and 2,
     * by averaging surrounding valid grid values made by previous steps.
     * </PRE>
     */
    public void computeShadowAreas() {
        int numStep3 = 0;

        /* loop on all main grid points */
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                /* if value not yet computed */
                if (this.valueFound.get(i, j) != 1) {
                    int na = 0;
                    double sumValues = 0.0;

                    /*
                     * search in all directions from this point; find nearest
                     * good value in that direction, if any
                     */
                    for (SearchDir dir : SearchDir.values()) {
                        ContourValueDistance cvd = new ContourValueDistance();
                        boolean found = findAdjacentValue(i, j, dir, cvd);
                        if (found) {
                            sumValues += cvd.value;
                            na++;
                        }
                    } /* end search in all directions */
                    if (na != 0) {
                        this.finalResultData
                                .set(i, j, (float) (sumValues / na));
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, 3); // the value was found by
                        // step 3
                        numStep3++;
                    }
                } /* end if value not yet computed */
            } /* end of loop on all main grid points - j indexed loop */
        } /* end of loop on all main grid points - i indexed loop */
        if (logger.isDebugEnabled()) {
            logger.debug("SIRS: Number of grid point values found by step 3 is"
                    + numStep3);
        }
    }

    /**
     * Performs a smoothing of grid values in the change area.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * smooth recomputed values in change area.
     * Not a big influence; but does really help avoid single erratic data
     * points that can lead to complex contours over small areas (mainly a
     * problem if you make three or more successive contour steps on
     * the same change area.)
     * NOTE PHENOMENON: averaging moves contours away from the steeper
     * gradient. Usual effect is that a new grid's contour is not
     * exactly under the modified contour used to make it - it is offset
     * on the side ways from a steeper gradient. Often &quot;behind&quot;
     * a moved contour.
     * Implementation:
     * simple idea but
     * much of the code is concerned with dont-go-beyond-grid-edge checking.
     * 
     * <PRE>
     */
    public void smoothSIRSData() {
        /* construct original values array plus one cell around edge */

        /* grid index limits for new array */
        int newXMin = Math.max(this.xMin - 1, 0);
        int newXMax = Math.min(this.xMax + 1, this.xDim - 1);
        int newYMin = Math.max(this.yMax - 1, 0);
        int newYMax = Math.min(this.yMax + 1, this.yDim - 1);

        /*
         * NOTE: Don't know if this is the most efficient way to do this, but by
         * delegating to the Grid2D class allows for use of the nio based
         * methods used by that class.
         */
        Grid2DFloat orig = this.finalResultData.subGrid(newXMin, newYMin,
                newXMax, newYMax);

        /*
         * 9 point smoothing of values in change area; greatest weight on center
         * value of each 3X3 group: Legacy comments: (possible weights tested:
         * A-B 2 - 16 4 - 10.667 9-9=equal weights of 1/9) Weights tested here
         * all give similar results; all significantly different from no
         * smoothing results.
         */
        for (int i = newXMin + 1; i < newXMax; i++) {
            for (int j = newYMin + 1; j < newYMax; j++) {
                int ii = i - newXMin;
                int jj = j - newYMin;
                double temp = orig.get(ii, jj)
                        / 4
                        + (orig.get(ii - 1, jj + 1) + orig.get(ii, jj + 1)
                                + orig.get(ii + 1, jj + 1)
                                + orig.get(ii - 1, jj) + orig.get(ii + 1, jj)
                                + orig.get(ii - 1, jj - 1)
                                + orig.get(ii, jj - 1) + orig.get(ii + 1,
                                jj - 1)) / 10.667;
                this.finalResultData.set(i, j, (float) temp);
            }
        }
    }

    /**
     * Computes measure of error for newly computed grid points.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * computes measures of error of new grid point values computed
     * by SIRS and prints them; not used operationally.
     * Not an assessment of entire grid.
     * </PRE>
     * 
     * @param step
     *            the SIRS step to assess (1, 2, or 3)
     */
    public void makeAssessment(int step) {
        int numFilled = 0; // counter of how many grid points were computed by
        // SIRS
        for (int i = 1; i < this.xDim; i++) {
            for (int j = 1; j < this.yDim; j++) {
                int how = this.howFound.get(i, j);
                if (how > 0 && how < 4) {
                    numFilled++;
                }
            }
        }
        logger.info("SIRS: percent of whole grid recomputed by SIRS = "
                + (100.0 * numFilled / (this.xDim * this.yDim)) + "%");

        double diff = 0.0;
        double avgDiff = 0.0;
        double maxDiff = 0.0;
        double avgValue = 0.0;
        float maxValue = this.oldData.get(0, 0);
        float minValue = this.oldData.get(0, 0);
        double error = 0.0;
        int nSum = 0;

        /* find size of values in grid */
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                float tempVal = this.oldData.get(i, j);
                avgValue += tempVal;
                nSum++;
                if (tempVal > maxValue) {
                    maxValue = tempVal;
                }
                if (tempVal < minValue) {
                    minValue = tempVal;
                }
            }
        }
        avgValue /= nSum;
        /*
         * compute error measures legacy code had commented out printf's - not
         * included here
         */
        nSum = 0;
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                if (this.howFound.get(i, j) == step) {
                    error = this.oldData.get(i, j)
                            - this.finalResultData.get(i, j);
                    /* difference from true value as a percent of avg grid value */
                    diff = 100.0 * Math.abs(error) / avgValue;
                    avgDiff += error;
                    nSum++;
                    if (diff > maxDiff) {
                        maxDiff = diff;
                    }
                }
            }
        }
        /*
         * NOTE: there was a loop to print an indication of how value was found
         * - it has been eliminated.
         */
        if (nSum != 0) {
            avgDiff /= nSum;
        }
        /*
         * NOTE: average error, and max error as a percent of data range, prove
         * to be the best guide to SIRS performance, allowing equal comparison
         * between all knids of data.
         */
        float range = maxValue - minValue;
        logger.info("      step " + step + "test:");
        logger.info("SIRS: avg grid value is " + avgValue);
        logger.info("SIRS: max grid value is " + maxValue);
        logger.info("SIRS: min grid value is " + minValue);
        logger.info("SIRS: data range is " + (range));
        logger.info("      max SIRS error this step is "
                + (maxDiff * avgValue / range) + "%");
        logger.info("      avg SIRS error this step is "
                + (avgDiff * avgValue / range) + "%");
    }
}
