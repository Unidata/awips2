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
import java.util.Collections;
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
 * Computes a grid of values from contours. Rehosted from ContourAnalyzer.H and
 * ContourAnalyzer.C by Stuart Wier.
 * <P>
 * <em>Note:</em> In doing the rehost, all logging has been performed using a
 * single logging facility {@code log4j}. The {@code searchDir enum} has been
 * moved into a separate Java enumeration
 * {@link com.raytheon.viz.gfe.contours.util.SearchDir SearchDir}. Finally, the
 * {@code SeqOf<T>} class has been replaced by {@code java.util.ArrayList} which
 * provides the required functionality.
 * <P>
 * Legacy Documentation:
 * 
 * <PRE>
 * Computes a grid of values from contours.
 * Uses a simple analysis scheme. Contour points with values are used as
 * the input or control for the analysis.
 * For any grid point to be computed, the analysis uses only contours
 * that can be &quot;seen&quot; from the grid point to prevent farther contours
 * from forcing the grid value to a value different outside the bounds of the
 * adjacent contours. &quot;Seen&quot; means able to reach a contour with a
 * straight line from the grid point without crossing another contour first.
 * Grid point values in most cases are computed by an analysis, using
 * contour line points (with position and value for each point) as
 * input to the analysis. The weighing function is one over r squared.
 * Only the nearest contours are used to supply input for the analysis
 * for any one grid point.
 * To prevent recomputing the entire grid for a few changes in contours,
 * this used the concept of new and old contours. New contours are
 * those created or modified just before analysis was requested.
 * Old contours are unmodifed and should be derived from the grid point
 * values supplied; the new contours will not match the grid point values
 * supplied as input to this method. Only grid points that can &quot;see&quot; new
 * contours are recomputed.  Areas in the grid were the
 * contours are not modified are not changed (the original grid is
 * available through the constructor).
 * A special case is if all contours are new; then redo the entire grid.
 * If you want to make a all-new grid, be sure that all contours are
 * flagged as new.
 * In the case of grid points that can only &quot;see&quot; one contour, compute
 * the grid value from gradients made from the nearest and next nearest
 * contours. In this case true analysis would converge on the average
 * contour value, contrary to the user's expectations of
 * implied (but not drawn) gradients. Of course if the only contour seen
 * is an old contour, then no recomputation is performed.
 * Note that some grid points can be in a position where no contours are seen,
 * since the seeing routine only looks in eight directions. This only happens
 * near the margins of the grid and if the contours supplied cover only
 * part of the grid. IN this case if all new contours are supplied the
 * grid point values are recomputed, using averages of neighbor points.
 * If the grid is not all new, then nothing is recomputed in these points.
 * The user may supply limits of an area to recompute. This has two
 * advantages, but is optional. The advantages are 1. less time used,
 * and 2. not recomputing grid points outside an area of interest
 * or outside the area where changes were made to contours by editing
 * (a new or modified contour may be long and wander all over the grid
 *  whereas the edits to it may be in a small area of the grid).
 * SIRS techniques are used to search for contours.
 * Note this code is coded for 8 search directions for the SIRS-type
 * radial searching to look for contours &quot;seen by&quot; grid points. Making
 * more directions requires recoding the three &quot;find&quot; functions of this class,
 * quite an extensive job.
 * constructor called with
 *  ContourAnalyzer(const Grid2D&lt;float&gt;&amp; dataGrid, int xDim, int yDim,
 *          const SeqOf&lt;CLine&gt; &amp; contourLines,
 *          const float xOrigin, const float yOrigin,
 *          const float xGridRatio, const float yGridRatio)
 * input is
 *   dataGrid - an original grid of float values. Contours were made
 *              from this data. They were then modified.
 *   contourLines - a collection of contour lines stored in CLine objects.
 *              These usually are the original contour lines made from
 *              &quot;dataGrid&quot; and some new contour lines; or some of the
 *              original contour lines modified in value or
 *              position or both. The CLine class provides an indicator
 *              if the line is modified. New lines also noted as &quot;modified.&quot;
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
 *              &quot;Change Area.&quot;  The limits of area to be recomputed;
 *               or boundary of area where contours were changed.  The
 *               limits should actually exceed the positions of changed
 *               contour positions by one or two main grid points so that
 *               there is room for a reasonable transition zone.
 *   bool clampOn - if on, prevents recomputed grid values from exceeding
 *               next contour level up or down from nearest contour level.
 *               Prevents new contour lines popping up in areas of
 *               large gradient. Allows user to remove max and min contours
 *               when editing. Otherwise this class would put them back.
 *               RECOMMENDED TRUE.
 *   float max, min  - Upper and lower limits that grid values made here
 *               are not allowed to exceed. Should match ifpServer
 *               database limits per Parm, and be derived from a Parm.
 * Calling method to do make a grid from contours
 *    1. construct a ContourAnalyzer object called for example &quot;analyzer&quot;
 *    ContourAnalyzer analyzer(dataGrid, xDim, yDim,
 *      allContours, _xOrigin, _yOrigin, _xGridRatio, _yGridRatio);
 *    2. call member function analyzer.recomputeGrid()
 *       that returns the new modified grid.
 * </PRE>
 * 
 * Note: Functionality that was common to ContourAnalyzer.C and SIRSGrid.C was
 * abstracted into a common base class, {@link AbstractGfeAnalyzer}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13Mar2008    968        MW Fegan    Initial implementation. Rehosted
 *                                      from legacy C++ ContourAnalyzer.H
 *                                      and ContourAnalyzer.C
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class ContourAnalyzer extends AbstractGfeAnalyzer {
    /* various flags to control processing */
    private boolean allNew;

    /* for analyzing the grid */
    private int nsta = -1;

    private ArrayList<Float> sLon = new ArrayList<Float>();

    private ArrayList<Float> sLat = new ArrayList<Float>();

    private ArrayList<Float> values = new ArrayList<Float>();

    private ArrayList<Float> seenValues = new ArrayList<Float>(8);

    /**
     * Constructor.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Load all data about grid &amp; contours into the
     * &quot;ContourAnalyzer&quot; data structure.
     * Implementation:
     * Shift input contour point positions to subgrid coordinates.
     * Truncate positions beyond grid limits to lie on edge.
     * Find and fill any gaps in contours.
     * Remove crossed-over contours.
     * Requires about 0.05 sec on the HP 750 for typical 73x73 grid and contours.
     * 0.013 sec on the Dell 500 Mhz PC.
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
     * @param clampOn
     *            if true, keeps grid point values bounded by neighboring
     *            contours
     * @param max
     *            maximum allowable grid value
     * @param min
     *            minimum allowable grid value
     */
    public ContourAnalyzer(Grid2DFloat dataGrid, List<CLine> contourLines,
            float xOrigin, float yOrigin, float xGridRatio, float yGridRatio,
            int xMin, int xMax, int yMin, int yMax, boolean clampOn, float max,
            float min) {
        super(dataGrid, xMin, xMax, yMin, yMax, clampOn, max, min);
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        /* retrieve data dimensions */
        this.xDim = dataGrid.getXdim();
        this.yDim = dataGrid.getYdim();

        /* zero out basic structures - these may be redundant */
        this.nsta = 0;
        this.sLat.clear();
        this.sLon.clear();
        this.values.clear();

        /* create and zero out results buffer */
        this.finalResultData = new Grid2DFloat(this.xDim, this.yDim);
        this.finalResultData.setAllValues((float) 0.0);

        if (contourLines.size() == 1) {
            /* trivial case - single contour */
            this.oneLine = true;
            this.oneValue = contourLines.get(0).getContourLevel();
        } else {
            /* normal case, many (>1) contour line */
            this.oneLine = false;

            this.valueFound = new Grid2DInteger(this.xDim, this.yDim);
            this.valueFound.setAllValues(0);

            this.howFound = new Grid2DInteger(this.xDim, this.yDim);
            this.howFound.setAllValues(-9);

            /*
             * note the area in grid that does not need to be recomputed, and
             * set grid point values there from "oldData" values (no change)
             */
            setChangeArea();

            logger.info("limits of area to recompute are x " + this.xMin
                    + " to " + this.xMax + " , y " + this.yMin + " to "
                    + this.yMax);

            /* set how many subgrid points there are per one main grid cell side */
            setSubGridFactor(4);

            /* read in data for input to analysis */
            readContourData(contourLines, xOrigin, yOrigin, xGridRatio,
                    yGridRatio);

            /*
             * read in data for using SIRS searching technique to determine
             * where grid points are to be recomputed. read in data from
             * modified or new contours
             */
            readNewContours(contourLines, xOrigin, yOrigin, xGridRatio,
                    yGridRatio);
            /* read in data from UN-modified (old) contours */
            readOldContours(contourLines, xOrigin, yOrigin, xGridRatio,
                    yGridRatio);
        }
        stopWatch.stop();
        this.timeUsed = stopWatch.getWallClockTime();
        logger.info("    ContourAnalyzer cstr         "
                + stopWatch.getWallClockTime() + " seconds");
    }

    /**
     * Recomputes grid values.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * The public function to make a data grid from contours.
     * 
     * Call this, for example as analyzer.recomputeGrid()
     * after constructing ContourAnalyzer object &quot;analyzer&quot;
     * </PRE>
     * 
     * @return a copy of the original grid
     */
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

        /*
         * step 1. for all grid points where more than one new or modified
         * contour can be seen, do analysis
         */
        doAnalysis();

        /*
         * step 2: compute grid points in sight of only one contour
         */
        gridValueFromGradient();

        /*
         * step 3: compute remaining grid points in "shadow areas" where no
         * contours are seen, or for points behind TWO contours of the same
         * value - no gradient (typically few or no grid points fall in this
         * case).
         */

        computeShadowAreas();

        /*
         * step 4: make sure computed grid values do not exceed limits given in
         * input
         */
        trimGridValues();

        stopWatch.stop();
        this.timeUsed += stopWatch.getWallClockTime();

        logger.info("    ContourAnalyzer.RecomputeGrid() used " + this.timeUsed
                + " seconds");

        try {
            return this.finalResultData.clone();
        } catch (CloneNotSupportedException e) {
            return new Grid2DFloat();
        }
    }

    /* helper methods */
    /**
     * Initializes analysis grids by loading grid points where contour points
     * fall on grid points.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * Trace along all contours, loading information
     * into arrays used by analysis.
     * Truncate positions outside grid to lie in grid.
     * This method gets exact contour positions input,
     * but misses the gap filling points made in other functions.
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
    protected void readContourData(List<CLine> contourLines, float xOrigin,
            float yOrigin, float xGridRatio, float yGridRatio) {
        float x;
        float y;

        /*
         * x and y width of grid in screen coordinates (distance across xDim
         * steps across the grid, not how many points) (result is basically
         * pixels across the width, then xDim -1 is number of cells and
         * 1/xGridRatio is pixels/cell.)
         */
        float xWrldWidth = ((this.xDim - 1)) / xGridRatio;
        float yWrldWidth = ((this.yDim - 1)) / yGridRatio;

        int lineIndex;
        float contourLevel;

        this.allNew = true;
        /* loop over the contour lines */
        for (lineIndex = 0; lineIndex < contourLines.size(); lineIndex++) {
            /* get the line currently being worked */
            CLine cline = contourLines.get(lineIndex);
            contourLevel = cline.getContourLevel();

            /* if an old contour line is supplied, not all lines are new */
            if (!cline.isModified()) {
                this.allNew = false;
            }

            /* loop over the points on this contour line */
            LineString line = cline.getLineString();
            for (int ptIndex = 0; ptIndex < line.getNumPoints(); ptIndex++) {

                /* get the position in world coordinates */
                Coordinate z = new Coordinate(line.getCoordinateN(ptIndex));

                /*
                 * check for input points beyond grid limits; set to nearest
                 * grid limit if so. world coordinates
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

                /* convert coordinates to grid indices */
                x = (float) (xGridRatio * (z.x - xOrigin));
                y = (float) (yGridRatio * (z.y - yOrigin));

                /* OK, load the data used by analysis */
                this.nsta++;
                this.sLat.add(y);
                this.sLon.add(x);
                this.values.add(contourLevel);
            }
        }
        /*
         * Note: more contour points are added to these arrays (this.values,
         * etc.) in routines readNewContours and readOldContours which create
         * new gap-filling contour points.
         */
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
    protected void readNewContours(List<CLine> contourLines, float xOrigin,
            float yOrigin, float xGridRatio, float yGridRatio) {
        /*
         * x and y width of grid in screen coordinates (distance across xDim
         * steps across the grid, not how many points) (result is basically
         * pixels across the width, then xDim -1 is number of cells and
         * 1/xGridRatio is pixels/cell.)
         */
        float xWrldWidth = ((this.xDim - 1)) / xGridRatio;
        float yWrldWidth = ((this.yDim - 1)) / yGridRatio;

        /*
         * conversion factors, screen to subgrid coordinates (how many subgrid
         * cells per world coordinate)
         */
        float xTerm = xGridRatio * this.subGridFactor;
        float yTerm = yGridRatio * this.subGridFactor;

        /*
         * Now add in contour information. Trace along all contours, loading
         * information needed. Truncate positions outside grid to lie in grid.
         * Find and fill gaps in contours, if there are any gaps. Load arrays
         * needed. If contour lies exactly on a grid point, set values
         * accordingly.
         * 
         * Do not use old contours that have points at same position as new or
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
             * construct list of input contour levels load first one
             */
            int clSize = this.contourLevels.size();
            /* this contour's level */
            float contourLevel = cline.getContourLevel();

            if (clSize == 0) {
                this.contourLevels.add(contourLevel);
            } else if (this.contourLevels.get(clSize - 1) != contourLevel) {
                this.contourLevels.add(contourLevel);
            }

            // use only a MODIFIED or NEW contour line
            // if not a modified contour
            if (!cline.isModified()) {
                continue;
            }

            if (logger.isDebugEnabled()) {
                logger.debug("new line, index " + lineIndex + "  level "
                        + contourLevel);
            }

            /*
             * loop over every point on this contour line compute subgrid point
             * indices check for gaps in contours and set proper values in gap
             * set proper values in subgrids at contour points
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
                 * Input contour positions need not be snapped to any grid
                 * points but for SIRS searching technique you must snap them to
                 * nearest subgrid points, which is done in the next step. (This
                 * does NOT effect recomputation (analysis) algorithms, it is
                 * only used for deciding which main grid points to recompute.)
                 */

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
                /* check for gaps in the contour line, and set values there */
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

                    /*
                     * 
                     */
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

                            if (logger.isDebugEnabled()) {
                                logger.debug("   gap point " + m + "," + n);
                            }

                            /*
                             * load new gap-filling points into input contour
                             * data arrays used for analysis; use main grid
                             * coords (these new additional contour line points
                             * may differ from the best-fit curve to the input
                             * contour points by as much distance as
                             * a/subgridfactor, but even with this error having
                             * no gaps in the contour line add much more value
                             * than any degradation from perfection by the
                             * slight error in contour line point position
                             */

                            // 
                            // float xx = (float) m / (float)
                            // this.subGridFactor;
                            // float yy = (float) n / (float)
                            // this.subGridFactor;
                            float xx = m / this.subGridFactor;
                            float yy = n / this.subGridFactor;

                            this.nsta++;
                            this.sLat.add(yy);
                            this.sLon.add(xx);
                            this.values.add(contourLevel);

                        }
                    }
                } // end if checking for gaps in contour
                oldi = i;
                oldj = j;

                /*
                 * Set the key values radial searching works with: which subgrid
                 * points are on contour lines; what is the contour level here,
                 * and is it a new or modified contour.
                 */
                this.onContour.set(i, j, lineIndex + 1);
                this.contourValue.set(i, j, contourLevel);
                this.contourNew.set(i, j, (byte) 1);

                /*
                 * Set point values where contours happen to be already defined
                 * on the main grid points. This prevents some odd values
                 * computed right on contours, and also speeds things, by a very
                 * small amount, roughly 5%.
                 */
                if (i % this.subGridFactor == 0 && j % this.subGridFactor == 0) {
                    i /= this.subGridFactor;
                    j /= this.subGridFactor;
                    this.finalResultData.set(i, j, contourLevel);
                    this.valueFound.set(i, j, 1);
                    this.howFound.set(i, j, 9); // 9 = value copied new contour
                    // level
                }
            } /* end loop over one contour line's points */

            if (logger.isDebugEnabled()) {
                logger.debug("   modified cont line " + lineIndex
                        + " at level " + contourLevel);

            }

        } /* end loop over all contour lines */

        if (logger.isDebugEnabled()) {
            logger.debug("  contour levels input = " + this.contourLevels);
        }

    }

    /**
     * Reads data from unchanged contours.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * read in data from unchanged contours
     * &lt;BR&gt;
     * Implementation:
     * also fills in gaps or holes in contours
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
    protected void readOldContours(List<CLine> contourLines, float xOrigin,
            float yOrigin, float xGridRatio, float yGridRatio) {
        int i;
        int j;
        int oldi = 0;
        int oldj = 0;

        /* keep list of added positions for each line (temporary) */
        ArrayList<Integer> iList = new ArrayList<Integer>();
        ArrayList<Integer> jList = new ArrayList<Integer>();

        /*
         * x and y width of grid in world coordinates (often pixels) (distance
         * across xDim steps across the grid, not how many points) (result is
         * basically pixels across the width, when xDim-1 is number of cells and
         * 1/xGridRatio is pixels/cell.)
         */
        float xWrldWidth = ((this.xDim - 1)) / xGridRatio;
        float yWrldWidth = ((this.yDim - 1)) / yGridRatio;

        /*
         * conversion factors, world to subgrid coordinates (how many subgrid
         * cells per world coordinates)
         */
        float xTerm = xGridRatio * this.subGridFactor;
        float yTerm = yGridRatio * this.subGridFactor;

        /* loop over every contour line for this data grid */
        for (int lineIndex = 0; lineIndex < contourLines.size(); lineIndex++) {
            iList.clear();
            jList.clear();

            /* get complete information for this contour line */
            CLine cline = contourLines.get(lineIndex);

            /* if this line is modified go try the next contour line in the loop */
            if (cline.isModified()) {
                continue;
            }

            float contourLevel = cline.getContourLevel();

            if (logger.isDebugEnabled()) {
                logger.debug("   load old contour line, index " + lineIndex
                        + "  level " + contourLevel);
            }

            /*
             * Check for crossing contours, and for gaps in contours. loop over
             * every point on this contour line compute subgrid point indices
             * check for gaps in contours and set proper values in gap set
             * proper values in subgrids at contour points.
             */
            LineString line = cline.getLineString();
            for (int ptIndex = 0; ptIndex < line.getNumPoints(); ptIndex++) {
                /* get position in world coordinates */
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
                 * truncation
                 */
                i = (int) Math.round(xTerm * (z.x - xOrigin));
                j = (int) Math.round(yTerm * (z.y - yOrigin));

                /*
                 * Crossing Contours Test. If this point on an old contour is
                 * also the same as a point on a new or modified contour line,
                 * quit adding in this old contour line; remove data already set
                 * from this line, and go and try next line.
                 */
                if (this.contourNew.get(i, j) == 1) {

                    if (logger.isDebugEnabled()) {
                        logger.debug("  crossing contour found, index "
                                + lineIndex + ", new level"
                                + this.contourValue.get(i, j) + " position "
                                + i + ", " + j);
                    }

                    iList.add(i);
                    jList.add(j);
                    /* remove points from this crossed old line already added */
                    removePoints(iList, jList);
                    break; // break off point loop and go to next line
                }
                if (ptIndex == 0) {
                    oldi = i;
                    oldj = j;
                }
                /* check for gaps in the contour line, and set values there */
                if (Math.abs(i - oldi) > 1 || Math.abs(j - oldj) > 1) {
                    /* how bing is the gap in x and y directions */
                    int numx = Math.abs(i - oldi); // typically 1 or 2
                    int numy = Math.abs(j - oldj); // typically 1 or 2

                    if (logger.isDebugEnabled()) {
                        logger.debug("     gap from " + oldi + ", " + oldj
                                + " to " + i + ", " + j);
                    }

                    int maxStep = Math.max(numx, numy);

                    if (logger.isDebugEnabled()) {
                        logger.debug("     gap size " + maxStep);
                    }
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
                                 * already add in
                                 */
                                removePoints(iList, jList);
                                /* go to next line */
                                break;
                            }
                            /*
                             * set SIRS search control values where contours
                             * should be in gaps
                             */
                            this.onContour.set(m, n, lineIndex + 1);
                            this.contourValue.set(m, n, contourLevel);
                            this.contourNew.set(m, n, (byte) 0);
                            iList.add(m);
                            jList.add(n);

                            if (logger.isDebugEnabled()) {
                                logger.debug("   gap pt " + m + ", " + n);
                            }

                            /*
                             * load new gap-filling points into input contour
                             * data arrays used for analysis; convert to main
                             * grid coord
                             */
                            // float xx = (float) m / (float)
                            // this.subGridFactor;
                            // float yy = (float) n / (float)
                            // this.subGridFactor;
                            float xx = m / this.subGridFactor;
                            float yy = n / this.subGridFactor;
                            this.nsta++;
                            this.sLat.add(yy);
                            this.sLon.add(xx);
                            this.values.add(contourLevel);
                        }
                    }
                } /* end if checking for gaps in contour */
                oldi = i;
                oldj = j;

                /*
                 * set the key values radial searching works with. which subgrid
                 * points are on contour lines; what is the contour level there,
                 * and is it a new of modified contour.
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

    /**
     * Computes values for grid points between contours having different values.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * For points on grid between contours of differing values and in sight of
     * a modified contour, compute a new grid point value.
     * 
     * For typical data, this is where most grid point values are computed.
     * 
     * Implementation:
     * Check each unassigned point on the main grid.
     * If in unmodified area, copy data from existing data grid.
     * If in sight of modified contours, recompute by analysis.
     * 
     * It may not be possible to determine some grid point values here,
     * and steps 2 or 3 may apply.
     * 
     * Two actions are combined here - copying old data for
     * main grid points which are not in sight of a modified contour, and
     * computing a new grid value if in sight of a modified (or all new) contour.
     * </PRE>
     */
    protected void doAnalysis() {
        float cv;
        double dist;
        boolean modContourFound;
        int modContourPoint;
        int numDiffValues;

        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        float cPrevious;

        int numStep1 = 0;

        /* check all main grid points */
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                if (this.valueFound.get(i, j) != 1) { // no value set for this
                    // point yet
                    modContourPoint = 0;
                    numDiffValues = 0;
                    cPrevious = (float) -9999.0;
                    this.seenValues.clear();

                    /* search in all directions from this main grid point */
                    for (SearchDir dir : SearchDir.values()) {
                        /*
                         * find nearest contour value & distance in this
                         * direction return whether it is a modified contour.
                         */
                        ContourValueDistance cvd = new ContourValueDistance();
                        modContourFound = findNearestContour(i, j, dir, cvd);
                        cv = (float) cvd.value;
                        dist = cvd.distance;
                        if (modContourFound && dist != 0) {
                            modContourPoint = 1; // can this be a boolean?
                        }

                        /* compute average value using ALL "seen" values */
                        if (dist != (float) 0.0) {
                            /*
                             * count how many different contour values seen and
                             * add to list of different values seen
                             */
                            if (numDiffValues == 0 || cv != cPrevious) {
                                numDiffValues++;
                                this.seenValues.add(cv);
                            }
                            cPrevious = cv;
                        }
                    } /* end of search in all [8] directions from this point */
                    /*
                     * if a modified contour seen, and 2 or more different
                     * contours were seen, do analysis for this grid point.
                     */
                    if (modContourPoint == 1 && numDiffValues > 1) {
                        this.finalResultData.set(i, j, analyzeAtPoint(i, j));
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, 1); // found by analysis
                        /* add up how many data points made by analysis */
                        numStep1++;
                    } else if (modContourPoint != 1 && !this.allNew) {
                        /*
                         * this prevents recomputing the entire grid, except
                         * when making an all new grid
                         * 
                         * if this point is not in sight of a modified contour
                         * copy data from old grid (no change)
                         */
                        this.finalResultData.set(i, j, this.oldData.get(i, j));
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, -1); // means old data value
                        // copied
                    }
                } /* end if (valueFount != 1) */
            }
        } /* end loop on all grid (i,j) points */

        if (logger.isDebugEnabled()) {
            logger.debug("    ContourAnalyzer has " + this.nsta
                    + " input points");
        }
        logger.info("    CountourAnalyzer analyzed " + numStep1
                + " grid points");

        /*
         * Note: the points in the grid which do not now have values determined
         * already should be recomputed by step 2 or 3.
         */

        stopWatch.stop();
        if (logger.isDebugEnabled()) {
            logger.debug("   analysis used " + stopWatch.getWallClockTime()
                    + " seconds");
        }

    }

    /**
     * Determines the correct grid value for a point not on a contour.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Determine the value at a single grid point, using analysis of
     *  input values (contour points' positions and values)
     * 
     * Implementation:
     * This short routine is the heart of the entire class. It is a kind
     * of analysis using squared distance weighing, and only using
     * input values (contour values) for contours &quot;seen&quot; from the
     * grid point being analysed so that distant contours do not contribute.
     * Distance units are grid cells.
     * If distance measure is less than figure shown (say 0.0001) then
     * do not recompute but rather use the value of the nearest contour point.
     * </PRE>
     * 
     * @param i
     *            horizontal index of the point
     * @param j
     *            vertical index of the point
     * 
     * @return computed grid value of the point
     */
    protected float analyzeAtPoint(int i, int j) {
        int test = 0;
        float data = (float) 0.0;
        double disqd;
        float sumOneOnDsqd = (float) 0.0;
        float sumVod = (float) 0.0;
        float gLat = j;
        float gLon = i;
        int numCVs = this.seenValues.size();
        boolean ok;
        float tVal = (float) -9999.0;

        for (int k = 0; k < this.nsta; k++) {
            ok = false;
            for (int jj = 0; jj < numCVs; jj++) {
                if (this.values.get(k).equals(this.seenValues.get(jj))) {
                    ok = true;
                    break;
                }
            }
            if (ok) {
                /* distance squared weighing */
                float tLat = this.sLat.get(k);
                float tLon = this.sLon.get(k);
                tVal = this.values.get(k);
                disqd = (gLon - tLon) * (gLon - tLon) + (gLat - tLat)
                        * (gLat - tLat);
                if (disqd < 0.0001) {
                    test = 1;
                    data = tVal;
                }
                /*
                 * Legacy notes: convert filter or weighing function to
                 * Gaussian; this does no help & doubles the total time to make
                 * a grid from contours. DISQD = exp(DISQD)
                 * 
                 * 1/R filter; in principle this should give better results than
                 * the 1/R2 function, but in proctice the results arevery
                 * similar and this increases total time to make grid from
                 * contours by about 30% to 50%. DISQD = sqrt(DISQG)
                 */
                sumVod += tVal / disqd;
                sumOneOnDsqd += 1.0 / disqd;
            }
            if (test == 1) {
                break;
            }
        } /* end of "for K = 0" loop */

        /*
         * Note this was modified from if (test == 1) { data = tVal; } else {
         * data = SUMVOD / SUMONEONDSQD; } which has a redundant assignment
         * since data is set to tVal when test is set to 1.
         */
        if (test != 1) {
            data = sumVod / sumOneOnDsqd;
        }
        return data;
    }

    /**
     * Computes grid values based on gradients when unable to see contour.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * Make grid values for points where two or more different contours
     * can NOT be seen among the nearest contours.
     * Use gradients of contours seen: nearest and next further-out contours.
     * 
     * Implementation:
     * For grid points that can only &quot;see&quot; nearest contours of one value
     * 
     * two cases: if the point cannot see a grid edge it is inside a closed
     * contour and extrapolated gradients should be ised. (Analysis will
     * give a poor answer, converging on the mean.
     * 
     * of if you see at least one pair of contours in some direction
     * using findDistantContours() (even is a grid edge
     * is also visible) then also compute values from gradient (s)
     * </PRE>
     */
    protected void gridValueFromGradient() {
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        int numFound;
        int numcavg;
        int numstep2 = 0;

        double[] c1 = new double[8]; // nearest contour values found in each
        // direction
        double[] d1 = new double[8]; // distance to a contour found in each
        // direction
        double[] grad = new double[8]; // gradient in each direction
        double[] v = new double[8]; // first estimate of grid point value
        double top;
        double bottom;
        double cavg;
        double gridValue;

        /* Sort contour levels into increasing value order */
        Collections.sort(contourLevels);

        /*
         * Now extend range of contour levels by one more up and down to provide
         * contour levels used for limiting values
         */
        if (this.contourLevels.size() > 1) {
            float delta = this.contourLevels.get(1) - this.contourLevels.get(0);
            this.contourLevels.add(0, this.contourLevels.get(0) - delta);
            delta = this.contourLevels.get(this.contourLevels.size() - 1)
                    - this.contourLevels.get(this.contourLevels.size() - 2);
            this.contourLevels.add(this.contourLevels.get(this.contourLevels
                    .size() - 1)
                    + delta);
        }
        if (logger.isInfoEnabled()) {
            logger.info("  extended contour level = "
                    + this.contourLevels.toString());
        }

        /* check all main grid points */
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                /* if value not yet provided */
                if (this.valueFound.get(i, j) != 1) {
                    cavg = 0.0;
                    numcavg = 0;
                    top = 0.0;
                    bottom = 0.0;

                    /* search in all directions from this grid point */
                    for (SearchDir dir : SearchDir.values()) {
                        int k = dir.ord;
                        numFound = -1;
                        grad[k] = 0.0;
                        /*
                         * find first and second contours in direction dir (2
                         * returned) or contour plus grid edge (1 returned)
                         */
                        ContourValueDistance cvda = new ContourValueDistance();
                        ContourValueDistance cvdb = new ContourValueDistance();
                        numFound = findDistantContours(i, j, dir, cvda, cvdb);

                        if (numFound == 1 || numFound == 2) {
                            c1[k] = cvda.value;
                            d1[k] = cvda.distance;

                            /*
                             * compose average of surrounding contour values
                             * (every CA should be the same for a valid step2
                             * point)
                             */
                            cavg += cvda.value;
                            numcavg++;

                            /*
                             * if ca = cb then in a shadow zone (do step 3); (da
                             * == db makes zero divide but should never occur -
                             * that would mean two contours at the same point.)
                             */

                            /* if saw TWO DIFFERENT contours in this direction */
                            // if (!cvda.equals(cvdb)) {
                            if (cvda.value != cvdb.value
                                    && cvda.distance != cvdb.distance) {
                                grad[k] = cvdb.gradient(cvda);
                                /*
                                 * estimate the value at this grid point, given
                                 * by gradient at this distance
                                 */
                                v[k] = cvda.value - grad[k] * cvda.distance;
                            }
                        }
                        /* augment values used for composing final average */
                        if (grad[k] != 0.0 && d1[k] != 0.0) {
                            top += v[k] / d1[k];
                            bottom += 1.0 / d1[k];
                        }
                    } /* end looking in all directions from this point */

                    /* making grid value from gradient */
                    if (numcavg != 0.0 && bottom != 0.0) {

                        /*
                         * ave. contour value surrounding this point; default
                         * value
                         */
                        cavg /= numcavg;

                        /* compute value from average gradients */
                        gridValue = top / bottom;

                        /*
                         * ensure that new grid value does not exceed next
                         * contour level, up or down
                         */
                        if (this.clampOn) {

                            /*
                             * compare grid value to neighboring contour values;
                             * requires contours listed in increasing value
                             * order
                             */
                            for (int jj = 1; jj < this.contourLevels.size() - 2; jj++) {
                                if (cavg == this.contourLevels.get(jj)) {
                                    if (gridValue < this.contourLevels
                                            .get(jj - 1)) {
                                        gridValue = this.contourLevels
                                                .get(jj - 1) + 0.01;
                                    } else if (gridValue > this.contourLevels
                                            .get(jj + 1)) {
                                        gridValue = this.contourLevels
                                                .get(jj + 1) - 0.01;
                                    }
                                }
                            }
                            /* force check at ends of contour value range */
                            if (gridValue > this.contourLevels
                                    .get(this.contourLevels.size() - 1)) {
                                gridValue = this.contourLevels
                                        .get(this.contourLevels.size() - 1) - 0.001;
                            } else if (gridValue < this.contourLevels.get(0)) {
                                gridValue = this.contourLevels.get(0) + 0.001;
                            }
                            /*
                             * case of cavg not one of the contour values (can
                             * happen - unresolved issue
                             */
                            // if (!match)
                            // add logging message
                        }
                        this.finalResultData.set(i, j, (float) gridValue);
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, 2); // found by step 2
                        numstep2++;

                    } /* end if making from gradient */
                } /* end if found a grid point to make value for */
            } /* end loop on all grid points */
        }

        logger.info("   ContourAnalyzer step 2 did " + numstep2
                + " grid points");

        stopWatch.stop();
        logger.info("    step 2 " + stopWatch.getWallClockTime() + " seconds");
    }

    /**
     * Computes any grid values not determined by steps 1 & 2.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * Case of remaining unfilled grid values.
     * One situation is all contours seen from a grid point are of the same value,
     * and furthermore all contours beyond the first are ALSO the exact
     * same value.  Very rare but it can happen.
     * More common, points aht see no contours at all.
     * Fills in spots not done by steps 1 and 2,
     * by averaging surrounding valid grid values made by previous steps.
     * Does simple average of neighbor points values; distance weighing average
     * might be an improvement.
     * </PRE>
     */
    protected void computeShadowAreas() {
        int numStep3 = 0; // counter of how many grid values made here
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                if (this.valueFound.get(i, j) != 1) { // value not yet computed
                    // here
                    int na = 0;
                    double sumValues = 0.0;
                    /*
                     * search in all directions from this point find nearest
                     * good value in that direction, if any
                     */
                    for (SearchDir dir : SearchDir.values()) {
                        ContourValueDistance cvd = new ContourValueDistance();
                        boolean found = findAdjacentValue(i, j, dir, cvd);
                        if (found) {
                            sumValues += cvd.value;
                            na++;
                        }
                    }
                    if (na != 0) {
                        this.finalResultData
                                .set(i, j, (float) (sumValues / na));
                        this.valueFound.set(i, j, 1);
                        this.howFound.set(i, j, 3); // the values was found by
                        // step 3
                        numStep3++;
                    }
                }
            }
        }
        logger.info("   ContourAnalyzer step 3 did  " + numStep3
                + " grid points");
    }
}
