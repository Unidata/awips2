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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DInteger;
import com.raytheon.viz.gfe.contours.util.ContourValueDistance;
import com.raytheon.viz.gfe.contours.util.SearchDir;

/**
 * This is an abstract base class that contains the functionality that is common
 * to the SIRS analysis algorithms. It is based on analysis of the original NWS
 * C++ code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13Mar2008    968        MW Fegan    Initial Implementation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public abstract class AbstractGfeAnalyzer implements IGfeAnalyzer {
    /** the logging facility - change level to limit logging */
    protected Log logger = LogFactory.getLog(getClass());

    /** contains the data originally used to create the contours */
    protected final Grid2DFloat oldData;

    /** final grid following processing */
    public Grid2DFloat finalResultData;

    /** "flag it have made the value here" */
    protected Grid2DInteger valueFound;

    /** "indicator number of what step used" */
    protected Grid2DInteger howFound;

    /** "temporary contour number counter" */
    protected Grid2DInteger onContour;

    /** "contour value at this point is onContour" */
    protected Grid2DFloat contourValue;

    /** "flag if contour modified or new" */
    protected Grid2DByte contourNew;

    /** "levels of input contours" */
    protected List<Float> contourLevels;

    /* main grid dimensions */
    protected int xDim = -1;

    protected int yDim = -1;

    /* points outlining change ares where grid recomputes */
    protected int xMin = -1;

    protected int xMax = -1;

    protected int yMin = -1;

    protected int yMax = -1;

    /* min and max of entire grid after recompute */
    protected float gridMax = 0;

    protected float gridMin = 0;

    /* used for trivial case of only one contour line */
    protected boolean oneLine = false;

    protected float oneValue = 0;

    /* various flags to control processing */
    protected boolean clampOn;

    /* for timing */
    protected double timeUsed = -1;

    /** "how many subgrid pts per main grid point" */
    protected int subGridFactor = -1;

    /** "dimension of subgrid arrays in x" */
    protected int sgxDim = -1;

    /** "dimension of subgrid arrays in y" */
    protected int sgyDim = -1;

    /**
     * Constructor.
     */
    public AbstractGfeAnalyzer(Grid2DFloat dataGrid, int xMin, int xMax, int yMin,
            int yMax, boolean clampOn, float max, float min) {
        this.oldData = dataGrid;
        this.xMin = xMin;
        this.xMax = xMax;
        this.yMin = yMin;
        this.yMax = yMax;
        this.clampOn = clampOn;
        this.gridMin = min;
        this.gridMax = max;
        this.contourLevels = new ArrayList<Float>();

    }

    @Override
    public Grid2DFloat getFinalResultData() {
        return this.finalResultData;
    }

    /**
     * Applies existing grid values to points not being changed.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * For points outside change area, use the original grid point values.
     * Only recomputes grid values inside the change area.
     * The change area limits are input in cstr arguments xMin, xMax, etc.
     * Units of limits are main grid indices.
     * This makes for higher speed.
     * </PRE>
     */
    protected void setChangeArea() {
        /*
         * For all main grid points outside the change area, set new grid values
         * to the original or "old" grid value.
         */
        for (int i = 0; i < xDim; i++) {
            for (int j = 0; j < yDim; j++) {
                if (i < xMin || i > xMax || j < yMin || j > yMax) {
                    this.finalResultData.set(i, j, this.oldData.get(i, j));
                    this.valueFound.set(i, j, 1);
                    this.howFound.set(i, j, -1); // means old data value copied
                }
            }
        }
    }

    /**
     * Determines the number of sub grid points for each main grid point.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * Set how many sub grid points there are for each main data grid point.
     * set sgxDim and sgyDim, the dimensions of the subgrid.
     * Create working subgrid arrays.
     * Implementation:
     * typically 2 to 8.
     * 4 seems to work fine; it is not clear if 8 is any better
     * </PRE>
     * 
     * @param factor
     *            the number of sub-grid units per grid unit
     */
    protected void setSubGridFactor(int factor) {
        this.subGridFactor = factor;
        this.sgxDim = (this.xDim - 1) * this.subGridFactor + 1;
        this.sgyDim = (this.yDim - 1) * this.subGridFactor + 1;

        /*
         * Create empty 2D subgrid arrays - use only where contours are defined
         * temporary contour counter or label contour level valid at this point
         * (if any) boolean if contour modified or new
         */
        this.onContour = new Grid2DInteger(sgxDim, sgyDim);
        this.onContour.setAllValues(0);
        this.contourValue = new Grid2DFloat(sgxDim, sgyDim);
        this.contourValue.setAllValues((float) 0.0);
        this.contourNew = new Grid2DByte(sgxDim, sgyDim);
        this.contourNew.setAllValues((byte) 0);

    }

    /**
     * Removes data from point lists when a backtrack is required.
     * <P>
     * Legacy documentation:
     * 
     * <PRE>
     * For data added to the SIRS search data from some 
     * unchanged contour that is crossed by a new or modified contour, remove data.
     * &lt;BR&gt;
     * Implementation:
     * take off all the grid data at the points listed
     * </PRE>
     * 
     * @param iList
     *            list of i values for points
     * @param jList
     *            list of j values for points
     */
    protected void removePoints(List<Integer> iList, List<Integer> jList) {
        int max = iList.size();
        /* index of last entry in the list */
        int last = max - 1;

        /*
         * could use a smaller number for max if desired, if only want to remove
         * part of the crossed contour near the crossing.
         * 
         * remove data for the points, most recent goes out first
         */
        for (int n = last; n >= 0; n--) {
            int i = iList.get(n);
            int j = jList.get(n);
            this.onContour.clear(i, j);
            this.contourValue.clear(i, j);
            this.contourNew.clear(i, j);

            if (logger.isDebugEnabled()) {
                logger.debug("   removing data at " + i + "," + j);
            }
        }
    }

    /**
     * Ensures that the grid doesn't contain any values outside the range
     * specified by {@link #gridMin} to {@link #gridMax}.
     */
    protected void trimGridValues() {
        for (int i = 0; i < this.xDim; i++) {
            for (int j = 0; j < this.yDim; j++) {
                float gridVal = this.finalResultData.get(i, j);
                if (gridVal > this.gridMax) {
                    this.finalResultData.set(i, j, this.gridMax);
                } else if (gridVal < this.gridMin) {
                    this.finalResultData.set(i, j, this.gridMin);
                }
            }
        }
    }

    /**
     * Finds the contour closest to the specified (i,j) point.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Used to support SIRS step 1.
     * Search from given input grid position (i,j) in given direction &quot;dir.&quot;
     * Determine contour value of nearest contour seen, and also distance to it.
     * Return 1 if found a modified contour, otherwise 0.
     * Computed value &quot;distance&quot; not equal to 0 if any contour was found.
     * 
     * Implementation:
     * 
     * distance units are subgrid cells.
     * 
     * some of this code deals with the &quot;leakage problem.&quot;  If a contour lies
     * on a NE-SW or NW-SE diagonal, some searches for it along the other
     * diagonal will pass through without seeing it.  (But the search will
     * find it on a verical or horizontal search.) The problem is that a
     * search on a diagonal may find another contour much farther away. Such
     * must be considered and excluded since only the nearest contour will
     * give the best results.
     * </PRE>
     * 
     * @param i
     *            horizontal index of the point
     * @param j
     *            vertical index of the point
     * @param dir
     *            the direction to search
     * @param cvd
     *            contains the found grid value and computed distance
     * 
     * @return true if the search encountered a contour
     */
    protected boolean findNearestContour(int i, int j, SearchDir dir,
            ContourValueDistance cvd) {
        int sgi = i * this.subGridFactor; // equiv subgrid index to i
        int sgj = j * this.subGridFactor; // equiv subgrid index to j
        int n; // loop control
        int nLimit; // loop control
        int sgii = sgi; // temp variable
        int sgjj = sgj; // temp variable

        cvd.distance = 0.0;
        switch (dir) {
        case N:
            nLimit = this.sgyDim - sgj;
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgjj = sgj + n; // index with an offset to move N into the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    /* simplify this dist calc since no change in sgi */
                    cvd.distance = n;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case S:
            nLimit = sgj + 1;
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgjj = sgj - n; // index with an offset to move S into the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case E:
            nLimit = this.sgxDim - sgi;
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgii = sgi + n; // index with an offset to move E into the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case W:
            nLimit = sgi + 1;
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgii = sgi - n; // index with an offset to move E into the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case NE:
            nLimit = Math.min((this.sgyDim - sgj - 1), (this.sgxDim - sgi - 1));
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgii = sgi + n; // index with an offset to move E into the grid
                sgjj = sgj + n; // index with an offset to move N into the grid
                /* look to the right */
                if (this.onContour.get(sgii, sgjj - 1) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak NE r");
                    }
                    cvd.value = this.contourValue.get(sgii, sgjj - 1);
                    /*
                     * this formula as approximation to increase speed;
                     * pythagoras is correct
                     */
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii, sgjj - 1) == 1);
                }
                /* look to the left */
                if (this.onContour.get(sgii - 1, sgjj) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak NE l");
                    }
                    cvd.value = this.contourValue.get(sgii - 1, sgjj);
                    /*
                     * this formula as approximation to increase speed;
                     * pythagoras is correct
                     */
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii - 1, sgjj) == 1);
                }
                /* look at point */
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n * 1.414;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case SE:
            nLimit = Math.min(sgj, (sgxDim - sgi - 1));
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgii = sgi + n; // index with an offset to move E into the grid
                sgjj = sgj - n; // index with an offset to move S into the grid
                /* look to the right */
                if (this.onContour.get(sgii - 1, sgjj) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak SE r");
                    }
                    cvd.value = this.contourValue.get(sgii - 1, sgjj);
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii - 1, sgjj) == 1);
                }
                /* look to the left */
                if (this.onContour.get(sgii, sgjj + 1) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak SE l");
                    }
                    cvd.value = this.contourValue.get(sgii, sgjj + 1);
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii, sgjj + 1) == 1);
                }
                /* look at point */
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n * 1.414;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case SW:
            nLimit = Math.min(sgj, sgi);
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgii = sgi - n; // index with an offset to move W into the grid
                sgjj = sgj - n; // index with an offset to move S into the grid
                /* look to the left */
                if (this.onContour.get(sgii + 1, sgjj) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak SW l");
                    }
                    cvd.value = this.contourValue.get(sgii + 1, sgjj);
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii + 1, sgjj) == 1);
                }
                /* look to the right */
                if (this.onContour.get(sgii, sgjj + 1) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak SW r");
                    }
                    cvd.value = this.contourValue.get(sgii, sgjj + 1);
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii, sgjj + 1) == 1);
                }
                /* look at point */
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n * 1.414;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        case NW:
            nLimit = Math.min(this.sgyDim - sgj - 1, sgi);
            for (n = 1; n < nLimit; n++) { // n just a loop counter; not an
                // index
                sgii = sgi - n; // index with an offset to move W into the grid
                sgjj = sgj + n; // index with an offset to move N into the grid
                /* look to the right */
                if (this.onContour.get(sgii + 1, sgjj) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak NW r");
                    }
                    cvd.value = this.contourValue.get(sgii + 1, sgjj);
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii + 1, sgjj) == 1);
                }
                /* look to the left */
                if (this.onContour.get(sgii, sgjj - 1) > 0) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("    diag leak NW l");
                    }
                    cvd.value = this.contourValue.get(sgii, sgjj - 1);
                    cvd.distance = n * 1.414 - 0.414;
                    return (this.contourNew.get(sgii, sgjj - 1) == 1);
                }
                /* look at point */
                if (this.onContour.get(sgii, sgjj) > 0) {
                    cvd.value = this.contourValue.get(sgii, sgjj);
                    cvd.distance = n * 1.414;
                    return (this.contourNew.get(sgii, sgjj) == 1);
                }
            }
            return false; // failed to see contour in this direction
        default:
            /* Note: if this happens, there is a bad software problem */
            logger.warn(" findNearestContour: Bad direction " + dir);
            return false;
        }
    }

    /**
     * Finds the first two contours in the specified direction.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Used to support step 2.
     * Search from given input main grid position (i,j) in given direction &quot;dir.&quot;
     * Note contour values and distances of first and second contours seen.
     * Can use the distance and value at a grid edge point (if it is valid)
     * in place of a second contour.
     * If no contours seen, note distance d1 from (i,j) to edge in this dir,
     * Return value for this direction:
     * 2 if two contours seen,
     * 1 if one contour and the grid edge with a valid grid point there
     * 0 if no contours seen
     * 0 if one contour seen and the grid edge had no set value
     * </PRE>
     * 
     * @param i
     *            horizontal index of the point
     * @param j
     *            vertical index of the point
     * @param dir
     *            the direction to search
     * @param cvd1
     *            contains the found grid value and computed distance of first
     *            point
     * @param cvd2
     *            contains the found grid value and computed distance of second
     *            point
     * 
     * @return indicates number of contour found - see return values in legacy
     *         documentation
     */
    protected int findDistantContours(int i, int j, SearchDir dir,
            ContourValueDistance cvd1, ContourValueDistance cvd2) {
        int sgi = i * this.subGridFactor; // equiv subgrid index to main i
        int sgj = j * this.subGridFactor; // equiv subgrid index to main j
        int nLimit; // loop control
        int sgii = sgi; // temporary subgrid i index
        int sgjj = sgj; // temporary subgrid j index

        int nFound = 0;
        int ii;
        int jj;

        switch (dir) {
        case N:
            nLimit = this.sgyDim - sgj;
            for (int n = 1; n < nLimit; n++) {
                sgjj = sgj + n; // offset to move N in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, return 0 and distance
                     * to edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0;
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values for second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case S:
            nLimit = sgj + 1;
            for (int n = 1; n < nLimit; n++) {
                sgjj = sgj - n; // offset to move S in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0;
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values for second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case E:
            nLimit = this.sgxDim - sgi;
            for (int n = 1; n < nLimit; n++) {
                sgii = sgi + n; // offset to move E in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0;
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values for second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case W:
            nLimit = sgi + 1;
            for (int n = 1; n < nLimit; n++) {
                sgii = sgi - n; // offset to move W in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0;
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values for second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case NE:
            nLimit = Math.min(this.sgyDim - sgj, this.sgxDim - sgi);
            for (int n = 1; n < nLimit; n++) {
                sgii = sgi + n; // offset to move E in the grid
                sgjj = sgj + n; // offset to move N in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0; /* sign of no contour found */
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values of a second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                    return 0; /* can't use grid edge so quit */
                } else if (this.onContour.get(sgii + 1, sgjj) > 0
                        && this.onContour.get(sgii, sgjj + 1) > 0
                        && this.onContour.get(sgii + 1, sgjj) == this.onContour
                                .get(sgii, sgjj + 1)) {
                    /* catch crossings of contours at right angle to this dir */
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii + 1, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii + 1, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        nFound = 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case SE:
            nLimit = Math.min(sgj + 1, this.sgxDim - sgi);
            for (int n = 1; n < nLimit; n++) {
                sgii = sgi + n; // offset to move E in the grid
                sgjj = sgj - n; // offset to move S in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0; /* sign of no contour found */
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values of a second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                    return 0; /* can't use grid edge so quit */
                } else if (this.onContour.get(sgii + 1, sgjj) > 0
                        && this.onContour.get(sgii, sgjj - 1) > 0
                        && this.onContour.get(sgii + 1, sgjj) == this.onContour
                                .get(sgii, sgjj - 1)) {
                    /* catch crossings of contours at right angle to this dir */
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii + 1, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii + 1, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        nFound = 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case SW:
            nLimit = Math.min(sgj + 1, sgi + 1);
            for (int n = 1; n < nLimit; n++) {
                sgii = sgi - n; // offset to move W in the grid
                sgjj = sgj - n; // offset to move S in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0; /* sign of no contour found */
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values of a second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                    return 0; /* can't use grid edge so quit */
                } else if (this.onContour.get(sgii - 1, sgjj) > 0
                        && this.onContour.get(sgii, sgjj - 1) > 0
                        && this.onContour.get(sgii - 1, sgjj) == this.onContour
                                .get(sgii, sgjj - 1)) {
                    /* catch crossings of contours at right angle to this dir */
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii - 1, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii - 1, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        nFound = 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        case NW:
            nLimit = Math.min(this.sgyDim - sgj, sgi + 1);
            for (int n = 1; n < nLimit; n++) {
                sgii = sgi - n; // offset to move W in the grid
                sgjj = sgj + n; // offset to move N in the grid
                if (this.onContour.get(sgii, sgjj) > 0) {
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                        nFound = 1;
                    }
                } else if (n == nLimit - 1 && nFound == 0) {
                    /*
                     * at edge & found no contours at all, compute distance to
                     * edge of grid from (i,j)
                     */
                    cvd1.value = 0.0;
                    cvd1.distance = findDistance(sgi, sgj, sgii, sgjj);
                    return 0; /* sign of no contour found */
                } else if (n == nLimit - 1 && nFound == 1) {
                    /* use grid edge values of a second contour, if possible */
                    ii = sgii / this.subGridFactor;
                    jj = sgjj / this.subGridFactor;
                    if (this.valueFound.get(ii, jj) == 1) {
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj);
                        cvd2.value = this.finalResultData.get(ii, jj);
                        return 1;
                    }
                    return 0; /* can't use grid edge so quit */
                } else if (this.onContour.get(sgii - 1, sgjj) > 0
                        && this.onContour.get(sgii, sgjj + 1) > 0
                        && this.onContour.get(sgii - 1, sgjj) == this.onContour
                                .get(sgii, sgjj + 1)) {
                    /* catch crossings of contours at right angle to this dir */
                    if (nFound == 1) {
                        cvd2.value = this.contourValue.get(sgii - 1, sgjj);
                        cvd2.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        return 2;
                    } else if (nFound == 0) {
                        cvd1.value = this.contourValue.get(sgii - 1, sgjj);
                        cvd1.distance = findDistance(sgi, sgj, sgii, sgjj) + 0.707;
                        nFound = 1;
                    }
                }
            }
            return 0; /* failed to see contours in this direction */
        default:
            /* Note: if this happens, there is a bad software problem */
            logger.warn(" findDistantContours: Bad direction " + dir);
            return 0;
        }
    }

    /**
     * Finds value of closest point in specified direction.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Used to support step 3.
     * Search from given input grid position (i,j) in given direction &quot;dir.&quot;
     * Note value of nearest point already set by prvious code here.
     * Return 0 if no good value seen; otherwise return 1.
     * Implementation:
     * DOES NOT check if a contour is crossed.
     * That is a refinement which may be added later.
     * </PRE>
     * 
     * @param i
     *            horizontal index of the point
     * @param j
     *            vertical index of the point
     * @param dir
     *            the direction to search
     * @param cvd
     *            contains the found grid value and computed distance (only
     *            value is used)
     * 
     * @return true if the search was successful
     */
    protected boolean findAdjacentValue(int i, int j, SearchDir dir,
            ContourValueDistance cvd) {
        int nLimit; // loop control
        int ii = i; // temporary i index
        int jj = j; // temporary j index
        switch (dir) {
        case N:
            nLimit = this.yDim - j;
            for (int n = 1; n < nLimit; n++) {
                jj = j + n; // offset to move N in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case S:
            nLimit = j + 1;
            for (int n = 1; n < nLimit; n++) {
                jj = j - n; // offset to move S in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case E:
            nLimit = this.xDim - i;
            for (int n = 1; n < nLimit; n++) {
                ii = i + n; // offset to move E in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case W:
            nLimit = i + 1;
            for (int n = 1; n < nLimit; n++) {
                ii = i - n; // offset to move W in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case NE:
            nLimit = Math.min(this.yDim - j, this.xDim - i);
            for (int n = 1; n < nLimit; n++) {
                ii = i + n; // offset to move E in the grid
                jj = j + n; // offset to move N in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case SE:
            nLimit = Math.min(j + 1, this.xDim - i);
            for (int n = 1; n < nLimit; n++) {
                ii = i + n; // offset to move E in the grid
                jj = j - n; // offset to move S in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case SW:
            nLimit = Math.min(j + 1, i + 1);
            for (int n = 1; n < nLimit; n++) {
                ii = i - n; // offset to move W in the grid
                jj = j - n; // offset to move S in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false; // failed to find a value in this direction
        case NW:
            nLimit = Math.min(this.yDim - j, i + 1);
            for (int n = 1; n < nLimit; n++) {
                ii = i - n; // offset to move W in the grid
                jj = j + n; // offset to move N in the grid
                /* if value here already found, use it */
                int tHow = this.howFound.get(ii, jj);
                if (tHow > -1 && tHow < 3) {
                    cvd.value = this.finalResultData.get(ii, jj);
                    return true;
                }
            }
            return false;
        default:
            logger.warn(" findAdjacentValue: Bad direction. " + dir);
            return false; // failed to find a value in this direction
        }
    }

    /**
     * Computes the distance between two grid points. Units are grids.
     * <P>
     * Legacy Documentation:
     * 
     * <PRE>
     * Find double distance bewteen two int coordinate pairs.
     * Returns distance in double float precision.
     * </PRE>
     * 
     * @param x1
     *            horizontal index of first point
     * @param y1
     *            vertical index of first point
     * @param x2
     *            horizontal index of second point
     * @param y2
     *            vertical index of second point
     * 
     * @return the computed distance
     */
    private double findDistance(int x1, int y1, int x2, int y2) {
        return Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
    }

}
