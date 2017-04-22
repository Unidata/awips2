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
package com.raytheon.viz.gfe.interpolation;

import java.awt.Point;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2D;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DInteger;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Derived from the Interp ABC. Computes coefficients for interpolation of real
 * numbers from spline functions.
 * 
 * The sequence of baseDataIndices contains the indexes that contain the base
 * data. The sequence of data slices should contain a mix of "NULL-type" and
 * valid data slices. The mathematical algorithms for spline interpolation for a
 * single time-value function are coded here. Assumes that the base data are
 * valid.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 2, 2008		#1161	randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class ContInterp extends Interp {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Parm.class);

    // Tuneable parameter: number of sectors in a circle around each data point,
    // used to control changes in shapes of advected areas.
    // Any number equal to 8 or more should do ok; any number > 0 will run.
    private static final int _numSectors = 32;

    protected static class BaseInfo {
        // array name; data provided by input
        public float[] knownDataValues;

        // array name; data computed here
        public double[] splineCoeff;

        BaseInfo() {
            knownDataValues = null;
            splineCoeff = null;
        }
    }

    /**
     * @param dataslices
     * @param baseDataIndices
     * @param parmid
     * @param gridparminfo
     * @param gridTimes
     */
    public ContInterp(List<IGridSlice> dataslices, int[] baseDataIndices,
            ParmID parmid, GridParmInfo gridparminfo, List<TimeRange> gridTimes) {
        super(dataslices, baseDataIndices, parmid, gridparminfo, gridTimes);
    }

    @Override
    public abstract IGridSlice interpolate(int index);

    /**
     * Allocates the memory of proper size for the input baseInfo item. Does not
     * initialize the values. This is called by derived classes when they are
     * constructed.
     * 
     * @param baseInfo
     */
    protected Grid2D<BaseInfo> allocateBaseInfoGrid() {
        // get size of our grid
        Point size = getGridParmInfo().getGridLoc().gridSize();

        // logEvent << " Grid size is " << size << endl;

        // so far baseInfo exists as zero size, having been constructed by
        // default. Give it the proper size.
        Grid2D<BaseInfo> baseInfo = new Grid2D<BaseInfo>(size.x, size.y);

        // new up arrays of proper length for each point in the baseInfo
        // (numberOfBaseSlices() is defined in Interp class)
        for (int i = 0; i < size.x; i++) {
            for (int j = 0; j < size.y; j++) {
                BaseInfo bi = new BaseInfo();
                bi.knownDataValues = new float[getNumberOfBaseSlices()];
                bi.splineCoeff = new double[getNumberOfBaseSlices()];
                baseInfo.set(i, j, bi);
            }
        }

        return baseInfo;
    }

    // Deallocates the arrays in the baseInfo grid specified.
    // This is called by derived classes.
    /**
     * @param baseInfo
     */
    protected void deallocateBaseInfoGrid(Grid2D<BaseInfo> baseInfo) {
        for (int i = 0; i < baseInfo.getXDim(); i++) {
            for (int j = 0; j < baseInfo.getYDim(); j++) {
                baseInfo.get(i, j).knownDataValues = null;
                baseInfo.get(i, j).splineCoeff = null;
            }
        }
    }

    /**
     * Interpolates the grid specified by "index" given the base info grid.
     * Returns the interpolated grid as a two-dimensional array of floats.
     * 
     * @param baseInfo
     * @param index
     * @return
     */
    protected Grid2DFloat interpolateGrid(final Grid2D<BaseInfo> baseInfo,
            int index) {
        // create an empty output grid
        Grid2DFloat outGrid = new Grid2DFloat(baseInfo.getXDim(),
                baseInfo.getYDim());

        // get end times of that data gridSlice; make its interp time (mid point
        // time)
        Date intTime = _gridTimes.get(index).getCenterTime();

        // now interpolate the grid: for each point in the grid, evaulate
        // the spline function proper to that point, at the given interpTime
        int n = getNumberOfBaseSlices();
        double[] knownTimes = new double[n];
        double[] knownValues = new double[n];
        double[] coeffs = new double[n];

        for (int bdCount = 0; bdCount < n; bdCount++) {
            int soi = getBaseDataIndices()[bdCount];
            // Determine local time for this base data gridSlice.
            // (knownTimeValues() is an Interp class func)
            // first, get long time interval from first one
            long ritime = getKnownTimeValues()[soi].getTime()
                    - getKnownTimeValues()[0].getTime();
            // convert to a double, and load it into working array
            knownTimes[bdCount] = ritime;
        }

        for (int ix = 0; ix < baseInfo.getXDim(); ix++) {
            for (int jy = 0; jy < baseInfo.getYDim(); jy++) {
                double interpTime;
                // Load known values from data structures into working
                // variables.
                // loop through each base data gridSlice
                // to get the grid point value at this grid point, and the time
                for (int bdCount = 0; bdCount < n; bdCount++) {
                    // load this grid point's value and spline coefficients,
                    // previously stored in the baseInfro structure
                    knownValues[bdCount] = baseInfo.get(ix, jy).knownDataValues[bdCount];
                    coeffs[bdCount] = baseInfo.get(ix, jy).splineCoeff[bdCount];
                }

                // convert input AbsTime value "interpTime" into a double,
                // relative to the first data gridSlice in the request,
                // just like other times.
                long ritime = intTime.getTime()
                        - getKnownTimeValues()[0].getTime();
                interpTime = ritime;

                // Do computation.
                double result = knownValues[0], timeDiff, c1, c2;
                int endindex = 0;
                int aveIndex;
                int startindex = n - 1;

                while (startindex - endindex > 1) {
                    aveIndex = (startindex + endindex) / 2;
                    if (knownTimes[aveIndex] > interpTime) {
                        startindex = aveIndex;
                    } else {
                        endindex = aveIndex;
                    }
                }

                timeDiff = knownTimes[startindex] - knownTimes[endindex];
                if (timeDiff == 0.0) {
                    statusHandler.handle(Priority.PROBLEM,
                            "base time values are the same");
                } else {
                    c1 = (knownTimes[startindex] - interpTime) / timeDiff;
                    c2 = (interpTime - knownTimes[endindex]) / timeDiff;

                    result = c1
                            * knownValues[endindex]
                            + c2
                            * knownValues[startindex]
                            + ((c1 * c1 * c1 - c1) * coeffs[endindex] + (c2
                                    * c2 * c2 - c2)
                                    * coeffs[startindex])
                            * (timeDiff * timeDiff) / 6.0;
                }

                outGrid.set(ix, jy, Math.max(Math.min((float) result,
                        getGridParmInfo().getMaxValue()), getGridParmInfo()
                        .getMinValue()));
            }
        }

        return outGrid;
    }

    /**
     * Routine that calls for calculation of the the spline coefficients for all
     * the spline functions determined for a baseInfo grid.
     * 
     * There is one such function for each point in the baseInfo grid.
     * 
     * @param baseInfo
     */
    protected void initializeSplineCoeff(Grid2D<BaseInfo> baseInfo) {
        // Calculate the spline coefficients for a spline curve
        // fitted to the seq of data values at each grid point.
        // There is one such spline curve for the values at
        // each geographic point in the baseInfo grid.
        int n = getNumberOfBaseSlices();

        double[] times = new double[n];
        double[] values = new double[n];
        double[] coeffs = new double[n];
        double[] a = new double[n];

        for (int ix = 0; ix < baseInfo.getXDim(); ix++) {
            for (int jy = 0; jy < baseInfo.getYDim(); jy++) {
                // Load the data into working arrays.
                // loop through each base data gridSlice to get the time,
                // and to get the grid point value at this grid point
                final BaseInfo bi = baseInfo.get(ix, jy);
                for (int bdCount = 0; bdCount < n; bdCount++) {
                    // this grid point's value from this base data gridSlice
                    // (previously stored in the baseInfo structure)
                    values[bdCount] = bi.knownDataValues[bdCount];

                    // the index to this base data gridSlice in the request's
                    int soi = getBaseDataIndices()[bdCount];

                    // Determine local time for this base data gridSlice.
                    // (knownTimeValues() is an Interp class func)
                    // first, get time separation from time of first known data
                    long ritime = getKnownTimeValues()[soi].getTime()
                            - getKnownTimeValues()[0].getTime();
                    // convert to a double, and load it into working array
                    times[bdCount] = ritime;

                    // load initial zero values for coeffs
                    coeffs[bdCount] = 0.0;
                    a[bdCount] = 0.0;
                }

                for (int i = 1; i <= n - 2; i++) {
                    double diff = (times[i] - times[i - 1])
                            / (times[i + 1] - times[i - 1]);
                    double b = diff * coeffs[i - 1] + 2.0;
                    coeffs[i] = (diff - 1.0) / b;
                    a[i] = (6.0
                            * ((values[i + 1] - values[i])
                                    / (times[i + 1] - times[i]) - (values[i] - values[i - 1])
                                    / (times[i] - times[i - 1]))
                            / (times[i + 1] - times[i - 1]) - diff * a[i - 1])
                            / b;
                }

                for (int k = n - 2; k >= 0; k--) {
                    coeffs[k] = coeffs[k] * coeffs[k + 1] + a[k];
                    bi.splineCoeff[k] = coeffs[k];
                }
                if (n != 0) {
                    bi.splineCoeff[n - 1] = coeffs[n - 1];
                }
            }
        }
    }

    /**
     * find "center" of grid points in indicatorGrid whose value == testValue;
     * presumably these are grouped togehter in a distinct area, neither
     * scattered nor covereing a large part of the grid, but in every case this
     * function will give an answer.
     * 
     * center is x,y in " units" of grid index values.
     * 
     * 0,0 is returned if no points of this value are found; this may be used as
     * a signal that there are no such points. In no other case can 0,0
     * returned, (except the trivial case of one valid point at 0,0).
     * 
     * Used in computing areas of interest used in advection calculations.
     * 
     * This is a center of mass of all points in the area. Other possibilities
     * exist, such as center of mass of just the boundary points.
     * 
     * @param indicatorGrid
     * @param testValue
     * @return
     */
    protected Coordinate computeAreaCenter(final Grid2DInteger indicatorGrid,
            final int testValue) {
        int i, j, sumX = 0, sumY = 0, N = 0;

        // check every point on the grid for points of this value,
        // and sum the x and y positions for those points.
        for (i = 0; i < indicatorGrid.getXDim(); i++) {
            for (j = 0; j < indicatorGrid.getYDim(); j++) {
                if (indicatorGrid.get(i, j) == testValue) {
                    sumX += i;
                    sumY += j;
                    N++;
                }
            }
        }

        // determine average of x and y values.
        // return 0,0 if no points of the test value were found.
        double centerX;
        double centerY;
        if (N != 0) {
            centerX = sumX / N;
            centerY = sumY / N;
        } else {
            centerX = 0;
            centerY = 0;
        }

        return new Coordinate(centerX, centerY);
    }

    /**
     * Find angle sector number (numbers 0 to numSectors for 0 to 360 degrees)
     * that this x,y pair lies in.
     * 
     * @param x
     * @param y
     * @return
     */
    private int findAngleSector(int x, int y) {
        double angle = 0.0;

        // find the angle in radians:
        if (x != 0) {
            angle = Math.atan2(y, x);
        } else {
            if (y > 0) {
                angle = Math.PI / 2.0;
            } else if (y < 0) {
                angle = -Math.PI / 2.0;
            }
        }

        // angle is in range -pi to +pi radians

        // (A) number of radians per sector = 2 * pi /(how many sectors in
        // circle)
        double step = 2.0 * Math.PI / _numSectors;

        // compartmentalize into sectors:
        int sector = (int) (0.5 + (angle + Math.PI) / step) - 1;

        sector = Math.max(Math.min(sector, _numSectors - 1), 0);

        return sector;
    }

    /**
     * Create a new grid of values from two previously known scalar grids
     * 
     * Works for input grids with background values of 0.0 which covers
     * (normally) most of the grid. Non-zero areas are assumed to move rather
     * than change value in place.
     * 
     * Zeta is fractional distance from first known grid before the interpolated
     * grid to the known grid afterwards. Ranges from 0.0 at first known grid to
     * 1.0 at last known grid. Really only 0.0<zeta<1.0 correct.
     * 
     * Input is base or known grids just preceding and following grid to
     * interpolate. "firstGrid", "lastGrid"
     * 
     * 1. define "featureStart" and "featureEnd" indicator grids showing where
     * the "moving feature" is in the first and last grids. For this function,
     * the moving feature is where any and all data > zero. For a more powerful
     * interpolation with advection, have the user interactively define the
     * location of the moving feature on the preceding and following grids.
     * 
     * 2. Find center of moving featuer in first and last grids
     * 
     * 3. Compute components of motion of the moving feature.
     * 
     * 4. Determine the working grids. There is a core area with non-zero values
     * found in both known grids, a "fadeArea" grid showing value of points with
     * non-zero values in first grid which translate with the area to points in
     * the last the grid with zero value, and the growArea which is like the
     * fadeArea but for points whose value appears to grow from zero.
     * 
     * 5. Derive interpolated grid (floats) from working grids. The core area
     * translates and changes in value. The fadeArea points translate, fade down
     * to zero value, and also there is a shrinking boundary which pulls in,
     * beyond which fadeArea points do no contribute to the interpolated grid.
     * The growArea points translate, grow from zero to the final known grid
     * value, and appear only inside an enlarging boundary which grows linearly.
     * Motion of center is linear by interpolation fraction. Change of values in
     * coreArea, fadeArea, and growArea from initial known value to final values
     * are linear. Expansion of growArea boundary and contraction of fadeArea
     * boundary is linear by interpolation fraction.
     * 
     * 6. Convert result grid of floats to grid of bytes (key indices)
     * 
     * @param firstGrid
     * @param lastGrid
     * @param zeta
     * @return
     */
    protected Grid2DFloat autoScalarAdvection(final Grid2DFloat firstGrid,
            final Grid2DFloat lastGrid, float zeta) {
        int i, j;
        float maxFAdist = 0.0f;
        float maxGAdist = 0.0f;
        float dist;

        int xDim = firstGrid.getXdim();
        int yDim = firstGrid.getYdim();

        // System.out.println(" Interpolate with automatic advection at "+zeta);

        if (zeta < 0.0 || zeta > 1.0) {
            statusHandler.handle(Priority.PROBLEM,
                    " zeta value outside proper range 0-1 for interpolation: "
                            + zeta + ";  will use 0.5");
            // use this zeta to get some kind of result - the mid-way point
            zeta = 0.5f;
        }

        Grid2DInteger featureStart = new Grid2DInteger(xDim, yDim);
        Grid2DInteger featureEnd = new Grid2DInteger(xDim, yDim);
        Grid2DInteger coreArea = new Grid2DInteger(xDim, yDim);
        Grid2DFloat coreChange = new Grid2DFloat(xDim, yDim);
        Grid2DFloat fadeArea = new Grid2DFloat(xDim, yDim);
        Grid2DFloat growArea = new Grid2DFloat(xDim, yDim);
        Grid2DFloat result = new Grid2DFloat(xDim, yDim);

        // Growth and fading of advected area is controlled by independent
        // sectors.
        // These hold max and min distances to limits of grow and fade areas in
        // each sector around the center of the moving area.
        // Second dimension (2) is not variable; holds upper and lower dist
        // limits.
        Grid2DFloat gaControl = new Grid2DFloat(_numSectors, 2);
        Grid2DFloat faControl = new Grid2DFloat(_numSectors, 2);

        // set zeros in distance control info for _growArea (ga)
        // and _fadeArea (fa).
        // first value (j=0) is min distance; second (j=1) is max distance,
        // for grow Area and _fadeArea points in the angle sector i.

        // 1. For each point on the first or last grid, if data value is > 0.0,
        // set indicator value there. This is to indicate where the "moving
        // feature" is located.
        for (i = 0; i < xDim; i++) {
            for (j = 0; j < yDim; j++) {
                if (firstGrid.get(i, j) > 0.0) {
                    featureStart.set(i, j, 1);
                }
                if (lastGrid.get(i, j) > 0.0) {
                    featureEnd.set(i, j, 1);
                }
            }
        }

        // Special case:
        // look to see whether all non-0 background values have the same value.
        // In that case keep the unique value. The moving feature of single
        // value will move and change shape but every point involved will
        // always have the same identical value.
        boolean found = false, oneType = false;
        float singleValue = 0.0f;
        for (i = 0; i < xDim; i++) {
            for (j = 0; j < yDim; j++) {
                if (featureStart.get(i, j) == 1) {
                    if (!found) // set value when first found a valid one
                    {
                        singleValue = firstGrid.get(i, j);
                        oneType = true;
                        found = true;
                    }
                    if (firstGrid.get(i, j) != singleValue) {
                        oneType = false;
                        break; // have found > 1 type, so can quit looking.
                    }
                }

                if (featureEnd.get(i, j) == 1) {
                    ;
                }
                {
                    if (!found) {
                        singleValue = lastGrid.get(i, j);
                        oneType = true;
                        found = true;
                    }
                    if (lastGrid.get(i, j) != singleValue) {
                        oneType = false;
                        break;
                    }
                }
            }
        }

        if (oneType) {
            statusHandler.handle(Priority.DEBUG,
                    "   Interpolating areas of single value = " + singleValue);
        }

        // 2. find center of moving feature in first and last grids.
        int testValue = 1;
        Coordinate center1 = computeAreaCenter(featureStart, testValue);
        Coordinate center2 = computeAreaCenter(featureEnd, testValue);

        // Special case:
        // if one grid center x,y pair is 0,0, that means no points of that
        // kind of value were found in that grid. Reset the
        // 0,0 to the other center x,y so the area does not move to the
        // 0,0 corner. It will grow (from nothing) or shrink (to nothing)
        // in the same place.
        if (center1.x == 0.0 && center1.y == 0.0) {
            center1.x = center2.x;
            center1.y = center2.y;
        }
        if (center2.x == 0.0 && center2.y == 0.0) {
            center2.x = center1.x;
            center2.y = center1.y;
        }

        // 3. compute components of motion of the moving feature. The change in
        // position of centers. units are integer grid cell index units.
        // (the 0.5 is to round to the nearest integer, so not truncate)
        int dx = (int) (center2.x - center1.x + 0.5);
        int dy = (int) (center2.y - center1.y + 0.5);

        // 5. Determine the core, fade, and grow area working grids.
        // Look at every grid point; compute corresponding translated
        // coordinates tx, ty. Find points in coreArea (coreChange), and
        // in fadeArea and growArea.
        int tx, ty;
        int deltax, deltay, sector;
        for (i = 0; i < xDim; i++) {
            for (j = 0; j < yDim; j++) {
                // corresponding point in the last grid; translated x and y
                tx = i + dx;
                ty = j + dy;

                // make sure translated point is still on grid:
                if (tx >= 0 && tx < xDim && ty >= 0 && ty < yDim) {
                    // determine distance of this point from center of area;
                    dist = (float) ((center1.x - i) * (center1.x - i) + (center1.y - j)
                            * (center1.y - j));

                    // if this point has value>0 on first grid, and >0 at
                    // the corresponding position in the last grid, it is
                    // part of the moving core. Retain its change in value.
                    if (firstGrid.get(i, j) > 0.0 && lastGrid.get(tx, ty) > 0.0) {
                        coreChange.set(i, j,
                                lastGrid.get(tx, ty) - firstGrid.get(i, j));
                        coreArea.set(i, j, 1);
                    }

                    // if there is value>0 at start, but nothing at end, this
                    // point value fades away as it moves, from first value
                    if (firstGrid.get(i, j) > 0.0
                            && lastGrid.get(tx, ty) == 0.0) {
                        fadeArea.set(i, j, firstGrid.get(i, j));
                        // reset max distance, if needed.
                        if (dist > maxFAdist) {
                            maxFAdist = dist;
                        }

                        // compute which angle sector this point lies in
                        deltax = i - (int) (center1.x + 0.5);
                        deltay = j - (int) (center1.y + 0.5);
                        sector = findAngleSector(deltax, deltay);

                        // if dist exceeds an existing limit, reassign it.
                        // set max distance to edge of _fadeArea in this sector
                        if (dist > faControl.get(sector, 1)) {
                            faControl.set(sector, 1, dist);
                        }
                        // set min distance to edge of _fadeArea in this sector
                        if (dist < faControl.get(sector, 0)
                                || faControl.get(sector, 0) == 0.0) {
                            faControl.set(sector, 0, dist);
                        }
                    }

                    // if there is nothing at start, but is in last grid,
                    // this point value grows towards the last value
                    // as it moves.
                    if (firstGrid.get(i, j) == 0.0
                            && lastGrid.get(tx, ty) > 0.0) {
                        growArea.set(i, j, lastGrid.get(tx, ty));
                        // reset max distance, if needed.
                        if (dist > maxGAdist) {
                            maxGAdist = dist;
                        }

                        // compute which angle sector this point lies in
                        deltax = i - (int) (center1.x + 0.5);
                        deltay = j - (int) (center1.y + 0.5);
                        sector = findAngleSector(deltax, deltay);

                        // if dist exceeds an existing limit, reassign it.
                        // set max distance to edge of _growArea in this sector
                        if (dist > gaControl.get(sector, 1)) {
                            gaControl.set(sector, 1, dist);
                        }

                        // set min distance to edge of _growArea in this sector
                        if (dist < gaControl.get(sector, 0)
                                || gaControl.get(sector, 0) == 0.0) {
                            gaControl.set(sector, 0, dist);
                        }
                    }
                }
            }
        }

        // cout<<" first grid: "<<endl;
        // cout<< firstGrid<<endl;
        // cout<<" last grid: "<<endl;
        // cout<< lastGrid<<endl;
        // cout<<" coreChange: "<<endl;
        // cout<< coreChange<<endl;
        // cout<<" fadeArea: "<<endl;
        // cout<< fadeArea<<endl;
        // cout<<" growarea : "<<endl;
        // cout<< growArea<<endl;

        // Note. coreChange is non-zero only for points where both featureStart
        // and the corresponding featureEnd point are non-zero, and for
        // positions
        // whose corresponding translated position is still in the grid. This is
        // the "core area".

        // 6. Now compute the interpolated grid.
        // for every point on the first grid
        for (i = 0; i < xDim; i++) {
            for (j = 0; j < yDim; j++) {
                // corresponding translated positions
                tx = i + (int) ((dx * zeta) + 0.5);
                ty = j + (int) ((dy * zeta) + 0.5);

                // determine distance of this point from center of area;
                dist = (float) ((center1.x - i) * (center1.x - i) + (center1.y - j)
                        * (center1.y - j));

                // compute which angle sector this point lies in,
                // and the distance to furthest out edge of growArea and
                // fadeArea
                deltax = i - (int) (center1.x + 0.5);
                deltay = j - (int) (center1.y + 0.5);
                sector = findAngleSector(deltax, deltay);
                maxFAdist = faControl.get(sector, 1);
                maxGAdist = gaControl.get(sector, 1);

                if (tx >= 0 && tx < xDim && ty >= 0 && ty < yDim) {
                    if (fadeArea.get(i, j) != 0.0) {
                        // use, fading out, marginal areas around core which
                        // translate but do not appear in final moving feature.
                        // use contracting boundary as limit of shown fadeArea
                        // pts.
                        if (dist < maxFAdist * (1.0 - zeta)) {
                            if (oneType) {
                                result.set(tx, ty, singleValue);
                            } else {
                                result.set(tx, ty,
                                        (1.0f - zeta) * fadeArea.get(i, j));
                            }
                        } else {
                            result.set(tx, ty, 0.0f);
                        }
                    }

                    if (growArea.get(i, j) != 0.0f) {
                        // bring up marginal areas around core in moving feature
                        // but which do not appear near core at first.
                        if (dist < maxGAdist * zeta) {
                            if (oneType) {
                                result.set(tx, ty, singleValue);
                            } else {
                                result.set(tx, ty, zeta * growArea.get(i, j));
                            }
                        } else {
                            result.set(tx, ty, 0.0f);
                        }
                    }

                    if (coreArea.get(i, j) == 1) {
                        if (oneType) {
                            result.set(tx, ty, singleValue);
                        } else {
                            // translate core,
                            // and change values linearly as you go.
                            result.set(tx, ty, firstGrid.get(i, j) + zeta
                                    * coreChange.get(i, j));
                        }
                    }
                }
            }
        }
        // This interpolated grid (float values) is done.

        // cout<<" Interpolated grid"<<endl;
        // cout<< result<<endl;

        return result;
    }
}
