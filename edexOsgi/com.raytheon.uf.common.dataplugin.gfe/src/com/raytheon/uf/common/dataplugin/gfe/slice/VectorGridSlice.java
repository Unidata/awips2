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
import java.nio.FloatBuffer;
import java.util.List;

import jep.INumpyable;

import com.raytheon.uf.common.cache.CacheException;
import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.ICache;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.IGrid2D;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Vector version of GridSlice
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/30/2008              chammack    Stubbed-out class based on AWIPS I
 * 02/22/2008   879        rbell       Legacy conversion, extended ScalarSlice
 * 06/10/2009   2159       rjpeter     Updated checkDims to check dirGrid for null
 * 04/23/2013   1949       rjpeter     Updated wind checks to keep float precision.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class VectorGridSlice extends ScalarGridSlice implements Cloneable,
        INumpyable {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VectorGridSlice.class);

    private static final float DEG_IN_CIRCLE = 360.0f;

    @DynamicSerializeElement
    protected Grid2DFloat dirGrid;

    protected String dirCacheId;

    /**
     * Constructor for serialization only.
     */
    public VectorGridSlice() {

    }

    /**
     * Constructor for VectorGridSlice
     * 
     * @param mag
     *            Magnitude grid
     * @param dir
     *            Grid2DFloat of directions in degrees
     * @param validTime
     * @param gfeRecord
     */
    public VectorGridSlice(TimeRange validTime, GFERecord gfeRecord,
            Grid2DFloat mag, Grid2DFloat dir) {
        super(validTime, gfeRecord, mag);
        setDirGrid(dir);
    }

    /**
     * Constructor for VectorGridSlice taking data, grid parm info, valid time
     * and history
     * 
     * @param mag
     *            magnitude
     * @param dir
     *            Grid2DFloat of directions in degrees
     * @param validTime
     *            valid time
     * @param gpi
     *            grid parm info
     * @param history
     *            grid history
     */
    public VectorGridSlice(TimeRange validTime, GridParmInfo gpi,
            GridDataHistory[] history, Grid2DFloat mag, Grid2DFloat dir) {
        super(validTime, gpi, history, mag);
        setDirGrid(dir);
    }

    public VectorGridSlice(TimeRange validTime, GridParmInfo gpi,
            List<GridDataHistory> history, Grid2DFloat mag, Grid2DFloat dir) {
        this(validTime, gpi, history
                .toArray(new GridDataHistory[history.size()]), mag, dir);
    }

    /**
     * Copy constructor, defaults to no caching on copy
     * 
     * @param rhs
     *            Slice to copy
     */
    public VectorGridSlice(VectorGridSlice rhs) {
        this(rhs, false);
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     *            Slice to copy
     * @param useCache
     *            Whether or not to use cache initially. Useful when copying
     *            structure and will need to immediately modify, allowing for
     *            data to only be written to cache once.
     */
    public VectorGridSlice(VectorGridSlice rhs, boolean useCache) {
        super(rhs, useCache);
        Grid2DFloat dGrid = null;
        try {
            dGrid = rhs.getDirGrid().clone();
        } catch (CloneNotSupportedException e) {
            dGrid = new Grid2DFloat();
        }
        setDirGrid(dGrid);
    }

    /**
     * @return magnitudeGrid
     */
    public Grid2DFloat getMagGrid() {
        return getScalarGrid();
    }

    /**
     * @param magGrid
     */
    public void setMagGrid(Grid2DFloat magGrid) {
        setScalarGrid(magGrid);
    }

    /**
     * @return dirGrid
     */
    public Grid2DFloat getDirGrid() {
        if (useCache && (dirCacheId != null)) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                return (Grid2DFloat) diskCache.getFromCache(dirCacheId);
            } catch (CacheException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to load data from GFE cache.", e);
            }
        }

        return this.dirGrid;
    }

    /**
     * @param directionGrid
     */
    public void setDirGrid(Grid2DFloat directionGrid) {
        this.dirGrid = directionGrid;

        if (useCache) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                if (this.dirGrid != null) {
                    if (dirCacheId != null) {
                        diskCache.addToCache(dirCacheId, this.dirGrid);
                    } else {
                        dirCacheId = diskCache.addToCache(this.dirGrid);
                    }
                } else if (dirCacheId != null) {
                    // dirGrid is null, remove previous cache entry
                    diskCache.removeFromCache(dirCacheId);
                    dirCacheId = null;
                }

                this.dirGrid = null;
            } catch (Exception e) {
                // failed to move to local cache, don't remove from memory
                statusHandler.handle(Priority.WARN,
                        "Failed to move data to local cache", e);
            }
        }
    }

    @Override
    public void assign(IGridSlice rhs) {
        if (!(rhs instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempted to assign VectorGridSlice to non-VectorGridSlice object");
        }

        super.assign(rhs);

        Grid2DFloat thisDirGrid = getDirGrid();
        Grid2DFloat rhsDirGrid = ((VectorGridSlice) rhs).getDirGrid();

        if (rhsDirGrid != null) {
            if ((thisDirGrid.getXdim() != rhsDirGrid.getXdim())
                    || (thisDirGrid.getYdim() != rhsDirGrid.getYdim())) {
                throw new IllegalArgumentException(
                        "Supplied grid is not of same dimension");
            }

            thisDirGrid.assign(rhsDirGrid);
        } else {
            thisDirGrid = null;
        }

        // copy to cache
        setDirGrid(thisDirGrid);
    }

    /**
     * The assignment operator for VectorGridSlice will not copy over the
     * dirGrid. Invoke this to include copying the dirGrid.
     * 
     * @param rhs
     *            slice to assign from
     * @param editArea
     *            Grid2DBit to use when copying
     * @param directionGrid
     */
    public void assign(VectorGridSlice rhs, Grid2DBit editArea,
            Grid2DFloat directionGrid) {
        super.operateEquals(Op.ASSIGN, rhs, editArea);
        Grid2DFloat dGrid = getDirGrid();
        dGrid.copyWithMask(directionGrid, editArea);

        // copy to cache
        setDirGrid(dGrid);
    }

    @Override
    public String isValid() {
        String nullTest = "";

        if ((nullTest = super.isValid()) != null) {
            return nullTest;
        }

        Grid2DFloat dGrid = getDirGrid();
        if ((dGrid == null) || !dGrid.isValid()) {
            return "Direction grid is invalid";
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (!(obj instanceof VectorGridSlice)) {
            return false;
        }

        VectorGridSlice rhs = (VectorGridSlice) obj;
        Grid2DFloat thisDirGrid = getDirGrid();
        Grid2DFloat rhsDirGrid = rhs.getDirGrid();

        if (thisDirGrid == null) {
            if (rhsDirGrid == null) {
                return true;
            }

            return false;
        }
        return thisDirGrid.equals(rhsDirGrid);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder rVal = new StringBuilder(super.toString());

        rVal.append("Vector Magnitude grid: ").append(getMagGrid())
                .append("\n");
        rVal.append("Vector Direction grid: ").append(getDirGrid())
                .append("\n");

        return rVal.toString();
    }

    /**
     * Makes a vector grid slice from u and v components.
     * 
     * Converts the u and v into mag and dir and then calls the appropriate
     * GridSlice constructor.
     * 
     * @param u
     * @param v
     * @param validTime
     * @param gfeRecord
     * @return the grid slice
     */
    public static VectorGridSlice makeGridSliceFromUV(Grid2DFloat u,
            Grid2DFloat v, TimeRange validTime, GFERecord gfeRecord) {
        Grid2DFloat magGrid, dirGrid;
        magGrid = new Grid2DFloat(u.getXdim(), u.getYdim());
        dirGrid = new Grid2DFloat(magGrid);
        // convert the u, v grids to mag and dir
        int i, j;
        for (i = 0; i < u.getXdim(); i++) {
            for (j = 0; j < u.getYdim(); j++) {
                magGrid.set(
                        i,
                        j,
                        (float) Math.sqrt(u.get(i, j) * u.get(i, j)
                                + v.get(i, j) * v.get(i, j)));
                float dir = (float) Math.toDegrees(Math.atan2(u.get(i, j),
                        v.get(i, j)));
                while (dir < 0.0f) {
                    dir += DEG_IN_CIRCLE;
                }
                while (dir >= DEG_IN_CIRCLE) {
                    dir -= DEG_IN_CIRCLE;
                }

                dirGrid.set(i, j, dir);
            }
        }

        return new VectorGridSlice(validTime, gfeRecord, magGrid, dirGrid);
    }

    /**
     * Makes a vector grid slice from u and v components.
     * 
     * Converts the u and v into mag and dir and then calls the appropriate
     * GridSlice constructor.
     * 
     * @param u
     * @param v
     * @param validTime
     * @param gridParmInfo
     * @param history
     * @return the grid slice
     */
    public static VectorGridSlice makeGridSliceFromUV(Grid2DFloat u,
            Grid2DFloat v, TimeRange validTime, GridParmInfo gridParmInfo,
            GridDataHistory[] history) {
        Grid2DFloat magGrid, dirGrid;
        magGrid = new Grid2DFloat(u.getXdim(), u.getYdim());
        dirGrid = new Grid2DFloat(magGrid);

        // convert the u, v grids to mag and dir
        int i, j;
        for (i = 0; i < u.getXdim(); i++) {
            for (j = 0; j < u.getYdim(); j++) {
                magGrid.set(
                        i,
                        j,
                        (float) Math.sqrt(u.get(i, j) * u.get(i, j)
                                + v.get(i, j) * v.get(i, j)));
                float dir = (float) Math.toDegrees(Math.atan2(u.get(i, j),
                        v.get(i, j)));
                while (dir < 0.0f) {
                    dir += DEG_IN_CIRCLE;
                }
                while (dir >= DEG_IN_CIRCLE) {
                    dir -= DEG_IN_CIRCLE;
                }

                dirGrid.set(i, j, dir);
            }
        }

        return new VectorGridSlice(validTime, gridParmInfo, history, magGrid,
                dirGrid);
    }

    /**
     * Returns the u component of the grid if it is of type vector. Otherwise
     * returns an invalid grid.
     * 
     * Calculate and return the u component of the grid.
     * 
     * @return the u component grid
     */
    public Grid2DFloat vectorUGrid() {
        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();
        Grid2DFloat uGrid = new Grid2DFloat(mGrid.getXdim(), mGrid.getYdim());
        int i, j;
        for (i = 0; i < uGrid.getXdim(); i++) {
            for (j = 0; j < uGrid.getYdim(); j++) {
                float angle = (float) Math.toRadians(dGrid.get(i, j));
                uGrid.set(i, j, (float) (Math.sin(angle) * mGrid.get(i, j)));
            }
        }
        return uGrid;
    }

    /**
     * Returns the v component of the grid if it is of type vector. Otherwise
     * returns an invalid grid.
     * 
     * Calculate and return the v component of the grid.
     * 
     * @return the v component grid
     */
    public Grid2DFloat vectorVGrid() {
        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();
        Grid2DFloat vGrid = new Grid2DFloat(mGrid.getXdim(), mGrid.getYdim());
        int i, j;
        for (i = 0; i < vGrid.getXdim(); i++) {
            for (j = 0; j < vGrid.getYdim(); j++) {
                float angle = (float) Math.toRadians(dGrid.get(i, j));
                vGrid.set(i, j, (float) (Math.cos(angle) * mGrid.get(i, j)));
            }
        }
        return vGrid;
    }

    @Override
    protected String checkDims() {
        String nullTest = "";
        if ((nullTest = super.checkDims()) != null) {
            return nullTest;
        }

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        if ((mGrid.getXdim() != dGrid.getXdim())
                || (mGrid.getYdim() != dGrid.getYdim())) {
            return "Magnitude and Direction grids have different dimensions";
        }

        return null;
    }

    @Override
    protected String checkDataLimits() {
        String nullTest = "";
        if ((nullTest = super.checkDataLimits()) != null) {
            return nullTest;
        }

        Grid2DFloat dGrid = getDirGrid();

        FloatBuffer dir = dGrid.getBuffer();
        int size = dir.capacity();
        for (int i = 0; i < size; i++) {
            float thisDir = dir.get(i);
            while (thisDir < 0.0f) {
                thisDir += DEG_IN_CIRCLE;
            }
            while (thisDir >= DEG_IN_CIRCLE) {
                thisDir -= DEG_IN_CIRCLE;
            }
            dir.put(i, thisDir);
        }
        return null;
    }

    /**
     * @param gs
     * @param editArea
     * @return the verticalMotion grid
     */
    public ScalarGridSlice verticalMotion(VectorGridSlice gs, Grid2DBit editArea) {
        Grid2DFloat mGrid = getMagGrid();

        if ((mGrid.getXdim() != editArea.getXdim())
                || (mGrid.getYdim() != editArea.getYdim())) {
            throw new IllegalArgumentException(
                    "This and editArea grids have different dimensions");
        }

        Grid2DFloat gsmGrid = gs.getMagGrid();
        if ((mGrid.getXdim() != gsmGrid.getXdim())
                || (mGrid.getYdim() != gsmGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This and supplied grids have different dimensions");
        }

        Point ll = new Point();
        Point ur = new Point();

        editArea.extremaOfSetBits(ll, ur);

        // make a grid and set all values to 0
        Grid2DFloat floatGrid;
        try {
            floatGrid = mGrid.clone();
        } catch (CloneNotSupportedException e1) {
            floatGrid = new Grid2DFloat();
        }
        floatGrid.setAllValues(0.0f);
        float maxFloat = -1.0f;
        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; i <= ur.y; i++) {
                floatGrid.set(i, j, findTerrainVerticalMotion(i, j, gs));
                // Save the maximum absolute value
                if (Math.abs(floatGrid.get(i, j)) > maxFloat) {
                    maxFloat = Math.abs(floatGrid.get(i, j));
                }
            }
        }

        // Now divide by the max value to normalize values from -1 -> 1
        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; i <= ur.y; i++) {
                floatGrid.set(i, j, floatGrid.get(i, j) / maxFloat);
            }
        }

        // Make a new GridSlice with the new grid
        GridParmInfo pInfo = new GridParmInfo(gridParmInfo.getParmID(),
                gridParmInfo.getGridLoc(), GridType.SCALAR, "",
                "Vertical Motion", -1.0f, 1.0f, 2, false,
                gridParmInfo.getTimeConstraints(), false);

        GridDataHistory newGDH[] = new GridDataHistory[this.gridDataHistory
                .size()];
        for (int i = 0; i < this.gridDataHistory.size(); i++) {
            try {
                newGDH[i] = this.gridDataHistory.get(i).clone();
            } catch (CloneNotSupportedException e) {
                newGDH[i] = new GridDataHistory();
            }
        }
        TimeRange sliceTimeRange = this.validTime.clone();
        return new ScalarGridSlice(sliceTimeRange, pInfo, newGDH, floatGrid);
    }

    /**
     * At grid point (xc, yc) multiply horizontal wind components by terrain
     * slope vector to find vertical motion of air (meters per s) induced by
     * going up or down along terrain.
     * 
     * Result units are meters per second, positive for upward motion. + value =
     * air going up; - value = air going down;
     * 
     * ARGUMENTS: INPUT: grid point position indices (xc, yc)
     * 
     * returns: vertical wind speed component
     * 
     * USES: wind values _windspeedkt (in KNOTS) and _winddir in degrees.
     * 
     * NOTE input _winddir is "direction FROM" in degrees clockwise from true
     * NORTH (NOT from east, going ccw as in conventional radial coordinates).
     * Stuart Wier June 1999
     * 
     * @param xc
     * @param yc
     * @param elevation
     * @return the vertical wind speed component
     */
    public float findTerrainVerticalMotion(int xc, int yc,
            VectorGridSlice elevation) {
        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();
        float vertAirSpeed = 0.0f;

        // Determine wind u and v components.

        // First compute wind vector angle from north, in radians.
        float rads = (float) Math.toRadians((dGrid.get(xc, yc) - 180.0));

        // u and v components
        // (convert from knots to meters per second 1.94384 knots / m/s )
        float uw = (float) (Math.sin(rads) * mGrid.get(xc, yc) / 1.94384);
        float vw = (float) (Math.cos(rads) * mGrid.get(xc, yc) / 1.94384);

        // (you might already have uw and vw to work with;
        // make sure they are in meters per second.)

        // find slope vector components (svx, svy) at this point (xc, yc).
        // Direction is that of maximum slope and magnitude is the
        // slope = rise/run, unitless.
        float thisFloatArray[] = findSlopeVector(xc, yc,
                elevation.getScalarGrid());
        float svx = thisFloatArray[0], svy = thisFloatArray[1];

        // logEvent <<" wind uw = "<< uw <<" vw = "<< vw <<std::endl;
        // logEvent <<" slope svx = "<<svx<< " svy = "<< svy <<std::endl;

        // multiply (dot product) wind vector by slope vector
        // to get the value of the vertical air motion.
        vertAirSpeed = uw * svx + vw * svy;

        return vertAirSpeed; // meters per second, positive upward.
    }

    /**
     * Create, for this grid point (xc,yc) in a grid of topography or elevation
     * values, the vector (svx, svy) whose direction is in the direction of
     * maximum rate of increase in elevation, and whose magnitude is the slope,
     * unitless, rise over run, for example 100m / 1000m = 0.10. Typical
     * magnitudes should be in the range of 0.0 to 0.1, usually close to 0.0.
     * 
     * This vector represents an AVERAGE slope at the point, a plane fitted to
     * the surrounding 8 elevation values. It is zero if the point is at the
     * bottom of a trough or on top a ridge, or if the point is equally above or
     * below all surrounding points. This is the result required by meteorology:
     * such cases have no terrain induced flow at that point, though there may
     * be strong terrain induced flow nearby.
     * 
     * ARGUMENTS: Input: grid indices (xc,yc) in elevation grid. makes: vector x
     * and y components (svx, svy) of the slope vector.
     * 
     * USES: elevation grid, _elevation a Grid2D<float> grid size, _xGridSize,
     * _yGridSize average grid spacing, _aveGridSpace MUST BE SAME UNITS AS
     * ELEVATION DATA
     * 
     * Typical grid point separation is about 10 km in IFPS and the elevation
     * associated with each grid point is the average elevation for its
     * vicinity, so the data allows only an approximation of the slope at any
     * point, and is correct only in the sense of terrain smoothed to the grid
     * resolution and even then will be only an estimate. Much better than doing
     * nothing but not likely to be the actual slope at a particular single
     * point.
     * 
     * For speed, uses average grid spacing, in same units as elevation,
     * _aveGridSpace. This will not be exactly right but is going to be at least
     * as accurate as the assumption of computing the average slope at the grid
     * point from eight widely-separated neighbor grid points' average
     * elevations.
     * 
     * NOTE: assumes the _elevation grid is aligned on True North. If not, then
     * a correction is required before using this result vector in a true north
     * system. If the orientation of the elevation grid is close to but not
     * quite true north, the error of using this without correction is off 2
     * degrees, error = 3% off 5 degrees, error 9% off 10 degrees, error 18% all
     * of which may be less than the error of estimating local slope from eight
     * remote average elevations. Stuart Wier June 1999
     * 
     * @param xc
     * @param yc
     * @param elevation
     * @return the slope vector
     */
    public float[] findSlopeVector(int xc, int yc, Grid2DFloat elevation) {
        throw new UnsupportedOperationException("");

        // int i, j, count = 0;
        // float dist, slope, xcomp, ycomp, sumxcomp = 0.0f, sumycomp = 0.0f;
        //
        // // the elevation of the center grid point at xc,yc.
        // float centerh = elevation.get(xc, yc);
        //
        // // grid spacing must be in same units as elevation. If you use
        // // kilometers here, the slopes will be extremely steep!
        // // must be positive.
        // Point nominalRes = gridInfo().gridLoc().nominalResolution();
        // float _aveGridSpace = (float) (nominalRes.x + nominalRes.y) * 1000 /
        // 2;
        // Point gridSize = new Point(scalarGrid.getXDim(),
        // scalarGrid.getYDim());
        //
        // for (i = xc - 1; i <= xc + 1; i++)
        // for (j = yc - 1; j <= yc + 1; j++) {
        // // skip indices beyond limits of grid
        // if (i < 0 || j < 0 || i >= gridSize.x || j >= gridSize.y)
        // continue;
        //
        // // components of vector pointing from the center xc,yc
        // // to the grid point (i,j)
        // xcomp = i - xc; // typically +/- 1 or 0
        // ycomp = j - yc; // typically +/- 1 or 0
        //
        // // if at center point; distance is 0, do not compute
        // if (i == xc && j == yc)
        // continue;
        //
        // // distance between pair of grid points
        // dist = _aveGridSpace
        // * (float) Math.sqrt(xcomp * xcomp + ycomp * ycomp);
        //
        // // error trap to avoid 0 divide; should never occur
        // if (dist == 0.0)
        // continue;
        //
        // // slope from center to the other grid point; + if up from
        // // center
        // // (dist and _elevation values must be in same units)
        // slope = (elevation.get(i, j) - centerh) / dist;
        //
        // // multiply original components by slope to get the slope vector
        // // components from (xc,yc) to (i,j),
        // // and add into summation of all x and y components
        // sumxcomp += xcomp * slope;
        // sumycomp += ycomp * slope;
        // count++;
        // }
        //
        // // average all slope vectors to neighbor points
        // float svx = sumxcomp / count;
        // float svy = sumycomp / count;
        //
        // // ensure "reasonable" values - less than 45 degrees
        // if (Math.abs(svx) > 1.0)
        // svx = svx / Math.abs(svx);
        // if (Math.abs(svy) > 1.0)
        // svy = svy / Math.abs(svy);
        //
        // return new float[] { svx, svy };
    }

    @Override
    public IContinuousSlice min(IContinuousSlice gs) {
        if (!(gs instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "Supplied GridSlice is not Scalar");
        }

        VectorGridSlice rhs = (VectorGridSlice) gs;
        Grid2DFloat thisMagGrid = getMagGrid();
        Grid2DFloat rhsMagGrid = rhs.getMagGrid();

        if ((thisMagGrid.getXdim() != rhsMagGrid.getXdim())
                || (thisMagGrid.getYdim() != rhsMagGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This and supplied GridSlice are different dimensions");
        }

        // create new grid with no caching
        VectorGridSlice newGS = new VectorGridSlice(rhs, false);
        Grid2DFloat thisDirGrid = getDirGrid();
        Grid2DFloat newMagGrid = newGS.getMagGrid();
        Grid2DFloat newDirGrid = newGS.getDirGrid();

        FloatBuffer thisB = thisMagGrid.getBuffer();
        FloatBuffer thisDB = thisDirGrid.getBuffer();
        FloatBuffer rhsB = rhsMagGrid.getBuffer();
        FloatBuffer newB = newMagGrid.getBuffer();
        FloatBuffer newDB = newDirGrid.getBuffer();

        float thisF;
        float thisD;
        float rhsF;
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            thisF = thisB.get(i);
            thisD = thisDB.get(i);
            rhsF = rhsB.get(i);
            if (rhsF > thisF) {
                newB.put(i, thisF);
                newDB.put(i, thisD);
            }
        }

        newGS.setMagGrid(newMagGrid);
        newGS.setDirGrid(newDirGrid);

        return newGS;
    }

    @Override
    public IContinuousSlice max(IContinuousSlice gs) {
        if (!(gs instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "Supplied GridSlice is not Scalar");
        }

        VectorGridSlice rhs = (VectorGridSlice) gs;
        Grid2DFloat thisMagGrid = getMagGrid();
        Grid2DFloat rhsMagGrid = rhs.getMagGrid();

        if ((thisMagGrid.getXdim() != rhsMagGrid.getXdim())
                || (thisMagGrid.getYdim() != rhsMagGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This and supplied GridSlice are different dimensions");
        }

        // create new grid with no caching
        VectorGridSlice newGS = new VectorGridSlice(rhs, false);
        Grid2DFloat thisDirGrid = getDirGrid();
        Grid2DFloat newMagGrid = newGS.getMagGrid();
        Grid2DFloat newDirGrid = newGS.getDirGrid();

        FloatBuffer thisB = thisMagGrid.getBuffer();
        FloatBuffer thisDB = thisDirGrid.getBuffer();
        FloatBuffer rhsB = rhsMagGrid.getBuffer();
        FloatBuffer newB = newMagGrid.getBuffer();
        FloatBuffer newDB = newDirGrid.getBuffer();

        float thisF;
        float thisD;
        float rhsF;
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            thisF = thisB.get(i);
            thisD = thisDB.get(i);
            rhsF = rhsB.get(i);
            if (rhsF < thisF) {
                newB.put(i, thisF);
                newDB.put(i, thisD);
            }
        }

        newGS.setMagGrid(newMagGrid);
        newGS.setDirGrid(newDirGrid);

        return newGS;
    }

    private Grid2DBit eq(float value) {
        Grid2DBit bits;

        int thisIntArray[] = parseVector(value);
        int mag = thisIntArray[0], dir = thisIntArray[1];

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        Grid2DBit magBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        Grid2DBit dirBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        if ((mag != 0) || ((dir == 0) && (mag == 0))) { // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) == mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) { // Test Direction
            int lower = dir - 5;
            int upper = dir + 5;
            boolean cross360 = false;
            if (lower < 0) {
                lower = 360 + lower;
                cross360 = true;
            }
            if (upper > 360) {
                upper = upper % 360;
                cross360 = true;
            }
            if (cross360) {
                for (int i = 0; i < mGrid.getXdim(); i++) {
                    for (int j = 0; j < mGrid.getYdim(); j++) {
                        if ((dGrid.get(i, j) >= lower)
                                || (dGrid.get(i, j) <= upper)) {
                            dirBits.set(i, j);
                        }
                    }
                }
            } else {
                for (int i = 0; i < mGrid.getXdim(); i++) {
                    for (int j = 0; j < mGrid.getYdim(); j++) {
                        if ((dGrid.get(i, j) >= lower)
                                && (dGrid.get(i, j) <= upper)) {
                            dirBits.set(i, j);
                        }
                    }
                }
            }
        }
        if ((mag != 0) && (dir != 0)) {
            // "AND" magnitude and direction
            bits = magBits.and(dirBits);
        } else if (dir != 0) {
            bits = dirBits;
        } else {
            bits = magBits;
        }

        return bits;
    }

    private Grid2DBit neq(float value) {
        Grid2DBit bits = eq(value);
        bits.negate();
        return bits;
    }

    private Grid2DBit gt(float value) {
        Grid2DBit bits;

        int thisIntArray[] = parseVector(value);
        int mag = thisIntArray[0], dir = thisIntArray[1];

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        Grid2DBit magBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        Grid2DBit dirBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());

        if ((mag != 0) || ((dir == 0) && (mag == 0))) { // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) > mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) { // Test Direction
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getXdim(); j++) {
                    if (dGrid.get(i, j) > dir) {
                        dirBits.set(i, j);
                    }
                }
            }
        }
        if ((mag != 0) && (dir != 0)) {
            // "AND" magnitude and direction
            bits = magBits.and(dirBits);
        } else if (dir != 0) {
            bits = dirBits;
        } else {
            bits = magBits;
        }

        return bits;
    }

    private Grid2DBit gtEq(float value) {
        Grid2DBit bits;

        int thisIntArray[] = parseVector(value);
        int mag = thisIntArray[0], dir = thisIntArray[1];

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        Grid2DBit magBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        Grid2DBit dirBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());

        if ((mag != 0) || ((dir == 0) && (mag == 0))) { // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) >= mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) { // Test Direction
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getXdim(); j++) {
                    if (dGrid.get(i, j) >= dir) {
                        dirBits.set(i, j);
                    }
                }
            }
        }
        if ((mag != 0) && (dir != 0)) {
            // "AND" magnitude and direction
            bits = magBits.and(dirBits);
        } else if (dir != 0) {
            bits = dirBits;
        } else {
            bits = magBits;
        }

        return bits;
    }

    private Grid2DBit lt(float value) {
        Grid2DBit bits;

        int thisIntArray[] = parseVector(value);
        int mag = thisIntArray[0], dir = thisIntArray[1];

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        Grid2DBit magBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        Grid2DBit dirBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());

        if ((mag != 0) || ((dir == 0) && (mag == 0))) { // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) < mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) { // Test Direction
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getXdim(); j++) {
                    if (dGrid.get(i, j) < dir) {
                        dirBits.set(i, j);
                    }
                }
            }
        }
        if ((mag != 0) && (dir != 0)) {
            // "AND" magnitude and direction
            bits = magBits.and(dirBits);
        } else if (dir != 0) {
            bits = dirBits;
        } else {
            bits = magBits;
        }

        return bits;
    }

    private Grid2DBit ltEq(float value) {
        Grid2DBit bits;

        int thisIntArray[] = parseVector(value);
        int mag = thisIntArray[0], dir = thisIntArray[1];

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        Grid2DBit magBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        Grid2DBit dirBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());

        if ((mag != 0) || ((dir == 0) && (mag == 0))) { // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) <= mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) { // Test Direction
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getXdim(); j++) {
                    if (dGrid.get(i, j) <= dir) {
                        dirBits.set(i, j);
                    }
                }
            }
        }
        if ((mag != 0) && (dir != 0)) {
            // "AND" magnitude and direction
            bits = magBits.and(dirBits);
        } else if (dir != 0) {
            bits = dirBits;
        } else {
            bits = magBits;
        }

        return bits;
    }

    @Override
    public Grid2DBit comparisonOperate(Op op, float value) {
        switch (op) {
        case EQ:
            return this.eq(value);
        case NOT_EQ:
            return this.neq(value);
        case GT:
            return this.gt(value);
        case GT_EQ:
            return this.gtEq(value);
        case LT:
            return this.lt(value);
        case LT_EQ:
            return this.ltEq(value);
        default:
            throw new IllegalArgumentException("Operator " + op
                    + " not supported");
        }
    }

    @Override
    public Grid2DBit almost(float value, float fuzz) {
        Grid2DBit bits;

        int thisIntArray[] = parseVector(value);
        int mag = thisIntArray[0], dir = thisIntArray[1];

        Grid2DFloat mGrid = getMagGrid();
        Grid2DFloat dGrid = getDirGrid();

        Grid2DBit magBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        Grid2DBit dirBits = new Grid2DBit(mGrid.getXdim(), mGrid.getYdim());
        if ((mag != 0) || ((dir == 0) && (mag == 0))) { // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (Math.abs(mGrid.get(i, j) - mag) <= fuzz) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) { // Test Direction
            int lower = dir - 5;
            int upper = dir + 5;
            boolean cross360 = false;
            if (lower < 0) {
                lower = 360 + lower;
                cross360 = true;
            }
            if (upper > 360) {
                upper = upper % 360;
                cross360 = true;
            }
            if (cross360) {
                for (int i = 0; i < mGrid.getXdim(); i++) {
                    for (int j = 0; j < mGrid.getYdim(); j++) {
                        if ((dGrid.get(i, j) >= lower)
                                || (dGrid.get(i, j) <= upper)) {
                            dirBits.set(i, j);
                        }
                    }
                }
            } else {
                for (int i = 0; i < mGrid.getXdim(); i++) {
                    for (int j = 0; j < mGrid.getYdim(); j++) {
                        if ((dGrid.get(i, j) >= lower)
                                && (dGrid.get(i, j) <= upper)) {
                            dirBits.set(i, j);
                        }
                    }
                }
            }
        }
        if ((mag != 0) && (dir != 0)) {
            // "AND" magnitude and direction
            bits = magBits.and(dirBits);
        } else if (dir != 0) {
            bits = dirBits;
        } else {
            bits = magBits;
        }

        return bits;
    }

    /**
     * Parses a vector value and returns the mag and dir components.
     * 
     * The value could be various lengths and formats: 1/2/3 digits is magnitude
     * only 4 digits is ddff (2-digit direction, 2-digit magnitude) 5 digits is
     * ddfff (2-digit direction, 3-digit magnitude)
     * 
     * Leading zero's are preserved by designating them as leading 9's
     * 
     * Convert 2-digit direction to 3-digit i.e. 36 --> 360 degrees
     * 
     * @param value
     * @return
     */
    private int[] parseVector(float value) {
        int mag, dir;
        if (value < 1000.0) {
            mag = (int) value;
            dir = 0;
        } else if (value < 10000.0) { // 2 digit magnitude
            dir = (int) (value / 100.0);
            mag = (int) (value - (100 * dir));
        } else { // 3 digit magnitude
            dir = (int) (value / 1000.0);
            mag = (int) (value - (1000 * dir));
        }
        if (dir == 99) {
            dir = 0;
        }
        if (dir > 90) {
            dir = dir - 90;
        }
        dir = dir * 10;

        return new int[] { mag, dir };
    }

    /**
     * Cloned grid without caching.
     */
    @Override
    public VectorGridSlice clone() throws CloneNotSupportedException {
        TimeRange aValidTime = this.validTime.clone();
        GridParmInfo aGpi = this.gridParmInfo.clone();
        GridDataHistory[] aHistory = new GridDataHistory[this.gridDataHistory
                .size()];
        for (int i = 0; i < aHistory.length; i++) {
            GridDataHistory thisGDH = this.gridDataHistory.get(i);
            if (thisGDH != null) {
                aHistory[i] = thisGDH.clone();
            }
        }
        Grid2DFloat aGrid = getScalarGrid();
        if (aGrid != null) {
            aGrid = aGrid.clone();
        }
        Grid2DFloat aDirectionGrid = getDirGrid();
        if (aDirectionGrid != null) {
            aDirectionGrid = aDirectionGrid.clone();
        }
        VectorGridSlice rval = new VectorGridSlice(aValidTime, aGpi, aHistory,
                aGrid, aDirectionGrid);
        return rval;
    }

    @Override
    public Object[] getNumPy() {
        return new Object[] { this.getMagGrid().getFloats(),
                this.getDirGrid().getFloats() };
    }

    @Override
    public int getNumpyX() {
        return this.getMagGrid().getXdim();
    }

    @Override
    public int getNumpyY() {
        return this.getMagGrid().getYdim();
    }

    @Override
    protected void moveDataToLocalCache() {
        super.moveDataToLocalCache();
        setDirGrid(getDirGrid());
    }

    @Override
    protected void moveDataToMem() {
        super.moveDataToMem();
        if (dirCacheId != null) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");
                this.dirGrid = (Grid2DFloat) diskCache.getFromCache(dirCacheId);
                diskCache.removeFromCache(dirCacheId);
            } catch (CacheException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to load data from GFE cache.", e);
                return;
            }
        }

        dirCacheId = null;
    }

    @Override
    public boolean isPopulated() {
        if (super.isPopulated()) {
            if (useCache) {
                return dirCacheId != null;
            }

            return dirGrid != null;
        }

        return false;
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        if (dirCacheId != null) {
            @SuppressWarnings("unchecked")
            ICache<IGrid2D> diskCache = CacheFactory.getInstance().getCache(
                    "GFE");

            diskCache.removeFromCache(dirCacheId);
        }
    }
}
