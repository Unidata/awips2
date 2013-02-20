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
import java.nio.FloatBuffer;
import java.util.Date;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.slice.IContinuousSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Placeholder for VectorGridData
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Class Skeleton.
 * 03/25/2008   879        rbell       Legacy conversion.
 * 02/19/2013   1637       randerso    Added throws declarations to translateDataFrom
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class VectorGridData extends OrderedGridData implements Cloneable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VectorGridData.class);

    public VectorGridData(Parm aParm, IGridSlice aSlice) {
        super(aParm, aSlice);
        if (!(aSlice instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "VectorGridSlice required for VectorGridData");
        }
    }

    @Override
    public Grid2DBit doDelta(Date time, float delta, boolean taper,
            Grid2DBit pointsToChange) {
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();

        if (magGrid.getXdim() != pointsToChange.getXdim()
                || magGrid.getYdim() != pointsToChange.getYdim()) {
            throw new IllegalArgumentException(
                    "Dimension mismatch in doDelta: " + magGrid.getXdim() + ','
                            + magGrid.getYdim() + ' '
                            + pointsToChange.getXdim() + ','
                            + pointsToChange.getYdim());
        }

        // indicators whehter to change the magnitude or direction
        // values
        boolean doMag = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.MAGNITUDE)
                || this.parm.getParmState().getVectorMode()
                        .equals(ParmState.VectorMode.BOTH);
        boolean doDir = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.DIRECTION);

        Point ll = new Point();
        Point ur = new Point();

        // check if there any points at all in the selected group, and
        // if
        // so, also get the grid position limits encompassing them.
        if (pointsToChange.extremaOfSetBits(ll, ur)) {
            float minLimit = this.getMinValue();
            float maxLimit = this.getMaxValue();

            // check all the points in this region
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    // if this is one of the points selected to change
                    if (pointsToChange.get(i, j) != 0) {
                        // if changing magnitude, add the (possibly
                        // adjusted) delta
                        if (doMag) {
                            float value = magGrid.get(i, j) + delta;

                            // Ensure we don't exceed parm limits
                            if (value < minLimit) {
                                value = minLimit;
                            } else if (value > maxLimit) {
                                value = maxLimit;
                            }

                            magGrid.set(i, j, value);
                        }

                        // if changing direction, increase or decrease
                        // the direction by 10 degrees, according to the
                        // sign of delta, possibly adjusted to make a taper near
                        // edge of group.
                        if (doDir) {
                            float val = dirGrid.get(i, j);
                            if (delta > 0) {
                                val += 10f;
                            } else {
                                val -= 10f;
                            }

                            // make such dir has not gone out of bounds by
                            // that change
                            while (val >= 360.0) {
                                val -= 360;
                            }
                            while (val < 0.0) {
                                val += 360;
                            }

                            dirGrid.set(i, j, val);
                        }
                    }
                }
            }
        }

        // If the taper flag is set, taper the edges.
        if (taper) {
            taperGrid(time, pointsToChange);
        }

        if (doMag) {
            thisSlice.setMagGrid(magGrid);
        }

        if (doDir) {
            thisSlice.setDirGrid(dirGrid);
        }
        return pointsToChange;
    }

    /**
     * Using the data within and around the grid cells specified, fills in data
     * for the specified grid cells. Returns the points modified.
     * 
     * Calls interpSpatialGap in OrderedGridData.
     * 
     * @param time
     * @param pointsToFillIn
     * @return
     */
    @Override
    protected Grid2DBit doFillIn(Date time, Grid2DBit pointsToFillIn) {
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();

        if (magGrid.getXdim() != pointsToFillIn.getXdim()
                || magGrid.getYdim() != pointsToFillIn.getYdim()) {
            throw new IllegalArgumentException(
                    "Dimension mismatch in doFillIn: " + magGrid.getXdim()
                            + ',' + magGrid.getYdim() + ' '
                            + pointsToFillIn.getXdim() + ','
                            + pointsToFillIn.getYdim());
        }

        boolean editMag = true;
        boolean editDir = true;
        if (this.parm.getParmState().getVectorMode() == VectorMode.DIRECTION) {
            editMag = false;
        } else if (this.parm.getParmState().getVectorMode() == VectorMode.MAGNITUDE) {
            editDir = false;
        }

        // get the grids
        Grid2DFloat mGrid;
        Grid2DFloat dGrid;
        try {
            VectorGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            mGrid = slice.getMagGrid();
            dGrid = slice.getDirGrid();
        } catch (CloneNotSupportedException e) {
            mGrid = new Grid2DFloat();
            dGrid = new Grid2DFloat();
        }

        // Interpolate the speed separately
        if (editMag) {
            interpSpatialGap(pointsToFillIn, mGrid, this.getMinValue(),
                    this.getMaxValue(), magGrid);

            // copy back to cache
            thisSlice.setMagGrid(magGrid);
        }

        // Decompose the direction and interpolate the components
        if (editDir) {
            Grid2DFloat uComp, vComp;
            Grid2DBit allCells = new Grid2DBit(dGrid.getXdim(),
                    dGrid.getYdim(), true);

            Grid2DFloat[] outGrids = dirToUV(allCells, dGrid);
            uComp = outGrids[0];
            vComp = outGrids[1];

            interpSpatialGap(pointsToFillIn, uComp, -this.getMaxValue(),
                    this.getMaxValue(), uComp);
            interpSpatialGap(pointsToFillIn, vComp, -this.getMaxValue(),
                    this.getMaxValue(), vComp);

            // reassemble the direction
            uVToDir(pointsToFillIn, uComp, vComp, dirGrid);

            // copy back to cache
            thisSlice.setDirGrid(dirGrid);
        }

        return pointsToFillIn;
    }

    public float getMagValue(int x, int y) {
        populate();
        return getMagGrid().get(x, y);
    }

    public float getDirValue(int x, int y) {
        populate();
        return getDirGrid().get(x, y);
    }

    @Override
    public WxValue getWxValue(int x, int y) {
        return new VectorWxValue(getMagGrid().get(x, y),
                getDirGrid().get(x, y), this.parm);
    }

    @Override
    public void set(Point gridLoc, WxValue wxValue) {
        if (!(wxValue instanceof VectorWxValue)) {
            throw new IllegalArgumentException(
                    "Expect VectorWxValue for VectorGridData");
        }

        VectorWxValue vWxValue = (VectorWxValue) wxValue;
        this.pointSet(vWxValue.getMag(), vWxValue.getDir(), gridLoc);
    }

    public void set(Grid2DFloat mag, Grid2DFloat dir, Grid2DBit editArea) {
        populate();
        checkOkayForEdit();
        gridSet(mag, dir, editArea);
        setChangedPoints(editArea);
    }

    private void gridSet(Grid2DFloat mag, Grid2DFloat dir, Grid2DBit editArea) {
        // indicators whether to change the magnitude or direction values
        boolean doMag = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.MAGNITUDE)
                || this.parm.getParmState().getVectorMode()
                        .equals(ParmState.VectorMode.BOTH);
        boolean doDir = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.DIRECTION)
                || this.parm.getParmState().getVectorMode()
                        .equals(ParmState.VectorMode.BOTH);

        Point dim = this.parm.getGridInfo().getGridLoc().gridSize();
        if (mag.getXdim() != dim.x || mag.getYdim() != dim.y
                || dir.getXdim() != dim.x || dir.getYdim() != dim.y
                || editArea.getXdim() != dim.x || editArea.getYdim() != dim.y) {
            throw new IllegalArgumentException(
                    "bad values/points dimensions for grid for: "
                            + this.parm.getParmID() + " magDim="
                            + mag.getXdim() + ',' + mag.getYdim() + " dirDim="
                            + dir.getXdim() + ',' + dir.getYdim()
                            + " pointsDim=" + editArea.getXdim() + ','
                            + editArea.getYdim() + " parmDim=" + dim);
        }

        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();

        // get values out of grid and assign them
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();
        for (int i = 0; i < dim.x; i++) {
            for (int j = 0; j < dim.y; j++) {
                if (editArea.get(i, j) != 0) {
                    if (doMag) {
                        float v = mag.get(i, j);
                        if (v < minLimit) {
                            v = minLimit;
                        } else if (v > maxLimit) {
                            v = maxLimit;
                        }
                        magGrid.set(i, j, v);
                    }

                    if (doDir) {
                        float v = dir.get(i, j);
                        while (v > 360.0) {
                            v -= 360.0;
                        }
                        while (v < 0.0) {
                            v += 360.0;
                        }
                        if (v == 360.0) {
                            v = 0.0f;
                        }
                        dirGrid.set(i, j, v);
                    }
                }
            }
        }

        // copy to cache
        if (doMag) {
            thisSlice.setMagGrid(magGrid);
        }
    }

    public void pointSet(float value, Point gridLoc) {
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        if (!magGrid.isValid(gridLoc.x, gridLoc.y)) {
            return;
        }

        // indicators whether to change the magnitude or direction values
        boolean doMag = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.MAGNITUDE)
                || this.parm.getParmState().getVectorMode()
                        .equals(ParmState.VectorMode.BOTH);
        if (!doMag) {
            return;
        }

        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();

        float thisValue = value;

        if (thisValue < minLimit) {
            thisValue = minLimit;
        } else if (thisValue > maxLimit) {
            thisValue = maxLimit;
        }
        magGrid.set(gridLoc.x, gridLoc.y, thisValue);

        // copy back to cache
        thisSlice.setMagGrid(magGrid);
    }

    /**
     * Function for setting vector values. Stores the grid value into the grid
     * after doing limit checking.
     * 
     * @param mag
     * @param dir
     * @param gridLoc
     * @return
     */
    private boolean pointSet(float mag, float dir, Point gridLoc) {
        VectorGridSlice thisSlice = getVectorSlice();

        if (!thisSlice.getMagGrid().isValid(gridLoc.x, gridLoc.y)) {
            return false;
        }

        // indicators whether to change the magnitude or direction values
        boolean doMag = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.MAGNITUDE)
                || this.parm.getParmState().getVectorMode()
                        .equals(ParmState.VectorMode.BOTH);
        boolean doDir = this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.DIRECTION)
                || this.parm.getParmState().getVectorMode()
                        .equals(ParmState.VectorMode.BOTH);

        if (doMag) {
            pointSet(mag, gridLoc); // set the magnitude component
        }

        if (doDir) {
            float thisDir = dir;

            while (thisDir < 0.0) {
                thisDir += 360.0;
            }
            while (thisDir > 360.0) {
                thisDir -= 360.0;
            }
            if (thisDir == 360.0) {
                thisDir = 0.0f;
            }
            Grid2DFloat dirGrid = thisSlice.getDirGrid();
            dirGrid.set(gridLoc.x, gridLoc.y, thisDir);

            // copy back to cache
            thisSlice.setDirGrid(dirGrid);
        }

        return true;
    }

    @Override
    public VectorGridData clone() throws CloneNotSupportedException {
        return new VectorGridData(this.parm, this.gridSlice.clone());
    }

    @Override
    protected boolean translateDataFrom(IGridData source)
            throws FactoryException {
        if (!(source instanceof VectorGridData)) {
            throw new IllegalArgumentException(
                    "Expected VectorGridData as source.");
        }

        VectorGridData vectorSource = (VectorGridData) source;

        Unit<?> sourceUnit = vectorSource.getParm().getGridInfo()
                .getUnitObject();
        Unit<?> thisUnit = this.getParm().getGridInfo().getUnitObject();

        if (!sourceUnit.equals(thisUnit)) {
            UnitConverter uc;
            try {
                uc = sourceUnit.getConverterTo(thisUnit);
            } catch (ConversionException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        e1.getLocalizedMessage(), e1);
                return false;
            }
            try {
                vectorSource = vectorSource.clone();
                float[] sourceData = vectorSource.getMagGrid().getBuffer()
                        .array();
                for (int i = 0; i < sourceData.length; i++) {
                    sourceData[i] = (float) uc.convert(sourceData[i]);
                }
            } catch (CloneNotSupportedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();
        float sourceMinLimit = vectorSource.getMinValue();
        float sourceMaxLimit = vectorSource.getMaxValue();

        if (vectorSource.getParm().getGridInfo().getGridLoc()
                .equals(this.parm.getGridInfo().getGridLoc())
                && maxLimit >= sourceMaxLimit && minLimit <= sourceMinLimit) {
            vectorSource.populate();
            setMagGrid(vectorSource.getMagGrid());
            setDirGrid(vectorSource.getDirGrid());
        } else {
            RemapGrid remap = new RemapGrid(vectorSource.getParm()
                    .getGridInfo().getGridLoc(), this.parm.getGridInfo()
                    .getGridLoc());

            try {
                Grid2DFloat outMagGrid = new Grid2DFloat(
                        getMagGrid().getXdim(), getMagGrid().getYdim());
                Grid2DFloat outDirGrid = new Grid2DFloat(
                        getDirGrid().getXdim(), getDirGrid().getYdim());
                remap.remap(vectorSource.getMagGrid(),
                        vectorSource.getDirGrid(), -99999.99f, maxLimit,
                        minLimit, minLimit, outMagGrid, outDirGrid);
                setMagGrid(outMagGrid);
                setDirGrid(outDirGrid);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return true;
    }

    @Override
    protected Grid2DBit doSmooth(Date time, Grid2DBit pointsToSmooth) {
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();

        if (magGrid.getXdim() != pointsToSmooth.getXdim()
                || magGrid.getYdim() != pointsToSmooth.getYdim()) {
            throw new IllegalArgumentException(
                    "Dimension mismatch in doSmooth: " + magGrid.getXdim()
                            + ',' + magGrid.getYdim() + ' '
                            + pointsToSmooth.getXdim() + ','
                            + pointsToSmooth.getYdim());
        }

        // get the grids
        VectorGridSlice slice;
        Grid2DFloat oMagGrid;
        Grid2DFloat oDirGrid;
        try {
            slice = this.getVectorSlice().clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            oMagGrid = slice.getMagGrid();
            oDirGrid = slice.getDirGrid();
        } catch (CloneNotSupportedException e) {
            oMagGrid = new Grid2DFloat();
            oDirGrid = new Grid2DFloat();
        }

        // Check the vector editing preferences to see what we can modify
        boolean editMag = true;
        boolean editDir = true;
        if (this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.DIRECTION)) {
            editMag = false;
        } else if (this.parm.getParmState().getVectorMode()
                .equals(ParmState.VectorMode.MAGNITUDE)) {
            editDir = false;
        }

        // for each point in the set of selected points, copy original
        // grid's point value to working grid, including offset.
        Point ll = new Point();
        Point ur = new Point();
        int newx, newy, i, j, numpoints;
        float sumMag, sumUComp, sumVComp, smoothmag = 0.0f, smoothdir = 0.0f;

        // Get the smooth factor and divide by 2 for the loop
        int ss = this.parm.getParmState().getSmoothSize() / 2;

        // if selected points contains valid points, get grid limits and proceed
        if (pointsToSmooth.extremaOfSetBits(ll, ur)) {
            // modify some points in the region of selected points
            for (i = ll.x; i <= ur.x; i++) {
                for (j = ll.y; j <= ur.y; j++) {
                    // if this point is one to smooth
                    if (pointsToSmooth.get(i, j) != 0) {
                        // Compute smoothed values for this position.
                        // There are many possible ways to do this.
                        // nine point average with equal weighing on each point;
                        // (uses fewer than nine points if near grid edge,
                        // but always has at least four values to average.)
                        numpoints = 0;
                        sumMag = 0.0f;
                        sumUComp = 0.0f;
                        sumVComp = 0.0f;
                        for (newx = i - ss; newx <= i + ss; newx++) {
                            for (newy = j - ss; newy <= j + ss; newy++) {
                                // if inside grid limits, make smoothed value
                                if (oMagGrid.isValid(newx, newy)) {
                                    numpoints++;
                                    if (editMag) {
                                        sumMag += oMagGrid.get(newx, newy);
                                    }
                                    if (editDir) {
                                        sumUComp += Math.sin(Math
                                                .toRadians(oDirGrid.get(newx,
                                                        newy)));
                                        sumVComp += Math.cos(Math
                                                .toRadians(oDirGrid.get(newx,
                                                        newy)));
                                    }
                                }
                            }
                        }
                        // Calculate the avg of the mag, uComp and vComp
                        if (numpoints > 0) {
                            if (editMag) {
                                smoothmag = sumMag / numpoints;
                            }
                            if (editDir) {
                                if (sumUComp == 0.0 && sumVComp == 0.0) {
                                    smoothdir = 0.0f;
                                } else {
                                    smoothdir = (float) Math.toDegrees(Math
                                            .atan2(sumUComp, sumVComp));
                                }
                            }
                        } else {
                            throw new IllegalStateException(
                                    "No points found when smoothing scalars.");
                        }

                        float maxLimit = this.parm.getGridInfo().getMaxValue();

                        // check that smooth values are inside parm max and min,
                        // if smoothing algorithm can make results exceeding
                        // the max or min input value, which is impossible
                        // if it is an averaging routine.
                        if (smoothmag < 0.0) {
                            smoothmag = 0.0f;
                        }
                        if (smoothmag > maxLimit) {
                            smoothmag = maxLimit;
                        }

                        while (smoothdir < 0.0) {
                            smoothdir += 360.0;
                        }
                        while (smoothdir > 360) {
                            smoothdir -= 360.0;
                        }
                        if (smoothdir == 360.0) {
                            smoothdir = 0.0f;
                        }

                        if (editMag) {
                            magGrid.set(i, j, smoothmag);
                        }
                        if (editDir) {
                            dirGrid.set(i, j, smoothdir);
                        }
                    }
                }
            }

            if (editMag) {
                thisSlice.setMagGrid(magGrid);
            }

            if (editDir) {
                thisSlice.setDirGrid(dirGrid);
            }
        }

        // return the set of points that were modified
        return pointsToSmooth;
    }

    @Override
    protected Grid2DBit doSet(WxValue value, Grid2DBit pointsToSet) {
        if (!(value instanceof VectorWxValue)) {
            throw new IllegalArgumentException(
                    "Expected WxValue of type VectorWxValue");
        }

        VectorWxValue vectorValue = (VectorWxValue) value;
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();

        if (magGrid.getXdim() != pointsToSet.getXdim()
                || magGrid.getYdim() != pointsToSet.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doSet: "
                    + magGrid.getXdim() + ',' + magGrid.getYdim() + ' '
                    + pointsToSet.getXdim() + ',' + pointsToSet.getYdim());
        }

        boolean doMag = this.parm.getParmState().getVectorMode() == ParmState.VectorMode.MAGNITUDE
                || this.parm.getParmState().getVectorMode() == ParmState.VectorMode.BOTH;
        boolean doDir = this.parm.getParmState().getVectorMode() == ParmState.VectorMode.DIRECTION
                || this.parm.getParmState().getVectorMode() == ParmState.VectorMode.BOTH;

        Point ll = new Point(), ur = new Point();

        float mag = vectorValue.getMag(); // get mag from WxValue
        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();
        if (mag < minLimit) {
            mag = minLimit;
        }
        if (mag > maxLimit) {
            mag = maxLimit;
        }

        float dir = vectorValue.getDir(); // get dir from WxValue
        while (dir < 0.0) {
            dir += 360.0;
        }
        while (dir > 360.0) {
            dir -= 360.0;
        }
        if (dir == 360.0) {
            dir = 0.0f;
        }

        // any to modify?
        if (pointsToSet.extremaOfSetBits(ll, ur)) {
            // modify the points
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    if (pointsToSet.get(i, j) != 0) {
                        if (doMag) {
                            magGrid.set(i, j, mag);
                        }
                        if (doDir) {
                            dirGrid.set(i, j, dir);
                        }
                    }
                }
            }

            // copy back to cache
            if (doMag) {
                thisSlice.setMagGrid(magGrid);
            }
            if (doDir) {
                thisSlice.setDirGrid(dirGrid);
            }
        }

        return pointsToSet;
    }

    @Override
    protected IGridSlice doGridMin(IGridSlice gridSlice) {
        if (!(gridSlice instanceof VectorGridSlice)) {
            throw new IllegalArgumentException("Expected VectorGridSlice");
        }

        return getVectorSlice().min((IContinuousSlice) gridSlice);
    }

    @Override
    protected IGridSlice doGridMax(IGridSlice gridSlice) {
        if (!(gridSlice instanceof VectorGridSlice)) {
            throw new IllegalArgumentException("Expected VectorGridSlice");
        }

        return getVectorSlice().max((IContinuousSlice) gridSlice);
    }

    @Override
    protected IGridSlice doGridMultiply(float factor) {
        return getVectorSlice().operate(Op.MULTIPLY, factor);
    }

    @Override
    protected IGridSlice doGridSum(IGridSlice gridSlice) {
        if (!(gridSlice instanceof VectorGridSlice)) {
            throw new IllegalArgumentException("Expected VectorGridSlice");
        }

        return getVectorSlice().sum((IContinuousSlice) gridSlice);
    }

    @Override
    protected Grid2DBit doContiguous(Date time, Point location) {
        Point size = getGridSlice().getGridInfo().getGridLoc().gridSize();
        Grid2DBit valid = new Grid2DBit(size.x, size.y, true);

        // get the grid
        VectorGridSlice slice;
        Grid2DFloat originalGrid;
        try {
            slice = this.getVectorSlice().clone();
            if (iscMode()) {
                valid = getISCGrid(time, slice);
            }
            originalGrid = slice.getMagGrid();
        } catch (CloneNotSupportedException e) {
            originalGrid = new Grid2DFloat();
        }

        Grid2DBit contig = new Grid2DBit(size.x, size.y);
        float fuzz = getParm().getParmState().getFuzzValue();
        float value = originalGrid.get(location.x, location.y);

        Point ll = new Point();
        Point ur = new Point();

        // check if points to check contains valid points, and get grid limits
        if (valid.extremaOfSetBits(ll, ur)) {
            // for all points in the region of valid points
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    // if this point is one to check
                    if (valid.getAsBoolean(i, j)
                            && Math.abs(originalGrid.get(i, j) - value) <= fuzz) {
                        contig.set(i, j);
                    }
                }
            }
        }

        // return the points that were changed
        return contig.contiguousBitArray(location);
    }

    @Override
    protected Grid2DBit doPencilStretch(Date time, WxValue value,
            Coordinate path[], Grid2DBit editArea) {
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();

        if (magGrid.getXdim() != editArea.getXdim()
                || magGrid.getYdim() != editArea.getYdim()) {
            throw new IllegalArgumentException(
                    "Dimension mismatch in doPencilStretch: "
                            + magGrid.getXdim() + ',' + magGrid.getYdim() + ' '
                            + editArea.getXdim() + ',' + editArea.getYdim());
        }

        // Convert to grid coordinates
        Point gridPointPath[] = convertToGridCoords(path);

        // fill in any gaps and edges
        gridPointPath = this.parm.getGridInfo().getGridLoc()
                .connectGridPoints(gridPointPath);

        Grid2DBit gridCells = calculatePencilInfluence(gridPointPath, magGrid);

        // Make a Grid2DBit and set every point in gridPointPath
        Grid2DBit pathGrid = new Grid2DBit(this.parm.getGridInfo().getGridLoc()
                .gridSize().x,
                this.parm.getGridInfo().getGridLoc().gridSize().y);

        // Assign the value to the gridPointPath
        for (int i = 0; i < gridPointPath.length; i++) {
            pathGrid.set(gridPointPath[i].x, gridPointPath[i].y);
        }

        // save the previous state of the mag, dir grids
        Grid2DFloat saveMag;
        try {
            saveMag = magGrid.clone();
        } catch (CloneNotSupportedException e) {
            saveMag = new Grid2DFloat();
        }
        Grid2DFloat saveDir;
        try {
            saveDir = dirGrid.clone();
        } catch (CloneNotSupportedException e) {
            saveDir = new Grid2DFloat();
        }

        // Now set the value of the pathPoints to value depending on the edit
        // mode
        boolean doMag = this.parm.getParmState().getVectorMode() == ParmState.VectorMode.MAGNITUDE
                || this.parm.getParmState().getVectorMode() == ParmState.VectorMode.BOTH;

        if (doMag) { // Normal pencil operation
            // TODO: disable caching across operations

            // Now set the value of the pathPoints to value
            doSet(value, pathGrid);

            // Fill and smooth once in using the pencil influence
            doFillIn(time, gridCells);

            smooth(time, gridCells);

            // TODO: renable caching
        } else { // modify dir only so use the streamline function

            // Find the pencil tool width in world coords
            gridCells = this.parm
                    .getGridInfo()
                    .getGridLoc()
                    .gridCellSwath(path,
                            this.parm.getParmState().getPencilWidth());
            doStreamlines(time, path, gridCells);
        }

        Grid2DBit changedGrid = gridCells.or(pathGrid);

        // restrict the changes to the edit area, if set
        if (editArea.isAnyBitsSet()) // undo everything outside the edit area
        {
            Grid2DBit undoArea = changedGrid.xor(editArea).and(changedGrid);
            Point ll = new Point(), ur = new Point();
            undoArea.extremaOfSetBits(ll, ur);

            // adjust to the area that was really changed
            changedGrid.andEquals(editArea);

            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    if (undoArea.get(i, j) != 0) {
                        magGrid.set(i, j, saveMag.get(i, j));
                        dirGrid.set(i, j, saveDir.get(i, j));
                    }
                }
            }
        }

        // save back to cache
        if (doMag) {
            thisSlice.setMagGrid(magGrid);
        } else {
            thisSlice.setDirGrid(dirGrid);
        }

        return changedGrid;
    }

    /**
     * This function is called to modify just the direction component of the
     * Vector
     * 
     * @param time
     * @param path
     * @param gridCells
     */
    private void doStreamlines(Date time, Coordinate path[], Grid2DBit gridCells) {
        Point ll = new Point(), ur = new Point();

        if (!gridCells.extremaOfSetBits(ll, ur)) {
            return; // no bits set - nothing to do
        }

        Coordinate worldCoord;
        float u, v; // direction components
        float dir;
        Grid2DFloat dirGrid = getDirGrid();

        // Change the direction for each set grid cell
        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (gridCells.get(i, j) != 0) // if the grid cell is set
                {
                    // get the u, v from the closest streamline point
                    worldCoord = MapUtil.gridCoordinateToLatLon(new Coordinate(
                            i, j), PixelOrientation.CENTER, this.parm
                            .getGridInfo().getGridLoc());

                    float floatArray[] = getClosestUV(path, worldCoord);
                    u = floatArray[0];
                    v = floatArray[1];

                    // get the dir from u, v
                    dir = uvToDir(u, v);

                    // modify the grid's direction
                    dirGrid.set(i, j, dir);
                }
            }
        }

        // Finally, smooth the edges of the modified area
        smooth(time, fringe(gridCells));

        // internal method so don't need to set back to cache, up to calling
        // method to recache
        return;
    }

    /**
     * Searches the specified path for the closest point and then uses that
     * point plus the next one to determine the u and v component of a vector
     * along the path.
     * 
     * Run through the points in path and calculate the closest point. Then get
     * the u and v components by calculating the difference between the closest
     * point and the next point on the path. Return u and v.
     * 
     * @param path
     * @param worldCoord
     * @return
     */
    private float[] getClosestUV(Coordinate path[], Coordinate worldCoord) {
        float dist;
        float minDist = Float.MAX_VALUE;
        int minIndex = 0;

        // Don't process the first and last two because edge effects
        for (int i = 2; i < path.length - 2; i++) {
            dist = (float) worldCoord.distance(path[i]);
            if (dist < minDist && dist != 0.0) {
                minDist = dist;
                minIndex = i;
            }
        }

        float u = (float) (path[minIndex - 1].x - path[minIndex + 1].x);
        float v = (float) (path[minIndex - 1].y - path[minIndex + 1].y);

        return new float[] { u, v };
    }

    private Grid2DFloat[] dirToUV(final Grid2DBit gridCells,
            final Grid2DFloat dirGrid) {
        // Make sure u and v component grids are proper size
        Grid2DFloat uComp = new Grid2DFloat(dirGrid.getXdim(),
                dirGrid.getYdim());
        Grid2DFloat vComp = new Grid2DFloat(dirGrid.getXdim(),
                dirGrid.getYdim());

        // Get the extrema for efficiency
        Point lowerLeft = new Point();
        Point upperRight = new Point();
        if (!gridCells.extremaOfSetBits(lowerLeft, upperRight)) {
            // no bits set, so we're outta here
            return new Grid2DFloat[] { uComp, vComp };
        }

        // expand the extrema by a grid cell in each direction while
        // checking for bounds
        if (lowerLeft.x > 0) {
            lowerLeft.x -= 1;
        }
        if (lowerLeft.y > 0) {
            lowerLeft.y -= 1;
        }
        if (upperRight.x < gridCells.getXdim() - 1) {
            upperRight.x += 1;
        }
        if (upperRight.y < gridCells.getYdim() - 1) {
            upperRight.y += 1;
        }

        double angle; // used only for efficiency

        // Convert all of the points within the expanded bounds
        for (int i = lowerLeft.x; i <= upperRight.x; i++) {
            for (int j = lowerLeft.y; j <= upperRight.y; j++) {
                angle = Math.toRadians(dirGrid.get(i, j));
                uComp.set(i, j, (float) Math.sin(angle));
                vComp.set(i, j, (float) Math.cos(angle));
            }
        }
        return new Grid2DFloat[] { uComp, vComp };
    }

    /**
     * Converts the supplied u and v component grids into a a grid of direction
     * and returns it through the argument list.
     * 
     * Make sure the grids are valid and for each grid cell calculate the
     * direction based on the u and v values at that grid cell.
     * 
     * @param gridCells
     * @param uComp
     * @param vComp
     */
    private void uVToDir(final Grid2DBit gridCells, final Grid2DFloat uComp,
            final Grid2DFloat vComp, final Grid2DFloat dirGrid) {

        // Get the extrema for efficiency
        Point lowerLeft = new Point();
        Point upperRight = new Point();
        if (!gridCells.extremaOfSetBits(lowerLeft, upperRight)) {
            return;
        }

        float dirValue;
        for (int i = lowerLeft.x; i <= upperRight.x; i++) {
            for (int j = lowerLeft.y; j <= upperRight.y; j++) {
                if (gridCells.getAsBoolean(i, j)) {

                    // Check for domain error first
                    if (uComp.get(i, j) == 0.0f && vComp.get(i, j) == 0.0f) {
                        dirValue = 0.0f;
                    } else {
                        dirValue = (float) Math.toDegrees(Math.atan2(
                                uComp.get(i, j), vComp.get(i, j)));
                    }
                    // Keep the direction in range
                    while (dirValue < 0.0) {
                        dirValue += 360.0;
                    }
                    while (dirValue > 360) {
                        dirValue -= 360.0;
                    }
                    if (dirValue == 360.0) {
                        dirValue = 0.0f;
                    }
                    dirGrid.set(i, j, dirValue);
                }
            }
        }

        return;
    }

    /**
     * Converts the specified u and v component into a directi
     * 
     * @param u
     * @param v
     * @return
     */
    private float uvToDir(float u, float v) {
        float dir = 0;
        // Check for domain error first
        if (u == 0.0 && v == 0.0) {
            dir = 0.0f;
        } else {
            dir = (float) Math.toDegrees(Math.atan2(u, v));
        }
        // Keep the direction in range
        while (dir < 0.0) {
            dir += 360.0;
        }
        while (dir > 360) {
            dir -= 360.0;
        }
        if (dir == 360.0) {
            dir = 0.0f;
        }

        return dir;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#doCopy(java.util.
     * Date, com.raytheon.edex.grid.Grid2DBit, java.awt.Point)
     */
    @Override
    protected Grid2DBit doCopy(Date time, Grid2DBit pointsToCopy, Point delta) {
        VectorGridSlice thisSlice = getVectorSlice();
        Grid2DFloat magGrid = thisSlice.getMagGrid();
        Grid2DFloat dirGrid = thisSlice.getDirGrid();

        if (magGrid.getXdim() != pointsToCopy.getXdim()
                || magGrid.getYdim() != pointsToCopy.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doCopy: "
                    + magGrid.getXdim() + ',' + magGrid.getYdim() + ' '
                    + pointsToCopy.getXdim() + ',' + pointsToCopy.getYdim());
        }

        // get the grids
        Grid2DFloat originalMagGrid;
        Grid2DFloat originalDirGrid;
        try {
            VectorGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            originalMagGrid = slice.getMagGrid();
            originalDirGrid = slice.getDirGrid();
        } catch (CloneNotSupportedException e) {
            originalMagGrid = new Grid2DFloat();
            originalDirGrid = new Grid2DFloat();
        }

        // for each point in the set of selected points, copy original
        // grid's point value to working grid, including offset.
        Point ll = new Point(), ur = new Point();
        int newx, newy, i, j;

        // if points to copy contains valid points, get grid limits and
        // proceed
        if (pointsToCopy.extremaOfSetBits(ll, ur)) {
            // modify some points in the region of selected points
            for (i = ll.x; i <= ur.x; i++) {
                for (j = ll.y; j <= ur.y; j++) {
                    // if this point is one to copy
                    if (pointsToCopy.get(i, j) > 0) {
                        // determine the new position
                        newx = i + delta.x;
                        newy = j + delta.y;

                        // if inside grid limits, copy value to new
                        // position
                        // of working grid.
                        if (magGrid.isValid(newx, newy)) {
                            magGrid.set(newx, newy, originalMagGrid.get(i, j));
                            dirGrid.set(newx, newy, originalDirGrid.get(i, j));
                        }
                    }
                }
            }

            // copy values back to cache
            thisSlice.setMagGrid(magGrid);
            thisSlice.setDirGrid(dirGrid);
        }

        // return the grid positions that were changed in value
        return pointsToCopy.translate(delta);
    }

    @Override
    public void setGridSlice(IGridSlice gridSlice) {
        if (!(gridSlice instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "Called VectorGridData.setGridSlice with "
                            + gridSlice.getClass().getSimpleName());
        }
        this.gridSlice = gridSlice;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#setGridSliceDataToNull
     * ()
     */
    @Override
    protected void setGridSliceDataToNull() {
        // Clone the gridSlice with no data
        this.gridSlice = new VectorGridSlice(this.gridSlice.getValidTime(),
                this.gridSlice.getGridInfo(), this.gridSlice.getHistory(),
                null, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#isPopulated()
     */
    @Override
    public synchronized boolean isPopulated() {
        return ((VectorGridSlice) this.gridSlice).isPopulated();
    }

    private Grid2DFloat getMagGrid() {
        return getVectorSlice().getMagGrid();
    }

    private Grid2DFloat getDirGrid() {
        return getVectorSlice().getDirGrid();
    }

    private void setMagGrid(Grid2DFloat grid) {
        getVectorSlice().setMagGrid(grid);
    }

    private void setDirGrid(Grid2DFloat grid) {
        getVectorSlice().setDirGrid(grid);
    }

    public VectorGridSlice getVectorSlice() {
        return (VectorGridSlice) getGridSlice();
    }

    @Override
    protected boolean doValid() {
        String emsg = "Grid contains data which exceeds max/min specs for this parm. ";

        if (!getGridTime().isValid() || getParm() == null
                || getGridSlice() == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid grid time, bad parm or data slice");
            return false; // time, parm, or data slice not valid
        }
        // if (!dataTypeOkay()) {
        // return false;
        // }

        // check grid size
        Grid2DFloat magGrid = getMagGrid();
        Grid2DFloat dirGrid = getDirGrid();
        Point dim = getParm().getGridInfo().getGridLoc().gridSize();
        Point magGridDim = magGrid.getGridSize();
        Point dirGridDim = dirGrid.getGridSize();
        if (!magGridDim.equals(dim) || !dirGridDim.equals(dim)) {
            statusHandler.handle(Priority.PROBLEM, "Grid dimensions m="
                    + magGridDim + " d=" + dirGridDim
                    + " do not match Parm dimensions" + dim);
            return false;
        }

        // check data values
        float minLimit = getParm().getGridInfo().getMinValue();
        float maxLimit = getParm().getGridInfo().getMaxValue();
        FloatBuffer magData = magGrid.getBuffer();
        FloatBuffer dirData = dirGrid.getBuffer();
        for (int j = 0; j < magData.capacity(); j++) {
            float mag = magData.get(j);
            if (mag < minLimit || mag > maxLimit) {
                statusHandler.handle(Priority.PROBLEM, emsg + "MagData=" + mag
                        + " Min=" + minLimit + " Max=" + maxLimit);
                return false;
            }
            float dir = dirData.get(j);
            if (dir < 0f || dir >= 360f) {
                statusHandler.handle(Priority.PROBLEM, emsg + "DirData=" + dir
                        + "Min=0 Max=360");
                return false;
            }
        }
        return true;
    }
}
