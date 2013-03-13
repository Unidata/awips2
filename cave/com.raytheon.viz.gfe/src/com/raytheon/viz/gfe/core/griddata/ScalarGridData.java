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

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.slice.IContinuousSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Placeholder for ScalarGridData
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial Class Skeleton.
 * 03/13/2008   879        rbell       Legacy conversion.
 * 05/20/2009   #2159      rjpeter     Fixed doDelta
 * 02/19/2013   1637       randerso    Added throws declarations to translateDataFrom
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ScalarGridData extends OrderedGridData implements Cloneable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScalarGridData.class);

    public ScalarGridData(Parm aParm, IGridSlice aSlice) {
        super(aParm, aSlice);
        if (!(aSlice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "ScalarGridSlice required for ScalarGridData");
        }
    }

    public Grid2DBit comparisonOperate(float value, Op op) {
        Grid2DFloat grid = getGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
        case NOT_EQ:
        case GT:
        case GT_EQ:
        case LT:
        case LT_EQ:
            bits = getScalarSlice().comparisonOperate(op, value);
            break;
        case ALMOST:
            bits = getScalarSlice().almost(value,
                    this.parm.getParmState().getFuzzValue());
            break;
        case NOT_ALMOST:
            bits = getScalarSlice().almost(value,
                    this.parm.getParmState().getFuzzValue());
            bits.negate();
            break;
        default:
            throw new IllegalArgumentException("Op not supported: "
                    + op.toString());
        }

        return bits;
    }

    public Grid2DBit comparisonOperate(final IGridData gridData, Op op) {
        if (!(gridData instanceof ScalarGridData)) {
            throw new IllegalArgumentException(
                    "IGridData not of type ScalarGridData");
        }

        ScalarGridSlice rhsScalarGridSlice = (ScalarGridSlice) gridData
                .getGridSlice();

        Grid2DFloat grid = getGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
        case NOT_EQ:
        case GT:
        case GT_EQ:
        case LT:
        case LT_EQ:
            bits = getScalarSlice().comparisonOperate(op, rhsScalarGridSlice);
            break;
        case ALMOST:
            bits = getScalarSlice().almost(rhsScalarGridSlice,
                    this.parm.getParmState().getFuzzValue());
            break;
        case NOT_ALMOST:
            bits = getScalarSlice().almost(rhsScalarGridSlice,
                    this.parm.getParmState().getFuzzValue());
            bits.negate();
            break;
        default:
            throw new IllegalArgumentException("Op not supported: "
                    + op.toString());
        }

        return bits;
    }

    public void operate(float value, Op op, Grid2DBit editArea) {
        getScalarSlice().operateEquals(op, value, editArea);
    }

    public void operate(final IGridData gridData, Op op, Grid2DBit editArea) {
        if (!(gridData instanceof ScalarGridData)) {
            throw new IllegalArgumentException(
                    "IGridData not of type ScalarGridData");
        }

        getScalarSlice().operateEquals(op,
                (ScalarGridSlice) gridData.getGridSlice(), editArea);
    }

    public void operate(final WxValue value, Op op, Grid2DBit editArea) {
        if (!(value instanceof ScalarWxValue)) {
            throw new IllegalArgumentException(
                    "WxValue not of type ScalarWxValue");
        }

        this.operate(((ScalarWxValue) value).getValue(), op, editArea);
    }

    public void limitValue(float min, float max, Grid2DBit editArea) {
        getScalarSlice().limitValue(min, max, editArea);
    }

    public float getValue(int x, int y) {
        populate();
        return getGrid().get(x, y);
    }

    // -- protected
    // --------------------------------------------------------------
    // ScalarGridData::doDelta()
    //
    // Virtual function adjusting the value of one or more gridpoints
    // by adding the delta value submitted, adjusted by a taper
    // at edges, if requested.
    // Returns the set of gridpoints modified.
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    @Override
    protected Grid2DBit doDelta(final Date time, float delta, boolean taper,
            final Grid2DBit pointsToChange) {
        Point ll = new Point();
        Point ur = new Point();
        float sum;

        Grid2DFloat grid = getGrid();
        if (grid.getXdim() != pointsToChange.getXdim()
                || grid.getYdim() != pointsToChange.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doDelta");
        }

        boolean dataChanged = false;

        // check for points to modify, and get limits in grid or area to work on
        if (pointsToChange.extremaOfSetBits(ll, ur)) {
            // loop over all points in the grid area that contains points to
            // change
            float minLimit = this.getMinValue();
            float maxLimit = this.getMaxValue();

            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    // if this point is one to be modified
                    if (pointsToChange.get(i, j) > 0) {
                        // compute sum of grid value plus adjusted delta
                        sum = grid.get(i, j) + delta;

                        // make sure new value does not exceed limits of AFPS db
                        if (sum < minLimit) {
                            sum = minLimit;
                        } else if (sum > maxLimit) {
                            sum = maxLimit;
                        }

                        grid.set(i, j, sum);
                        dataChanged = true;
                    }
                }
            }
        }

        // If the taper flag is set, taper the edges.
        if (taper) {
            taperGrid(time, pointsToChange);
        }

        if (dataChanged) {
            setGrid(grid);
        }

        return pointsToChange;
    }

    public boolean applyDeltaTaper(Date time, float delta,
            Grid2DBit pointsToChange, Point gridCoord) {
        if (delta == 0.0) {
            return false; // nothing to change
        }
        populate();
        checkOkayForEdit();

        // Make the change and remember the changes.
        return setChangedPoints(doDeltaTaper(time, delta, pointsToChange,
                gridCoord));
    }

    protected Grid2DBit doDeltaTaper(final Date time, float delta,
            final Grid2DBit pointsToChange, Point coord) {
        Point ll = new Point(), ur = new Point();
        float sum;

        Grid2DFloat grid = getGrid();
        if (grid.getXdim() != pointsToChange.getXdim()
                || grid.getYdim() != pointsToChange.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doDelta");
        }

        // check for points to modify, and get limits in grid or area to work on
        if (pointsToChange.extremaOfSetBits(ll, ur)) {
            // Get the taper grid
            Grid2DFloat taperGrid = computeTaperGrid(coord, pointsToChange);

            float minLimit = this.getMinValue();
            float maxLimit = this.getMaxValue();

            // loop over all points in the grid area that contains points to
            // change
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    // if this point is one to be modified
                    if (pointsToChange.get(i, j) != 0) {
                        // compute sum of grid value plus adjusted delta
                        sum = grid.get(i, j) + delta * taperGrid.get(i, j);

                        // make sure new value does not exceed limits of AFPS db
                        if (sum < minLimit) {
                            sum = minLimit;
                        } else if (sum > maxLimit) {
                            sum = maxLimit;
                        }

                        grid.set(i, j, sum);
                    }
                }
            }

            // copy back to cache
            setGrid(grid);
        }

        return pointsToChange;
    }

    @Override
    public void set(Point gridLoc, WxValue wxValue) {
        if (!(wxValue instanceof ScalarWxValue)) {
            throw new IllegalArgumentException(
                    "Expect ScalarWxValue for ScalarGridData");
        }

        this.set(gridLoc, ((ScalarWxValue) wxValue).getValue());
    }

    public void set(Point gridLoc, float value) {
        populate();
        checkOkayForEdit();

        pointSet(value, gridLoc);
        setChangedPoints(gridLoc);
    }

    /**
     * Stores the grid into the storage grid after doing checking
     * 
     * @param values
     *            the values to set
     * @param points
     *            the points to set
     */
    protected void gridSet(final Grid2DFloat values, final Grid2DBit points) {

        Grid2DFloat grid = getGrid();
        int dimX = grid.getXdim();
        int dimY = grid.getYdim();

        if (values.getXdim() != dimX || values.getYdim() != dimY
                || points.getXdim() != dimX || points.getYdim() != dimY) {
            throw new IllegalArgumentException(
                    "bad values/points dimensions for grid for: "
                            + getParm().getGridInfo().getParmID()
                            + " valuesDim=" + values.getXdim() + ','
                            + values.getYdim() + " pointsDim="
                            + points.getXdim() + ',' + points.getYdim()
                            + " parmDim=" + dimX + " " + dimY);
        }

        // get values out of grid and assign them
        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();

        for (int i = 0; i < dimX; i++) {
            for (int j = 0; j < dimY; j++) {
                if (points.get(i, j) > 0) {
                    float v = values.get(i, j);
                    if (v < minLimit) {
                        v = minLimit;
                    } else if (v > maxLimit) {
                        v = maxLimit;
                    }
                    grid.set(i, j, v);
                }
            }
        }

        // copy back to cache
        setGrid(grid);

        return;
    }

    /**
     * Sets multiple (scalar) values in the grid.
     * 
     * @param value
     *            the values to set
     * @param points
     *            the points to set
     */
    public void set(Grid2DFloat value, Grid2DBit points) {
        populate();
        checkOkayForEdit();

        gridSet(value, points);
        setChangedPoints(points);

        return;
    }

    @Override
    public WxValue getWxValue(int x, int y) {
        return new ScalarWxValue(getValue(x, y), this.parm);
    }

    protected void pointSet(float value, Point loc) {
        float newValue = value;
        if (value < this.getMinValue()) {
            newValue = this.getMinValue();
        } else if (value > this.getMaxValue()) {
            newValue = this.getMaxValue();
        }

        Grid2DFloat grid = getGrid();
        grid.set(loc.x, loc.y, newValue);

        // copy back to cache
        setGrid(grid);
    }

    @Override
    protected boolean translateDataFrom(IGridData source)
            throws FactoryException, TransformException {
        if (!(source instanceof ScalarGridData)) {
            throw new IllegalArgumentException(
                    "Expected ScalarGridData as source.");
        }

        ScalarGridData scalarSource = (ScalarGridData) source;

        Unit<?> sourceUnit = scalarSource.getParm().getGridInfo()
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
                scalarSource = scalarSource.clone();
                FloatBuffer sourceData = scalarSource.getGrid().getBuffer();
                for (int i = 0; i < sourceData.capacity(); i++) {
                    sourceData.put(i, (float) uc.convert(sourceData.get(i)));
                }
            } catch (CloneNotSupportedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();
        float sourceMinLimit = scalarSource.getMinValue();
        float sourceMaxLimit = scalarSource.getMaxValue();

        if (scalarSource.getParm().getGridInfo().getGridLoc()
                .equals(this.parm.getGridInfo().getGridLoc())
                && maxLimit >= sourceMaxLimit && minLimit <= sourceMinLimit) {
            setGrid(scalarSource.getGrid());
        } else {
            RemapGrid remap = new RemapGrid(scalarSource.getParm()
                    .getGridInfo().getGridLoc(), this.parm.getGridInfo()
                    .getGridLoc());
            Grid2DFloat scalarGrid = remap.remap(scalarSource.getGrid(),
                    -99999.99f, maxLimit, minLimit, minLimit);
            setGrid(scalarGrid);
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#doSmooth(java.util
     * .Date, com.raytheon.edex.grid.Grid2DBit)
     */
    @Override
    protected Grid2DBit doSmooth(Date time, Grid2DBit pointsToSmooth) {
        // get the grid
        ScalarGridSlice thisSlice = getScalarSlice();
        Grid2DFloat grid = thisSlice.getScalarGrid();

        if (grid.getXdim() != pointsToSmooth.getXdim()
                || grid.getYdim() != pointsToSmooth.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doSmooth");
        }

        Grid2DFloat originalGrid;
        try {
            ScalarGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            originalGrid = slice.getScalarGrid();
        } catch (CloneNotSupportedException e) {
            originalGrid = new Grid2DFloat();
        }

        Point ll = new Point(), ur = new Point();
        int newx, newy, i, j, numpoints;
        float sum, smoothV = 0.0f;
        // Get the smooth factor and divide by 2 for the loop
        int ss = getParm().getParmState().getSmoothSize() / 2;

        // check if points to smooth contains valid points, and get grid limits
        if (pointsToSmooth.extremaOfSetBits(ll, ur)) {
            // for all points in the region of selected points
            for (i = ll.x; i <= ur.x; i++) {
                for (j = ll.y; j <= ur.y; j++) {
                    // if this point is one to smooth
                    if (pointsToSmooth.get(i, j) > 0) {
                        // Compute a smoothed value "smoothV" for this position.
                        // There are many possible ways to do this.

                        // nine point average;
                        // uses fewer than nine points if near grid edge,
                        // but always has at least four values to average.
                        numpoints = 0;
                        sum = 0.0f;
                        for (newx = i - ss; newx <= i + ss; newx++) {
                            for (newy = j - ss; newy <= j + ss; newy++) {
                                // if inside grid limits, make a smoothed value
                                if (originalGrid.isValid(newx, newy)) {
                                    numpoints++;
                                    sum += originalGrid.get(newx, newy);
                                }
                            }
                        }
                        if (numpoints > 0) {
                            smoothV = sum / numpoints;
                        } else {
                            throw new IllegalStateException(
                                    "No points found when smoothing scalars.");
                        }

                        // check that smoothV is inside database max and min,
                        // if smoothing algorithm can make results exceeding
                        // the max or min input value, which is impossible
                        // if it is an averaging routine.

                        grid.set(i, j, smoothV);
                    }
                }
            }
        }

        // set data back to cache
        thisSlice.setScalarGrid(grid);

        // return the points that were changed
        return pointsToSmooth;

    }

    @Override
    protected Grid2DBit doSet(WxValue value, Grid2DBit pointsToSet) {
        if (!(value instanceof ScalarWxValue)) {
            throw new IllegalArgumentException(
                    "Expected WxValue of type ScalarWxValue");
        }

        ScalarWxValue scalarValue = (ScalarWxValue) value;
        Grid2DFloat grid = getGrid();
        if (grid.getXdim() != pointsToSet.getXdim()
                || grid.getYdim() != pointsToSet.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doSet: "
                    + grid.getXdim() + ',' + grid.getYdim() + ' '
                    + pointsToSet.getXdim() + ',' + pointsToSet.getYdim());
        }

        Point ll = new Point(), ur = new Point();
        float v = scalarValue.getValue(); // extract the floating-point value
        // from the WxValue
        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();
        if (v < minLimit) {
            v = minLimit;
        } else if (v > maxLimit) {
            v = maxLimit;
        }

        // any to modify?
        if (pointsToSet.extremaOfSetBits(ll, ur)) {
            // modify the points
            for (int i = ll.x; i <= ur.x; i++) {
                for (int j = ll.y; j <= ur.y; j++) {
                    if (pointsToSet.get(i, j) != 0) {
                        grid.set(i, j, v);
                    }
                }
            }

            // copy grid back to cache
            setGrid(grid);
        }
        return pointsToSet;
    }

    @Override
    public ScalarGridData clone() throws CloneNotSupportedException {
        ScalarGridData sgd = new ScalarGridData(this.parm,
                this.gridSlice.clone());
        return sgd;
    }

    @Override
    protected IGridSlice doGridMin(IGridSlice gridSlice) {
        if (!(gridSlice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException("Expected ScalarGridSlice");
        }

        return getScalarSlice().min((IContinuousSlice) gridSlice);
    }

    @Override
    protected IGridSlice doGridMax(IGridSlice gridSlice) {
        if (!(gridSlice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException("Expected ScalarGridSlice");
        }

        return getScalarSlice().max((IContinuousSlice) gridSlice);
    }

    @Override
    protected IGridSlice doGridMultiply(float factor) {
        return getScalarSlice().operate(Op.MULTIPLY, factor);
    }

    @Override
    protected IGridSlice doGridSum(IGridSlice gridSlice) {
        if (!(gridSlice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException("Expected ScalarGridSlice");
        }

        return getScalarSlice().sum((IContinuousSlice) gridSlice);
    }

    @Override
    protected Grid2DBit doContiguous(Date time, Point location) {
        Point size = getGridSlice().getGridInfo().getGridLoc().gridSize();
        Grid2DBit valid = new Grid2DBit(size.x, size.y, true);

        // get the grid
        ScalarGridSlice slice;
        Grid2DFloat originalGrid;
        try {
            slice = this.getScalarSlice().clone();
            if (iscMode()) {
                valid = getISCGrid(time, slice);
            }
            originalGrid = slice.getScalarGrid();
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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.OrderedGridData#doPencilStretch(java
     * .util.Date, com.raytheon.viz.gfe.core.wxvalue.WxValue,
     * com.vividsolutions.jts.geom.Coordinate[],
     * com.raytheon.edex.grid.Grid2DBit)
     */
    @Override
    protected Grid2DBit doPencilStretch(Date time, WxValue value,
            Coordinate path[], Grid2DBit editArea) {
        Grid2DFloat grid = getGrid();
        if (grid.getXdim() != editArea.getXdim()
                || grid.getYdim() != editArea.getYdim()) {
            throw new IllegalArgumentException(
                    "Dimension mismatch in doPencilStretch: " + grid.getXdim()
                            + ',' + grid.getYdim() + ' ' + editArea.getXdim()
                            + ',' + editArea.getYdim());
        }

        // Convert to grid coordinates
        Point gridPointPath[] = this.convertToGridCoords(path);

        // fill in any gaps and edges
        gridPointPath = this.parm.getGridInfo().getGridLoc()
                .connectGridPoints(gridPointPath);

        Grid2DBit gridCells = this
                .calculatePencilInfluence(gridPointPath, grid);

        // Make a Grid2DBit and set every point in gridPointPath
        Grid2DBit pathGrid = new Grid2DBit(this.parm.getGridInfo().getGridLoc()
                .gridSize().x,
                this.parm.getGridInfo().getGridLoc().gridSize().y);

        // Assign the value to the gridPointPath
        for (Point p : gridPointPath) {
            pathGrid.set(p.x, p.y);
        }

        // save the original grid values
        Grid2DFloat saveGrid;
        try {
            saveGrid = grid.clone();
        } catch (CloneNotSupportedException e) {
            saveGrid = new Grid2DFloat();
        }

        // TODO: temp turn off grid cache

        // Now set the value of the pathPoints to value
        doSet(value, pathGrid);

        // Fill and smooth once in using the pencil influence
        doFillIn(time, gridCells);

        // TODO
        smooth(time, gridCells.or(pathGrid));

        Grid2DBit changedGrid = gridCells.or(pathGrid);

        // restrict the changes to the edit area, if set
        if (editArea.isAnyBitsSet()) // undo everything outside the edit area
        {
            Grid2DBit undoArea = changedGrid.xor(editArea).and(changedGrid);
            gridSet(saveGrid, undoArea);
            // adjust the area that was really changed
            changedGrid.andEquals(editArea);
        }

        // TODO renable grid caching

        return changedGrid;
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
        // get the grid
        ScalarGridSlice thisSlice = getScalarSlice();
        Grid2DFloat thisGrid = thisSlice.getScalarGrid();

        if (thisGrid.getXdim() != pointsToFillIn.getXdim()
                || thisGrid.getYdim() != pointsToFillIn.getYdim()) {
            throw new IllegalArgumentException(
                    "Dimension mismatch in doFillIn: " + thisGrid.getXdim()
                            + ',' + thisGrid.getYdim() + ' '
                            + pointsToFillIn.getXdim() + ','
                            + pointsToFillIn.getYdim());
        }

        Grid2DFloat workingGrid;
        try {
            ScalarGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                getISCGrid(time, slice);
            }
            workingGrid = slice.getScalarGrid();
        } catch (CloneNotSupportedException e) {
            workingGrid = new Grid2DFloat();
        }

        interpSpatialGap(pointsToFillIn, workingGrid, this.getMinValue(),
                this.getMaxValue(), thisGrid);

        // set data back to cache
        thisSlice.setScalarGrid(thisGrid);

        return pointsToFillIn;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.griddata.AbstractGridData#doCopy(java.util.
     * Date, com.raytheon.edex.grid.Grid2DBit,
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected Grid2DBit doCopy(Date time, Grid2DBit pointsToCopy, Point delta) {
        // get the grid
        ScalarGridSlice thisSlice = getScalarSlice();
        Grid2DFloat thisGrid = thisSlice.getScalarGrid();
        if (thisGrid.getXdim() != pointsToCopy.getXdim()
                || thisGrid.getYdim() != pointsToCopy.getYdim()) {
            throw new IllegalArgumentException("Dimension mismatch in doCopy: "
                    + thisGrid.getXdim() + ',' + thisGrid.getYdim() + ' '
                    + pointsToCopy.getXdim() + ',' + pointsToCopy.getYdim());
        }

        // copy the grid
        Grid2DFloat originalGrid;
        try {
            ScalarGridSlice slice = thisSlice.clone();
            if (iscMode()) {
                // Return value being thrown on floor?
                getISCGrid(time, slice);
            }
            originalGrid = slice.getScalarGrid();
        } catch (CloneNotSupportedException e) {
            originalGrid = new Grid2DFloat();
        }

        Point ll = new Point(), ur = new Point();
        int newx, newy, i, j;

        // for each point in the set of selected points, copy original
        // grid's point value to working grid, including offset.
        // if poinstToCopy contains valid points, get area limits ll and
        // ur.
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
                        // position of working grid.
                        if (thisGrid.isValid(newx, newy)) {
                            thisGrid.set(newx, newy, originalGrid.get(i, j));
                        }
                    }
                }
            }
        }

        // copy data back to cache
        thisSlice.setScalarGrid(thisGrid);
        return pointsToCopy.translate(delta);

    }

    /**
     * The cached state of previous slice will be preserved.
     */
    @Override
    public void setGridSlice(IGridSlice gridSlice) {
        if (!(gridSlice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Called ScalarGridData.setGridSlice with "
                            + gridSlice.getClass().getSimpleName());
        }
        if (this.gridSlice != null) {
            // clear out previous cache
            ((ScalarGridSlice) this.gridSlice).setScalarGrid(null);
        }
        boolean wasCached = this.gridSlice.getUseCache();
        this.gridSlice = gridSlice;
        this.gridSlice.setUseCache(wasCached);
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
        this.gridSlice = new ScalarGridSlice(this.gridSlice.getValidTime(),
                this.gridSlice.getGridInfo(), this.gridSlice.getHistory(), null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.griddata.IGridData#isPopulated()
     */
    @Override
    public synchronized boolean isPopulated() {
        return ((ScalarGridSlice) this.gridSlice).isPopulated();
    }

    private Grid2DFloat getGrid() {
        return ((ScalarGridSlice) getGridSlice()).getScalarGrid();
    }

    private void setGrid(Grid2DFloat grid) {
        ((ScalarGridSlice) getGridSlice()).setScalarGrid(grid);
    }

    public ScalarGridSlice getScalarSlice() {
        return (ScalarGridSlice) getGridSlice();
    }

    @Override
    protected boolean doValid() {
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
        Point dim = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DFloat grid = getGrid();
        Point gridDim = grid.getGridSize();
        if (!gridDim.equals(dim)) {
            statusHandler.handle(Priority.PROBLEM, "Grid dimensions " + gridDim
                    + " do not match Parm dimensions " + dim);
            return false;
        }

        // check data values
        float minLimit = getParm().getGridInfo().getMinValue();
        float maxLimit = getParm().getGridInfo().getMaxValue();
        FloatBuffer data = grid.getBuffer();
        for (int j = 0; j < data.capacity(); j++) {
            float f = data.get(j);
            if (f < minLimit || f > maxLimit) {
                statusHandler.handle(Priority.PROBLEM,
                        "Grid contains data which "
                                + "exceeds max/min specifications for "
                                + "this parm. Data=" + f + " Min=" + minLimit
                                + " Max=" + maxLimit);
                return false;
            }
        }

        return true;
    }
}
