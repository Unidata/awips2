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

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

import tec.uom.se.format.SimpleUnitFormat;

/**
 * GridData class dealing with a scalar grid.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 29, 2008           chammack  Initial Class Skeleton.
 * Mar 13, 2008  879      rbell     Legacy conversion.
 * May 20, 2009  2159     rjpeter   Fixed doDelta
 * Feb 19, 2013  1637     randerso  Added throws declarations to
 *                                  translateDataFrom
 * Aug 02, 2016  5744     mapeters  Remove unused cache code
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Changes to support IDataObject. Code cleanup
 * Apr 15, 2019  7596     lsingh    Upgraded Units Framework to JSR-363. Handled
 *                                  unit conversion.
 *
 * </pre>
 *
 * @author chammack
 */
public class ScalarGridData extends OrderedGridData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScalarGridData.class);

    /**
     * Constructor
     *
     * @param aParm
     * @param aSlice
     * @param unsaved
     *            true if data is unsaved and must not be depopulated
     */
    public ScalarGridData(Parm aParm, IGridSlice aSlice, boolean unsaved) {
        super(aParm, aSlice, unsaved);
        if (!(aSlice instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "aSlice must be an instance of ScalarGridSlice, received: "
                            + aSlice.getClass().getName());
        }
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    public ScalarGridData(ScalarGridData other) {
        super(other);
    }

    /**
     * Return bit mask based on grid values, specified value, and comparison
     * operation.
     *
     * @param value
     * @param op
     * @return see above
     */
    public Grid2DBit comparisonOperate(float value, Op op) {
        Grid2DFloat grid = getScalarGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
        case NOT_EQ:
        case GT:
        case GT_EQ:
        case LT:
        case LT_EQ:
            bits = getDataObject().comparisonOperate(op, value);
            break;
        case ALMOST:
            bits = getDataObject().almost(value,
                    this.parm.getParmState().getFuzzValue());
            break;
        case NOT_ALMOST:
            bits = getDataObject().almost(value,
                    this.parm.getParmState().getFuzzValue());
            bits.negate();
            break;
        default:
            throw new IllegalArgumentException(
                    "Op not supported: " + op.toString());
        }

        return bits;
    }

    /**
     *
     * Return bit mask based on grid values, current pickup value, and specified
     * operation.
     *
     * @param gridData
     * @param op
     * @return see above
     */
    public Grid2DBit comparisonOperate(final IGridData gridData, Op op) {
        if (!(gridData instanceof ScalarGridData)) {
            throw new IllegalArgumentException(
                    "gridData must be an instance of ScalarGridData, received: "
                            + gridData.getClass().getName());
        }

        ScalarDataObject scalarDataObject = (ScalarDataObject) gridData
                .getDataObject();

        Grid2DFloat grid = getScalarGrid();
        Grid2DBit bits = new Grid2DBit(grid.getXdim(), grid.getYdim());
        switch (op) {
        case EQ:
        case NOT_EQ:
        case GT:
        case GT_EQ:
        case LT:
        case LT_EQ:
            bits = getDataObject().comparisonOperate(op, scalarDataObject);
            break;
        case ALMOST:
            bits = getDataObject().almost(scalarDataObject,
                    this.parm.getParmState().getFuzzValue());
            break;
        case NOT_ALMOST:
            bits = getDataObject().almost(scalarDataObject,
                    this.parm.getParmState().getFuzzValue());
            bits.negate();
            break;
        default:
            throw new IllegalArgumentException(
                    "Op not supported: " + op.toString());
        }

        return bits;
    }

    /**
     * Operate on data with given value and operation mode over the supplied
     * edit area.
     *
     * @param value
     * @param op
     * @param editArea
     */
    public void operate(float value, Op op, Grid2DBit editArea) {
        getDataObject().operateEquals(op, value, editArea);
    }

    /**
     * Operate on data with given grid and operation mode over the supplied edit
     * area.
     *
     * @param gridData
     * @param op
     * @param editArea
     */
    public void operate(final IGridData gridData, Op op, Grid2DBit editArea) {
        if (!(gridData instanceof ScalarGridData)) {
            throw new IllegalArgumentException(
                    "gridData must be an instance of ScalarGridData, received: "
                            + gridData.getClass().getName());
        }

        getDataObject().operateEquals(op,
                (ScalarDataObject) gridData.getDataObject(), editArea);
    }

    /**
     *
     * Operate on grid with the given value, operation, over the edit area.
     *
     * @param value
     * @param op
     * @param editArea
     */
    public void operate(final WxValue value, Op op, Grid2DBit editArea) {
        if (!(value instanceof ScalarWxValue)) {
            throw new IllegalArgumentException(
                    "value must be an instance of ScalarWxValue, received: "
                            + value.getClass().getName());
        }

        this.operate(((ScalarWxValue) value).getValue(), op, editArea);
    }

    /**
     * Limit the values in this grid to the given min/max over the edit area.
     *
     * @param min
     * @param max
     * @param editArea
     */
    public void limitValue(float min, float max, Grid2DBit editArea) {
        getDataObject().limitValue(min, max, editArea);
    }

    /**
     * Get a value from a specified grid cell
     *
     * @param x
     * @param y
     * @return the grid cell value
     */
    public float getValue(int x, int y) {
        return getScalarGrid().get(x, y);
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

        Grid2DFloat grid = getScalarGrid();
        if ((grid.getXdim() != pointsToChange.getXdim())
                || (grid.getYdim() != pointsToChange.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToChange have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToChange.getXdim(),
                    pointsToChange.getYdim()));
        }

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
                    }
                }
            }
        }

        // If the taper flag is set, taper the edges.
        if (taper) {
            taperGrid(time, pointsToChange);
        }

        return pointsToChange;
    }

    /**
     * Applies the given delta value to the identified points using a special
     * taper function that is based on the specified gridCoord. Returns true for
     * success.
     *
     * @param time
     * @param delta
     * @param pointsToChange
     * @param gridCoord
     * @return true if successful
     */
    public boolean applyDeltaTaper(Date time, float delta,
            Grid2DBit pointsToChange, Point gridCoord) {
        if (delta == 0.0) {
            // nothing to change
            return false;
        }
        checkOkayForEdit();

        // Make the change and remember the changes.
        return setChangedPoints(
                doDeltaTaper(time, delta, pointsToChange, gridCoord));
    }

    protected Grid2DBit doDeltaTaper(final Date time, float delta,
            final Grid2DBit pointsToChange, Point coord) {
        Point ll = new Point(), ur = new Point();
        float sum;

        Grid2DFloat grid = getScalarGrid();
        if ((grid.getXdim() != pointsToChange.getXdim())
                || (grid.getYdim() != pointsToChange.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToChange have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToChange.getXdim(),
                    pointsToChange.getYdim()));
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
                        sum = grid.get(i, j) + (delta * taperGrid.get(i, j));

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
        }

        return pointsToChange;
    }

    @Override
    public void set(Point gridLoc, WxValue wxValue) {
        if (!(wxValue instanceof ScalarWxValue)) {
            throw new IllegalArgumentException(
                    "wxValue must be an instance of ScalarWxValue, received: "
                            + wxValue.getClass().getName());
        }

        this.set(gridLoc, ((ScalarWxValue) wxValue).getValue());
    }

    /**
     * Set a particular grid cell to a value
     *
     * @param gridLoc
     * @param value
     */
    public void set(Point gridLoc, float value) {
        checkOkayForEdit();

        pointSet(value, gridLoc);
        setChangedPoints(gridLoc);
    }

    /**
     * Stores the grid into the storage grid after doing checking
     *
     * @param values
     *            the values to set
     * @param pointsToChange
     *            the points to set
     */
    protected void gridSet(final Grid2DFloat values,
            final Grid2DBit pointsToChange) {

        Grid2DFloat grid = getScalarGrid();
        int dimX = grid.getXdim();
        int dimY = grid.getYdim();

        if ((values.getXdim() != dimX) || (values.getYdim() != dimY)) {
            throw new IllegalArgumentException(String.format(
                    "This grid and values have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    dimX, dimY, values.getXdim(), values.getYdim()));
        }

        if ((pointsToChange.getXdim() != dimX)
                || (pointsToChange.getYdim() != dimY)) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToChange have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    dimX, dimY, pointsToChange.getXdim(),
                    pointsToChange.getYdim()));
        }

        // get values out of grid and assign them
        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();

        for (int i = 0; i < dimX; i++) {
            for (int j = 0; j < dimY; j++) {
                if (pointsToChange.get(i, j) > 0) {
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

        return;
    }

    /**
     * Sets multiple (scalar) values in the grid.
     *
     * @param value
     *            the values to set
     * @param pointsToChange
     *            the points to set
     */
    public void set(Grid2DFloat value, Grid2DBit pointsToChange) {
        checkOkayForEdit();

        gridSet(value, pointsToChange);
        setChangedPoints(pointsToChange);
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

        Grid2DFloat grid = getScalarGrid();
        grid.set(loc.x, loc.y, newValue);
    }

    @Override
    protected boolean translateDataFrom(IGridData source)
            throws FactoryException, TransformException {
        if (!(source instanceof ScalarGridData)) {
            throw new IllegalArgumentException(
                    "source must be an instance of ScalarGridData, received: "
                            + source.getClass().getName());
        }

        ScalarGridData scalarSource = (ScalarGridData) source;

        Unit<?> sourceUnit = scalarSource.getParm().getGridInfo()
                .getUnitObject();
        Unit<?> thisUnit = this.getParm().getGridInfo().getUnitObject();

        if (!sourceUnit.equals(thisUnit)) {
            UnitConverter uc;
            try {
                uc = sourceUnit.getConverterToAny(thisUnit);
            } catch (IncommensurableException | UnconvertibleException e1) {
                SimpleUnitFormat fm = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.ASCII);
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to convert unit " + fm.format(sourceUnit)
                                + " to unit " + fm.format(thisUnit),
                        e1);
                return false;
            }
            scalarSource = scalarSource.copy();
            FloatBuffer sourceData = scalarSource.getScalarGrid().getBuffer();
            for (int i = 0; i < sourceData.capacity(); i++) {
                sourceData.put(i, (float) uc.convert(sourceData.get(i)));
            }
        }

        float minLimit = this.getMinValue();
        float maxLimit = this.getMaxValue();
        float sourceMinLimit = scalarSource.getMinValue();
        float sourceMaxLimit = scalarSource.getMaxValue();

        if (scalarSource.getParm().getGridInfo().getGridLoc()
                .equals(this.parm.getGridInfo().getGridLoc())
                && (maxLimit >= sourceMaxLimit)
                && (minLimit <= sourceMinLimit)) {
            setScalarGrid(scalarSource.getScalarGrid());
        } else {
            RemapGrid remap = new RemapGrid(
                    scalarSource.getParm().getGridInfo().getGridLoc(),
                    this.parm.getGridInfo().getGridLoc());
            Grid2DFloat scalarGrid = remap.remap(scalarSource.getScalarGrid(),
                    -99999.99f, maxLimit, minLimit, minLimit);
            setScalarGrid(scalarGrid);
        }
        return true;
    }

    @Override
    protected Grid2DBit doSmooth(Date time, Grid2DBit pointsToSmooth) {
        Grid2DFloat grid = getScalarGrid();

        if ((grid.getXdim() != pointsToSmooth.getXdim())
                || (grid.getYdim() != pointsToSmooth.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToSmooth have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToSmooth.getXdim(),
                    pointsToSmooth.getYdim()));
        }

        Grid2DFloat originalGrid;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            originalGrid = ((ScalarDataObject) p.getSecond().getDataObject())
                    .getScalarGrid();
        } else {
            originalGrid = getScalarGrid().copy();
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
                        for (newx = i - ss; newx <= (i + ss); newx++) {
                            for (newy = j - ss; newy <= (j + ss); newy++) {
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

        // return the points that were changed
        return pointsToSmooth;

    }

    @Override
    protected Grid2DBit doSet(WxValue value, Grid2DBit pointsToSet) {
        if (!(value instanceof ScalarWxValue)) {
            throw new IllegalArgumentException(
                    "value must be an instance of ScalarWxValue, received: "
                            + value.getClass().getName());
        }

        ScalarWxValue scalarValue = (ScalarWxValue) value;
        Grid2DFloat grid = getScalarGrid();
        if ((grid.getXdim() != pointsToSet.getXdim())
                || (grid.getYdim() != pointsToSet.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToSet have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToSet.getXdim(),
                    pointsToSet.getYdim()));
        }

        Point ll = new Point(), ur = new Point();
        // extract the floating-point value
        float v = scalarValue.getValue();

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
        }
        return pointsToSet;
    }

    @Override
    public ScalarGridData copy() {
        return new ScalarGridData(this);
    }

    @Override
    protected IDataObject doGridMin(IDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        return getDataObject().min((IContinuousDataObject) dataObject);
    }

    @Override
    protected IDataObject doGridMax(IDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        return getDataObject().max((IContinuousDataObject) dataObject);
    }

    @Override
    protected IDataObject doGridMultiply(float factor) {
        return getDataObject().operate(Op.MULTIPLY, factor);
    }

    @Override
    protected IDataObject doGridSum(IDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        return getDataObject().sum((IContinuousDataObject) dataObject);
    }

    @Override
    protected Grid2DBit doContiguous(Date time, Point location) {
        Point size = getGridInfo().getGridLoc().gridSize();
        Grid2DBit valid = new Grid2DBit(size.x, size.y, true);

        // get the grid
        Grid2DFloat originalGrid;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            valid = p.getFirst();
            originalGrid = ((ScalarDataObject) p.getSecond().getDataObject())
                    .getScalarGrid();
        } else {
            originalGrid = getScalarGrid();
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
                    if (valid.getAsBoolean(i, j) && (Math
                            .abs(originalGrid.get(i, j) - value) <= fuzz)) {
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
        Grid2DFloat grid = getScalarGrid();
        if ((grid.getXdim() != editArea.getXdim())
                || (grid.getYdim() != editArea.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied editArea have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), editArea.getXdim(),
                    editArea.getYdim()));
        }

        // Convert to grid coordinates
        Point gridPointPath[] = this.convertToGridCoords(path);

        // fill in any gaps and edges
        gridPointPath = this.parm.getGridInfo().getGridLoc()
                .connectGridPoints(gridPointPath);

        Grid2DBit gridCells = this.calculatePencilInfluence(gridPointPath,
                grid);

        // Make a Grid2DBit and set every point in gridPointPath
        Grid2DBit pathGrid = new Grid2DBit(
                this.parm.getGridInfo().getGridLoc().gridSize().x,
                this.parm.getGridInfo().getGridLoc().gridSize().y);

        // Assign the value to the gridPointPath
        for (Point p : gridPointPath) {
            pathGrid.set(p.x, p.y);
        }

        // save the original grid values
        Grid2DFloat saveGrid;
        saveGrid = grid.copy();

        // Now set the value of the pathPoints to value
        doSet(value, pathGrid);

        // Fill and smooth once in using the pencil influence
        doFillIn(time, gridCells);
        smooth(time, gridCells.or(pathGrid));

        Grid2DBit changedGrid = gridCells.or(pathGrid);

        // restrict the changes to the edit area, if set
        if (editArea.isAnyBitsSet()) {
            // undo everything outside the edit area
            Grid2DBit undoArea = changedGrid.xor(editArea).and(changedGrid);
            gridSet(saveGrid, undoArea);

            // adjust the area that was really changed
            changedGrid.andEquals(editArea);
        }

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
     * @return mask of changed points
     */
    @Override
    protected Grid2DBit doFillIn(Date time, Grid2DBit pointsToFillIn) {
        // get the grid
        Grid2DFloat grid = getScalarGrid();

        if ((grid.getXdim() != pointsToFillIn.getXdim())
                || (grid.getYdim() != pointsToFillIn.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToFillIn have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToFillIn.getXdim(),
                    pointsToFillIn.getYdim()));
        }

        Grid2DFloat workingGrid;
        if (iscMode()) {
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            workingGrid = ((ScalarDataObject) p.getSecond().getDataObject())
                    .getScalarGrid();
        } else {
            workingGrid = getScalarGrid().copy();
        }

        interpSpatialGap(pointsToFillIn, workingGrid, this.getMinValue(),
                this.getMaxValue(), grid);
        return pointsToFillIn;
    }

    @Override
    protected Grid2DBit doCopy(Date time, Grid2DBit pointsToCopy, Point delta) {
        // get the grid
        Grid2DFloat grid = getScalarGrid();
        if ((grid.getXdim() != pointsToCopy.getXdim())
                || (grid.getYdim() != pointsToCopy.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and pointsToCopy have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    grid.getXdim(), grid.getYdim(), pointsToCopy.getXdim(),
                    pointsToCopy.getYdim()));
        }

        // copy the grid
        Grid2DFloat originalGrid;
        if (iscMode()) {
            // Return value being thrown on floor?
            Pair<Grid2DBit, IGridData> p = getISCGrid(time);
            originalGrid = ((ScalarDataObject) p.getSecond().getDataObject())
                    .getScalarGrid();
        } else {
            originalGrid = grid.copy();
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
                        if (grid.isValid(newx, newy)) {
                            grid.set(newx, newy, originalGrid.get(i, j));
                        }
                    }
                }
            }
        }

        return pointsToCopy.translate(delta);

    }

    @Override
    public void setDataObject(IDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }
        super.setDataObject(dataObject);
    }

    @Override
    public synchronized ScalarDataObject getDataObject() {
        return (ScalarDataObject) super.getDataObject();
    }

    private Grid2DFloat getScalarGrid() {
        return getDataObject().getScalarGrid();
    }

    private void setScalarGrid(Grid2DFloat grid) {
        ScalarDataObject dataObject = getDataObject();
        dataObject.setScalarGrid(grid);
        setDataObject(dataObject);
    }

    @Override
    protected boolean doValid() {
        if (!getGridTime().isValid() || (getParm() == null)
                || (getDataObject() == null)) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid grid time, bad parm or data slice");
            // time, parm, or data slice not valid
            return false;
        }

        // check grid size
        Point dim = getParm().getGridInfo().getGridLoc().gridSize();
        Grid2DFloat grid = getScalarGrid();
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
            if ((f < minLimit) || (f > maxLimit)) {
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

    @Override
    protected IGridSlice createSlice() {
        return new ScalarGridSlice(getGridTime(), getGridInfo(), getHistory(),
                getScalarGrid());
    }
}
