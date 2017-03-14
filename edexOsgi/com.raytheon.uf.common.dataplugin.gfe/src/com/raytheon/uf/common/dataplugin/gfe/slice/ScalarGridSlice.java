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
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.util.List;

import jep.NDArray;

import com.raytheon.uf.common.cache.CacheException;
import com.raytheon.uf.common.cache.CacheFactory;
import com.raytheon.uf.common.cache.ICache;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
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
 * ScalarGridSlice contains a grid and it's attribute information.
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/30/2008              chammack    Stubbed-out class based on AWIPS I
 * 02/20/2008   879        rbell       Legacy conversion
 * 06/10/2009   2159       rjpeter     Updated checkDims to check scalarGrid for null
 * 08/13/2013   1571       randerso    Removed toString to stop it from hanging the 
 *                                     debugger when trying to display the grid
 * Apr 23, 2015 4259       njensen     Updated for new JEP API
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class ScalarGridSlice extends AbstractGridSlice implements
        IContinuousSlice, Cloneable {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScalarGridSlice.class);

    /**
     * Reference to scalar data. Do no use directly. Go through getScalarGrid so
     * that useCache is properly used to access data.
     */
    @DynamicSerializeElement
    protected Grid2DFloat scalarGrid;

    protected String cacheId;

    /**
     * Constructor for serialization only
     */
    public ScalarGridSlice() {
        super();
    }

    /**
     * Constructor for scalar GridSlice taking the data, attributes, valid time,
     * and history.
     * 
     * @param grid
     *            the scalar grid
     * @param validTime
     *            the valid time of the slice
     * @param gfeRecord
     *            the database record
     */
    public ScalarGridSlice(TimeRange validTime, GFERecord gfeRecord,
            Grid2DFloat grid) {
        super(validTime, gfeRecord);
        setScalarGrid(grid);
    }

    /**
     * Constructor for scalar GridSlice taking the data, grid parm info and
     * history
     * 
     * @param validTime
     *            the valid time of the slice
     * @param gpi
     *            the grid metadata
     * @param history
     *            the grid history
     * @param grid
     *            the scalar grid
     */
    public ScalarGridSlice(TimeRange validTime, GridParmInfo gpi,
            GridDataHistory[] history, Grid2DFloat grid) {
        super(validTime, gpi, history);
        setScalarGrid(grid);
    }

    public ScalarGridSlice(TimeRange validTime, GridParmInfo gpi,
            List<GridDataHistory> history, Grid2DFloat grid) {
        this(validTime, gpi, history
                .toArray(new GridDataHistory[history.size()]), grid);
    }

    /**
     * Copy constructor, defaults to no caching on the copy
     * 
     * @param rhs
     *            ScalarGridSlice to copy
     */
    public ScalarGridSlice(ScalarGridSlice rhs) {
        this(rhs, false);
    }

    /**
     * Copy constructor
     * 
     * @param rhs
     *            ScalarGridSlice to copy
     * @param useCache
     *            Whether or not to use cache initially. Useful when copying
     *            structure and will need to immediately modify, allowing for
     *            data to only be written to cache once.
     */
    public ScalarGridSlice(ScalarGridSlice rhs, boolean useCache) {
        super(rhs);
        this.useCache = useCache;
        Grid2DFloat grid = null;

        try {
            grid = rhs.getScalarGrid().clone();
        } catch (CloneNotSupportedException e) {
            grid = new Grid2DFloat();
        }

        setScalarGrid(grid);
    }

    @Override
    public void assign(IGridSlice rhs) {
        if (!(rhs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Attempted to assign ScalarGridSlice to non-ScalarGridSlice object");
        }

        super.assign(rhs);

        Grid2DFloat rhsScalarGrid = ((ScalarGridSlice) rhs).getScalarGrid();

        if (rhsScalarGrid != null) {
            Grid2DFloat scalarGrid = getScalarGrid();
            if ((scalarGrid.getXdim() != rhsScalarGrid.getXdim())
                    || (scalarGrid.getYdim() != rhsScalarGrid.getYdim())) {
                throw new IllegalArgumentException(
                        "Supplied grid is not of same dimension");
            }

            scalarGrid.assign(rhsScalarGrid);
            setScalarGrid(scalarGrid);
        } else {
            setScalarGrid(null);
        }
    }

    /**
     * Return the scalar grid
     * 
     * @return the scalar grid
     */
    public Grid2DFloat getScalarGrid() {
        if (useCache && (cacheId != null)) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                return (Grid2DFloat) diskCache.getFromCache(cacheId);
            } catch (CacheException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to load data from GFE cache.", e);
            }
        }

        return this.scalarGrid;
    }

    /**
     * @param scalarGrid
     *            the scalarGrid to set
     */
    public void setScalarGrid(Grid2DFloat scalarGrid) {
        this.scalarGrid = scalarGrid;

        if (useCache) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                if (this.scalarGrid != null) {
                    if (cacheId != null) {
                        diskCache.addToCache(cacheId, this.scalarGrid);
                    } else {
                        cacheId = diskCache.addToCache(this.scalarGrid);
                    }
                } else if (cacheId != null) {
                    // scalarGrid is null, remove previous cache entry
                    diskCache.removeFromCache(cacheId);
                    cacheId = null;
                }

                this.scalarGrid = null;
            } catch (Exception e) {
                // failed to move to local cache, don't remove from memory
                statusHandler.handle(Priority.WARN,
                        "Failed to move data to local cache", e);
            }
        }
    }

    @Override
    public Grid2DBit comparisonOperate(Op op, float value) {
        Grid2DFloat thisGrid = getScalarGrid();
        Grid2DBit rVal = new Grid2DBit(thisGrid.getXdim(), thisGrid.getYdim());

        FloatBuffer thisB = thisGrid.getBuffer();
        ByteBuffer rValB = rVal.getBuffer();
        float thisF;
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            thisF = thisB.get(i);
            byte newB = 0;
            switch (op) {
            case EQ:
                if (thisF == value) {
                    newB = 1;
                }
                break;
            case NOT_EQ:
                if (thisF != value) {
                    newB = 1;
                }
                break;
            case GT:
                if (thisF > value) {
                    newB = 1;
                }
                break;
            case GT_EQ:
                if (thisF >= value) {
                    newB = 1;
                }
                break;
            case LT:
                if (thisF < value) {
                    newB = 1;
                }
                break;
            case LT_EQ:
                if (thisF <= value) {
                    newB = 1;
                }
                break;
            default:
                throw new IllegalArgumentException("Operator " + op
                        + " not supported");
            }
            rValB.put(i, newB);
        }

        return rVal;
    }

    @Override
    public Grid2DBit comparisonOperate(Op op, IContinuousSlice cs) {
        if (!(cs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException("Supplied GridSlice not Scalar");
        }

        ScalarGridSlice rhs = (ScalarGridSlice) cs;
        Grid2DFloat thisGrid = getScalarGrid();
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((thisGrid.getXdim() != rhsGrid.getXdim())
                || (thisGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This grid and supplied grid have different dimensions");
        }

        Grid2DBit rVal = new Grid2DBit(thisGrid.getXdim(), thisGrid.getYdim());

        FloatBuffer thisB = thisGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        ByteBuffer rValB = rVal.getBuffer();
        if (thisB == rhsB) {
            throw new IllegalArgumentException(
                    "This and supplied slice have the same float buffers");
        }
        float thisF;
        float rhsF;
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            thisF = thisB.get(i);
            rhsF = rhsB.get(i);
            byte newB = 0;
            switch (op) {
            case EQ:
                if (thisF == rhsF) {
                    newB = 1;
                }
                break;
            case NOT_EQ:
                if (thisF != rhsF) {
                    newB = 1;
                }
                break;
            case GT:
                if (thisF > rhsF) {
                    newB = 1;
                }
                break;
            case GT_EQ:
                if (thisF >= rhsF) {
                    newB = 1;
                }
                break;
            case LT:
                if (thisF < rhsF) {
                    newB = 1;
                }
                break;
            case LT_EQ:
                if (thisF <= rhsF) {
                    newB = 1;
                }
                break;
            default:
                throw new IllegalArgumentException("Operator " + op
                        + " not supported");
            }
            rValB.put(i, newB);
        }

        return rVal;
    }

    @Override
    public void limitValue(float minValue, float maxValue, Grid2DBit editArea) {
        Grid2DFloat grid = getScalarGrid();
        if ((editArea.getXdim() != grid.getXdim())
                || (editArea.getYdim() != grid.getYdim())) {
            throw new IllegalArgumentException(
                    "This grid and edit area have different dimensions");
        }

        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    if (grid.get(i, j) < minValue) {
                        grid.set(i, j, minValue);
                    } else if (grid.get(i, j) > maxValue) {
                        grid.set(i, j, maxValue);
                    }
                }
            }
        }

        // copy grid back to cache
        setScalarGrid(grid);
    }

    @Override
    public void limitValue(float minValue, float maxValue) {
        synchronized (this) {
            Grid2DFloat grid = getScalarGrid();
            FloatBuffer b = grid.getBuffer();
            int size = b.capacity();
            for (int i = 0; i < size; i++) {
                if (b.get(i) < minValue) {
                    b.put(i, minValue);
                } else if (b.get(i) > maxValue) {
                    b.put(i, maxValue);
                }
            }
            // copy back to cache
            setScalarGrid(grid);
        }
    }

    @Override
    public IContinuousSlice max(IContinuousSlice gs) {
        if (!(gs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Supplied GridSlice is not Scalar");
        }

        ScalarGridSlice rhs = (ScalarGridSlice) gs;
        Grid2DFloat thisGrid = getScalarGrid();
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((thisGrid.getXdim() != rhsGrid.getXdim())
                || (thisGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This and supplied GridSlice are different dimensions");
        }

        // copy grid with no caching, caching enabled exterior to this
        ScalarGridSlice newGS = new ScalarGridSlice(rhs, false);
        Grid2DFloat newGrid = newGS.getScalarGrid();

        FloatBuffer thisB = thisGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        FloatBuffer newB = newGrid.getBuffer();
        for (int i = 0; i < thisB.capacity(); i++) {
            if (rhsB.get(i) < thisB.get(i)) {
                newB.put(i, thisB.get(i));
            }
        }

        return newGS;
    }

    @Override
    public IContinuousSlice min(IContinuousSlice gs) {
        if (!(gs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Supplied GridSlice is not Scalar");
        }

        ScalarGridSlice rhs = (ScalarGridSlice) gs;

        Grid2DFloat thisGrid = getScalarGrid();
        Grid2DFloat rhsGrid = rhs.getScalarGrid();
        if ((thisGrid.getXdim() != rhsGrid.getXdim())
                || (thisGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This and supplied GridSlice are different dimensions");
        }

        // copy grid with no caching
        ScalarGridSlice newGS = new ScalarGridSlice(rhs, false);

        FloatBuffer thisB = thisGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        Grid2DFloat newGrid = newGS.getScalarGrid();
        FloatBuffer newB = newGrid.getBuffer();
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            if (rhsB.get(i) > thisB.get(i)) {
                newB.put(i, thisB.get(i));
            }
        }

        return newGS;
    }

    @Override
    public void operateEquals(Op op, float value, Grid2DBit editArea) {
        Grid2DFloat grid = getScalarGrid();

        if ((grid.getXdim() != editArea.getXdim())
                || (grid.getYdim() != editArea.getYdim())) {
            throw new IllegalArgumentException(
                    "This grid and edit area have different dimensions");
        }

        FloatBuffer b = grid.getBuffer();
        ByteBuffer e = editArea.getBuffer();
        float f;
        int size = b.capacity();
        for (int i = 0; i < size; i++) {
            f = b.get(i);
            if (e.get(i) != 0) {
                float newF;
                switch (op) {
                case ADD:
                    newF = f + value;
                    break;
                case SUBTRACT:
                    newF = f - value;
                    break;
                case MULTIPLY:
                    newF = f * value;
                    break;
                case DIVIDE:
                    newF = f / value;
                    break;
                case ASSIGN:
                    newF = value;
                    break;
                default:
                    throw new IllegalArgumentException("Operator " + op
                            + " not supported");
                }
                b.put(i, newF);
            }
        }

        // copy grid back to cache
        setScalarGrid(grid);
    }

    @Override
    public void operateEquals(Op op, float value) {
        Grid2DFloat grid = getScalarGrid();

        if (op == Op.ASSIGN) {
            grid.setAllValues(value);
            setScalarGrid(grid);
            return;
        }

        FloatBuffer b = grid.getBuffer();
        float f;
        int size = b.capacity();
        for (int i = 0; i < size; i++) {
            f = b.get(i);
            float newF;
            switch (op) {
            case ADD:
                newF = f + value;
                break;
            case SUBTRACT:
                newF = f - value;
                break;
            case MULTIPLY:
                newF = f * value;
                break;
            case DIVIDE:
                newF = f / value;
                break;
            default:
                throw new IllegalArgumentException("Operator " + op
                        + " not supported");
            }
            b.put(i, newF);
        }

        // copy grid back to cache
        setScalarGrid(grid);
    }

    @Override
    public void operateEquals(Op op, IContinuousSlice cs, Grid2DBit editArea) {
        if (!(cs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException("Supplied GridSlice not Scalar");
        }

        ScalarGridSlice rhs = (ScalarGridSlice) cs;
        Grid2DFloat thisGrid = getScalarGrid();
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((thisGrid.getXdim() != editArea.getXdim())
                || (thisGrid.getYdim() != editArea.getYdim())) {
            throw new IllegalArgumentException(
                    "This grid and edit area have different dimensions");
        }
        if ((thisGrid.getXdim() != rhsGrid.getXdim())
                || (thisGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This grid and supplied grid have different dimensions");
        }

        FloatBuffer thisB = thisGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        ByteBuffer editB = editArea.getBuffer();
        float thisF;
        float rhsF;
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            thisF = thisB.get(i);
            rhsF = rhsB.get(i);
            if (editB.get(i) != 0) {
                float newF;
                switch (op) {
                case ADD:
                    newF = thisF + rhsF;
                    break;
                case SUBTRACT:
                    newF = thisF - rhsF;
                    break;
                case MULTIPLY:
                    newF = thisF * rhsF;
                    break;
                case DIVIDE:
                    newF = thisF / rhsF;
                    break;
                case ASSIGN:
                    newF = rhsF;
                    break;
                default:
                    throw new IllegalArgumentException("Operator " + op
                            + " not supported");
                }
                thisB.put(i, newF);
            }
        }

        // copy grid back to cache if necessary
        setScalarGrid(thisGrid);
    }

    @Override
    public void operateEquals(Op op, IContinuousSlice cs) {
        if (!(cs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException("Supplied GridSlice not Scalar");
        }

        ScalarGridSlice rhs = (ScalarGridSlice) cs;

        if (op == Op.ASSIGN) {
            Grid2DFloat grid = null;
            try {
                grid = rhs.getScalarGrid().clone();
            } catch (CloneNotSupportedException e) {
                grid = new Grid2DFloat();
            }
            setScalarGrid(grid);
            return;
        }

        Grid2DFloat thisGrid = getScalarGrid();
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((thisGrid.getXdim() != rhsGrid.getXdim())
                || (thisGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(
                    "This grid and supplied grid have different dimensions");
        }

        FloatBuffer thisB = thisGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        float thisF;
        float rhsF;
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            thisF = thisB.get(i);
            rhsF = rhsB.get(i);
            float newF;
            switch (op) {
            case ADD:
                newF = thisF + rhsF;
                break;
            case SUBTRACT:
                newF = thisF - rhsF;
                break;
            case MULTIPLY:
                newF = thisF * rhsF;
                break;
            case DIVIDE:
                newF = thisF / rhsF;
                break;
            case ASSIGN:
                newF = rhsF;
                break;
            default:
                throw new IllegalArgumentException("Operator " + op
                        + " not supported");
            }
            thisB.put(i, newF);
        }

        // set grid back to cache
        setScalarGrid(thisGrid);
    }

    @Override
    public ScalarGridSlice operate(Op op, float value, Grid2DBit editArea) {
        ScalarGridSlice rVal = new ScalarGridSlice(this);

        rVal.operateEquals(op, value, editArea);

        return rVal;
    }

    @Override
    public ScalarGridSlice operate(Op op, IContinuousSlice cs,
            Grid2DBit editArea) {
        ScalarGridSlice rVal = new ScalarGridSlice(this);

        rVal.operateEquals(op, cs, editArea);

        return rVal;
    }

    @Override
    public ScalarGridSlice operate(Op op, float value) {
        ScalarGridSlice rVal = new ScalarGridSlice(this);

        rVal.operateEquals(op, value);

        return rVal;
    }

    @Override
    public ScalarGridSlice operate(Op op, IContinuousSlice cs) {
        ScalarGridSlice rVal = new ScalarGridSlice(this);

        rVal.operateEquals(op, cs);

        return rVal;
    }

    @Override
    public IContinuousSlice sum(IContinuousSlice gs) {
        if (!(gs instanceof ScalarGridSlice)) {
            throw new IllegalArgumentException(
                    "Supplied GridSlice is not Scalar");
        }

        ScalarGridSlice rVal = new ScalarGridSlice(this);

        rVal.operateEquals(Op.ADD, gs);

        return rVal;
    }

    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (!(obj instanceof ScalarGridSlice)) {
            return false;
        }

        Grid2DFloat grid = getScalarGrid();
        Grid2DFloat objGrid = ((ScalarGridSlice) obj).getScalarGrid();
        if (grid == null) {
            if (objGrid == null) {
                return true;
            }
            return false;
        }
        return grid.equals(objGrid);
    }

    @Override
    public String isValid() {

        String stringTest;
        if ((stringTest = super.isValid()) != null) {
            return stringTest;
        }

        if ((stringTest = checkDims()) != null) {
            return stringTest;
        }

        // check if the minimum and maximum values are exceeded
        if ((stringTest = checkDataLimits()) != null) {
            return stringTest;
        }

        // must be good if we got here!
        return null;
    }

    /**
     * Checks dimensions of grids with dimensions specified in GridParmInfo to
     * ensure they are the same. Returns the status.
     * 
     * @return String if issue, otherwise null if ok.
     */
    protected String checkDims() {
        int x = 0, y = 0; // grid dimensions
        Grid2DFloat grid = getScalarGrid();

        if (grid == null) {
            return "Grid data not populated";
        }

        x = grid.getXdim();
        y = grid.getYdim();

        if ((x != gridParmInfo.getGridLoc().getNx())
                || (y != gridParmInfo.getGridLoc().getNy())) {
            return "Grid Dimensions and GridParmInfo Dimensions are not identical GridDim: "
                    + x
                    + ","
                    + y
                    + " GridParmInfoDim: "
                    + gridParmInfo.getGridLoc().getNx()
                    + ","
                    + gridParmInfo.getGridLoc().getNy();
        }

        return null;
    }

    /**
     * Checks data values of grids with max/min limits in GridParmInfo to ensure
     * that the data is within limits. Returns the status.
     * 
     * The status is set to MaxValueExceed or MinValueExceed on failure.
     * 
     * @return String if issue, otherwise null if ok.
     */
    protected String checkDataLimits() {
        // get max/min limits from GridParmInfo
        float maxV = gridParmInfo.getMaxValue();
        float minV = gridParmInfo.getMinValue();

        synchronized (this) {
            Grid2DFloat grid = getScalarGrid();
            FloatBuffer b = grid.getBuffer();
            float f;
            for (int i = 0; i < b.capacity(); i++) {
                f = b.get(i);
                if ((f < minV) || (f > maxV)) {
                    return "Data Values Exceeded in Grid at coordinate: "
                            + (i % grid.getXdim()) + ',' + (i / grid.getXdim())
                            + " Value=" + f + " MinAllowed=" + minV
                            + " MaxAllowed=" + maxV;
                }
            }
        }

        return null;
    }

    /**
     * Returns a Grid2DBit that corresponds to the gridcells whose difference
     * between the cell value and the specified value are within the specified
     * fuzz.
     * 
     * @param value
     *            to compare to
     * @param fuzz
     *            allowed difference in value
     * @return Grid2DBit repesenting locations that are almost equal
     */
    public Grid2DBit almost(float value, float fuzz) {
        Grid2DFloat grid = getScalarGrid();
        Grid2DBit rVal = new Grid2DBit(grid.getXdim(), grid.getYdim());

        FloatBuffer thisB = grid.getBuffer();
        ByteBuffer rValB = rVal.getBuffer();
        for (int i = 0; i < thisB.capacity(); i++) {
            byte newB = 0;
            if (Math.abs(thisB.get(i) - value) <= fuzz) {
                newB = 1;
            }
            rValB.put(i, newB);
        }

        return rVal;
    }

    /**
     * Returns a Grid2DBit that corresponds to the grid cells whose difference
     * between the cell value and the specified value are within the specified
     * fuzz.
     * 
     * @param rhs
     *            to compare to
     * @param fuzz
     *            allowed difference in value
     * @return Grid2DBit representing locations that are almost equal
     */
    public Grid2DBit almost(ScalarGridSlice rhs, float fuzz) {
        Grid2DFloat grid = getScalarGrid();
        Grid2DBit rVal = new Grid2DBit(grid.getXdim(), grid.getYdim());

        FloatBuffer thisB = grid.getBuffer();
        FloatBuffer rhsB = rhs.getScalarGrid().getBuffer();
        ByteBuffer rValB = rVal.getBuffer();
        if (thisB == rhsB) {
            throw new IllegalArgumentException(
                    "This and supplied slice have the same float buffers");
        }
        for (int i = 0; i < thisB.capacity(); i++) {
            byte newB = 0;
            if (Math.abs(thisB.get(i) - rhsB.get(i)) <= fuzz) {
                newB = 1;
            }
            rValB.put(i, newB);
        }

        return rVal;
    }

    /**
     * Clone does not copy cache settings.
     */
    @Override
    public ScalarGridSlice clone() throws CloneNotSupportedException {
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

        ScalarGridSlice rval = new ScalarGridSlice(aValidTime, aGpi, aHistory,
                aGrid);
        return rval;
    }

    @Override
    public Object getNDArray() {
        /*
         * FIXME We reverse the x and y dimensions because that's what AWIPS 1
         * did and that makes the pre-existing python code compatible. Java
         * ordering is x,y while python is ordering is y,x. It's confusing and
         * questionable at best so someday someone should correct all that. Good
         * luck.
         */
        return new NDArray<float[]>(getScalarGrid().getFloats(),
                getScalarGrid().getYdim(), getScalarGrid().getXdim());
    }

    @Override
    protected void moveDataToLocalCache() {
        setScalarGrid(getScalarGrid());
    }

    @Override
    protected void moveDataToMem() {
        if (cacheId != null) {
            try {
                @SuppressWarnings("unchecked")
                ICache<IGrid2D> diskCache = CacheFactory.getInstance()
                        .getCache("GFE");

                this.scalarGrid = (Grid2DFloat) diskCache.getFromCache(cacheId);
                diskCache.removeFromCache(cacheId);
            } catch (CacheException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to load data from GFE cache.", e);
                return;
            }
        }

        cacheId = null;
    }

    public boolean isPopulated() {
        if (useCache) {
            return cacheId != null;
        }

        return scalarGrid != null;
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        if (cacheId != null) {
            @SuppressWarnings("unchecked")
            ICache<IGrid2D> diskCache = CacheFactory.getInstance().getCache(
                    "GFE");

            diskCache.removeFromCache(cacheId);
        }
    }
}
