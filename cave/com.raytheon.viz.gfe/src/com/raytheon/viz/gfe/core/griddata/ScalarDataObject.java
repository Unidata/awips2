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
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;

import jep.NDArray;

/**
 * Scalar Data Object
 *
 * Contains the grid for a GFE Scalar grid
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2017            randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class ScalarDataObject implements IContinuousDataObject {

    protected Grid2DFloat scalarGrid;

    /**
     * Constructor
     *
     * @param grid
     */
    public ScalarDataObject(Grid2DFloat grid) {
        this.scalarGrid = grid;
    }

    /**
     * Copy constructor
     *
     * @param other
     */
    public ScalarDataObject(ScalarDataObject other) {
        this.scalarGrid = other.scalarGrid.copy();
    }

    @Override
    public void assign(IDataObject rhs) {
        if (!(rhs instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "rhs must be an instance of ScalarDataObject, received: "
                            + rhs.getClass().getName());
        }

        Grid2DFloat rhsGrid = ((ScalarDataObject) rhs).getScalarGrid();

        if (rhsGrid != null) {
            if ((scalarGrid.getXdim() != rhsGrid.getXdim())
                    || (scalarGrid.getYdim() != rhsGrid.getYdim())) {
                throw new IllegalArgumentException(String.format(
                        "This grid and supplied grid have different dimensions.\n"
                                + "Expected: [%d,%d], received: [%d,%d]",
                        scalarGrid.getXdim(), scalarGrid.getYdim(),
                        rhsGrid.getXdim(), rhsGrid.getYdim()));
            }

            scalarGrid.assign(rhsGrid);
        } else {
            this.scalarGrid = null;
        }
    }

    /**
     * Return the scalar grid
     *
     * @return the scalar grid
     */
    public Grid2DFloat getScalarGrid() {
        return this.scalarGrid;
    }

    /**
     * @param scalarGrid
     *            the scalarGrid to set
     */
    public void setScalarGrid(Grid2DFloat scalarGrid) {
        this.scalarGrid = scalarGrid;
    }

    @Override
    public Grid2DBit comparisonOperate(Op op, float value) {
        Grid2DBit rVal = new Grid2DBit(scalarGrid.getXdim(),
                scalarGrid.getYdim());

        FloatBuffer thisB = scalarGrid.getBuffer();
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
                throw new IllegalArgumentException(
                        "Operator " + op + " not supported");
            }
            rValB.put(i, newB);
        }

        return rVal;
    }

    @Override
    public Grid2DBit comparisonOperate(Op op,
            IContinuousDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        ScalarDataObject rhs = (ScalarDataObject) dataObject;
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((scalarGrid.getXdim() != rhsGrid.getXdim())
                || (scalarGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    rhsGrid.getXdim(), rhsGrid.getYdim()));
        }

        Grid2DBit rVal = new Grid2DBit(scalarGrid.getXdim(),
                scalarGrid.getYdim());

        FloatBuffer thisB = scalarGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        ByteBuffer rValB = rVal.getBuffer();
        if (thisB == rhsB) {
            throw new IllegalArgumentException(
                    "This and supplied data object have the same float buffers");
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
                throw new IllegalArgumentException(
                        "Operator " + op + " not supported");
            }
            rValB.put(i, newB);
        }

        return rVal;
    }

    @Override
    public void limitValue(float minValue, float maxValue, Grid2DBit editArea) {
        if ((editArea.getXdim() != scalarGrid.getXdim())
                || (editArea.getYdim() != scalarGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied editArea have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    editArea.getXdim(), editArea.getYdim()));
        }

        Point ll = new Point();
        Point ur = new Point();
        editArea.extremaOfSetBits(ll, ur);

        for (int i = ll.x; i <= ur.x; i++) {
            for (int j = ll.y; j <= ur.y; j++) {
                if (editArea.get(i, j) != 0) {
                    if (scalarGrid.get(i, j) < minValue) {
                        scalarGrid.set(i, j, minValue);
                    } else if (scalarGrid.get(i, j) > maxValue) {
                        scalarGrid.set(i, j, maxValue);
                    }
                }
            }
        }
    }

    @Override
    public void limitValue(float minValue, float maxValue) {
        synchronized (this) {
            FloatBuffer b = scalarGrid.getBuffer();
            int size = b.capacity();
            for (int i = 0; i < size; i++) {
                if (b.get(i) < minValue) {
                    b.put(i, minValue);
                } else if (b.get(i) > maxValue) {
                    b.put(i, maxValue);
                }
            }
        }
    }

    @Override
    public IContinuousDataObject max(IContinuousDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        ScalarDataObject rhs = (ScalarDataObject) dataObject;
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((scalarGrid.getXdim() != rhsGrid.getXdim())
                || (scalarGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    rhsGrid.getXdim(), rhsGrid.getYdim()));
        }

        // copy grid with no caching, caching enabled exterior to this
        ScalarDataObject newGS = new ScalarDataObject(rhs);
        Grid2DFloat newGrid = newGS.getScalarGrid();

        FloatBuffer thisB = scalarGrid.getBuffer();
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
    public IContinuousDataObject min(IContinuousDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        ScalarDataObject rhs = (ScalarDataObject) dataObject;
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((scalarGrid.getXdim() != rhsGrid.getXdim())
                || (scalarGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    rhsGrid.getXdim(), rhsGrid.getYdim()));
        }

        // copy grid with no caching
        ScalarDataObject newDO = new ScalarDataObject(rhs);

        FloatBuffer thisB = scalarGrid.getBuffer();
        FloatBuffer rhsB = rhsGrid.getBuffer();
        Grid2DFloat newGrid = newDO.getScalarGrid();
        FloatBuffer newB = newGrid.getBuffer();
        int size = thisB.capacity();
        for (int i = 0; i < size; i++) {
            if (rhsB.get(i) > thisB.get(i)) {
                newB.put(i, thisB.get(i));
            }
        }

        return newDO;
    }

    @Override
    public void operateEquals(Op op, float value, Grid2DBit editArea) {
        if ((scalarGrid.getXdim() != editArea.getXdim())
                || (scalarGrid.getYdim() != editArea.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied editArea have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    editArea.getXdim(), editArea.getYdim()));
        }

        FloatBuffer b = scalarGrid.getBuffer();
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
                    throw new IllegalArgumentException(
                            "Operator " + op + " not supported");
                }
                b.put(i, newF);
            }
        }
    }

    @Override
    public void operateEquals(Op op, float value) {
        if (op == Op.ASSIGN) {
            scalarGrid.setAllValues(value);
            return;
        }

        FloatBuffer b = scalarGrid.getBuffer();
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
                throw new IllegalArgumentException(
                        "Operator " + op + " not supported");
            }
            b.put(i, newF);
        }
    }

    @Override
    public void operateEquals(Op op, IContinuousDataObject dataObject,
            Grid2DBit editArea) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        ScalarDataObject rhs = (ScalarDataObject) dataObject;
        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((scalarGrid.getXdim() != editArea.getXdim())
                || (scalarGrid.getYdim() != editArea.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied editArea have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    editArea.getXdim(), editArea.getYdim()));
        }
        if ((scalarGrid.getXdim() != rhsGrid.getXdim())
                || (scalarGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    rhsGrid.getXdim(), rhsGrid.getYdim()));
        }

        FloatBuffer thisB = scalarGrid.getBuffer();
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
                    throw new IllegalArgumentException(
                            "Operator " + op + " not supported");
                }
                thisB.put(i, newF);
            }
        }
    }

    @Override
    public void operateEquals(Op op, IContinuousDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        ScalarDataObject rhs = (ScalarDataObject) dataObject;

        if (op == Op.ASSIGN) {
            Grid2DFloat grid = null;
            grid = rhs.getScalarGrid().copy();
            this.scalarGrid = grid;
            return;
        }

        Grid2DFloat rhsGrid = rhs.getScalarGrid();

        if ((scalarGrid.getXdim() != rhsGrid.getXdim())
                || (scalarGrid.getYdim() != rhsGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    scalarGrid.getXdim(), scalarGrid.getYdim(),
                    rhsGrid.getXdim(), rhsGrid.getYdim()));
        }

        FloatBuffer thisB = scalarGrid.getBuffer();
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
                throw new IllegalArgumentException(
                        "Operator " + op + " not supported");
            }
            thisB.put(i, newF);
        }
    }

    @Override
    public ScalarDataObject operate(Op op, float value, Grid2DBit editArea) {
        ScalarDataObject rVal = new ScalarDataObject(this);

        rVal.operateEquals(op, value, editArea);

        return rVal;
    }

    @Override
    public ScalarDataObject operate(Op op, IContinuousDataObject dataObject,
            Grid2DBit editArea) {
        ScalarDataObject rVal = new ScalarDataObject(this);

        rVal.operateEquals(op, dataObject, editArea);

        return rVal;
    }

    @Override
    public ScalarDataObject operate(Op op, float value) {
        ScalarDataObject rVal = new ScalarDataObject(this);

        rVal.operateEquals(op, value);

        return rVal;
    }

    @Override
    public ScalarDataObject operate(Op op, IContinuousDataObject dataObject) {
        ScalarDataObject rVal = new ScalarDataObject(this);

        rVal.operateEquals(op, dataObject);

        return rVal;
    }

    @Override
    public IContinuousDataObject sum(IContinuousDataObject dataObject) {
        if (!(dataObject instanceof ScalarDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of ScalarDataObject, received: "
                            + dataObject.getClass().getName());
        }

        ScalarDataObject rVal = new ScalarDataObject(this);

        rVal.operateEquals(Op.ADD, dataObject);

        return rVal;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + ((scalarGrid == null) ? 0 : scalarGrid.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ScalarDataObject other = (ScalarDataObject) obj;
        if (scalarGrid == null) {
            if (other.scalarGrid != null) {
                return false;
            }
        } else if (!scalarGrid.equals(other.scalarGrid)) {
            return false;
        }
        return true;
    }

    /**
     * Checks dimensions of grids with dimensions specified in GridParmInfo to
     * ensure they are the same. Returns the status.
     *
     * @param x
     *            X dimension from GridParmInfo
     * @param y
     *            Y dimension from GridParmInfo
     *
     * @return String if issue, otherwise null if ok.
     */
    protected String checkDims(int x, int y) {
        if ((x != scalarGrid.getXdim()) || (y != scalarGrid.getYdim())) {
            return "Grid Dimensions and GridParmInfo Dimensions are not identical GridDim: "
                    + scalarGrid.getXdim() + "," + scalarGrid.getYdim()
                    + " GridParmInfoDim: " + x + "," + y;
        }

        return null;
    }

    /**
     * Checks data values of grids with max/min limits in GridParmInfo to ensure
     * that the data is within limits. Returns the status.
     *
     * The status is set to MaxValueExceed or MinValueExceed on failure.
     *
     * @param maxV
     *            max value from GridParmInfo
     * @param minV
     *            min value from GridParmInfo
     * @return String if issue, otherwise null if ok.
     */
    protected String checkDataLimits(float maxV, float minV) {
        synchronized (this) {
            FloatBuffer b = scalarGrid.getBuffer();
            float f;
            for (int i = 0; i < b.capacity(); i++) {
                f = b.get(i);
                if (Float.isNaN(f)) {
                    return "Grid contains data which is NaN at coordinate: "
                            + +(i % scalarGrid.getXdim()) + ','
                            + (i / scalarGrid.getXdim());
                }
                if ((f < minV) || (f > maxV)) {
                    return "Data Values Exceeded in Grid at coordinate: "
                            + (i % scalarGrid.getXdim()) + ','
                            + (i / scalarGrid.getXdim()) + " Value=" + f
                            + " MinAllowed=" + minV + " MaxAllowed=" + maxV;
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
        Grid2DBit rVal = new Grid2DBit(scalarGrid.getXdim(),
                scalarGrid.getYdim());

        FloatBuffer thisB = scalarGrid.getBuffer();
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
    public Grid2DBit almost(ScalarDataObject rhs, float fuzz) {
        Grid2DBit rVal = new Grid2DBit(scalarGrid.getXdim(),
                scalarGrid.getYdim());

        FloatBuffer thisB = scalarGrid.getBuffer();
        FloatBuffer rhsB = rhs.getScalarGrid().getBuffer();
        ByteBuffer rValB = rVal.getBuffer();
        if (thisB == rhsB) {
            throw new IllegalArgumentException(
                    "This and supplied data object have the same float buffers");
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

    @Override
    public ScalarDataObject copy() {
        return new ScalarDataObject(this);
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
        return new NDArray<>(scalarGrid.getFloats(), scalarGrid.getYdim(),
                scalarGrid.getXdim());
    }
}
