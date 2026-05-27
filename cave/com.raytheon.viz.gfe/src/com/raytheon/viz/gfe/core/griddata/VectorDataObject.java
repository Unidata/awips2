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

import java.nio.FloatBuffer;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.time.TimeRange;

import jep.NDArray;

/**
 * Vector Data Object
 *
 * Contains the direction grid for a GFE Vector Grid. The magnitude grid is
 * inherited from ScalarDataObject
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 2, 2018            randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class VectorDataObject extends ScalarDataObject {

    private static final float DEG_IN_CIRCLE = 360.0f;

    protected Grid2DFloat dirGrid;

    /**
     * Constructor for VectorDataObject
     *
     * @param mag
     *            Magnitude grid
     * @param dir
     *            Grid2DFloat of directions in degrees
     */
    public VectorDataObject(Grid2DFloat mag, Grid2DFloat dir) {
        super(mag);
        this.dirGrid = dir;
    }

    /**
     * Copy constructor
     *
     * @param other
     *
     */
    public VectorDataObject(VectorDataObject other) {
        super(other);
        this.dirGrid = other.getDirGrid().copy();
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
        return this.dirGrid;
    }

    /**
     * @param directionGrid
     */
    public void setDirGrid(Grid2DFloat directionGrid) {
        this.dirGrid = directionGrid;
    }

    @Override
    public void assign(IDataObject rhs) {
        if (!(rhs instanceof VectorDataObject)) {
            throw new IllegalArgumentException(
                    "rhs must be an instance of VectorDataObject, received: "
                            + rhs.getClass().getName());
        }

        super.assign(rhs);

        Grid2DFloat rhsDirGrid = ((VectorDataObject) rhs).getDirGrid();

        if (rhsDirGrid != null) {
            if ((dirGrid.getXdim() != rhsDirGrid.getXdim())
                    || (dirGrid.getYdim() != rhsDirGrid.getYdim())) {
                throw new IllegalArgumentException(String.format(
                        "This grid and supplied grid have different dimensions.\n"
                                + "Expected: [%d,%d], received: [%d,%d]",
                        dirGrid.getXdim(), dirGrid.getYdim(),
                        rhsDirGrid.getXdim(), rhsDirGrid.getYdim()));
            }

            dirGrid.assign(rhsDirGrid);
        } else {
            dirGrid = null;
        }
    }

    /**
     * The assignment operator for VectorDataObject will not copy over the
     * dirGrid. Invoke this to include copying the dirGrid.
     *
     * @param rhs
     *            data object to assign from
     * @param editArea
     *            Grid2DBit to use when copying
     * @param directionGrid
     */
    public void assign(VectorDataObject rhs, Grid2DBit editArea,
            Grid2DFloat directionGrid) {
        super.operateEquals(Op.ASSIGN, rhs, editArea);
        dirGrid.copyWithMask(directionGrid, editArea);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = (prime * result)
                + ((dirGrid == null) ? 0 : dirGrid.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        VectorDataObject other = (VectorDataObject) obj;
        if (dirGrid == null) {
            if (other.dirGrid != null) {
                return false;
            }
        } else if (!dirGrid.equals(other.dirGrid)) {
            return false;
        }
        return true;
    }

    /**
     * Makes a vector data object from u and v components.
     *
     * Converts the u and v into mag and dir and then calls the appropriate
     * DataObject constructor.
     *
     * @param u
     * @param v
     * @param validTime
     * @param gridParmInfo
     * @param history
     * @return the data object
     */
    public static VectorDataObject makeDataObjectFromUV(Grid2DFloat u,
            Grid2DFloat v, TimeRange validTime, GridParmInfo gridParmInfo,
            GridDataHistory[] history) {
        Grid2DFloat magGrid, dirGrid;
        magGrid = new Grid2DFloat(u.getXdim(), u.getYdim());
        dirGrid = new Grid2DFloat(magGrid);

        // convert the u, v grids to mag and dir
        int i, j;
        for (i = 0; i < u.getXdim(); i++) {
            for (j = 0; j < u.getYdim(); j++) {
                magGrid.set(i, j, (float) Math.sqrt((u.get(i, j) * u.get(i, j))
                        + (v.get(i, j) * v.get(i, j))));
                float dir = (float) Math
                        .toDegrees(Math.atan2(u.get(i, j), v.get(i, j)));
                while (dir < 0.0f) {
                    dir += DEG_IN_CIRCLE;
                }
                while (dir >= DEG_IN_CIRCLE) {
                    dir -= DEG_IN_CIRCLE;
                }

                dirGrid.set(i, j, dir);
            }
        }

        return new VectorDataObject(magGrid, dirGrid);
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
    protected String checkDims(int x, int y) {
        String stringTest = super.checkDims(x, y);

        if (stringTest == null) {
            if ((x != dirGrid.getXdim()) || (y != dirGrid.getYdim())) {
                stringTest = "Magnitude and Direction grids have different dimensions";
            }
        }

        return stringTest;
    }

    @Override
    protected String checkDataLimits(float maxV, float minV) {
        String stringTest = super.checkDataLimits(maxV, minV);

        if (stringTest == null) {
            Grid2DFloat dGrid = getDirGrid();

            FloatBuffer dir = dGrid.getBuffer();
            int size = dir.capacity();
            for (int i = 0; i < size; i++) {
                float thisDir = dir.get(i);

                if (Float.isNaN(thisDir)) {
                    stringTest = "Direction grid contains data which is NaN at coordinate: "
                            + +(i % dGrid.getXdim()) + ','
                            + (i / dGrid.getXdim());
                    break;
                }

                while (thisDir < 0.0f) {
                    thisDir += DEG_IN_CIRCLE;
                }
                while (thisDir >= DEG_IN_CIRCLE) {
                    thisDir -= DEG_IN_CIRCLE;
                }
                dir.put(i, thisDir);
            }
        }
        return stringTest;
    }

    @Override
    public IContinuousDataObject min(IContinuousDataObject dataObject) {
        if (!(dataObject instanceof VectorDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of VectorDataObject, received: "
                            + dataObject.getClass().getName());
        }

        VectorDataObject rhs = (VectorDataObject) dataObject;
        Grid2DFloat thisMagGrid = getMagGrid();
        Grid2DFloat rhsMagGrid = rhs.getMagGrid();

        if ((thisMagGrid.getXdim() != rhsMagGrid.getXdim())
                || (thisMagGrid.getYdim() != rhsMagGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    thisMagGrid.getXdim(), thisMagGrid.getYdim(),
                    rhsMagGrid.getXdim(), rhsMagGrid.getYdim()));
        }

        // create new grid with no caching
        VectorDataObject newGS = new VectorDataObject(rhs);
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
    public IContinuousDataObject max(IContinuousDataObject dataObject) {
        if (!(dataObject instanceof VectorDataObject)) {
            throw new IllegalArgumentException(
                    "dataObject must be an instance of VectorDataObject, received: "
                            + dataObject.getClass().getName());
        }

        VectorDataObject rhs = (VectorDataObject) dataObject;
        Grid2DFloat thisMagGrid = getMagGrid();
        Grid2DFloat rhsMagGrid = rhs.getMagGrid();

        if ((thisMagGrid.getXdim() != rhsMagGrid.getXdim())
                || (thisMagGrid.getYdim() != rhsMagGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "This grid and supplied grid have different dimensions.\n"
                            + "Expected: [%d,%d], received: [%d,%d]",
                    thisMagGrid.getXdim(), thisMagGrid.getYdim(),
                    rhsMagGrid.getXdim(), rhsMagGrid.getYdim()));
        }

        // create new grid with no caching
        VectorDataObject newGS = new VectorDataObject(rhs);
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
        if ((mag != 0) || ((dir == 0) && (mag == 0))) {
            // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) == mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) {
            // Test Direction
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

        if ((mag != 0) || ((dir == 0) && (mag == 0))) {
            // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) > mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) {
            // Test Direction
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

        if ((mag != 0) || ((dir == 0) && (mag == 0))) {
            // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) >= mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) {
            // Test Direction
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

        if ((mag != 0) || ((dir == 0) && (mag == 0))) {
            // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) < mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) {
            // Test Direction
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

        if ((mag != 0) || ((dir == 0) && (mag == 0))) {
            // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (mGrid.get(i, j) <= mag) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) {
            // Test Direction
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
            throw new IllegalArgumentException(
                    "Operator " + op + " not supported");
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
        if ((mag != 0) || ((dir == 0) && (mag == 0))) {
            // Test Magnitude
            for (int i = 0; i < mGrid.getXdim(); i++) {
                for (int j = 0; j < mGrid.getYdim(); j++) {
                    if (Math.abs(mGrid.get(i, j) - mag) <= fuzz) {
                        magBits.set(i, j);
                    }
                }
            }
        }
        if (dir != 0) {
            // Test Direction
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
        } else if (value < 10000.0) {
            // 2 digit magnitude
            dir = (int) (value / 100.0);
            mag = (int) (value - (100 * dir));
        } else {
            // 3 digit magnitude
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

    @Override
    public VectorDataObject copy() {
        return new VectorDataObject(this);
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
        NDArray<?>[] arr = new NDArray[2];
        arr[0] = new NDArray<>(getMagGrid().getFloats(), getMagGrid().getYdim(),
                getMagGrid().getXdim());
        arr[1] = new NDArray<>(getDirGrid().getFloats(), getDirGrid().getYdim(),
                getDirGrid().getXdim());
        return arr;
    }
}
