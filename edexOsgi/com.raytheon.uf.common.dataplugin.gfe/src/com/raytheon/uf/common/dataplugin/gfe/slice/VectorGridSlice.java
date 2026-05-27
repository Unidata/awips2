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

import java.nio.FloatBuffer;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

import jep.NDArray;

/**
 * Vector version of GridSlice
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 30, 2008           chammack  Stubbed-out class based on AWIPS I
 * Feb 22, 2008  879      rbell     Legacy conversion, extended ScalarSlice
 * Jun 10, 2009  2159     rjpeter   Updated checkDims to check dirGrid for null
 * Apr 23, 2013  1949     rjpeter   Updated wind checks to keep float precision.
 * Aug 13, 2013  1571     randerso  Removed toString to stop it from hanging the
 *                                  debugger when trying to display the grid
 * Apr 23, 2015  4259     njensen   Updated for new JEP API
 * Aug 02, 2016  5744     mapeters  Removed dead cache code
 * Aug 08, 2016  5744     randerso  Fix bad clone method exposed by previous
 *                                  change
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Change clone() to copy(). Code cleanup
 *
 * </pre>
 *
 * @author chammack
 */
@DynamicSerialize
public class VectorGridSlice extends ScalarGridSlice {

    private static final float DEG_IN_CIRCLE = 360.0f;

    @DynamicSerializeElement
    protected Grid2DFloat dirGrid;

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
        this.dirGrid = dir;
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
        this.dirGrid = dir;
    }

    /**
     * Constructor for VectorGridSlice taking data, grid parm info, valid time
     * and history
     *
     * @param validTime
     * @param gpi
     * @param history
     * @param mag
     * @param dir
     */
    public VectorGridSlice(TimeRange validTime, GridParmInfo gpi,
            List<GridDataHistory> history, Grid2DFloat mag, Grid2DFloat dir) {
        this(validTime, gpi,
                history.toArray(new GridDataHistory[history.size()]), mag, dir);
    }

    /**
     * Copy constructor
     *
     * @param rhs
     *            Slice to copy
     */
    public VectorGridSlice(VectorGridSlice rhs) {
        super(rhs);
        this.dirGrid = rhs.getDirGrid().copy();
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
    public void assign(IGridSlice rhs) {
        if (!(rhs instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "rhs must be an instance of VectorGridSlice, received: "
                            + rhs.getClass().getName());
        }

        super.assign(rhs);

        Grid2DFloat rhsDirGrid = ((VectorGridSlice) rhs).getDirGrid();

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
        dirGrid.copyWithMask(directionGrid, editArea);
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

        if (!super.equals(obj)) {
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

    @Override
    public IContinuousSlice min(IContinuousSlice gs) {
        if (!(gs instanceof VectorGridSlice)) {
            throw new IllegalArgumentException(
                    "gs must be an instance of VectorGridSlice, received: "
                            + gs.getClass().getName());
        }

        VectorGridSlice rhs = (VectorGridSlice) gs;
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
        VectorGridSlice newGS = new VectorGridSlice(rhs);
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
                    "gs must be an instance of VectorGridSlice, received: "
                            + gs.getClass().getName());
        }

        VectorGridSlice rhs = (VectorGridSlice) gs;
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
        VectorGridSlice newGS = new VectorGridSlice(rhs);
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
    public VectorGridSlice copy() {
        return new VectorGridSlice(this);
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

    @Override
    public boolean isPopulated() {
        if (super.isPopulated()) {
            return dirGrid != null;
        }

        return false;
    }
}
