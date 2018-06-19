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
package com.raytheon.uf.common.dataplugin.gfe.grid;

import java.awt.Point;
import java.nio.ShortBuffer;
import java.util.Arrays;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implementation of the Short version of Grid2D.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 08, 2015  #4617     randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class Grid2DShort implements IGrid2D {
    /* error strings */
    private static final String DIM_INVALID = "Grid index out of range: user-specified=(%d, %d), maximum=(%d, %d)";

    private static final String TYPE_INVALID = "The input source grid must be of type Grid2DShort";

    private static final String NO_MATCH_GRIDS = "This grid, the input grid, and the mask grid must have equal dimensions";

    /**
     * The data buffer, holding the grid's contents.
     */
    private ShortBuffer buffer = null;

    /**
     * Width of the grid.
     */
    private int xDim = 0;

    /**
     * Height of the grid.
     */
    private int yDim = 0;

    /**
     * Constructor for creating a two-dimension grid to contain {@code int}s.
     * {@code xDim} and {@code yDim} specify the size of the grid. The buffer is
     * allocated but not initialized.
     * 
     * @param xDim
     *            width of the grid
     * @param yDim
     *            height of the grid
     */
    public Grid2DShort(int xDim, int yDim) {
        this.xDim = xDim;
        this.yDim = yDim;
        this.buffer = ShortBuffer.allocate(xDim * yDim);
    }

    /**
     * Constructor for creating a two-dimension grid containing {@code short}s.
     * {@code xDim} and {@code yDim} specify the size of the grid. The grid is
     * initialized using the {@code data} provided. A runtime exception may
     * result if the dimensions are out of synch with the data array.
     * 
     * @param xDim
     *            width of the grid
     * @param yDim
     *            height of the grid
     * @param data
     *            contains values to initialize the object
     */
    public Grid2DShort(int xDim, int yDim, short[] data) {
        this(xDim, yDim);
        if ((xDim * yDim) != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }
        this.buffer.put(data);
    }

    /**
     * Constructor for creating a two-dimension grid containing {@code short}s.
     * {@code xDim} and {@code yDim} specify the size of the grid. The grid is
     * initialized using the {@code data} provided. A
     * {@code IllegalArgumentException} is thrown if are out of synch with the
     * data provided.
     * 
     * @param xDim
     *            width of the grid
     * @param yDim
     *            height of the grid
     * @param data
     *            contains values to initialize the object. <b>NOTE:</b> this
     *            object assumes ownership of this Buffer so the caller should
     *            not modify it after calling this constructor.
     */
    public Grid2DShort(int xDim, int yDim, ShortBuffer data) {
        if ((xDim * yDim) != data.limit()) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.limit());
        }
        this.xDim = xDim;
        this.yDim = yDim;
        this.buffer = data;
    }

    /**
     * Copy constructor. Creates a two-dimension grid that is a copy of the
     * provided grid.
     * 
     * @param data
     *            the object to copy
     */
    public Grid2DShort(Grid2DShort data) {
        this.xDim = data.getXDim();
        this.yDim = data.getYDim();
        short temp[] = data.buffer.array();
        this.buffer = ShortBuffer.allocate(temp.length);
        this.buffer.put(temp);
    }

    /**
     * Returns the grid value at the specified coordinates. Throws a runtime
     * exception if the coordinates are not valid.
     * 
     * @param xDim
     *            horizontal coordinate of the desired grid point
     * @param yDim
     *            vertical coordinate of the desired grid point
     * 
     * @return the value at the desired grid point
     */
    public short get(int xDim, int yDim) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException(String.format(DIM_INVALID, xDim,
                    yDim, this.xDim - 1, this.yDim - 1));
        }
        return this.buffer.get((yDim * this.xDim) + xDim);
    }

    /**
     * Sets the specified value into the grid at the specified coordinates.
     * Throws a runtime exception if the coordinates are not valid.
     * 
     * @param xDim
     *            horizontal coordinate of the desired grid point
     * @param yDim
     *            vertical coordinate of the desired grid point
     * @param value
     *            the new grid value
     */
    public void set(int xDim, int yDim, short value) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException(String.format(DIM_INVALID, xDim,
                    yDim, this.xDim - 1, this.yDim - 1));
        }
        this.buffer.put((yDim * this.xDim) + xDim, value);
    }

    /**
     * Sets the specified value into the grid at the current insertion point.
     */
    public void set(short value) {
        this.buffer.put(value);
    }

    /**
     * Sets all grid points to the specified value.
     */
    public void setAllValues(short value) {
        Arrays.fill(this.buffer.array(), value);
    }

    /**
     * Replaces all occurrences of the specified value with the replacement
     * value.
     * 
     * @param oldVal
     *            grid value to replace
     * @param newVal
     *            replacement value
     */
    public void setAllOfValue(short oldVal, short newVal) {
        short[] data = this.buffer.array();
        for (int i = 0; i < data.length; i++) {
            if (data[i] == oldVal) {
                data[i] = newVal;
            }
        }
    }

    /**
     * Sets all grid values to zero.
     */
    public void clear() {
        setAllValues((short) 0);
    }

    /**
     * Sets the specified grid value to zero.
     * 
     * @param x
     *            horizontal coordinate grid point to clear
     * @param y
     *            vertical coordinate grid point to clear
     */
    public void clear(int x, int y) {
        buffer.put((y * xDim) + x, (short) 0);
    }

    /**
     * Creates a new two-dimension grid by translating this grid by the amount
     * specified by {@code delta}.
     * <P>
     * For example:
     * 
     * <PRE>
     *   {@literal delta = (i,j) (the translation point),}
     *   {@literal this = the current grid, and}
     *   {@literal new = the new grid}
     *   {@literal then}
     *   {@literal new.get(a,b) is undefined for a &lt; i, b &lt; j}
     *   {@literal new.get(a,b) is this.get(a-i,b-j)}
     * </PRE>
     * 
     * @param delta
     *            represents the desired translation
     * 
     * @return the translated grid
     */
    public Grid2DShort translate(Coordinate delta) {
        Grid2DShort retVal = new Grid2DShort(this.xDim, this.yDim);
        for (int x = 0; x < this.xDim; x++) {
            for (int y = 0; y < this.yDim; y++) {
                int nx = x + (int) delta.x;
                int ny = y + (int) delta.y;
                if (retVal.isValid(nx, ny)) {
                    retVal.set(nx, ny, this.get(x, y));
                }
            }
        }
        return retVal;
    }

    /**
     * Translates this grid by the specified amount. See
     * {@link #translate(Coordinate)} for details.
     * 
     * @param delta
     *            represents the desired translation
     * 
     * @return the translated grid
     */
    public Grid2DShort translateMe(Coordinate delta) {
        Grid2DShort temp = translate(delta);
        this.buffer = temp.buffer;
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.grid.IGrid2D#copyWithMask(com.raytheon.edex.grid.IGrid2D
     * , com.raytheon.edex.grid.Grid2DBit)
     */
    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2DShort)) {
            throw new IllegalArgumentException(TYPE_INVALID);
        }
        Grid2DShort conv = (Grid2DShort) sourceGrid;
        if (!isCompatible(conv) || !isCompatible(maskGrid)) {
            throw new IllegalArgumentException(NO_MATCH_GRIDS);
        }

        short[] data = this.buffer.array();
        short[] sourceData = conv.buffer.array();
        byte[] maskData = maskGrid.buffer.array();
        for (int i = 0; i < data.length; i++) {
            if (maskData[i] != (byte) 0) {
                data[i] = sourceData[i];
            }
        }
    }

    /**
     * Determines if the specified grid has the same dimension as this grid.
     * 
     * @param rhs
     *            the grid to compare
     * 
     * @return true if the dimensions match
     */
    public boolean isCompatible(IGrid2D rhs) {
        if (rhs instanceof Grid2DShort) {
            return (this.xDim == ((Grid2DShort) rhs).getXDim())
                    && (this.yDim == ((Grid2DShort) rhs).getYDim());
        } else if (rhs instanceof Grid2DBit) {
            return (this.xDim == ((Grid2DBit) rhs).getXdim())
                    && (this.yDim == ((Grid2DBit) rhs).getYdim());
        } else {
            return false;
        }
    }

    public ShortBuffer getBuffer() {
        return this.buffer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.grid.IGrid2D#getXDim()
     */
    public int getXDim() {
        return this.xDim;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.grid.IGrid2D#getYDim()
     */
    public int getYDim() {
        return this.yDim;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.grid.IGrid2D#isValid()
     */
    @Override
    public boolean isValid() {
        return (this.xDim > 0) && (this.yDim > 0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.grid.IGrid2D#isValid(int, int)
     */
    @Override
    public boolean isValid(int x, int y) {
        return ((x >= 0) && (y >= 0) && (x < xDim) && (y < yDim));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.grid.IGrid2D#subGrid(int, int, int, int)
     */
    @Override
    public IGrid2D subGrid(int minX, int minY, int maxX, int maxY) {
        Grid2DShort retVal = new Grid2DShort((maxX + 1) - minX, (maxY + 1)
                - minY);
        for (int y = minY; y < (maxY + 1); y++) {
            for (int x = minX; x < (maxX + 1); x++) {
                retVal.set(this.get(x, y));
            }
        }
        return retVal;
    }

    /**
     * Tests the specified object for equality to this object. The object is
     * equal this object if 1) it is the same object, or 2) it is a
     * {@code Grid2DInteger} object, has the same dimensions as this object, and
     * contains same grid values (at corresponding points) as this object.
     * 
     * @param rhs
     *            the object to test for equality
     * 
     * @return true if the object is equal (as defined) to this object
     */
    @Override
    public boolean equals(Object rhs) {
        if (!(rhs instanceof Grid2DShort)) {
            return false;
        }
        Grid2DShort conv = (Grid2DShort) rhs;
        if (conv == this) { // short circuit
            return true;
        }

        if ((this.xDim != conv.xDim) || (this.yDim != conv.yDim)) {
            return false;
        }

        short[] data = this.buffer.array();
        short[] rhsData = conv.buffer.array();
        for (int i = 0; i < data.length; i++) {
            if (data[i] != rhsData[i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public Grid2DShort clone() {
        return new Grid2DShort(this);
    }

    @Override
    public String toString() {
        StringBuffer retVal = new StringBuffer();
        retVal.append(xDim).append("X").append(yDim).append("\n[\n");
        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                retVal.append(this.get(x, y))
                        .append((x + 1) == xDim ? "" : ",");
            }
            retVal.append("\n");
        }
        retVal.append("]");

        return retVal.toString();
    }

    @Override
    public int getXdim() {
        return xDim;
    }

    @Override
    public int getYdim() {
        return yDim;
    }

    @Override
    public Point getGridSize() {
        return new Point(xDim, yDim);
    }
}
