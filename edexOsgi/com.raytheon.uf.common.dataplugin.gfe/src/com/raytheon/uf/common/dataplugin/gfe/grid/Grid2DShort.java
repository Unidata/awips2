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

import org.locationtech.jts.geom.Coordinate;

/**
 * Implementation of the Short version of Grid2D.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 08, 2015  4617     randerso  Initial creation
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Change clone() to copy(). Regenerated equals
 *                                  and hashCode
 * Feb 23, 2018  7178     randerso  Fix hashCode and equals to not depend on
 *                                  Buffer.array(). Code cleanup to better match
 *                                  Grid2DFloat.
 *
 * </pre>
 *
 * @author randerso
 */
public class Grid2DShort implements IGrid2D {
    /* error strings */
    private static final String DIM_INVALID = "Grid index out of range: user-specified=(%d, %d), maximum=(%d, %d)";

    /**
     * The data buffer, holding the grid's contents.
     */
    private ShortBuffer buffer = null;

    /**
     * Width of the grid.
     */
    private int xdim = 0;

    /**
     * Height of the grid.
     */
    private int ydim = 0;

    /**
     * Constructor for creating a two-dimension grid to contain {@code int}s.
     * {@code xdim} and {@code ydim} specify the size of the grid. The buffer is
     * allocated but not initialized.
     *
     * @param xdim
     *            width of the grid
     * @param ydim
     *            height of the grid
     */
    public Grid2DShort(int xdim, int ydim) {
        this.xdim = xdim;
        this.ydim = ydim;
        this.buffer = ShortBuffer.allocate(xdim * ydim);
    }

    /**
     * Constructor for creating a two-dimension grid containing {@code short}s.
     * {@code xdim} and {@code ydim} specify the size of the grid. The grid is
     * initialized using the {@code data} provided. A runtime exception may
     * result if the dimensions are out of synch with the data array.
     *
     * @param xdim
     *            width of the grid
     * @param ydim
     *            height of the grid
     * @param data
     *            contains values to initialize the object
     */
    public Grid2DShort(int xdim, int ydim, short[] data) {
        this(xdim, ydim);
        if ((xdim * ydim) != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xdim + "," + ydim
                            + ") " + data.length);
        }
        this.buffer.put(data);
    }

    /**
     * Constructor for creating a two-dimension grid containing {@code short}s.
     * {@code xdim} and {@code ydim} specify the size of the grid. The grid is
     * initialized using the {@code data} provided. A
     * {@code IllegalArgumentException} is thrown if are out of synch with the
     * data provided.
     *
     * @param xdim
     *            width of the grid
     * @param ydim
     *            height of the grid
     * @param data
     *            contains values to initialize the object. <b>NOTE:</b> this
     *            object assumes ownership of this Buffer so the caller should
     *            not modify it after calling this constructor.
     */
    public Grid2DShort(int xdim, int ydim, ShortBuffer data) {
        if ((xdim * ydim) != data.limit()) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xdim + "," + ydim
                            + ") " + data.limit());
        }
        this.xdim = xdim;
        this.ydim = ydim;
        this.buffer = data;
    }

    /**
     * Copy constructor. Creates a two-dimension grid that is a copy of the
     * provided grid.
     *
     * @param rhs
     *            the object to copy
     */
    public Grid2DShort(Grid2DShort rhs) {
        this(rhs.xdim, rhs.ydim);
        this.buffer.put(rhs.getBuffer());
    }

    /**
     * Returns the grid value at the specified coordinates. Throws a runtime
     * exception if the coordinates are not valid.
     *
     * @param xdim
     *            horizontal coordinate of the desired grid point
     * @param ydim
     *            vertical coordinate of the desired grid point
     *
     * @return the value at the desired grid point
     */
    public short get(int xdim, int ydim) {
        if (!isValid(xdim, ydim)) {
            throw new IllegalArgumentException(String.format(DIM_INVALID, xdim,
                    ydim, this.xdim - 1, this.ydim - 1));
        }
        return this.buffer.get((ydim * this.xdim) + xdim);
    }

    /**
     * Sets the specified value into the grid at the specified coordinates.
     * Throws a runtime exception if the coordinates are not valid.
     *
     * @param xdim
     *            horizontal coordinate of the desired grid point
     * @param ydim
     *            vertical coordinate of the desired grid point
     * @param value
     *            the new grid value
     */
    public void set(int xdim, int ydim, short value) {
        if (!isValid(xdim, ydim)) {
            throw new IllegalArgumentException(String.format(DIM_INVALID, xdim,
                    ydim, this.xdim - 1, this.ydim - 1));
        }
        this.buffer.put((ydim * this.xdim) + xdim, value);
    }

    /**
     * Sets the specified value into the grid at the current insertion point.
     *
     * @param value
     */
    public void set(short value) {
        this.buffer.put(value);
    }

    /**
     * Sets all grid points to the specified value.
     *
     * @param value
     */
    public void setAllValues(short value) {
        for (int i = 0; i < this.buffer.capacity(); i++) {
            this.buffer.put(i, value);
        }
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
        buffer.put((y * xdim) + x, (short) 0);
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
        Grid2DShort retVal = new Grid2DShort(this.xdim, this.ydim);
        for (int x = 0; x < this.xdim; x++) {
            for (int y = 0; y < this.ydim; y++) {
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

    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2DShort)) {
            throw new IllegalArgumentException(
                    "The input source grid must be of type Grid2DShort, received "
                            + sourceGrid.getClass().getName());
        }

        if ((this.xdim != sourceGrid.getXdim())
                || (this.ydim != sourceGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "Mismatched dimensions: this grid[%d,%d], sourceGrid[%d,%d]",
                    this.xdim, this.ydim, sourceGrid.getXdim(),
                    sourceGrid.getYdim()));
        }

        if ((this.xdim != maskGrid.xdim) || (this.ydim != maskGrid.ydim)) {
            throw new IllegalArgumentException(String.format(
                    "Mismatched dimensions: this grid[%d,%d], sourceGrid[%d,%d]",
                    this.xdim, this.ydim, maskGrid.xdim, maskGrid.ydim));
        }

        short[] data = this.buffer.array();
        short[] sourceData = ((Grid2DShort) sourceGrid).buffer.array();
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
            return (this.xdim == ((Grid2DShort) rhs).getXdim())
                    && (this.ydim == ((Grid2DShort) rhs).getYdim());
        } else if (rhs instanceof Grid2DBit) {
            return (this.xdim == ((Grid2DBit) rhs).getXdim())
                    && (this.ydim == ((Grid2DBit) rhs).getYdim());
        } else {
            return false;
        }
    }

    /**
     * @return backing data buffer
     */
    public ShortBuffer getBuffer() {
        if (buffer == null) {
            return null;
        }

        return (ShortBuffer) buffer.duplicate().rewind();
    }

    @Override
    public boolean isValid() {
        return (this.xdim > 0) && (this.ydim > 0);
    }

    @Override
    public boolean isValid(int x, int y) {
        return ((x >= 0) && (y >= 0) && (x < xdim) && (y < ydim));
    }

    @Override
    public IGrid2D subGrid(int minX, int minY, int maxX, int maxY) {
        Grid2DShort retVal = new Grid2DShort((maxX + 1) - minX,
                (maxY + 1) - minY);
        for (int y = minY; y < (maxY + 1); y++) {
            for (int x = minX; x < (maxX + 1); x++) {
                retVal.set(this.get(x, y));
            }
        }
        return retVal;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((buffer == null) ? 0 : getBuffer().hashCode());
        result = prime * result + xdim;
        result = prime * result + ydim;
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
        Grid2DShort other = (Grid2DShort) obj;
        if (xdim != other.xdim) {
            return false;
        }
        if (ydim != other.ydim) {
            return false;
        }

        if (buffer == null) {
            if (other.buffer != null) {
                return false;
            }
        } else if (!getBuffer().equals(other.getBuffer())) {
            return false;
        }
        return true;
    }

    @Override
    public Grid2DShort copy() {
        return new Grid2DShort(this);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(xdim).append("X").append(ydim).append("\n[\n");
        for (int y = 0; y < ydim; y++) {
            for (int x = 0; x < xdim; x++) {
                sb.append(this.get(x, y)).append((x + 1) == xdim ? "" : ",");
            }
            sb.append("\n");
        }
        sb.append("]");

        return sb.toString();
    }

    @Override
    public int getXdim() {
        return xdim;
    }

    @Override
    public int getYdim() {
        return ydim;
    }

    @Override
    public Point getGridSize() {
        return new Point(xdim, ydim);
    }
}
