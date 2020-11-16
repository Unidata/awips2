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
import java.nio.Buffer;
import java.util.Arrays;

/**
 *
 * Generic implementation of a Grid2D. DO NOT use this class for bits, bytes,
 * floats or integers. This class is meant to be used as an internal storage
 * class for non-primitive types.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 07, 2008  875      bphillip  Initial Creation.
 * Sep 01, 2014  3572     randerso  Removed unnecessary @SuppressWarnings to
 *                                  eliminate Java Warning
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Change clone() to copy(). Regenerated equals
 *                                  and hashCode
 *
 * </pre>
 *
 * @param <E>
 *            type of elements in this grid
 */
public class Grid2D<E> implements IGrid2D {

    /** The data array, holding the grid's contents */
    private Object[][] data;

    /** Width of the grid */
    private int xDim;

    /** Height of the grid */
    private int yDim;

    /**
     * Constructor for creating a two-dimensional grid containing floats. xDim
     * and yDim specify the size of the grid.
     *
     * @param xDim
     *            The width of the grid
     * @param yDim
     *            The height of the grid
     */
    public Grid2D(int xDim, int yDim) {
        this.xDim = xDim;
        this.yDim = yDim;
        data = new Object[yDim][xDim];
    }

    /**
     * Constructor for creating an initialized two-dimensional grid containing
     * given value of E.
     *
     * @param xDim
     *            The width of the grid
     * @param yDim
     *            The height of the grid
     * @param aValue
     *            The initial value
     */
    public Grid2D(int xDim, int yDim, E aValue) {
        this(xDim, yDim);
        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                set(x, y, aValue);
            }
        }
    }

    /**
     * Constructor for creating a two-dimensional grid containing instances of
     * E.
     *
     * @param xDim
     *            The width of the grid
     * @param yDim
     *            The height of the grid
     * @param data
     *            array of initialization data in row major order
     */
    public Grid2D(int xDim, int yDim, E[] data) {
        this(xDim, yDim);
        if ((xDim * yDim) != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }

        int index = 0;
        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                set(x, y, data[index]);
                index++;
            }
        }
    }

    /**
     * Constructor for creating a two-dimensional grid containing instances of
     * E.
     *
     * @param xDim
     *            Width of the grid
     * @param yDim
     *            Height of the grid
     * @param data
     *            Array [yDim][xDim] of initialization data
     */
    public Grid2D(int xDim, int yDim, E[][] data) {
        this(xDim, yDim);
        boolean invalid = yDim != data.length;
        for (int y = 0; y < yDim; y++) {
            if (xDim != data[y].length) {
                invalid = true;
                break;
            }
        }

        if (invalid) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }

        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                set(x, y, data[y][x]);
            }
        }

    }

    /**
     * Copy constructor
     *
     * @param rhs
     *            Grid2D to copy
     */
    public Grid2D(Grid2D<E> rhs) {
        this(rhs.getXDim(), rhs.getYDim());
        this.data = rhs.data;

    }

    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2D)) {
            throw new IllegalArgumentException(
                    "The input grid must be of type Grid2D, received "
                            + sourceGrid.getClass().getName());
        }

        if ((this.xDim != sourceGrid.getXdim())
                || (this.yDim != sourceGrid.getYdim())) {
            throw new IllegalArgumentException(String.format(
                    "Mismatched dimensions: this grid[%d,%d], sourceGrid[%d,%d]",
                    this.xDim, this.yDim, sourceGrid.getXdim(),
                    sourceGrid.getYdim()));
        }

        if ((this.xDim != maskGrid.xdim) || (this.yDim != maskGrid.ydim)) {
            throw new IllegalArgumentException(String.format(
                    "Mismatched dimensions: this grid[%d,%d], sourceGrid[%d,%d]",
                    this.xDim, this.yDim, maskGrid.xdim, maskGrid.ydim));
        }

        @SuppressWarnings("unchecked")
        Grid2D<E> sourceGrid2D = (Grid2D<E>) sourceGrid;

        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                if (maskGrid.get(x, y) != 0) {
                    set(x, y, sourceGrid2D.get(x, y));
                }
            }
        }

    }

    /**
     * Get element
     *
     * @param x
     *            column index
     * @param y
     *            row index
     * @return the element at [x,y]
     */
    public E get(int x, int y) {
        @SuppressWarnings("unchecked")
        E element = (E) data[y][x];
        return element;
    }

    /**
     * Set element
     *
     * @param x
     *            column index
     * @param y
     *            row index
     * @param obj
     *            value to set
     */
    public void set(int x, int y, E obj) {
        data[y][x] = obj;
    }

    /**
     * @return the backing data buffer
     */
    public Buffer getBuffer() {
        throw new UnsupportedOperationException(
                "getBuffer() method on generic Grid2D object not supported");
    }

    /**
     * @return the xDim
     */
    public int getXDim() {
        return xDim;
    }

    /**
     * @return the yDim
     */
    public int getYDim() {
        return yDim;
    }

    @Override
    public boolean isValid() {
        return this.xDim > 0;
    }

    @Override
    public boolean isValid(int x, int y) {
        return ((x < xDim) && (y < yDim) && (x >= 0) && (y >= 0));
    }

    @Override
    public Grid2D<E> subGrid(int minX, int minY, int maxX, int maxY) {

        Grid2D<E> rVal = new Grid2D<>((maxX + 1) - minX, (maxY + 1) - minY);
        int y1 = 0;
        for (int y = minY; y <= maxY; y++) {
            int x1 = 0;
            for (int x = minX; x <= maxX; x++) {
                rVal.set(x1, y1, get(x, y));
                x1++;
            }
            y1++;
        }
        return rVal;
    }

    @Override
    public Grid2D<E> copy() {
        return new Grid2D<>(this);
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + Arrays.deepHashCode(data);
        result = (prime * result) + xDim;
        result = (prime * result) + yDim;
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
        Grid2D<?> other = (Grid2D<?>) obj;
        if (!Arrays.deepEquals(data, other.data)) {
            return false;
        }
        if (xDim != other.xDim) {
            return false;
        }
        if (yDim != other.yDim) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder rVal = new StringBuilder();

        rVal.append(getXdim()).append("X").append(getYdim()).append("\n[\n");
        for (int y = 0; y < getYdim(); y++) {
            for (int x = 0; x < getXdim(); x++) {
                rVal.append(this.get(x, y));
                if ((x + 1) != getXdim()) {
                    rVal.append(",");
                }
            }
            rVal.append("\n");
        }
        rVal.append("]");

        return rVal.toString();
    }

}
