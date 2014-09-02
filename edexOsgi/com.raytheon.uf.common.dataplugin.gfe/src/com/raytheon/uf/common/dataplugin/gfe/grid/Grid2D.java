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

/**
 * 
 * Generic implementation of a Grid2D. DO NOT use this class for bits, bytes,
 * floats or integers. This class is meant to be used as an internal storage
 * class for non-primitive types.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 5/7/08       875        bphillip    Initial Creation.
 * Sep 01, 2014 3572       randerso    Removed unnecessary @SuppressWarnings 
 *                                     to eliminate Java Warning
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class Grid2D<E> implements IGrid2D, Cloneable {

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
        if (xDim * yDim != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }

        int index = 0;
        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                set(x, y, data[index++]);
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

    @SuppressWarnings("unchecked")
    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2D)) {
            throw new IllegalArgumentException(
                    "The input grid must be of type Grid2D");
        }

        Grid2D<E> sourceGrid2D = (Grid2D<E>) sourceGrid;

        if (this.xDim != sourceGrid2D.xDim || this.xDim != maskGrid.xdim
                || this.yDim != sourceGrid2D.yDim || this.yDim != maskGrid.ydim) {
            throw new IllegalArgumentException(
                    "This grid, the input grid, and the input mask grid must have equal dimensions");
        }

        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                if (maskGrid.get(x, y) != 0) {
                    set(x, y, sourceGrid2D.get(x, y));
                }
            }
        }

    }

    @SuppressWarnings("unchecked")
    public E get(int x, int y) {
        return (E) data[y][x];
    }

    public void set(int x, int y, E obj) {
        data[y][x] = obj;
    }

    public Buffer getBuffer() {
        throw new UnsupportedOperationException(
                "getBuffer() method on generic Grid2D object not supported");
    }

    public int getXDim() {
        return this.xDim;
    }

    public int getYDim() {
        return this.yDim;
    }

    @Override
    public boolean isValid() {
        return this.xDim > 0;
    }

    @Override
    public boolean isValid(int x, int y) {
        return (x < xDim && y < yDim && x >= 0 && y >= 0);
    }

    @Override
    public Grid2D<E> subGrid(int minX, int minY, int maxX, int maxY) {

        Grid2D<E> rVal = new Grid2D<E>(maxX + 1 - minX, maxY + 1 - minY);
        int y1 = 0;
        for (int y = minY; y <= maxY; y++) {
            int x1 = 0;
            for (int x = minX; x <= maxX; x++) {
                rVal.set(x1++, y1, get(x, y));
            }
            y1++;
        }
        return rVal;
    }

    @Override
    public Grid2D<E> clone() {
        return new Grid2D<E>(this);
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
    public boolean equals(Object obj) {
        if (!(obj instanceof Grid2D)) {
            return false;
        }
        Grid2D<?> rhs = (Grid2D<?>) obj;
        if (this.xDim != rhs.getXdim()) {
            return false;
        }
        if (this.yDim != rhs.getYdim()) {
            return false;
        }

        for (int y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++) {
                if (!get(x, y).equals(rhs.get(x, y))) {
                    return false;
                }
            }
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder rVal = new StringBuilder();

        rVal.append(getXdim()).append("X").append(getYdim()).append("\n[\n");
        for (int y = 0; y < getYdim(); y++) {
            for (int x = 0; x < getXdim(); x++) {
                rVal.append(this.get(x, y));
                if (x + 1 != getXdim()) {
                    rVal.append(",");
                }
            }
            rVal.append("\n");
        }
        rVal.append("]");

        return rVal.toString();
    }

}
