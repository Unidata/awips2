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
import java.nio.IntBuffer;

import jep.INumpyable;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Grid2D implementation for the boolean type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/14/09      1995       bphillip    Initial release
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Grid2DBoolean implements IGrid2D, Cloneable, INumpyable,
        ISerializableObject {

    /**
     * The data buffer, holding the grid's contents
     */
    @DynamicSerializeElement
    protected IntBuffer buffer;

    /**
     * Width of the grid.
     */
    @DynamicSerializeElement
    protected int xdim;

    /**
     * Height of the grid.
     */
    @DynamicSerializeElement
    protected int ydim;

    /**
     * Constructor for serialization only.
     */
    public Grid2DBoolean() {

    }

    /**
     * Constructor for creating a two-dimensional grid containing ints. xDim and
     * yDim specify the size of the grid.
     * 
     * @param xDim
     * @param yDim
     */
    public Grid2DBoolean(int xDim, int yDim) {
        this(xDim, yDim, IntBuffer.allocate(xDim * yDim));
    }

    /**
     * Constructor for creating an initialized two-dimensional grid containing
     * ints.
     * 
     * xDim and yDim specify the size of the grid.
     * 
     * aValue is the initial value
     * 
     * @param xDim
     * @param yDim
     * @param aValue
     */
    public Grid2DBoolean(int xDim, int yDim, boolean aValue) {
        this(xDim, yDim);
        setAllValues(aValue);
    }

    /**
     * Constructor for creating a two-dimensional grid containing ints. xDim and
     * yDim specify the size of the grid. data is an array of initialization
     * data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            array of initialization data
     */
    public Grid2DBoolean(int xDim, int yDim, boolean[] data) {
        this(xDim, yDim);
        if (xDim * yDim != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }
        int[] intData = new int[data.length];
        for (int i = 0; i < data.length; i++) {
            if (data[i]) {
                intData[i] = 1;
            } else {
                intData[i] = 0;
            }
        }
        this.buffer.put(intData, 0, data.length);
    }

    /**
     * Constructor for creating a two-dimensional grid containing ints. xDim and
     * yDim specify the size of the grid. data is a IntBuffer containing
     * initialization data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            IntBuffer of initialization data
     */
    public Grid2DBoolean(int xDim, int yDim, IntBuffer data) {
        if (xDim * yDim != data.limit()) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.limit());
        }
        this.xdim = xDim;
        this.ydim = yDim;
        this.buffer = data;
    }

    /**
     * 
     * Copy constructor
     * 
     * @param rhs
     *            Grid2DBoolean to copy
     */
    public Grid2DBoolean(Grid2DBoolean rhs) {
        this(rhs.xdim, rhs.ydim);
        this.buffer.put(rhs.buffer.array());
    }

    /**
     * @param xDim
     *            x coordinate of boolean to retrieve
     * @param yDim
     *            y coordinate of boolean to retrieve
     * @return the retrieved boolean
     */
    public boolean get(int xDim, int yDim) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions not valid");
        }
        return buffer.get(yDim * this.xdim + xDim) > 0;
    }

    /**
     * @param xDim
     *            x coordinate of boolean to set
     * @param yDim
     *            y coordinate of boolean to set
     * @param aValue
     *            value of boolean to set
     */
    public void set(int xDim, int yDim, boolean aValue) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions not valid");
        }
        if (aValue) {
            buffer.put(yDim * this.xdim + xDim, 1);
        } else {
            buffer.put(yDim * this.xdim + xDim, 0);
        }
    }

    /**
     * 
     * Sets all booleans to the given value.
     * 
     * @param aValue
     *            value to set all booleans to.
     */
    public void setAllValues(boolean aValue) {
        for (int i = 0; i < buffer.limit(); i++) {
            if (aValue) {
                buffer.put(i, 1);
            } else {
                buffer.put(i, 0);
            }
        }
    }

    @Override
    public boolean isValid() {
        return this.xdim > 0;
    }

    @Override
    public boolean isValid(int x, int y) {
        return (x < xdim && y < ydim && x >= 0 && y >= 0);
    }

    /**
     * The data buffer is cleared.
     */
    public void clear() {
        for (int i = 0; i < buffer.limit(); i++) {
            buffer.put(i, 0);
        }
    }

    /**
     * Set a particular coordinate to 0
     * 
     * @param x
     *            x coordinate to clear
     * @param y
     *            y coordinate to clear
     */
    public void clear(int x, int y) {
        buffer.put(y * xdim + x, 0);
    }

    /**
     * 
     * Translates the set booleans in this object by the amount specified in
     * deltaCoord and returns a new Grid2DBoolean.
     * 
     * @param deltaCoord
     *            coordinate representing the translation from each boolean's
     *            origin
     * @return the resulting translation
     */
    public Grid2DBoolean translate(Point deltaCoord) {
        // make another Grid2DBoolean
        Grid2DBoolean rVal = new Grid2DBoolean(this.xdim, this.ydim);

        for (int x = 0; x < this.xdim; x++) {
            for (int y = 0; y < this.ydim; y++) {
                if (rVal.isValid(x + deltaCoord.x, y + deltaCoord.y)) {
                    rVal.set(x + deltaCoord.x, y + deltaCoord.y, this.get(x, y));
                }
            }
        }

        return rVal;
    }

    /**
     * 
     * Translates this Grid2DBoolean by the amount specified. Returns a
     * reference to this object.
     * 
     * Uses translate() to translate the booleans, and then assigns the result
     * to this object using the assignment operator.
     * 
     * @param deltaCoord
     *            coordinate representing the translation from each boolean's
     *            origin
     * @return this Grid2DBoolean after the translation
     */
    public Grid2DBoolean translateMe(Point deltaCoord) {
        Grid2DBoolean t = translate(deltaCoord);
        this.buffer = t.buffer;
        return this;
    }

    public IntBuffer getBuffer() {
        return buffer;
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

    @Override
    public Grid2DBoolean subGrid(int minX, int minY, int maxX, int maxY) {
        Grid2DBoolean rVal = new Grid2DBoolean(maxX + 1 - minX, maxY + 1 - minY);
        for (int y = minY; y < maxY + 1; y++) {
            for (int x = minX; x < maxX + 1; x++) {
                boolean val = this.get(x, y);
                if (val) {
                    rVal.buffer.put(1);
                } else {
                    rVal.buffer.put(0);
                }
            }
        }
        return rVal;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Grid2DBoolean)) {
            return false;
        }

        Grid2DBoolean rhs = (Grid2DBoolean) obj;

        if (rhs == this) {
            return true;
        }

        boolean rVal = true;

        if (this.xdim == rhs.xdim && this.ydim == rhs.ydim) {
            for (int i = 0; i < this.buffer.limit(); i++) {
                if (this.buffer.get(i) != rhs.buffer.get(i)) {
                    rVal = false;
                    break;
                }
            }
        } else {
            rVal = false;
        }

        return rVal;
    }

    @Override
    public Grid2DBoolean clone() throws CloneNotSupportedException {
        Grid2DBoolean rVal = new Grid2DBoolean(this);
        return rVal;
    }

    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2DBoolean)) {
            throw new IllegalArgumentException(
                    "The input source grid must be of type Grid2DBoolean");
        }

        Grid2DBoolean sourceGrid2DBoolean = (Grid2DBoolean) sourceGrid;

        if (this.xdim != sourceGrid2DBoolean.xdim || this.xdim != maskGrid.xdim
                || this.ydim != sourceGrid2DBoolean.ydim
                || this.ydim != maskGrid.ydim) {
            throw new IllegalArgumentException(
                    "This grid, the input grid, and the input mask grid must have equal dimensions");
        }

        for (int i = 0; i < this.buffer.limit(); i++) {
            if (maskGrid.buffer.get(i) != 0) {
                this.buffer.put(i, sourceGrid2DBoolean.buffer.get(i));
            }
        }
    }

    public void setAllOfValue(boolean oldValue, boolean newValue) {
        for (int i = 0; i < buffer.limit(); i++) {
            if ((this.buffer.get(i) > 0 && oldValue)
                    || (this.buffer.get(i) == 0 && !oldValue)) {
                if (newValue) {
                    this.buffer.put(i, 1);
                } else {
                    this.buffer.put(i, 0);
                }
            }
        }
    }

    @Override
    public String toString() {
        String rVal = "";

        rVal += xdim + "X" + ydim + "\n[\n";
        for (int y = 0; y < ydim; y++) {
            for (int x = 0; x < xdim; x++) {
                rVal += this.get(x, y) + (x + 1 == xdim ? "" : ",");
            }
            rVal += "\n";
        }
        rVal += "]";

        return rVal;
    }

    @Override
    public Object[] getNumpy() {
        return new Object[] { buffer.array() };
    }

    @Override
    public int getNumpyX() {
        return xdim;
    }

    @Override
    public int getNumpyY() {
        return ydim;
    }

    /**
     * @param dim
     *            the xDim to set
     */
    public void setXdim(int dim) {
        xdim = dim;
    }

    /**
     * @param dim
     *            the yDim to set
     */
    public void setYdim(int dim) {
        ydim = dim;
    }

    /**
     * @param buffer
     *            the buffer to set
     */
    public void setBuffer(IntBuffer buffer) {
        this.buffer = buffer;
    }

}
