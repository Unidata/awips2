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
import java.nio.FloatBuffer;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

import jep.NDArray;

/**
 *
 * Implementation of the Float version of Grid2D.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 30, 2008  879      rbell     Initial Creation.
 * Sep 01, 2014  3572     randerso  Changed getNumpy to use getFloats()
 * Apr 23, 2015  4259     njensen   Updated for new JEP API
 * Dec 13, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Change clone() to copy(). Regenerated equals
 *                                  and hashCode
 * Feb 23, 2018  7178     randerso  Fix hashCode and equals to not depend on
 *                                  Buffer.array()
 *
 * </pre>
 *
 * @author rbell
 */
@DynamicSerialize
public class Grid2DFloat implements IGrid2D {

    /**
     * The data buffer, holding the grid's contents
     */
    @DynamicSerializeElement
    protected FloatBuffer buffer;

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
     * Static factory method for use in Python since it doesn't seem to work
     * with normal constructor with the same signature
     *
     * @param xDim
     * @param yDim
     * @param data
     *            array of initialization data
     * @return the grid
     */
    public static Grid2DFloat createGrid(int xDim, int yDim, float[] data) {
        return new Grid2DFloat(xDim, yDim, data);
    }

    /**
     * Constructor for serialization only.
     */
    public Grid2DFloat() {

    }

    /**
     * Constructor for creating a two-dimensional grid containing floats. xDim
     * and yDim specify the size of the grid.
     *
     * @param xDim
     * @param yDim
     */
    public Grid2DFloat(int xDim, int yDim) {
        this.xdim = xDim;
        this.ydim = yDim;

        // this.bytes = ByteBuffer.allocateDirect(xDim * yDim * Float.SIZE /
        // Byte.SIZE);
        // this.buffer = this.bytes.asFloatBuffer();
        this.buffer = FloatBuffer.allocate(xDim * yDim);
    }

    /**
     * Constructor for creating an initialized two-dimensional grid containing
     * floats.
     *
     * xDim and yDim specify the size of the grid.
     *
     * aValue is the initial value
     *
     * @param xDim
     * @param yDim
     * @param aValue
     */
    public Grid2DFloat(int xDim, int yDim, float aValue) {
        this(xDim, yDim);
        setAllValues(aValue);
    }

    /**
     * Constructor for creating a two-dimensional grid containing floats. xDim
     * and yDim specify the size of the grid. data is an array of initialization
     * data.
     *
     * @param xDim
     * @param yDim
     * @param data
     *            array of initialization data
     */
    public Grid2DFloat(int xDim, int yDim, float[] data) {
        this(xDim, yDim);
        if ((xDim * yDim) != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }
        this.buffer.put(data);
    }

    /**
     * Constructor for creating a two-dimensional grid containing floats. xDim
     * and yDim specify the size of the grid. data is a FloatBuffer containing
     * initialization data.
     *
     * @param xDim
     * @param yDim
     * @param data
     *            FloatBuffer of initialization data
     */
    public Grid2DFloat(int xDim, int yDim, FloatBuffer data) {
        if ((xDim * yDim) != data.limit()) {
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
     *            Grid2DFloat to copy
     */
    public Grid2DFloat(Grid2DFloat rhs) {
        this(rhs.xdim, rhs.ydim);
        this.buffer.put(rhs.getBuffer());
    }

    /**
     * @param xDim
     *            x coordinate of float to retrieve
     * @param yDim
     *            y coordinate of float to retrieve
     * @return the retrieved float
     */
    public float get(int xDim, int yDim) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions not valid");
        }
        return buffer.get((yDim * this.xdim) + xDim);
    }

    /**
     * @param xDim
     *            x coordinate of float to set
     * @param yDim
     *            y coordinate of float to set
     * @param aValue
     *            value of float to set
     */
    public void set(int xDim, int yDim, float aValue) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions not valid");
        }
        buffer.put((yDim * this.xdim) + xDim, aValue);
    }

    /**
     *
     * Sets all floats to the given value.
     *
     * @param aValue
     *            value to set all floats to.
     */
    public void setAllValues(float aValue) {
        for (int i = 0; i < this.buffer.capacity(); i++) {
            this.buffer.put(i, aValue);
        }
    }

    @Override
    public boolean isValid() {
        return this.xdim > 0;
    }

    @Override
    public boolean isValid(int x, int y) {
        return ((x < xdim) && (y < ydim) && (x >= 0) && (y >= 0));
    }

    /**
     * The data buffer is cleared.
     */
    public void clear() {
        this.setAllValues(0.0f);
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
        buffer.put((y * xdim) + x, 0f);
    }

    /**
     *
     * Translates the set floats in this object by the amount specified in
     * deltaCoord and returns a new Grid2DFloat.
     *
     * @param deltaCoord
     *            coordinate representing the translation from each float's
     *            origin
     * @return the resulting translation
     */
    public Grid2DFloat translate(Point deltaCoord) {
        // make another Grid2DFloat
        Grid2DFloat rVal = new Grid2DFloat(this.xdim, this.ydim);

        for (int x = 0; x < this.xdim; x++) {
            for (int y = 0; y < this.ydim; y++) {
                if (rVal.isValid(x + deltaCoord.x, y + deltaCoord.y)) {
                    rVal.set(x + deltaCoord.x, y + deltaCoord.y,
                            this.get(x, y));
                }
            }
        }

        return rVal;
    }

    /**
     *
     * Translates this Grid2DFloat by the amount specified. Returns a reference
     * to this object.
     *
     * Uses translate() to translate the floats, and then assigns the result to
     * this object using the assignment operator.
     *
     * @param deltaCoord
     *            coordinate representing the translation from each float's
     *            origin
     * @return this Grid2DFloat after the translation
     */
    public Grid2DFloat translateMe(Point deltaCoord) {
        Grid2DFloat t = translate(deltaCoord);
        this.buffer = t.buffer;
        return this;
    }

    /**
     * @return backing data buffer
     */
    public FloatBuffer getBuffer() {
        if (buffer == null) {
            return null;
        }

        return (FloatBuffer) buffer.duplicate().rewind();
    }

    /**
     * @return this grid's data as a 1-dimensional array
     */
    public float[] getFloats() {
        float[] f;
        if (this.buffer.hasArray()) {
            f = this.buffer.array();
        } else {
            f = new float[this.buffer.capacity()];
            this.getBuffer().get(f);
        }
        return f;
    }

    /**
     * Assign this grid to value of another
     *
     * @param other
     */
    public void assign(Grid2DFloat other) {
        this.xdim = other.xdim;
        this.ydim = other.ydim;
        this.buffer.put(other.getBuffer());
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
    public Grid2DFloat subGrid(int minX, int minY, int maxX, int maxY) {
        Grid2DFloat rVal = new Grid2DFloat((maxX + 1) - minX,
                (maxY + 1) - minY);
        for (int y = minY; y < (maxY + 1); y++) {
            for (int x = minX; x < (maxX + 1); x++) {
                rVal.buffer.put(this.get(x, y));
            }
        }
        return rVal;
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
        Grid2DFloat other = (Grid2DFloat) obj;
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
    public Grid2DFloat copy() {
        return new Grid2DFloat(this);
    }

    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2DFloat)) {
            throw new IllegalArgumentException(
                    "The input source grid must be of type Grid2DFloat, received "
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

        Grid2DFloat sourceGrid2DFloat = (Grid2DFloat) sourceGrid;

        FloatBuffer data = this.getBuffer();
        FloatBuffer sourceData = sourceGrid2DFloat.getBuffer();
        byte[] maskData = maskGrid.getBuffer().array();
        for (int i = 0; i < data.capacity(); i++) {
            if (maskData[i] != 0) {
                data.put(i, sourceData.get(i));
            }
        }
    }

    /**
     * Replace all instances of oldValue with newValue
     *
     * @param oldValue
     * @param newValue
     */
    public void setAllOfValue(float oldValue, float newValue) {
        FloatBuffer data = this.getBuffer();
        for (int i = 0; i < data.capacity(); i++) {
            if (data.get(i) == oldValue) {
                data.put(i, newValue);
            }
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append(xdim).append('X').append(ydim).append("\n[\n");
        for (int y = 0; y < ydim; y++) {
            for (int x = 0; x < xdim; x++) {
                sb.append(this.get(x, y)).append((x + 1) == xdim ? "" : ",");
            }
            sb.append('\n');
        }
        sb.append(']');

        return sb.toString();
    }

    /**
     * @return numpy NDArray
     */
    public NDArray<float[]> getNDArray() {
        /*
         * FIXME We reverse the x and y dimensions because that's what AWIPS 1
         * did and that makes the pre-existing python code compatible. Java
         * ordering is x,y while python is ordering is y,x. It's confusing and
         * questionable at best so someday someone should correct all that. Good
         * luck.
         */
        return new NDArray<>(getFloats(), ydim, xdim);
    }

    /**
     * Only for use by serialization. DO NOT USE FOR ANY OTHER PURPOSE
     *
     * @param dim
     *            the xDim to set
     */
    public void setXdim(int dim) {
        xdim = dim;
    }

    /**
     * Only for use by serialization. DO NOT USE FOR ANY OTHER PURPOSE
     *
     * @param dim
     *            the yDim to set
     */
    public void setYdim(int dim) {
        ydim = dim;
    }

    /**
     * Only for use by serialization. DO NOT USE FOR ANY OTHER PURPOSE
     *
     * @param buffer
     *            the buffer to set
     */
    public void setBuffer(FloatBuffer buffer) {
        this.buffer = buffer;
    }
}
