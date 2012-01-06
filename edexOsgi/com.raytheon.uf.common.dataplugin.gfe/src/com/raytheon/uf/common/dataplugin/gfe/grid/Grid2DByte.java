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
import java.nio.ByteBuffer;
import java.util.Arrays;

import jep.INumpyable;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Implementation of the Byte version of Grid2D.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2008 879        rbell       Initial Creation.
 * Oct 22, 2008 1624       wdougherty  Speed up translate method
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */
@DynamicSerialize
public class Grid2DByte implements IGrid2D, Cloneable, INumpyable,
        ISerializableObject {

    /**
     * The data buffer, holding the grid's contents
     */
    @DynamicSerializeElement
    protected ByteBuffer buffer;

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

    public static Grid2DByte createGrid(int xDim, int yDim, byte[] data) {
        return new Grid2DByte(xDim, yDim, data);
    }

    /**
     * Constructor for serialization only.
     */
    public Grid2DByte() {

    }

    /**
     * Constructor for creating a two-dimension grid containing bytes. xDim and
     * yDim specify the size of the grid.
     * 
     * @param xDim
     * @param yDim
     */
    public Grid2DByte(int xDim, int yDim) {
        this(xDim, yDim, ByteBuffer.allocate(xDim * yDim));
    }

    /**
     * Constructor for creating a two-dimension grid containing bytes. xDim and
     * yDim specify the size of the grid. data is an array of initialization
     * data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            array of initialization data
     */
    public Grid2DByte(int xDim, int yDim, byte[] data) {
        this(xDim, yDim);
        if (xDim * yDim != data.length) {
            throw new IllegalArgumentException(
                    "Dimensions do not match data length (" + xDim + "," + yDim
                            + ") " + data.length);
        }
        this.buffer.put(data);
    }

    /**
     * Constructor for creating a two-dimension grid containing bytes. xDim and
     * yDim specify the size of the grid. data is a ByteBuffer containing
     * initialization data.
     * 
     * @param xDim
     * @param yDim
     * @param data
     *            ByteBuffer of initialization data
     */
    public Grid2DByte(int xDim, int yDim, ByteBuffer data) {
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
     *            Grid2DByte to copy
     */
    public Grid2DByte(Grid2DByte rhs) {
        this(rhs.xdim, rhs.ydim);
        this.buffer.put(rhs.buffer.array());
    }

    /**
     * @param xDim
     * @param yDim
     * @param value
     */
    public Grid2DByte(int xDim, int yDim, byte value) {
        this(xDim, yDim);
        Arrays.fill(buffer.array(), value);
    }

    /**
     * @param xDim
     *            x coordinate of byte to retrieve
     * @param yDim
     *            y coordinate of byte to retrieve
     * @return the retrieved byte
     */
    public byte get(int xDim, int yDim) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions not valid");
        }
        return buffer.get(yDim * this.xdim + xDim);
    }

    /**
     * @param xDim
     *            x coordinate of byte to set
     * @param yDim
     *            y coordinate of byte to set
     * @param aValue
     *            value of byte to set
     */
    public void set(int xDim, int yDim, byte aValue) {
        if (!isValid(xDim, yDim)) {
            throw new IllegalArgumentException("Dimensions not valid");
        }
        buffer.put(yDim * this.xdim + xDim, aValue);
    }

    public void set(int xDim, int yDim, int aValue) {
        set(xDim, yDim, (byte) aValue);
    }

    /**
     * 
     * Sets all bytes to the given value.
     * 
     * @param aValue
     *            value to set all bytes to.
     */
    public void setAllValues(byte aValue) {
        Arrays.fill(buffer.array(), aValue);
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
        Arrays.fill(buffer.array(), (byte) 0);
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
        buffer.put(y * xdim + x, (byte) 0);
    }

    /**
     * 
     * Translates the set bytes in this object by the amount specified in
     * deltaCoord and returns a new Grid2DByte.
     * 
     * @param deltaCoord
     *            coordinate representing the translation from each byte's
     *            origin
     * @return the resulting translation
     */
    public Grid2DByte translate(Coordinate deltaCoord) {
        // make another Grid2DByte
        Grid2DByte rVal = new Grid2DByte(this.xdim, this.ydim, (byte) 0);

        if (Math.abs(deltaCoord.x) < xdim && Math.abs(deltaCoord.y) < ydim) {
            // Find iteration limits for X
            int fromXStart;
            int toXStart;
            int cols;
            if (deltaCoord.x < 0) {
                fromXStart = (int) -deltaCoord.x;
                toXStart = 0;
                cols = xdim - fromXStart;
            } else {
                fromXStart = 0;
                toXStart = (int) deltaCoord.x;
                cols = xdim - toXStart;
            }

            // Find iteration limits for Y
            int fromYStart;
            int toYStart;
            int rows;
            if (deltaCoord.y < 0) {
                fromYStart = (int) -deltaCoord.y;
                toYStart = 0;
                rows = ydim - fromYStart;
            } else {
                fromYStart = 0;
                toYStart = (int) deltaCoord.y;
                rows = ydim - toYStart;
            }

            // Get the internal arrays to copy between
            byte[] fromA = buffer.array();
            byte[] toA = rVal.getBuffer().array();

            // Calculate from/to array offsets of the first point.
            int fromOffset = fromYStart * xdim + fromXStart;
            int toOffset = toYStart * xdim + toXStart;

            // For each row, copy cols bytes of data.
            // Then update offsets for next row.
            for (int row = 0; row < rows; row++, fromOffset += xdim, toOffset += xdim) {
                System.arraycopy(fromA, fromOffset, toA, toOffset, cols);
            }
        }

        return rVal;
    }

    /**
     * 
     * Translates this Grid2DByte by the amount specified. Returns a reference
     * to this object.
     * 
     * Uses translate() to translate the bytes, and then assigns the result to
     * this object using the assignment operator.
     * 
     * @param deltaCoord
     *            coordinate representing the translation from each byte's
     *            origin
     * @return this Grid2DByte after the translation
     */
    public Grid2DByte translateMe(Coordinate deltaCoord) {
        Grid2DByte t = translate(deltaCoord);
        this.buffer = t.buffer;
        return this;
    }

    public ByteBuffer getBuffer() {
        return buffer;
    }

    public int getXdim() {
        return xdim;
    }

    public int getYdim() {
        return ydim;
    }

    public Point getGridSize() {
        return new Point(xdim, ydim);
    }

    @Override
    public Grid2DByte subGrid(int minX, int minY, int maxX, int maxY) {
        Grid2DByte rVal = new Grid2DByte(maxX + 1 - minX, maxY + 1 - minY);
        for (int y = minY; y < maxY + 1; y++) {
            for (int x = minX; x < maxX + 1; x++) {
                rVal.buffer.put(this.get(x, y));
            }
        }
        return rVal;
    }

    @Override
    public boolean equals(Object rhs) {
        if (!(rhs instanceof Grid2DByte)) {
            return false;
        }

        Grid2DByte rhsGrid2DByte = (Grid2DByte) rhs;

        if (this.xdim != rhsGrid2DByte.xdim || this.ydim != rhsGrid2DByte.ydim) {
            return false;
        }

        byte[] data = this.buffer.array();
        byte[] rhsData = rhsGrid2DByte.buffer.array();
        for (int i = 0; i < data.length; i++) {
            if (data[i] != rhsData[i]) {
                return false;
            }
        }
        return true;
    }

    @Override
    public Grid2DByte clone() {
        Grid2DByte rVal = new Grid2DByte(this);
        return rVal;
    }

    @Override
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid) {
        if (!(sourceGrid instanceof Grid2DByte)) {
            throw new IllegalArgumentException(
                    "The input source grid must be of type Grid2DByte");
        }

        Grid2DByte sourceGrid2DByte = (Grid2DByte) sourceGrid;

        if (this.xdim != sourceGrid2DByte.xdim || this.xdim != maskGrid.xdim
                || this.ydim != sourceGrid2DByte.ydim
                || this.ydim != maskGrid.ydim) {
            throw new IllegalArgumentException(
                    "This grid, the input grid, and the input mask grid must have equal dimensions");
        }

        byte[] data = this.buffer.array();
        byte[] sourceData = sourceGrid2DByte.buffer.array();
        byte[] maskData = maskGrid.buffer.array();
        for (int i = 0; i < data.length; i++) {
            if (maskData[i] != (byte) 0) {
                data[i] = sourceData[i];
            }
        }
    }

    public void setAllOfValue(byte oldValue, byte newValue) {
        byte[] data = this.buffer.array();
        for (int i = 0; i < data.length; i++) {
            if (data[i] == oldValue) {
                data[i] = newValue;
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
    public Object[] getNumPy() {
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
     * @param buffer
     *            the buffer to set
     */
    public void setBuffer(ByteBuffer buffer) {
        this.buffer = buffer;
    }

    /**
     * @param xdim
     *            the xdim to set
     */
    public void setXdim(int xdim) {
        this.xdim = xdim;
    }

    /**
     * @param ydim
     *            the ydim to set
     */
    public void setYdim(int ydim) {
        this.ydim = ydim;
    }

    public void assign(Grid2DByte other) {
        this.xdim = other.xdim;
        this.ydim = other.ydim;
        this.buffer.put(other.getBuffer());
    }

    public byte[] getBytes() {
        byte[] b;
        if (this.buffer.hasArray()) {
            b = this.buffer.array();
        } else {
            b = new byte[this.buffer.capacity()];
            this.getBuffer().get(b);
        }
        return b;
    }

}
