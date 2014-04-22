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
package com.raytheon.uf.common.numeric.buffer;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * 
 * Abstract class for buffer backed data implementation that can act as both a
 * source and destination.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 07, 2014  2791     bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class BufferWrapper implements DataSource,
        DataDestination {

    protected final int nx;

    protected final int ny;

    protected BufferWrapper(int nx, int ny) {
        this.nx = nx;
        this.ny = ny;
    }

    /**
     * @return the width of the data,
     */
    public int getNx() {
        return nx;
    }

    /**
     * @return the height of the data.
     */
    public int getNy() {
        return ny;
    }

    protected boolean validateRange(int x, int y) {
        if (y < 0 || y > ny - 1) {
            return false;
        } else if (x < 0 || x > nx - 1) {
            return false;
        }
        return true;
    }

    @Override
    public double getDataValue(int x, int y) {
        if (validateRange(x, y)) {
            return getDataValueInternal(x + y * nx);
        }
        return Double.NaN;
    }

    @Override
    public void setDataValue(double dataValue, int x, int y) {
        if (validateRange(x, y)) {
            setDataValueInternal(dataValue, x + y * nx);
        }
    }

    /**
     * @return the buffer that providing data for this wrapper.
     */
    public abstract Buffer getBuffer();

    /**
     * 
     * @return an array version of this data or null if no array is available.
     *         No array will be available if the data is not backed by an
     *         array(For example direct memory or ByteBufer.as*)
     */
    public abstract Object getArray();

    protected abstract double getDataValueInternal(int index);

    protected abstract void setDataValueInternal(double dataValue, int index);

    /**
     * @return The primitve class that is the type of data held in the buffer
     *         for this wrapper.
     */
    public abstract Class<? extends Number> getPrimitiveType();

    /**
     * Create a new buffer of a known primitive type.
     * 
     * @param primitiveType
     *            The primitive type, must be one of float.class, double.class,
     *            byte.class, short.class, int.class, or long.class.
     * @param nx
     *            the width of the data.
     * @param ny
     *            the height of the data.
     * @return a new BufferWrapper of the correct type.
     */
    public static BufferWrapper create(
            Class<? extends Number> primitiveType, int nx, int ny) {
        if (float.class.equals(primitiveType)) {
            return new FloatBufferWrapper(nx, ny);
        } else if (double.class.equals(primitiveType)) {
            return new DoubleBufferWrapper(nx, ny);
        } else if (byte.class.equals(primitiveType)) {
            return new ByteBufferWrapper(nx, ny);
        } else if (short.class.equals(primitiveType)) {
            return new ShortBufferWrapper(nx, ny);
        } else if (int.class.equals(primitiveType)) {
            return new IntBufferWrapper(nx, ny);
        } else if (long.class.equals(primitiveType)) {
            return new LongBufferWrapper(nx, ny);
        } else {
            throw new IllegalArgumentException(
                    "Cannot make a buffer wrapper for "
                            + primitiveType.getName());
        }
    }

    /**
     * Wrap an existing numeric Buffer in BufferWrapper for convenient 2D
     * indexing.
     * 
     * @param buffer
     *            any Buffer except CharBuffer, Characters aren't numbers.
     * @param nx
     *            the width of the data.
     * @param ny
     *            the height of the data.
     * @return a new BufferWrapper of the correct type.
     */
    public static BufferWrapper wrap(Buffer buffer, int nx,
            int ny) {
        if (buffer instanceof FloatBuffer) {
            return new FloatBufferWrapper((FloatBuffer) buffer, nx, ny);
        } else if (buffer instanceof DoubleBuffer) {
            return new DoubleBufferWrapper((DoubleBuffer) buffer, nx, ny);
        } else if (buffer instanceof ByteBuffer) {
            return new ByteBufferWrapper((ByteBuffer) buffer, nx, ny);
        } else if (buffer instanceof ShortBuffer) {
            return new ShortBufferWrapper((ShortBuffer) buffer, nx, ny);
        } else if (buffer instanceof IntBuffer) {
            return new IntBufferWrapper((IntBuffer) buffer, nx, ny);
        } else if (buffer instanceof LongBuffer) {
            return new LongBufferWrapper((LongBuffer) buffer, nx, ny);
        } else {
            throw new IllegalArgumentException(
                    "Cannot make a buffer wrapper for "
                            + buffer.getClass().getName());
        }
    }

    /**
     * Wrap an existing primitve array in BufferWrapper for convenient 2D
     * indexing.
     * 
     * @param array
     *            a primitve array, must be of type must be one of float[],
     *            double[], byte[], short[], int[], or long[].
     * @param nx
     *            the width of the data.
     * @param ny
     *            the height of the data.
     * @return a new BufferWrapper of the correct type.
     */
    public static BufferWrapper wrapArray(Object array, int nx, int ny) {
        if (array instanceof float[]) {
            return new FloatBufferWrapper((float[]) array, nx, ny);
        } else if (array instanceof double[]) {
            return new DoubleBufferWrapper((double[]) array, nx, ny);
        } else if (array instanceof byte[]) {
            return new ByteBufferWrapper((byte[]) array, nx, ny);
        } else if (array instanceof short[]) {
            return new ShortBufferWrapper((short[]) array, nx, ny);
        } else if (array instanceof int[]) {
            return new IntBufferWrapper((int[]) array, nx, ny);
        } else if (array instanceof long[]) {
            return new LongBufferWrapper((long[]) array, nx, ny);
        } else {
            throw new IllegalArgumentException(
                    "Cannot make a buffer wrapper for "
                            + array.getClass().getName());
        }
    }
}