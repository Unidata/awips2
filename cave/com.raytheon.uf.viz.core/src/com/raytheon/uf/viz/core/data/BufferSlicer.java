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
package com.raytheon.uf.viz.core.data;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

/**
 * Slices a subset of a Buffer object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class BufferSlicer {

    /**
     * Slices a Buffer represented by a 2D bounds totalBounds into a Buffer
     * represented by bounds dataBounds.
     * 
     * @param data
     * @param dataBounds
     * @param totalWidth
     * @param totalHeight
     * @return
     */
    public static Buffer slice(Buffer data, Rectangle dataBounds,
            Rectangle totalBounds) {
        if (dataBounds.getMinX() < totalBounds.getMinX()
                || dataBounds.getMinY() < totalBounds.getMinY()
                || dataBounds.getMaxX() > totalBounds.getMaxX()
                || dataBounds.getMaxY() > totalBounds.getMaxY()) {
            throw new RuntimeException(
                    "Data bounds defines region outside of buffer's total bounds");
        }

        data = duplicate(data);
        int dataSize = dataBounds.width * dataBounds.height;
        if (dataSize == totalBounds.width * totalBounds.height) {
            return data;
        }
        if (data instanceof ByteBuffer) {
            return slice((ByteBuffer) data, dataBounds, totalBounds, dataSize);
        } else if (data instanceof ShortBuffer) {
            return slice((ShortBuffer) data, dataBounds, totalBounds, dataSize);
        } else if (data instanceof IntBuffer) {
            return slice((IntBuffer) data, dataBounds, totalBounds, dataSize);
        } else if (data instanceof FloatBuffer) {
            return slice((FloatBuffer) data, dataBounds, totalBounds, dataSize);
        } else {
            throw new RuntimeException(
                    "Unhandled buffer passed in: " + data != null ? data
                            .getClass().getSimpleName() : null);
        }
    }

    private static Buffer duplicate(Buffer data) {
        if (data instanceof ByteBuffer) {
            return ((ByteBuffer) data).duplicate();
        } else if (data instanceof ShortBuffer) {
            return ((ShortBuffer) data).duplicate();
        } else if (data instanceof IntBuffer) {
            return ((IntBuffer) data).duplicate();
        } else if (data instanceof FloatBuffer) {
            return ((FloatBuffer) data).duplicate();
        } else {
            throw new RuntimeException(
                    "Unhandled buffer passed in: " + data != null ? data
                            .getClass().getSimpleName() : null);
        }
    }

    private static ByteBuffer slice(ByteBuffer data, Rectangle dataBounds,
            Rectangle totalBounds, int dataSize) {
        ByteBuffer newData = null;
        // Get our new Buffer object
        if (data.isDirect()) {
            newData = ByteBuffer.allocateDirect(dataSize).order(data.order());
        } else {
            newData = ByteBuffer.allocate(dataSize);
        }

        newData.position(0);
        byte[] bytes = new byte[dataBounds.width];
        for (int i = 0; i < dataBounds.height; ++i) {
            data.position((dataBounds.y * totalBounds.width + dataBounds.x) + i
                    * totalBounds.width);
            data.get(bytes);
            newData.put(bytes);
        }
        newData.rewind();
        return newData;
    }

    private static ShortBuffer slice(ShortBuffer data, Rectangle dataBounds,
            Rectangle totalBounds, int dataSize) {
        ShortBuffer newData = null;
        // Get our new Buffer object
        if (data.isDirect()) {
            newData = ByteBuffer.allocateDirect(dataSize * 2)
                    .order(data.order()).asShortBuffer();
        } else {
            newData = ShortBuffer.allocate(dataSize);
        }

        newData.position(0);
        short[] shorts = new short[dataBounds.width];
        for (int i = 0; i < dataBounds.height; ++i) {
            data.position((dataBounds.y * totalBounds.width + dataBounds.x) + i
                    * totalBounds.width);
            data.get(shorts);
            newData.put(shorts);
        }
        newData.rewind();
        return newData;
    }

    private static IntBuffer slice(IntBuffer data, Rectangle dataBounds,
            Rectangle totalBounds, int dataSize) {
        IntBuffer newData = null;
        // Get our new Buffer object
        if (data.isDirect()) {
            newData = ByteBuffer.allocateDirect(dataSize * 4)
                    .order(data.order()).asIntBuffer();
        } else {
            newData = IntBuffer.allocate(dataSize);
        }

        newData.position(0);
        int[] ints = new int[dataBounds.width];
        for (int i = 0; i < dataBounds.height; ++i) {
            data.position((dataBounds.y * totalBounds.width + dataBounds.x) + i
                    * totalBounds.width);
            data.get(ints);
            newData.put(ints);
        }
        newData.rewind();
        return newData;
    }

    private static FloatBuffer slice(FloatBuffer data, Rectangle dataBounds,
            Rectangle totalBounds, int dataSize) {
        FloatBuffer newData = null;
        // Get our new Buffer object
        if (data.isDirect()) {
            newData = ByteBuffer.allocateDirect(dataSize * 4)
                    .order(data.order()).asFloatBuffer();
        } else {
            newData = FloatBuffer.allocate(dataSize);
        }

        newData.position(0);
        float[] floats = new float[dataBounds.width];
        for (int i = 0; i < dataBounds.height; ++i) {
            data.position((dataBounds.y * totalBounds.width + dataBounds.x) + i
                    * totalBounds.width);
            data.get(floats);
            newData.put(floats);
        }
        newData.rewind();
        return newData;
    }
}
