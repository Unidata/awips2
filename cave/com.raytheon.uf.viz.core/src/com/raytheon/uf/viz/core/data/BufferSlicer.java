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
import java.nio.ByteOrder;
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
     * Given a Buffer, a Rectangle for the Buffer, and a totalBounds
     * 
     * @param data
     * @param dataBounds
     * @param totalWidth
     * @param totalHeight
     * @return
     */
    public static Buffer slice(Buffer data, Rectangle dataBounds,
            Rectangle totalBounds) {
        int dataSize = dataBounds.width * dataBounds.height;
        if (dataSize == totalBounds.width * totalBounds.height) {
            return data;
        }
        Buffer newData = null;
        // Get our new Buffer object
        if (data instanceof ByteBuffer) {
            if (data.isDirect()) {
                newData = ByteBuffer.allocateDirect(dataSize).order(
                        ByteOrder.nativeOrder());
            } else {
                newData = ByteBuffer.allocate(dataSize);
            }
        } else if (data instanceof ShortBuffer) {
            if (data.isDirect()) {
                newData = ByteBuffer.allocateDirect(dataSize * 2)
                        .order(ByteOrder.nativeOrder()).asShortBuffer();
            } else {
                newData = ShortBuffer.allocate(dataSize);
            }
        } else if (data instanceof IntBuffer) {
            if (data.isDirect()) {
                newData = ByteBuffer.allocateDirect(dataSize * 4)
                        .order(ByteOrder.nativeOrder()).asIntBuffer();
            } else {
                newData = IntBuffer.allocate(dataSize);
            }
        } else if (data instanceof FloatBuffer) {
            if (data.isDirect()) {
                newData = ByteBuffer.allocateDirect(dataSize * 4)
                        .order(ByteOrder.nativeOrder()).asFloatBuffer();
            } else {
                newData = FloatBuffer.allocate(dataSize);
            }
        } else {
            // Unsupported type
            return data;
        }

        // Synchronize on the data buffer if multi threads slicing same buffer
        // at the same time
        synchronized (data) {
            newData.position(0);
            for (int i = 0; i < dataBounds.height; ++i) {
                data.position((dataBounds.y * totalBounds.width + dataBounds.x)
                        + i * totalBounds.width);
                if (data instanceof ByteBuffer) {
                    byte[] bytes = new byte[dataBounds.width];
                    ((ByteBuffer) data).get(bytes);
                    ((ByteBuffer) newData).put(bytes);
                } else if (data instanceof ShortBuffer) {
                    short[] shorts = new short[dataBounds.width];
                    ((ShortBuffer) data).get(shorts);
                    ((ShortBuffer) newData).put(shorts);
                } else if (data instanceof IntBuffer) {
                    int[] ints = new int[dataBounds.width];
                    ((IntBuffer) data).get(ints);
                    ((IntBuffer) newData).put(ints);
                } else {
                    // FloatBuffer by default
                    float[] floats = new float[dataBounds.width];
                    ((FloatBuffer) data).get(floats);
                    ((FloatBuffer) newData).put(floats);
                }
            }
        }
        return newData.rewind();
    }
}
