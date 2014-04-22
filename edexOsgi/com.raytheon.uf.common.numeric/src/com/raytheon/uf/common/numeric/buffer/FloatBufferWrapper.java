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

import java.awt.Rectangle;
import java.nio.FloatBuffer;

/**
 * 
 * FloatBuffer data wrapper
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2014  2791     bsteffen     Reimplemnt extending BufferWrapper.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class FloatBufferWrapper extends BufferWrapper {

    protected final FloatBuffer buffer;

    public FloatBufferWrapper(FloatBuffer buffer, int nx, int ny) {
        super(nx, ny);
        this.buffer = buffer;
    }

    public FloatBufferWrapper(float[] array, int nx, int ny) {
        this(FloatBuffer.wrap(array), nx, ny);
    }

    public FloatBufferWrapper(int nx, int ny) {
        this(FloatBuffer.allocate(nx * ny), nx, ny);
    }

    public FloatBufferWrapper(FloatBuffer buffer, Rectangle dimensions) {
        this(buffer, dimensions.width, dimensions.height);
    }

    public FloatBufferWrapper(float[] array, Rectangle dimensions) {
        this(array, dimensions.width, dimensions.height);
    }

    public FloatBufferWrapper(Rectangle dimensions) {
        this(dimensions.width, dimensions.height);
    }

    @Override
    protected double getDataValueInternal(int index) {
        return buffer.get(index);
    }

    @Override
    public void setDataValueInternal(double dataValue, int index) {
        buffer.put(index, (float) dataValue);
    }

    @Override
    public FloatBuffer getBuffer() {
        return buffer;
    }

    @Override
    public float[] getArray() {
        if (buffer.hasArray()) {
            return buffer.array();
        }
        return null;
    }

    @Override
    public Class<Float> getPrimitiveType() {
        return float.class;
    }
}