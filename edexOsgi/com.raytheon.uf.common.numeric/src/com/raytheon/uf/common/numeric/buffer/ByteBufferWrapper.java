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
import java.nio.ByteBuffer;

/**
 * 
 * ByteBuffer data wrapper
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
public class ByteBufferWrapper extends BufferWrapper {

    protected final ByteBuffer buffer;

    public ByteBufferWrapper(ByteBuffer buffer, int nx, int ny) {
        super(nx, ny);
        this.buffer = buffer;
    }

    public ByteBufferWrapper(byte[] array, int nx, int ny) {
        this(ByteBuffer.wrap(array), nx, ny);
    }

    public ByteBufferWrapper(int nx, int ny) {
        this(ByteBuffer.allocate(nx * ny), nx, ny);
    }

    public ByteBufferWrapper(ByteBuffer buffer, Rectangle dimensions) {
        this(buffer, dimensions.width, dimensions.height);
    }

    public ByteBufferWrapper(byte[] array, Rectangle dimensions) {
        this(array, dimensions.width, dimensions.height);
    }

    public ByteBufferWrapper(Rectangle dimensions) {
        this(dimensions.width, dimensions.height);
    }

    @Override
    protected double getDataValueInternal(int index) {
        return buffer.get(index);
    }

    @Override
    public void setDataValueInternal(double dataValue, int index) {
        buffer.put(index, (byte) dataValue);
    }

    @Override
    public ByteBuffer getBuffer() {
        return buffer;
    }

    @Override
    public byte[] getArray() {
        if (buffer.hasArray()) {
            return buffer.array();
        }
        return null;
    }

    @Override
    public Class<Byte> getPrimitiveType() {
        return byte.class;
    }
}