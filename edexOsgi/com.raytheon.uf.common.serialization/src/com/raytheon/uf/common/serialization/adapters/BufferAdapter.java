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
package com.raytheon.uf.common.serialization.adapters;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.BufferUtil;

/**
 * Serialization adapter that handles java.nio.Buffer objects. Buffers are not
 * thread safe and therefore should only be serialized if no other threads are
 * using them
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 03, 2012            mschenke    Initial creation
 * Jul 23, 2013 2215       njensen     Updated for thrift 0.9.0
 * Apr 07, 2014 2968       njensen     Fixed thread safety issues with serialize()
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class BufferAdapter implements ISerializationTypeAdapter<Buffer> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
     * (com.raytheon.uf.common.serialization.ISerializationContext,
     * java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, Buffer buffer)
            throws SerializationException {
        buffer = BufferUtil.asReadOnly(buffer);
        serializer.writeBool(buffer.isDirect());
        buffer.position(0);
        ByteBuffer bb = null;
        byte[] bytes = null;
        if (buffer instanceof ByteBuffer) {
            serializer.writeByte((byte) 0);
            bytes = new byte[buffer.capacity()];
            bb = ByteBuffer.wrap(bytes);
            bb.put((ByteBuffer) buffer);
        } else if (buffer instanceof ShortBuffer) {
            serializer.writeByte((byte) 1);
            bytes = new byte[2 * buffer.capacity()];
            bb = ByteBuffer.wrap(bytes);
            bb.asShortBuffer().put((ShortBuffer) buffer);
        } else if (buffer instanceof FloatBuffer) {
            serializer.writeByte((byte) 2);
            bytes = new byte[4 * buffer.capacity()];
            bb = ByteBuffer.wrap(bytes);
            bb.asFloatBuffer().put((FloatBuffer) buffer);
        } else if (buffer instanceof IntBuffer) {
            serializer.writeByte((byte) 3);
            bytes = new byte[4 * buffer.capacity()];
            bb = ByteBuffer.wrap(bytes);
            bb.asIntBuffer().put((IntBuffer) buffer);
        } else if (buffer instanceof DoubleBuffer) {
            serializer.writeByte((byte) 4);
            bytes = new byte[8 * buffer.capacity()];
            bb = ByteBuffer.wrap(bytes);
            bb.asDoubleBuffer().put((DoubleBuffer) buffer);
        } else if (buffer instanceof LongBuffer) {
            serializer.writeByte((byte) 5);
            bytes = new byte[8 * buffer.capacity()];
            bb = ByteBuffer.wrap(bytes);
            bb.asLongBuffer().put((LongBuffer) buffer);
        } else {
            throw new SerializationException("Could not handle buffer type: "
                    + buffer.getClass());
        }
        bb.rewind();
        serializer.writeBuffer(bb);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#deserialize
     * (com.raytheon.uf.common.serialization.IDeserializationContext)
     */
    @Override
    public Buffer deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        boolean direct = deserializer.readBool();
        byte type = deserializer.readByte();
        ByteBuffer buffer = deserializer.readBuffer();
        if (buffer.isDirect() != direct) {
            ByteBuffer copyBuffer = direct ? ByteBuffer.allocateDirect(buffer
                    .capacity()) : ByteBuffer.allocate(buffer.capacity());
            copyBuffer.put(buffer);
            buffer = copyBuffer;
        }
        buffer.rewind();
        Buffer dataBuffer = null;
        switch (type) {
        case 0:
            dataBuffer = buffer;
            break;
        case 1:
            dataBuffer = buffer.asShortBuffer();
            break;
        case 2:
            dataBuffer = buffer.asFloatBuffer();
            break;
        case 3:
            dataBuffer = buffer.asIntBuffer();
            break;
        case 4:
            dataBuffer = buffer.asDoubleBuffer();
            break;
        case 5:
            dataBuffer = buffer.asLongBuffer();
            break;
        default:
            throw new SerializationException("Unrecognized buffer type: "
                    + type);
        }
        return dataBuffer;
    }

}
