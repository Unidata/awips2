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
package com.raytheon.uf.viz.remote.graphics.events.colormap;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapDataType;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.colormap.ColorMapDataEvent.ColorMapDataEventAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = ColorMapDataEventAdapter.class)
public class ColorMapDataEvent extends AbstractDispatchingObjectEvent {

    public static class ColorMapDataEventAdapter implements
            ISerializationTypeAdapter<ColorMapDataEvent> {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
         * (com.raytheon.uf.common.serialization.ISerializationContext,
         * java.lang.Object)
         */
        @Override
        public void serialize(ISerializationContext serializer,
                ColorMapDataEvent object) throws SerializationException {
            serializer.writeI32(object.getDisplayId());
            serializer.writeI32(object.getObjectId());
            ColorMapData colorMapData = object.getColorMapData();
            serializer.writeString(colorMapData.getDataType().name());
            int[] dimensions = colorMapData.getDimensions();
            serializer.writeI32(dimensions[0]);
            serializer.writeI32(dimensions[1]);
            Buffer buffer = colorMapData.getBuffer();
            serializer.writeBool(buffer.isDirect());
            buffer.position(0);
            ByteBuffer bb = null;
            byte[] bytes = null;
            switch (colorMapData.getDataType()) {
            case BYTE:
            case SIGNED_BYTE:
                bytes = new byte[buffer.capacity()];
                bb = ByteBuffer.wrap(bytes);
                bb.put((ByteBuffer) buffer);
                break;
            case SHORT:
            case UNSIGNED_SHORT:
                bytes = new byte[2 * buffer.capacity()];
                bb = ByteBuffer.wrap(bytes);
                bb.asShortBuffer().put((ShortBuffer) buffer);
                break;
            case FLOAT:
                bytes = new byte[4 * buffer.capacity()];
                bb = ByteBuffer.wrap(bytes);
                bb.asFloatBuffer().put((FloatBuffer) buffer);
                break;
            case INT:
                bytes = new byte[4 * buffer.capacity()];
                bb = ByteBuffer.wrap(bytes);
                bb.asIntBuffer().put((IntBuffer) buffer);
                break;
            }
            serializer.writeBinary(bb.array());
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.common.serialization.ISerializationTypeAdapter#
         * deserialize
         * (com.raytheon.uf.common.serialization.IDeserializationContext)
         */
        @Override
        public ColorMapDataEvent deserialize(
                IDeserializationContext deserializer)
                throws SerializationException {
            ColorMapDataEvent event = new ColorMapDataEvent();
            event.setDisplayId(deserializer.readI32());
            event.setObjectId(deserializer.readI32());
            ColorMapDataType dataType = ColorMapDataType.valueOf(deserializer
                    .readString());
            int[] dimensions = new int[] { deserializer.readI32(),
                    deserializer.readI32() };
            boolean direct = deserializer.readBool();
            byte[] bytes = deserializer.readBinary();
            ByteBuffer buffer = direct ? ByteBuffer
                    .allocateDirect(bytes.length) : ByteBuffer
                    .allocate(bytes.length);
            buffer.put(bytes);
            buffer.rewind();
            Buffer dataBuffer = null;
            switch (dataType) {
            case BYTE:
            case SIGNED_BYTE:
                dataBuffer = buffer;
                break;
            case SHORT:
            case UNSIGNED_SHORT:
                dataBuffer = buffer.asShortBuffer();
                break;
            case INT:
                dataBuffer = buffer.asIntBuffer();
                break;
            case FLOAT:
                dataBuffer = buffer.asFloatBuffer();
                break;
            }
            event.setColorMapData(new ColorMapData(dataBuffer, dimensions,
                    dataType));
            return event;
        }
    }

    private ColorMapData colorMapData;

    /**
     * @return the colorMapData
     */
    public ColorMapData getColorMapData() {
        return colorMapData;
    }

    /**
     * @param colorMapData
     *            the colorMapData to set
     */
    public void setColorMapData(ColorMapData colorMapData) {
        this.colorMapData = colorMapData;
    }

}
