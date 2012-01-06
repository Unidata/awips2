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

import java.nio.ByteBuffer;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Dynamic serialize adapter for ByteBuffers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ByteBufferAdapter implements ISerializationTypeAdapter<ByteBuffer> {

    @Override
    public ByteBuffer deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        ByteBuffer buf = null;

        byte[] b = deserializer.readBinary();
        buf = ByteBuffer.wrap(b);

        return buf;
    }

    @Override
    public void serialize(ISerializationContext serializer, ByteBuffer buffer)
            throws SerializationException {
        byte[] b;
        if (buffer.hasArray()) {
            b = buffer.array();
        } else {
            b = new byte[buffer.capacity()];
            ((ByteBuffer) buffer.duplicate().rewind()).get(b);
        }
        serializer.writeBinary(b);
    }

}
