/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;

/**
 * A data block represents something that has a size in bytes and the ability to write itself to a byte
 * buffer (similar to a byte array).
 */
public abstract class AMQDataBlock implements EncodableAMQDataBlock
{
    /**
     * Get the size of buffer needed to store the byte representation of this
     * frame.
     * @return unsigned integer
     */
    public abstract long getSize();

    /**
     * Writes the datablock to the specified buffer.
     * @param buffer
     */
    public abstract void writePayload(ByteBuffer buffer);

    public ByteBuffer toByteBuffer()
    {
        final ByteBuffer buffer = ByteBuffer.allocate((int)getSize());

        writePayload(buffer);    
        buffer.flip();
        return buffer;
    }

    public java.nio.ByteBuffer toNioByteBuffer()
    {
        final java.nio.ByteBuffer buffer = java.nio.ByteBuffer.allocate((int) getSize());

        ByteBuffer buf = ByteBuffer.wrap(buffer);
        writePayload(buf);    
        buffer.flip();
        return buffer;
    }

}
