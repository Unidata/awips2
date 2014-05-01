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

public class SmallCompositeAMQDataBlock extends AMQDataBlock implements EncodableAMQDataBlock
{
    private AMQDataBlock _firstFrame;

    private AMQDataBlock _block;

    public SmallCompositeAMQDataBlock(AMQDataBlock block)
    {
        _block = block;
    }

    /**
     * The encoded block will be logically first before the AMQDataBlocks which are encoded
     * into the buffer afterwards.
     * @param encodedBlock already-encoded data
     * @param block a block to be encoded.
     */
    public SmallCompositeAMQDataBlock(AMQDataBlock encodedBlock, AMQDataBlock block)
    {
        this(block);
        _firstFrame = encodedBlock;
    }

    public AMQDataBlock getBlock()
    {
        return _block;
    }

    public AMQDataBlock getFirstFrame()
    {
        return _firstFrame;
    }

    public long getSize()
    {
        long frameSize = _block.getSize();

        if (_firstFrame != null)
        {

            frameSize += _firstFrame.getSize();
        }
        return frameSize;
    }

    public void writePayload(ByteBuffer buffer)
    {
        if (_firstFrame != null)
        {
            _firstFrame.writePayload(buffer);
        }
        _block.writePayload(buffer);

    }

    public String toString()
    {
        if (_block == null)
        {
            return "No blocks contained in composite frame";
        }
        else
        {
            StringBuilder buf = new StringBuilder(this.getClass().getName());
            buf.append("{encodedBlock=").append(_firstFrame);

            buf.append(" _block=[").append(_block.toString()).append("]");

            buf.append("}");
            return buf.toString();
        }
    }
}
