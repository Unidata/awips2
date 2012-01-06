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
import org.apache.mina.common.IoSession;
import org.apache.mina.filter.codec.ProtocolEncoderOutput;
import org.apache.mina.filter.codec.demux.MessageEncoder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Set;

public final class AMQDataBlockEncoder implements MessageEncoder
{
    private static final Logger _logger = LoggerFactory.getLogger(AMQDataBlockEncoder.class);

    private final Set _messageTypes = Collections.singleton(EncodableAMQDataBlock.class);

    public AMQDataBlockEncoder()
    { }

    public void encode(IoSession session, Object message, ProtocolEncoderOutput out) throws Exception
    {
        final AMQDataBlock frame = (AMQDataBlock) message;

        final ByteBuffer buffer = frame.toByteBuffer();

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Encoded frame byte-buffer is '" + EncodingUtils.convertToHexString(buffer) + "'");
        }
        
        out.write(buffer);
    }

    public Set getMessageTypes()
    {
        return _messageTypes;
    }
}
