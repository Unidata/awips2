package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;

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
public abstract class DeferredDataBlock extends AMQDataBlock
{
    private AMQDataBlock _underlyingDataBlock;


    public long getSize()
    {
        if(_underlyingDataBlock == null)
        {
            _underlyingDataBlock = createAMQDataBlock();
        }
        return _underlyingDataBlock.getSize();
    }

    public void writePayload(ByteBuffer buffer)
    {
        if(_underlyingDataBlock == null)
        {
            _underlyingDataBlock = createAMQDataBlock();
        }
        _underlyingDataBlock.writePayload(buffer);
    }

    abstract protected AMQDataBlock createAMQDataBlock();

}
