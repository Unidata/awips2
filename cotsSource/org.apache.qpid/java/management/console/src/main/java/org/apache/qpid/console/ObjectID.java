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
package org.apache.qpid.console;

import org.apache.qpid.transport.codec.*;

public class ObjectID
{
    protected long first;
    protected long second;

    public ObjectID()
    {
    }

    public ObjectID(Decoder dec)
    {
        first = dec.readUint64();
        second = dec.readUint64();
    }

    public ObjectID(long first, long second)
    {
        this.first = first;
        this.second = second;
    }

    public long agentBank()
    {
        return (this.first & 0x000000000FFFFFFF);
    }

    public long brokerBank()
    {
        return (this.first & 0x0000FFFFF0000000L) >> 28;
    }

    public void encode(Encoder enc)
    {
        enc.writeUint64(first);
        enc.writeUint64(second);
    }

    public long flags()
    {
        return (this.first & 0xF000000000000000L) >> 60;
    }

    public boolean isDurable()
    {
        return sequence() == 0;
    }

    public long objectNum()
    {
        return second;
    }

    public String routingCode()
    {
        return Agent.routingCode(agentBank(), brokerBank());
    }

    public long sequence()
    {
        return (this.first & 0x0FFF000000000000L) >> 48;
    }

    @Override
    public String toString()
    {
        return "" + flags() + "-" + sequence() + "-" + brokerBank() + "-"
                + agentBank() + "-" + objectNum();
    }
}