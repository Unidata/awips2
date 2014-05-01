package org.apache.qpid.server.flow;

import org.apache.qpid.server.message.ServerMessage;

import java.util.concurrent.atomic.AtomicLong;

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
public class BytesOnlyCreditManager extends AbstractFlowCreditManager
{
    private final AtomicLong _bytesCredit;

    public BytesOnlyCreditManager(long initialCredit)
    {
        _bytesCredit = new AtomicLong(initialCredit);
    }

    public long getMessageCredit()
    {
        return -1L;
    }

    public long getBytesCredit()
    {
        return _bytesCredit.get();
    }

    public void restoreCredit(long messageCredit, long bytesCredit)
    {
        _bytesCredit.addAndGet(bytesCredit);
        setSuspended(false);
    }

    public void removeAllCredit()
    {
        _bytesCredit.set(0L);
    }

    public boolean hasCredit()
    {
        return _bytesCredit.get() > 0L;
    }

    public boolean useCreditForMessage(ServerMessage msg)
    {
        final long msgSize = msg.getSize();
        if(hasCredit())
        {
            if(_bytesCredit.addAndGet(-msgSize) >= 0)
            {
                return true;
            }
            else
            {
                _bytesCredit.addAndGet(msgSize);
                setSuspended(true);
                return false;
            }
        }
        else
        {
            return false;
        }

    }

    public void setBytesCredit(long bytesCredit)
    {
        _bytesCredit.set( bytesCredit );
    }
}
