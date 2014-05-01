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
package org.apache.qpid.server.flow;

import org.apache.qpid.server.message.ServerMessage;

public class WindowCreditManager extends AbstractFlowCreditManager implements FlowCreditManager_0_10
{
    private volatile long _bytesCreditLimit;
    private volatile long _messageCreditLimit;

    private volatile long _bytesUsed;
    private volatile long _messageUsed;

     public WindowCreditManager()
     {
         this(0L, 0L);
     }

    public WindowCreditManager(long bytesCreditLimit, long messageCreditLimit)
    {
        _bytesCreditLimit = bytesCreditLimit;
        _messageCreditLimit = messageCreditLimit;
        setSuspended(!hasCredit());

    }


    public synchronized void setCreditLimits(final long bytesCreditLimit, final long messageCreditLimit)
    {
        _bytesCreditLimit = bytesCreditLimit;
        _messageCreditLimit = messageCreditLimit;

        setSuspended(!hasCredit());

    }


    public long getMessageCredit()
    {
         return _messageCreditLimit == -1L
                    ? Long.MAX_VALUE
                    : _messageUsed < _messageCreditLimit ? _messageCreditLimit - _messageUsed : 0L;
    }

    public long getBytesCredit()
    {
        return _bytesCreditLimit == -1L
                    ? Long.MAX_VALUE
                    : _bytesUsed < _bytesCreditLimit ? _bytesCreditLimit - _bytesUsed : 0L;
    }

    public synchronized void restoreCredit(final long messageCredit, final long bytesCredit)
    {
        boolean notifyIncrease = true;
        if(_messageCreditLimit > 0L)
        {
            notifyIncrease = (_messageUsed != _messageCreditLimit);
            _messageUsed -= messageCredit;

            //TODO log warning
            if(_messageUsed < 0L)
            {
                _messageUsed = 0;
            }
        }



        if(_bytesCreditLimit > 0L)
        {
            notifyIncrease = notifyIncrease && bytesCredit>0;
            _bytesUsed -= bytesCredit;

            //TODO log warning
            if(_bytesUsed < 0L)
            {
                _bytesUsed = 0;
            }

            if(notifyIncrease)
            {
                notifyIncreaseBytesCredit();
            }
        }



        setSuspended(!hasCredit());

    }



    public synchronized boolean hasCredit()
    {
        return (_bytesCreditLimit < 0L || _bytesCreditLimit > _bytesUsed)
                && (_messageCreditLimit < 0L || _messageCreditLimit > _messageUsed);
    }

    public synchronized boolean useCreditForMessage(final ServerMessage msg)
    {
        if(_messageCreditLimit >= 0L)
        {
            if(_messageUsed < _messageCreditLimit)
            {
                if(_bytesCreditLimit < 0L)
                {
                    _messageUsed++;

                    return true;
                }
                else if(_bytesUsed + msg.getSize() <= _bytesCreditLimit)
                {
                    _messageUsed++;
                    _bytesUsed += msg.getSize();

                    return true;
                }
                else
                {
                    //setSuspended(true);
                    return false;
                }
            }
            else
            {
                setSuspended(true);
                return false;
            }
        }
        else if(_bytesCreditLimit >= 0L)
        {
            if(_bytesUsed + msg.getSize() <= _bytesCreditLimit)
            {
                 _bytesUsed += msg.getSize();

                return true;
            }
            else
            {
                //setSuspended(true);
                return false;
            }

        }
        else
        {
            return true;
        }

    }

    public void stop()
    {
        if(_bytesCreditLimit > 0)
        {
            _bytesCreditLimit = 0;
        }
        if(_messageCreditLimit > 0)
        {
            _messageCreditLimit = 0;
        }

    }

    public synchronized void addCredit(long count, long bytes)
    {
        if(bytes > 0)
        {
            _bytesCreditLimit += bytes;
        }
        else if(bytes == -1)
        {
            _bytesCreditLimit = -1;
        }


        if(count > 0)
        {
            _messageCreditLimit += count;
        }
        else if(count == -1)
        {
            _messageCreditLimit = -1;
        }
    }

    public void clearCredit()
    {
        _bytesCreditLimit = 0l;
        _messageCreditLimit = 0l;
        setSuspended(true);
    }
}
