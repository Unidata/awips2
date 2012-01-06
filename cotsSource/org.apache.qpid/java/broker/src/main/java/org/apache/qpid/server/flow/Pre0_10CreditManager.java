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

public class Pre0_10CreditManager extends AbstractFlowCreditManager implements FlowCreditManager
{

    private volatile long _bytesCreditLimit;
    private volatile long _messageCreditLimit;

    private volatile long _bytesCredit;
    private volatile long _messageCredit;

    public Pre0_10CreditManager(long bytesCreditLimit, long messageCreditLimit)
    {
        _bytesCreditLimit = bytesCreditLimit;
        _messageCreditLimit = messageCreditLimit;
        _bytesCredit = bytesCreditLimit;
        _messageCredit = messageCreditLimit;
    }


    public synchronized void setCreditLimits(final long bytesCreditLimit, final long messageCreditLimit)
    {
        long bytesCreditChange = bytesCreditLimit - _bytesCreditLimit;
        long messageCreditChange = messageCreditLimit - _messageCreditLimit;



        if(bytesCreditChange != 0L)
        {
            if(bytesCreditLimit == 0L)
            {
                _bytesCredit = 0;
            }
            else
            {
                _bytesCredit += bytesCreditChange;
            }
        }


        if(messageCreditChange != 0L)
        {
            if(messageCreditLimit == 0L)
            {
                _messageCredit = 0;
            }
            else
            {
                _messageCredit += messageCreditChange;
            }
        }


        _bytesCreditLimit = bytesCreditLimit;
        _messageCreditLimit = messageCreditLimit;

        setSuspended(!hasCredit());

    }


    public long getMessageCredit()
    {
        return _messageCredit;
    }

    public long getBytesCredit()
    {
        return _bytesCredit;
    }

    public synchronized void restoreCredit(final long messageCredit, final long bytesCredit)
    {
        final long messageCreditLimit = _messageCreditLimit;
        boolean notifyIncrease = true;
        if(messageCreditLimit != 0L)
        {
            notifyIncrease = (_messageCredit != 0);
            long newCredit = _messageCredit + messageCredit;
            _messageCredit = newCredit > messageCreditLimit ? messageCreditLimit : newCredit;
        }


        final long bytesCreditLimit = _bytesCreditLimit;
        if(bytesCreditLimit != 0L)
        {
            long newCredit = _bytesCredit + bytesCredit;
            _bytesCredit = newCredit > bytesCreditLimit ? bytesCreditLimit : newCredit;
            if(notifyIncrease && bytesCredit>0)
            {
                notifyIncreaseBytesCredit();
            }
        }



        setSuspended(!hasCredit());

    }

    public synchronized void removeAllCredit()
    {
        _bytesCredit = 0L;
        _messageCredit = 0L;
        setSuspended(!hasCredit());
    }

    public synchronized boolean hasCredit()
    {
        return (_bytesCreditLimit == 0L || _bytesCredit > 0)
                && (_messageCreditLimit == 0L || _messageCredit > 0);
    }

    public synchronized boolean useCreditForMessage(final ServerMessage msg)
    {
        if(_messageCreditLimit != 0L)
        {
            if(_messageCredit != 0L)
            {
                if(_bytesCreditLimit == 0L)
                {
                    _messageCredit--;

                    return true;
                }
                else
                {
                    if((_bytesCredit >= msg.getSize()) || (_bytesCredit == _bytesCreditLimit))
                    {
                        _messageCredit--;
                        _bytesCredit -= msg.getSize();

                        return true;
                    }
                    else
                    {
                        //setSuspended(true);
                        return false;
                    }
                }
            }
            else
            {
                setSuspended(true);
                return false;
            }
        }
        else
        {
            if(_bytesCreditLimit == 0L)
            {

                return true;
            }
            else
            {
                if((_bytesCredit >= msg.getSize()) || (_bytesCredit == _bytesCreditLimit))
                {
                    _bytesCredit -= msg.getSize();

                    return true;
                }
                else
                {
                    //setSuspended(true);
                    return false;
                }
            }

        }

    }
}
