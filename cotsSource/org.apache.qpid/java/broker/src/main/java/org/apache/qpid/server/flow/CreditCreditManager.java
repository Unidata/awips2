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

public class CreditCreditManager extends AbstractFlowCreditManager implements FlowCreditManager_0_10
{
        private volatile long _bytesCredit;
        private volatile long _messageCredit;


    public CreditCreditManager()
     {
         this(0L, 0L);
     }

    public CreditCreditManager(long bytesCredit, long messageCredit)
    {
        _bytesCredit = bytesCredit;
        _messageCredit = messageCredit;
        setSuspended(!hasCredit());

    }


    public synchronized void setCreditLimits(final long bytesCredit, final long messageCredit)
    {
        _bytesCredit = bytesCredit;
        _messageCredit = messageCredit;

        setSuspended(!hasCredit());

    }


    public long getMessageCredit()
    {
         return _messageCredit == -1L
                    ? Long.MAX_VALUE
                    : _messageCredit;
    }

    public long getBytesCredit()
    {
        return _bytesCredit == -1L
                    ? Long.MAX_VALUE
                    : _bytesCredit;
    }

    public synchronized void restoreCredit(final long messageCredit, final long bytesCredit)
    {
        /*_bytesCredit = 0l;
        _messageCredit = 0l;
        setSuspended(true);*/
    }

    
    public synchronized void addCredit(final long messageCredit, final long bytesCredit)
    {
        boolean notifyIncrease = true;
        if(_messageCredit >= 0L && messageCredit > 0L)
        {
            notifyIncrease = _messageCredit != 0L;
            _messageCredit += messageCredit;
        }



        if(_bytesCredit >= 0L && bytesCredit > 0L)
        {
            notifyIncrease = notifyIncrease && bytesCredit>0;
            _bytesCredit += bytesCredit;



            if(notifyIncrease)
            {
                notifyIncreaseBytesCredit();
            }
        }



        setSuspended(!hasCredit());

    }

    public void clearCredit()
    {
        _bytesCredit = 0l;
        _messageCredit = 0l;
        setSuspended(true);
    }


    public synchronized boolean hasCredit()
    {
        // Note !=, if credit is < 0 that indicates infinite credit
        return (_bytesCredit != 0L  && _messageCredit != 0L);
    }

    public synchronized boolean useCreditForMessage(final ServerMessage msg)
    {
        if(_messageCredit >= 0L)
        {
            if(_messageCredit > 0)
            {
                if(_bytesCredit < 0L)
                {
                    _messageCredit--;

                    return true;
                }
                else if(msg.getSize() <= _bytesCredit)
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
            else
            {
                setSuspended(true);
                return false;
            }
        }
        else if(_bytesCredit >= 0L)
        {
            if(msg.getSize() <= _bytesCredit)
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
        else
        {
            return true;
        }

    }

    public synchronized void stop()
    {
        if(_bytesCredit > 0)
        {
            _bytesCredit = 0;
        }
        if(_messageCredit > 0)
        {
            _messageCredit = 0;
        }

    }


}
