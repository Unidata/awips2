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
package org.apache.qpid.server.subscription;

import org.apache.qpid.server.transport.ServerSession;
import org.apache.qpid.server.queue.QueueEntry;
import org.apache.log4j.Logger;

class ImplicitAcceptDispositionChangeListener implements ServerSession.MessageDispositionChangeListener
{
    private static final Logger _logger = Logger.getLogger(ImplicitAcceptDispositionChangeListener.class);


    private final QueueEntry _entry;
    private Subscription_0_10 _sub;

    public ImplicitAcceptDispositionChangeListener(QueueEntry entry, Subscription_0_10 subscription_0_10)
    {
        _entry = entry;
        _sub = subscription_0_10;
    }

    public void onAccept()
    {
        _logger.warn("MessageAccept received for message which is using NONE as the accept mode (likely client error)");
    }

    public void onRelease()
    {
        if(_entry.isAcquiredBy(_sub))
        {
            getSubscription().release(_entry);
        }
        else
        {
            _logger.warn("MessageRelease received for message which has not been acquired (likely client error)");
        }
    }

    public void onReject()
    {
        if(_entry.isAcquiredBy(_sub))
        {
            getSubscription().reject(_entry);
        }
        else
        {
            _logger.warn("MessageReject received for message which has not been acquired (likely client error)");
        }

    }

    public boolean acquire()
    {
        boolean acquired = _entry.acquire(getSubscription());
        //TODO - why acknowledge here??? seems bizarre...
      //  getSubscription().getSession().acknowledge(getSubscription(), _entry);
        return acquired;

    }

    public Subscription_0_10 getSubscription()
    {
        return _sub;
    }


}
