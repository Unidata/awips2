package org.apache.qpid.server.queue;
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


import org.apache.qpid.pool.ReadWriteRunnable;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.AMQException;
import org.apache.log4j.Logger;


class SubFlushRunner implements ReadWriteRunnable
{
    private static final Logger _logger = Logger.getLogger(SimpleAMQQueue.class);


    private final Subscription _sub;
    private final String _name;
    private static final long ITERATIONS = SimpleAMQQueue.MAX_ASYNC_DELIVERIES;

    public SubFlushRunner(Subscription sub)
    {
        _sub = sub;
        _name = "SubFlushRunner-"+_sub;
    }

    public void run()
    {

       
        Thread.currentThread().setName(_name);

        boolean complete = false;
        try
        {
            CurrentActor.set(_sub.getLogActor());
            complete = getQueue().flushSubscription(_sub, ITERATIONS);

        }
        catch (AMQException e)
        {
            _logger.error(e);
        }
        finally
        {
            CurrentActor.remove();
        }
        if (!complete && !_sub.isSuspended())
        {
            getQueue().execute(this);
        }


    }

    private SimpleAMQQueue getQueue()
    {
        return (SimpleAMQQueue) _sub.getQueue();
    }

    public boolean isRead()
    {
        return false;
    }

    public boolean isWrite()
    {
        return true;
    }
}
