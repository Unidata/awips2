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
package org.apache.qpid.server.queue;

import org.apache.qpid.AMQException;
import org.apache.qpid.server.subscription.Subscription;
import org.apache.qpid.server.message.AMQMessageHeader;
import org.apache.qpid.server.message.AMQMessage;

public class MockQueueEntry implements QueueEntry
{

    private AMQMessage _message;

    public boolean acquire()
    {
        return false;
    }

    public boolean acquire(Subscription sub)
    {
        return false;
    }

    public boolean acquiredBySubscription()
    {
        return false;
    }

    public boolean isAcquiredBy(Subscription subscription)
    {
        return false;
    }

    public void addStateChangeListener(StateChangeListener listener)
    {

    }

    public boolean delete()
    {
        return false;
    }

    public void dequeue()
    {

    }

    public void discard()
    {

    }

    public void routeToAlternate()
    {

    }

    public void dispose()
    {

    }

    public boolean expired() throws AMQException
    {
        return false;
    }

    public Subscription getDeliveredSubscription()
    {
        return null;
    }

    public boolean getDeliveredToConsumer()
    {
        return false;
    }

    public AMQMessage getMessage()
    {
        return _message;
    }

    public AMQQueue getQueue()
    {
        return null;
    }

    public long getSize()
    {
        return 0;
    }

    public boolean immediateAndNotDelivered()
    {
        return false;
    }

    public boolean isAcquired()
    {
        return false;
    }

    public boolean isDeleted()
    {
        return false;
    }


    public boolean isQueueDeleted()
    {

        return false;
    }


    public boolean isRejectedBy(Subscription subscription)
    {

        return false;
    }


    public void reject()
    {


    }


    public void reject(Subscription subscription)
    {


    }


    public void release()
    {


    }

    public boolean releaseButRetain()
    {
        return false;
    }


    public boolean removeStateChangeListener(StateChangeListener listener)
    {

        return false;
    }


    public void requeue()
    {


    }

    public void requeue(Subscription subscription)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }


    public void setDeliveredToSubscription()
    {


    }


    public void setRedelivered()
    {


    }

    public AMQMessageHeader getMessageHeader()
    {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isPersistent()
    {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public boolean isRedelivered()
    {
        return false;
    }


    public int compareTo(QueueEntry o)
    {

        return 0;
    }

    public void setMessage(AMQMessage msg)
    {
        _message = msg;
    }

}
