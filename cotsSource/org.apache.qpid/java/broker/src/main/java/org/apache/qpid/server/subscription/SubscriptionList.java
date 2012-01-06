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

import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.subscription.Subscription;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.nio.ByteBuffer;

public class SubscriptionList
{

    private final SubscriptionNode _head = new SubscriptionNode();

    private AtomicReference<SubscriptionNode> _tail = new AtomicReference<SubscriptionNode>(_head);
    private final AMQQueue _queue;
    private AtomicInteger _size = new AtomicInteger();


    public final class SubscriptionNode
    {
        private final AtomicBoolean _deleted = new AtomicBoolean();
        private final AtomicReference<SubscriptionNode> _next = new AtomicReference<SubscriptionNode>();
        private final Subscription _sub;


        public SubscriptionNode()
        {

            _sub = null;
            _deleted.set(true);
        }

        public SubscriptionNode(final Subscription sub)
        {
            _sub = sub;
        }


        public SubscriptionNode getNext()
        {

            SubscriptionNode next = nextNode();
            while(next != null && next.isDeleted())
            {

                final SubscriptionNode newNext = next.nextNode();
                if(newNext != null)
                {
                    _next.compareAndSet(next, newNext);
                    next = nextNode();
                }
                else
                {
                    next = null;
                }

            }
            return next;
        }

        private SubscriptionNode nextNode()
        {
            return _next.get();
        }

        public boolean isDeleted()
        {
            return _deleted.get();
        }


        public boolean delete()
        {
            if(_deleted.compareAndSet(false,true))
            {
                _size.decrementAndGet();
                advanceHead();
                return true;
            }
            else
            {
                return false;
            }
        }


        public Subscription getSubscription()
        {
            return _sub;
        }
    }


    public SubscriptionList(AMQQueue queue)
    {
        _queue = queue;
    }

    private void advanceHead()
    {
        SubscriptionNode head = _head.nextNode();
        while(head._next.get() != null && head.isDeleted())
        {

            final SubscriptionNode newhead = head.nextNode();
            if(newhead != null)
            {
                _head._next.compareAndSet(head, newhead);
            }
            head = _head.nextNode();
        }
    }


    public SubscriptionNode add(Subscription sub)
    {
        SubscriptionNode node = new SubscriptionNode(sub);
        for (;;)
        {
            SubscriptionNode tail = _tail.get();
            SubscriptionNode next = tail.nextNode();
            if (tail == _tail.get())
            {
                if (next == null)
                {
                    if (tail._next.compareAndSet(null, node))
                    {
                        _tail.compareAndSet(tail, node);
                        _size.incrementAndGet();
                        return node;
                    }
                }
                else
                {
                    _tail.compareAndSet(tail, next);
                }
            }
        }

    }

    public boolean remove(Subscription sub)
    {
        SubscriptionNode node = _head.getNext();
        while(node != null)
        {
            if(sub.equals(node._sub) && node.delete())
            {
                return true;
            }
            node = node.getNext();
        }
        return false;
    }


    public class SubscriptionNodeIterator
    {

        private SubscriptionNode _lastNode;

        SubscriptionNodeIterator(SubscriptionNode startNode)
        {
            _lastNode = startNode;
        }


        public boolean atTail()
        {
            return _lastNode.nextNode() == null;
        }

        public SubscriptionNode getNode()
        {

            return _lastNode;

        }

        public boolean advance()
        {

            if(!atTail())
            {
                SubscriptionNode nextNode = _lastNode.nextNode();
                while(nextNode.isDeleted() && nextNode.nextNode() != null)
                {
                    nextNode = nextNode.nextNode();
                }
                _lastNode = nextNode;
                return true;

            }
            else
            {
                return false;
            }

        }

    }


    public SubscriptionNodeIterator iterator()
    {
        return new SubscriptionNodeIterator(_head);
    }


    public SubscriptionNode getHead()
    {
        return _head;
    }

    public int size()
    {
        return _size.get();
    }



}



