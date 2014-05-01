package org.apache.qpid.server.queue;

import org.apache.qpid.server.message.InboundMessage;
import org.apache.qpid.server.message.ServerMessage;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
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
public class SimpleQueueEntryList implements QueueEntryList
{

    private final QueueEntryImpl _head;

    private volatile QueueEntryImpl _tail;

    static final AtomicReferenceFieldUpdater<SimpleQueueEntryList, QueueEntryImpl>
            _tailUpdater =
        AtomicReferenceFieldUpdater.newUpdater
        (SimpleQueueEntryList.class, QueueEntryImpl.class, "_tail");


    private final AMQQueue _queue;

    static final AtomicReferenceFieldUpdater<QueueEntryImpl, QueueEntryImpl>
                _nextUpdater =
            AtomicReferenceFieldUpdater.newUpdater
            (QueueEntryImpl.class, QueueEntryImpl.class, "_next");
    private AtomicLong _deletes = new AtomicLong(0L);


    public SimpleQueueEntryList(AMQQueue queue)
    {
        _queue = queue;
        _head = new QueueEntryImpl(this);
        _tail = _head;
    }

    

    void advanceHead()
    {
        _deletes.incrementAndGet();
        QueueEntryImpl head = _head.nextNode();
        boolean deleted = head.isDeleted();
        while(head._next != null && head.isDeleted())
        {

            deleted = true;
            final QueueEntryImpl newhead = head.nextNode();
            if(newhead != null)
            {
                if(_nextUpdater.compareAndSet(_head,head, newhead))
                {
                    _deletes.decrementAndGet();
                }
            }
            head = _head.nextNode();
        }

        if(!deleted)
        {
            deleted = true;
        }

        if(_deletes.get() > 1000L)
        {
            _deletes.set(0L);
            scavenge();
        }
    }

    void scavenge()
    {
        QueueEntryImpl root = _head;
        QueueEntryImpl next = root.nextNode();

        do
        {


            while(next._next != null && next.isDeleted())
            {

                final QueueEntryImpl newhead = next.nextNode();
                if(newhead != null)
                {
                    _nextUpdater.compareAndSet(root,next, newhead);
                }
                next = root.nextNode();
            }
            if(next._next != null)
            {
                if(!next.isDeleted())
                {
                    root = next;
                    next = root.nextNode();
                }
            }
            else
            {
                break;
            }

        } while (next != null && next._next != null);

    }



    public AMQQueue getQueue()
    {
        return _queue;
    }


    public QueueEntry add(ServerMessage message)
    {
        QueueEntryImpl node = new QueueEntryImpl(this, message);
        for (;;)
        {
            QueueEntryImpl tail = _tail;
            QueueEntryImpl next = tail.nextNode();
            if (tail == _tail)
            {
                if (next == null)
                {
                    node.setEntryId(tail.getEntryId()+1);
                    if (_nextUpdater.compareAndSet(tail, null, node))
                    {
                        _tailUpdater.compareAndSet(this, tail, node);

                        return node;
                    }
                }
                else
                {
                    _tailUpdater.compareAndSet(this,tail, next);
                }
            }
        }
    }

    public QueueEntry next(QueueEntry node)
    {
        return ((QueueEntryImpl)node).getNext();
    }


    public class QueueEntryIteratorImpl implements QueueEntryIterator
    {

        private QueueEntryImpl _lastNode;

        QueueEntryIteratorImpl(QueueEntryImpl startNode)
        {
            _lastNode = startNode;
        }


        public boolean atTail()
        {
            return _lastNode.nextNode() == null;
        }

        public QueueEntry getNode()
        {

            return _lastNode;

        }

        public boolean advance()
        {

            if(!atTail())
            {
                QueueEntryImpl nextNode = _lastNode.nextNode();
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


    public QueueEntryIterator iterator()
    {
        return new QueueEntryIteratorImpl(_head);
    }


    public QueueEntry getHead()
    {
        return _head;
    }

    static class Factory implements QueueEntryListFactory
    {

        public QueueEntryList createQueueEntryList(AMQQueue queue)
        {
            return new SimpleQueueEntryList(queue);
        }
    }
    

}
