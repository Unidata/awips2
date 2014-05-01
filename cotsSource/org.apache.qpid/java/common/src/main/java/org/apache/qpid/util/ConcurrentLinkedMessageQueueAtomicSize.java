/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicInteger;

public class ConcurrentLinkedMessageQueueAtomicSize<E> extends ConcurrentLinkedQueueAtomicSize<E> implements MessageQueue<E>
{
    private static final Logger _logger = LoggerFactory.getLogger(ConcurrentLinkedMessageQueueAtomicSize.class);

    protected Queue<E> _messageHead = new ConcurrentLinkedQueueAtomicSize<E>();

    protected AtomicInteger _messageHeadSize = new AtomicInteger(0);

    @Override
    public int size()
    {
        return super.size() + _messageHeadSize.get();
    }

    public int headSize()
    {
        return _messageHeadSize.get();
    }

    @Override
    public E poll()
    {
        if (_messageHead.isEmpty())
        {
            return super.poll();
        }
        else
        {
            E e = _messageHead.poll();

            if (_logger.isDebugEnabled())
            {
                _logger.debug("Providing item(" + e + ")from message head");
            }

            if (e != null)
            {
                _messageHeadSize.decrementAndGet();
            }

            return e;
        }
    }

    @Override
    public boolean remove(Object o)
    {

        if (_messageHead.isEmpty())
        {
            return super.remove(o);
        }
        else
        {
            if (_messageHead.remove(o))
            {
                _messageHeadSize.decrementAndGet();

                return true;
            }

            return super.remove(o);
        }
    }

    @Override
    public boolean removeAll(Collection<?> c)
    {
        if (_messageHead.isEmpty())
        {
            return super.removeAll(c);
        }
        else
        {
            // fixme this is super.removeAll but iterator here doesn't work
            // we need to be able to correctly decrement _messageHeadSize
            // boolean modified = false;
            // Iterator<?> e = iterator();
            // while (e.hasNext())
            // {
            // if (c.contains(e.next()))
            // {
            // e.remove();
            // modified = true;
            // _size.decrementAndGet();
            // }
            // }
            // return modified;

            throw new RuntimeException("Not implemented");
        }
    }

    @Override
    public boolean isEmpty()
    {
        return (_messageHead.isEmpty() && super.isEmpty());
    }

    @Override
    public void clear()
    {
        super.clear();
        _messageHead.clear();
    }

    @Override
    public boolean contains(Object o)
    {
        return _messageHead.contains(o) || super.contains(o);
    }

    @Override
    public boolean containsAll(Collection<?> o)
    {
        return _messageHead.containsAll(o) || super.containsAll(o);
    }

    @Override
    public E element()
    {
        if (_messageHead.isEmpty())
        {
            return super.element();
        }
        else
        {
            return _messageHead.element();
        }
    }

    @Override
    public E peek()
    {
        if (_messageHead.isEmpty())
        {
            return super.peek();
        }
        else
        {
            E o = _messageHead.peek();
            if (_logger.isDebugEnabled())
            {
                _logger.debug("Peeking item (" + o + ") from message head");
            }

            return o;
        }

    }

    @Override
    public Iterator<E> iterator()
    {
        final Iterator<E> mainMessageIterator = super.iterator();

        return new Iterator<E>()
            {
                final Iterator<E> _headIterator = _messageHead.iterator();
                final Iterator<E> _mainIterator = mainMessageIterator;

                Iterator<E> last;

                public boolean hasNext()
                {
                    return _headIterator.hasNext() || _mainIterator.hasNext();
                }

                public E next()
                {
                    if (_headIterator.hasNext())
                    {
                        last = _headIterator;

                        return _headIterator.next();
                    }
                    else
                    {
                        last = _mainIterator;

                        return _mainIterator.next();
                    }
                }

                public void remove()
                {
                    last.remove();
                    if(last == _mainIterator)
                    {
                        _size.decrementAndGet();
                    }
                    else
                    {
                        _messageHeadSize.decrementAndGet();                        
                    }
                }
            };
    }

    @Override
    public boolean retainAll(Collection<?> c)
    {
        throw new RuntimeException("Not Implemented");
    }

    @Override
    public Object[] toArray()
    {
        throw new RuntimeException("Not Implemented");
    }

    public boolean pushHead(E o)
    {
        if (_logger.isDebugEnabled())
        {
            _logger.debug("Adding item(" + o + ") to head of queue");
        }

        if (_messageHead.offer(o))
        {
            _messageHeadSize.incrementAndGet();

            return true;
        }

        return false;
    }
}
