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
package org.apache.qpid.util;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

public class ConcurrentLinkedQueueAtomicSize<E> extends ConcurrentLinkedQueue<E>
{
    AtomicInteger _size = new AtomicInteger(0);

    public int size()
    {
        return _size.get();
    }

    public boolean offer(E o)
    {

        if (super.offer(o))
        {
            _size.incrementAndGet();
            return true;
        }

        return false;
    }

    public E poll()
    {
        E e = super.poll();

        if (e != null)
        {
            _size.decrementAndGet();
        }

        return e;
    }

    @Override
    public boolean remove(Object o)
    {
        if (super.remove(o))
        {
            _size.decrementAndGet();
            return true;
        }

        return false;
    }
}
