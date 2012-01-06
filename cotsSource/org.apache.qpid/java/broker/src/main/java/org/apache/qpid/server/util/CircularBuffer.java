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
package org.apache.qpid.server.util;

import java.util.Iterator;

import org.apache.log4j.Logger;

public class CircularBuffer implements Iterable
{

    private static final Logger _logger = Logger.getLogger(CircularBuffer.class);

    private final Object[] _log;
    private int _size;
    private int _index;

    public CircularBuffer(int size)
    {
        _log = new Object[size];
    }

    public void add(Object o)
    {
        _log[_index++] = o;
        _size = Math.min(_size+1, _log.length);
        if(_index >= _log.length)
        {
            _index = 0;
        }
    }

    public Object get(int i)
    {
        if(i >= _log.length)
        {
            throw new ArrayIndexOutOfBoundsException(i);
        }
        return _log[index(i)];
    }

    public int size() {
        return _size;
    }

    public Iterator iterator()
    {
        return new Iterator()
        {
            private int i = 0;

            public boolean hasNext()
            {
                return i < _size;
            }

            public Object next()
            {
                return get(i++);
            }

            public void remove()
            {
                throw new UnsupportedOperationException();
            }
        };
    }

    public String toString()
    {
        StringBuilder s = new StringBuilder();
        boolean first = true;
        for(Object o : this)
        {
            if(!first)
            {
                s.append(", ");
            }
            else
            {
                first = false;
            }
            s.append(o);
        }
        return s.toString();
    }

    public void dump()
    {
        for(Object o : this)
        {
         _logger.info(o);
        }
    }

    int index(int i)
    {
        return _size == _log.length ? (_index + i) % _log.length : i;
    }

    public static void main(String[] artgv)
    {
        String[] items = new String[]{
                "A","B","C","D","E","F","G","H","I","J","K"
        };
        CircularBuffer buffer = new CircularBuffer(5);
        for(String s : items)
        {
            buffer.add(s);
            _logger.info(buffer);
        }
    }
}
