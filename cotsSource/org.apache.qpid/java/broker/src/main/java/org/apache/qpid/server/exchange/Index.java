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
package org.apache.qpid.server.exchange;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.queue.AMQQueue;

/**
 * An index of queues against routing key. Allows multiple queues to be stored
 * against the same key. Used in the DirectExchange.
 */
class Index
{
    private ConcurrentMap<AMQShortString, ArrayList<AMQQueue>> _index
            = new ConcurrentHashMap<AMQShortString, ArrayList<AMQQueue>>();
    private ConcurrentMap<String, ArrayList<AMQQueue>> _stringIndex
            = new ConcurrentHashMap<String, ArrayList<AMQQueue>>();


    synchronized boolean add(AMQShortString key, AMQQueue queue)
    {
        ArrayList<AMQQueue> queues = _index.get(key);
        if(queues == null)
        {
            queues = new ArrayList<AMQQueue>();
        }
        else
        {
            queues = new ArrayList<AMQQueue>(queues);
        }

        //next call is atomic, so there is no race to create the list
        _index.put(key, queues);
        _stringIndex.put(key.toString(), queues);

        if(queues.contains(queue))
        {
            return false;
        }
        else
        {
            return queues.add(queue);
        }
    }



    synchronized boolean remove(AMQShortString key, AMQQueue queue)
    {
        ArrayList<AMQQueue> queues = _index.get(key);
        if (queues != null)
        {
            queues = new ArrayList<AMQQueue>(queues);
            boolean removed = queues.remove(queue);
            if(removed)
            {
                if (queues.size() == 0)
                {
                    _index.remove(key);
                    _stringIndex.remove(key.toString());
                }
                else
                {
                    _index.put(key, queues);
                    _stringIndex.put(key.toString(), queues);
                }
            }
            return removed;
        }
        return false;
    }

    ArrayList<AMQQueue> get(AMQShortString key)
    {
        return _index.get(key);
    }

    ArrayList<AMQQueue> get(String key)
    {
        return _stringIndex.get(key);
    }


    Map<AMQShortString, List<AMQQueue>> getBindingsMap()
    {
        return new HashMap<AMQShortString, List<AMQQueue>>(_index);
    }
}
