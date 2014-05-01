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

import org.apache.qpid.server.util.AveragedRun;
import org.apache.qpid.server.util.TimedRun;
import org.apache.qpid.server.util.RunStats;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

public class QueuePerfTest extends TimedRun
{
    private final Factory _factory;
    private final int _queueCount;
    private final int _messages;
    private final String _msg = "";
    private List<Queue<String>> _queues;

    QueuePerfTest(Factory factory, int queueCount, int messages)
    {
        super(factory + ", " + queueCount + ", " + messages);
        _factory = factory;
        _queueCount = queueCount;
        _messages = messages;
    }

    protected void setup() throws Exception
    {
        //init
        int count = Integer.getInteger("prepopulate", 0);
//        System.err.println("Prepopulating with " + count + " items");
        _queues = new ArrayList<Queue<String>>(_queueCount);
        for (int i = 0; i < _queueCount; i++)
        {
            Queue<String> q = _factory.create();
            for(int j = 0; j < count; ++j)
            {
                q.add("Item"+ j);
            }
            _queues.add(q);
        }
        System.gc();
    }

    protected void teardown() throws Exception
    {
        System.gc();
    }

    protected void run() throws Exception
    {
        //dispatch
        for (int i = 0; i < _messages; i++)
        {
            for (Queue<String> q : _queues)
            {
                q.offer(_msg);
                q.poll();
            }
        }
    }

    static interface Factory
    {
        Queue<String> create();
    }

    static Factory CONCURRENT = new Factory()
    {
        public Queue<String> create()
        {
            return new ConcurrentLinkedQueue<String>();
        }

        public String toString()
        {
            return "ConcurrentLinkedQueue";
        }

    };

    static Factory SYNCHRONIZED = new Factory()
    {
        public Queue<String> create()
        {
            return new SynchronizedQueue<String>(new LinkedList<String>());
        }


        public String toString()
        {
            return "Synchronized LinkedList";
        }
    };

    static Factory PLAIN = new Factory()
    {
        public Queue<String> create()
        {
            return new LinkedList<String>();
        }

        public String toString()
        {
            return "Plain LinkedList";
        }
    };

    static class SynchronizedQueue<E> implements Queue<E>
    {
        private final Queue<E> queue;

        SynchronizedQueue(Queue<E> queue)
        {
            this.queue = queue;
        }

        public synchronized E element()
        {
            return queue.element();
        }

        public synchronized boolean offer(E o)
        {
            return queue.offer(o);
        }

        public synchronized E peek()
        {
            return queue.peek();
        }

        public synchronized E poll()
        {
            return queue.poll();
        }

        public synchronized E remove()
        {
            return queue.remove();
        }

        public synchronized int size()
        {
            return queue.size();
        }

        public synchronized boolean isEmpty()
        {
            return queue.isEmpty();
        }

        public synchronized boolean contains(Object o)
        {
            return queue.contains(o);
        }

        public synchronized Iterator<E> iterator()
        {
            return queue.iterator();
        }

        public synchronized Object[] toArray()
        {
            return queue.toArray();
        }

        public synchronized <T>T[] toArray(T[] a)
        {
            return queue.toArray(a);
        }

        public synchronized boolean add(E o)
        {
            return queue.add(o);
        }

        public synchronized boolean remove(Object o)
        {
            return queue.remove(o);
        }

        public synchronized boolean containsAll(Collection<?> c)
        {
            return queue.containsAll(c);
        }

        public synchronized boolean addAll(Collection<? extends E> c)
        {
            return queue.addAll(c);
        }

        public synchronized boolean removeAll(Collection<?> c)
        {
            return queue.removeAll(c);
        }

        public synchronized boolean retainAll(Collection<?> c)
        {
            return queue.retainAll(c);
        }

        public synchronized void clear()
        {
            queue.clear();
        }
    }

    static void run(String label, AveragedRun test) throws Exception
    {
        RunStats stats = test.call();
        System.out.println((label == null ? "" : label + ", ") + test
                + ", " + stats.getAverage() + ", " + stats.getMax() + ", " + stats.getMin());
    }

    public static void main(String[] argv) throws Exception
    {
        Factory[] factories = new Factory[]{PLAIN, SYNCHRONIZED, CONCURRENT};
        int iterations = 5;
        String label = argv.length > 0 ? argv[0]: null;
        System.out.println((label == null ? "" : "Label, ") + "Queue Type, No. of Queues, No. of Operations, Avg Time, Min Time, Max Time");
        //vary number of queues:

        for(Factory f : factories)
        {
            run(label, new AveragedRun(new QueuePerfTest(f, 100, 10000), iterations));
            run(label, new AveragedRun(new QueuePerfTest(f, 1000, 10000), iterations));
            run(label, new AveragedRun(new QueuePerfTest(f, 10000, 10000), iterations));
            run(label, new AveragedRun(new QueuePerfTest(f, 1000, 1000), iterations));
            run(label, new AveragedRun(new QueuePerfTest(f, 1000, 100000), iterations));
        }
    }

}
