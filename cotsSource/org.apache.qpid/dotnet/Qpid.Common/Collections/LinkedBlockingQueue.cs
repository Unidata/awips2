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
using System;
using System.Threading;

namespace Apache.Qpid.Collections
{
    public class LinkedBlockingQueue : BlockingQueue
    {             

        /*
         * A variant of the "two lock queue" algorithm.  The putLock gates
         * entry to put (and offer), and has an associated condition for
         * waiting puts.  Similarly for the takeLock.  The "count" field
         * that they both rely on is maintained as an atomic to avoid
         * needing to get both locks in most cases. Also, to minimize need
         * for puts to get takeLock and vice-versa, cascading notifies are
         * used. When a put notices that it has enabled at least one take,
         * it signals taker. That taker in turn signals others if more
         * items have been entered since the signal. And symmetrically for
         * takes signalling puts. Operations such as remove(Object) and
         * iterators acquire both locks.
         */

        /**
         * Linked list node class
         */
        internal class Node
        {
            /** The item, volatile to ensure barrier separating write and read */
            internal volatile Object item;
            internal Node next;
            internal Node(Object x) { item = x; }
        }

        /** The capacity bound, or Integer.MAX_VALUE if none */
        private readonly int capacity;

        /** Current number of elements */
        private volatile int count = 0;

        /** Head of linked list */
        private Node head;

        /** Tail of linked list */
        private Node last;

        /** Lock held by take, poll, etc */
        private readonly object takeLock = new Object(); //new SerializableLock();

        /** Lock held by put, offer, etc */
        private readonly object putLock = new Object();//new SerializableLock();

        /**
         * Signals a waiting take. Called only from put/offer (which do not
         * otherwise ordinarily lock takeLock.)
         */
        private void SignalNotEmpty()
        {
            lock (takeLock)
            {
                Monitor.Pulse(takeLock);
            }
        }

        /**
         * Signals a waiting put. Called only from take/poll.
         */
        private void SignalNotFull()
        {
            lock (putLock)
            {
                Monitor.Pulse(putLock);
            }
        }

        /**
         * Creates a node and links it at end of queue.
         * @param x the item
         */
        private void Insert(Object x)
        {
            last = last.next = new Node(x);
        }

        /**
         * Removes a node from head of queue,
         * @return the node
         */
        private Object Extract()
        {
            Node first = head.next;
            head = first;
            Object x = first.item;
            first.item = null;
            return x;
        }


        /**
         * Creates a <tt>LinkedBlockingQueue</tt> with a capacity of
         * {@link Integer#MAX_VALUE}.
         */
        public LinkedBlockingQueue() : this(Int32.MaxValue)
        {            
        }

        /**
         * Creates a <tt>LinkedBlockingQueue</tt> with the given (fixed) capacity.
         *
         * @param capacity the capacity of this queue
         * @throws IllegalArgumentException if <tt>capacity</tt> is not greater
         *         than zero
         */
        public LinkedBlockingQueue(int capacity)
        {
            if (capacity <= 0) throw new ArgumentException("Capacity must be positive, was passed " + capacity);
            this.capacity = capacity;
            last = head = new Node(null);
        }        

        // this doc comment is overridden to remove the reference to collections
        // greater in size than Integer.MAX_VALUE
        /**
         * Returns the number of elements in this queue.
         *
         * @return the number of elements in this queue
         */
        public int Size
        {
            get
            {
                return count;
            }            
        }

        // this doc comment is a modified copy of the inherited doc comment,
        // without the reference to unlimited queues.
        /**
         * Returns the number of additional elements that this queue can ideally
         * (in the absence of memory or resource constraints) accept without
         * blocking. This is always equal to the initial capacity of this queue
         * less the current <tt>size</tt> of this queue.
         *
         * <p>Note that you <em>cannot</em> always tell if an attempt to insert
         * an element will succeed by inspecting <tt>remainingCapacity</tt>
         * because it may be the case that another thread is about to
         * insert or remove an element.
         */
        public override int RemainingCapacity
        {
            get
            {
                return capacity - count;
            }            
        }

        /**
         * Inserts the specified element at the tail of this queue, waiting if
         * necessary for space to become available.
         *
         * @throws InterruptedException {@inheritDoc}
         * @throws NullPointerException {@inheritDoc}
         */
        public override void EnqueueBlocking(Object e)
        {
            if (e == null) throw new ArgumentNullException("Object must not be null");
            // Note: convention in all put/take/etc is to preset
            // local var holding count  negative to indicate failure unless set.
            int c = -1;
            lock (putLock) 
            {
                /*
                 * Note that count is used in wait guard even though it is
                 * not protected by lock. This works because count can
                 * only decrease at this point (all other puts are shut
                 * out by lock), and we (or some other waiting put) are
                 * signalled if it ever changes from
                 * capacity. Similarly for all other uses of count in
                 * other wait guards.
                 */                
                while (count == capacity)
                {
                    Monitor.Wait(putLock);
                }
                
                Insert(e);
                lock(this)
                {
                    c = count++;
                }
                if (c + 1 < capacity)
                {
                    Monitor.Pulse(putLock);
                }                    
            }

            if (c == 0)
            {
                SignalNotEmpty();
            }
        }
        
        /**
         * Inserts the specified element at the tail of this queue if it is
         * possible to do so immediately without exceeding the queue's capacity,
         * returning <tt>true</tt> upon success and <tt>false</tt> if this queue
         * is full.
         * When using a capacity-restricted queue, this method is generally
         * preferable to method {@link BlockingQueue#add add}, which can fail to
         * insert an element only by throwing an exception.
         *
         * @throws NullPointerException if the specified element is null
         */
        public override bool EnqueueNoThrow(Object e)
        {
            if (e == null) throw new ArgumentNullException("e must not be null");
            if (count == capacity)
            {
                return false;
            }
            int c = -1;
            lock (putLock) 
            {
                if (count < capacity) 
                {
                    Insert(e);
                    lock (this)
                    {
                        c = count++;
                    }
                    if (c + 1 < capacity)
                    {
                        Monitor.Pulse(putLock);
                    }
                }
            }
            if (c == 0)
            {
                SignalNotEmpty();
            }
            return c >= 0;
        }

        /**
         * Retrieves and removes the head of this queue, waiting if necessary
         * until an element becomes available.
         *
         * @return the head of this queue
         * @throws InterruptedException if interrupted while waiting
         */
        public override Object DequeueBlocking()
        {
            Object x;
            int c = -1;
            lock (takeLock) 
            {
                
                while (count == 0)
                {
                    Monitor.Wait(takeLock);
                }
                

                x = Extract();
                lock (this) { c = count--; }
                if (c > 1)
                {
                    Monitor.Pulse(takeLock);
                }
            }
            if (c == capacity)
            {
                SignalNotFull();
            }
            return x;
        }
        
        public Object Poll()
        {
            if (count == 0)
            {
                return null;
            }
            Object x = null;
            int c = -1;
            lock (takeLock) 
            {
                if (count > 0) 
                {
                    x = Extract();
                    lock (this) { c = count--; }
                    if (c > 1)
                    {
                        Monitor.Pulse(takeLock);
                    }
                }
            }
            if (c == capacity)
            {
                SignalNotFull();
            }
            return x;
        }


        public override Object Peek()
        {
            if (count == 0)
            {
                return null;
            }
            lock (takeLock) 
            {
                Node first = head.next;
                if (first == null)
                {
                    return null;
                }
                else
                {
                    return first.item;
                }
            }
        }
        
        public override String ToString()
        {
            lock (putLock) 
            {
                lock (takeLock) 
                {
                    return base.ToString();
                }
            }
        }

        /**
         * Atomically removes all of the elements from this queue.
         * The queue will be empty after this call returns.
         */
        public override void Clear()
        {
            lock (putLock) 
            {
                lock (takeLock) 
                {
                    head.next = null;                
                    last = head;
                    int c;
                    lock (this) 
                    {
                        c = count;
                        count = 0;
                    }
                    if (c == capacity)
                    {
                        Monitor.PulseAll(putLock);
                    }
                }
            }
        }                       
    }
}


