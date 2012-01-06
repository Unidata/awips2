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
    public class SynchronousQueue : BlockingQueue
    {
        /// <summary>
        /// Lock protecting both wait queues
        /// </summary> 
//        private readonly object _qlock = new object();
        
        /// <summary>
        /// Queue holding waiting puts
        /// </summary> 
//        private readonly WaitQueue _waitingProducers;
        
        /// <summary>
        /// Queue holding waiting takes
        /// </summary> 
//        private readonly WaitQueue _waitingConsumers;
        
        /**
         * Queue to hold waiting puts/takes; specialized to Fifo/Lifo below.
         * These queues have all transient fields, but are serializable
         * in order to recover fairness settings when deserialized.
         */
        internal abstract class WaitQueue
        {
            /** Creates, adds, and returns node for x. */
            internal abstract Node Enq(Object x);
            /** Removes and returns node, or null if empty. */
            internal abstract Node Deq();
            /** Removes a cancelled node to avoid garbage retention. */
            internal abstract void Unlink(Node node);
            /** Returns true if a cancelled node might be on queue. */
            internal abstract bool ShouldUnlink(Node node);
        }

        /**
         * FIFO queue to hold waiting puts/takes.
         */
        sealed class FifoWaitQueue : WaitQueue
        {            
            private Node head;
            private Node last;

            internal override Node Enq(Object x)
            {
                Node p = new Node(x);
                if (last == null)
                {
                    last = head = p;
                }
                else
                {
                    last = last.next = p;
                }
                return p;
            }

            internal override Node Deq()
            {
                Node p = head;
                if (p != null)
                {
                    if ((head = p.next) == null)
                    {
                        last = null;
                    }
                    p.next = null;
                }
                return p;
            }

            internal override bool ShouldUnlink(Node node)
            {
                return (node == last || node.next != null);
            }

            internal override void Unlink(Node node)
            {
                Node p = head;
                Node trail = null;
                while (p != null)
                {
                    if (p == node)
                    {
                        Node next = p.next;
                        if (trail == null)
                        {
                            head = next;
                        }
                        else
                        {
                            trail.next = next;
                        }
                        if (last == node)
                        {
                            last = trail;
                        }
                        break;
                    }
                    trail = p;
                    p = p.next;
                }
            }
        }

        /**
         * LIFO queue to hold waiting puts/takes.
         */
        sealed class LifoWaitQueue : WaitQueue
        {        
            private Node head;

            internal override Node Enq(Object x)
            {
                return head = new Node(x, head);
            }

            internal override Node Deq()
            {
                Node p = head;
                if (p != null)
                {
                    head = p.next;
                    p.next = null;
                }
                return p;
            }

            internal override bool ShouldUnlink(Node node)
            {
                // Return false if already dequeued or is bottom node (in which
                // case we might retain at most one garbage node)
                return (node == head || node.next != null);
            }

            internal override void Unlink(Node node)
            {
                Node p = head;
                Node trail = null;
                while (p != null)
                {
                    if (p == node)
                    {
                        Node next = p.next;
                        if (trail == null)
                            head = next;
                        else
                            trail.next = next;
                        break;
                    }
                    trail = p;
                    p = p.next;
                }
            }
        }

        /**
         * Nodes each maintain an item and handle waits and signals for
         * getting and setting it. The class extends
         * AbstractQueuedSynchronizer to manage blocking, using AQS state
         *  0 for waiting, 1 for ack, -1 for cancelled.
         */
        sealed internal class Node
        {            

            /** Synchronization state value representing that node acked */
            private const int ACK    =  1;
            /** Synchronization state value representing that node cancelled */
            private const int CANCEL = -1;

            internal int state = 0;

            /** The item being transferred */
            internal Object item;
            /** Next node in wait queue */
            internal Node next;

            /** Creates a node with initial item */
            internal Node(Object x)
            {
                item = x;
            }

            /** Creates a node with initial item and next */
            internal Node(Object x, Node n)
            {
                item = x;
                next = n;
            }

            /**
             * Takes item and nulls out field (for sake of GC)
             *
             * PRE: lock owned
             */
            private Object Extract()
            {
                Object x = item;
                item = null;
                return x;
            }

            /**
             * Tries to cancel on interrupt; if so rethrowing,
             * else setting interrupt state
             *
             * PRE: lock owned
             */
            /*private void checkCancellationOnInterrupt(InterruptedException ie)
                throws InterruptedException
            {
                if (state == 0) {
                    state = CANCEL;
                    notify();
                    throw ie;
                }
                Thread.currentThread().interrupt();
            }*/

            /**
             * Fills in the slot created by the consumer and signal consumer to
             * continue.
             */
            internal bool SetItem(Object x)            
            {
                lock (this)
                {
                    if (state != 0) return false;
                    item = x;
                    state = ACK;                    
                    Monitor.Pulse(this);
                    return true;
                }
            }

            /**
             * Removes item from slot created by producer and signal producer
             * to continue.
             */
            internal Object GetItem()
            {
                if (state != 0) return null;
                state = ACK;
                Monitor.Pulse(this);
                return Extract();
            }

            /**
             * Waits for a consumer to take item placed by producer.
             */
            internal void WaitForTake() //throws InterruptedException {
            {                
                while (state == 0)
                {
                    Monitor.Wait(this);
                }                
            }

            /**
             * Waits for a producer to put item placed by consumer.
             */
            internal object WaitForPut()
            {
                lock (this)
                {
                    while (state == 0) Monitor.Wait(this);
                } 
                return Extract();
            }

            private bool Attempt(long nanos)
            {
                if (state != 0) return true;
                if (nanos <= 0) {
                    state = CANCEL;
                    Monitor.Pulse(this);
                    return false;
                }
                
                while (true)
                {
                    Monitor.Wait(nanos);
                    //TimeUnit.NANOSECONDS.timedWait(this, nanos);
                    if (state != 0)
                    {
                        return true;
                    }
                        //nanos = deadline - Utils.nanoTime();
                        //if (nanos <= 0)
                    else
                    {
                        state = CANCEL;
                        Monitor.Pulse(this);
                        return false;
                    }
                }
            }

            /**
             * Waits for a consumer to take item placed by producer or time out.
             */
            internal bool WaitForTake(long nanos)
            {                
                return Attempt(nanos);                
            }

            /**
             * Waits for a producer to put item placed by consumer, or time out.
             */
            internal object WaitForPut(long nanos)
            {
                if (!Attempt(nanos))
                {
                    return null;
                }
                else
                {
                    return Extract();
                }
            }
        }

        public SynchronousQueue(bool strict)
        {
            // TODO !!!!
        }
        
        public override bool EnqueueNoThrow(object e)
        {
            throw new NotImplementedException();
        }

        public override void EnqueueBlocking(object e)
        {
            throw new NotImplementedException();
        }        

        public override object DequeueBlocking()
        {
            throw new NotImplementedException();
        }

        public override int RemainingCapacity
        {
            get
            {
                throw new NotImplementedException();
            }
        }
    }
}
