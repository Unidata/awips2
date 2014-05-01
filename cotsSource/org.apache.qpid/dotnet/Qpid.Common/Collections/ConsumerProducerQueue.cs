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
using System.Collections;
using System.Threading;


namespace Apache.Qpid.Collections
{
   /// <summary>
   /// Simple FIFO queue to support multi-threaded consumer
   /// and producers. It supports timeouts in dequeue operations.
   /// </summary>
   public sealed class ConsumerProducerQueue 
   {
      private Queue _queue = new Queue();
      private WaitSemaphore _semaphore = new WaitSemaphore();

      /// <summary>
      /// Put an item into the tail of the queue
      /// </summary>
      /// <param name="item"></param>
      public void Enqueue(object item)
      {
         lock ( _queue.SyncRoot )
         {
            _queue.Enqueue(item);
            _semaphore.Increment();
         }
      }

      /// <summary>
      /// Wait indefinitely for an item to be available
      /// on the queue.
      /// </summary>
      /// <returns>The object at the head of the queue</returns>
      public object Dequeue()
      {
         return Dequeue(Timeout.Infinite);
      }

      /// <summary>
      /// Wait up to the number of milliseconds specified
      /// for an item to be available on the queue
      /// </summary>
      /// <param name="timeout">Number of milliseconds to wait</param>
      /// <returns>The object at the head of the queue, or null 
      /// if the timeout expires</returns>
      public object Dequeue(long timeout)
      {
         if ( _semaphore.Decrement(timeout) )
         {
            lock ( _queue.SyncRoot )
            {
               return _queue.Dequeue();
            }
         }
         return null;
      }

      #region Simple Semaphore
      //
      // Simple Semaphore
      //

      class WaitSemaphore
      {
         private int _count;
         private AutoResetEvent _event = new AutoResetEvent(false);

         public void Increment()
         {
            Interlocked.Increment(ref _count);
            _event.Set();
         }

         public bool Decrement(long timeout)
         {
            if ( timeout > int.MaxValue )
               throw new ArgumentOutOfRangeException("timeout", timeout, "Must be <= Int32.MaxValue");

            int millis = (int) (timeout & 0x7FFFFFFF);
            if ( Interlocked.Decrement(ref _count) > 0 )
            {
               // there are messages in queue, so no need to wait
               return true;
            } else
            {
               return _event.WaitOne(millis, false);
            }
         }
      }
      #endregion // Simple Semaphore
   }
}
