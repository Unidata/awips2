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
using System.Text;
using System.Threading;
using Apache.Qpid.Collections;
using Apache.Qpid.Common;

namespace Apache.Qpid.Client.Util
{
   internal delegate void ThresholdMethod(int currentCount);

   /// <summary>
   /// Basic bounded queue used to implement prefetching.
   /// Notice we do the callbacks here asynchronously to
   /// avoid adding more complexity to the channel impl.
   /// </summary>
   internal class FlowControlQueue
   {
      private BlockingQueue _queue = new LinkedBlockingQueue();
      private int _itemCount;
      private int _lowerBound;
      private int _upperBound;
      private ThresholdMethod _underThreshold;
      private ThresholdMethod _overThreshold;

      public FlowControlQueue(
         int lowerBound, 
         int upperBound,
         ThresholdMethod underThreshold,
         ThresholdMethod overThreshold
         )
      {
         _lowerBound = lowerBound;
         _upperBound = upperBound;
         _underThreshold = underThreshold;
         _overThreshold = overThreshold;
      }

      public void Enqueue(object item)
      {
         _queue.EnqueueBlocking(item);
         int count = Interlocked.Increment(ref _itemCount);
         if ( _overThreshold != null )
         {
            if ( count == _upperBound )
            {
               _overThreshold.BeginInvoke(
                  count, new AsyncCallback(OnAsyncCallEnd), 
                  _overThreshold
                  );
            }
         }
      }

      public object Dequeue()
      {
         object item = _queue.DequeueBlocking();
         int count = Interlocked.Decrement(ref _itemCount);
         if ( _underThreshold != null )
         {
            if ( count == _lowerBound )
            {
               _underThreshold.BeginInvoke(
                  count, new AsyncCallback(OnAsyncCallEnd),
                  _underThreshold
                  );
            }
         }
         return item;
      }

      private void OnAsyncCallEnd(IAsyncResult res)
      {
         ThresholdMethod method = (ThresholdMethod)res.AsyncState;
         method.EndInvoke(res);
      }
   }
}
