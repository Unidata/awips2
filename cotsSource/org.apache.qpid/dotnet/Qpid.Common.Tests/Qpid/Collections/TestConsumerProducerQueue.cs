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
using NUnit.Framework;
using Apache.Qpid.Collections;

namespace Apache.Qpid.Collections.Tests
{
    [TestFixture]
    public class TestConsumerProducerQueue
    {
       private ConsumerProducerQueue _queue;

       [SetUp]
       public void SetUp()
       {
          _queue = new ConsumerProducerQueue();
       }

       [Test]
       public void CanDequeueWithInifiniteWait()
       {
          Thread producer = new Thread(new ThreadStart(ProduceFive));
          producer.Start();
          for ( int i = 0; i < 5; i++ )
          {
             object item = _queue.Dequeue();
             Assert.IsNotNull(item);
          }
       }

       [Test]
       public void ReturnsNullOnDequeueTimeout()
       {
          // queue is empty
          Assert.IsNull(_queue.Dequeue(500));
       }

       [Test]
       public void DequeueTillEmpty()
       {
          _queue.Enqueue(1);
          _queue.Enqueue(2);
          _queue.Enqueue(3);
          Assert.AreEqual(1, _queue.Dequeue());
          Assert.AreEqual(2, _queue.Dequeue());
          Assert.AreEqual(3, _queue.Dequeue());
          // no messages in queue, will timeout
          Assert.IsNull(_queue.Dequeue(500));
       }


       private void ProduceFive()
       {
          Thread.Sleep(1000);
          _queue.Enqueue("test item 1");
          _queue.Enqueue("test item 2");
          _queue.Enqueue("test item 3");
          Thread.Sleep(0);
          _queue.Enqueue("test item 4");
          _queue.Enqueue("test item 5");
       }
    }
}
