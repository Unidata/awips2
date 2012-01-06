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
using common.org.apache.qpid.transport.util;
using NUnit.Framework;
using org.apache.qpid.transport.util;

namespace test.transport.util
{
    [TestFixture]

    public class CircularBufferTest
    {
        private CircularBuffer<Object> _buf;
        private static readonly Logger _log = Logger.Get(typeof(CircularBufferTest));

        [Test]
        public void BlockingEnqueue()
        {
            _log.Debug("Running: BlockingEnqueue");
            const int size = 10;
            _buf = new CircularBuffer<Object>(size);
            // add size element anc check that the size +1 add blocks 
            for (int i = 1; i < size; i++ )
            {
                _buf.Enqueue(new object());
            }
            // check tha the buffer is now full 
            Thread t = new Thread(Go);
            t.Start();
            Thread.Sleep(100);
            // the trhead t should block until an element is dequeued 
            Assert.IsTrue(t.ThreadState == ThreadState.WaitSleepJoin);           
            _buf.Dequeue();
            // t should now be stopped 
            Thread.Sleep(100);
            Assert.IsTrue(t.ThreadState == ThreadState.Stopped);           
        }

        [Test]
        public void Close()
        {
            _log.Debug("Running: BlockingEnqueue");
            const int size = 10;
            _buf = new CircularBuffer<Object>(size);
            // add size element anc check that the size +1 add blocks 
            for (int i = 1; i < size; i++)
            {
                _buf.Enqueue(new object());
            }
            // check tha the buffer is now full 
            Thread t = new Thread(Go);
            t.Start();
            Thread.Sleep(1000);
            // the trhead t should block until the buffer is closed             
            Assert.IsTrue(t.ThreadState == ThreadState.WaitSleepJoin);
            _buf.Close();
            Thread.Sleep(100);
            // t should now be stopped 
            Assert.IsTrue(t.ThreadState == ThreadState.Stopped);           
        }

        void Go()
        {
            _buf.Enqueue(new object());
        }

    }
}
