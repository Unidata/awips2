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
using System.Collections.Generic;
using System.Threading;
using common.org.apache.qpid.transport.util;
using NUnit.Framework;
using org.apache.qpid.transport;
using org.apache.qpid.transport.codec;
using org.apache.qpid.transport.util;


namespace test.transport.util
{
    [TestFixture]
    public class ResultFutureTest
    {
        private static readonly Logger _log = Logger.Get(typeof (ByteEncoderTest));
        private static ResultFuture _future;

        [Test]
        public void getFutureTimeout()
        {
            _log.Debug("Running: getFutureTimeout");                        
            _future = new ResultFuture();
            _future.Session = new Session(new byte[1]);
            DateTime start = DateTime.Now;
            Struct result = _future.Get(1000);
            Assert.IsTrue(DateTime.Now.Subtract(start).TotalMilliseconds >= 1000);
            Assert.IsNull(result);           
        }

        [Test]
        public void getFuture()
        {
            _log.Debug("Running: getFuture");
            _future = new ResultFuture();
            _future.Session = new Session(new byte[1]);
            Thread t = new Thread(Go);
            t.Start();
            Struct result = _future.Get(2000);
            Assert.IsNotNull(result);
        }


        void Go()
        {
            Thread.Sleep(500);
            _future.Result = new myStruct();
        }
    }

    public class myStruct:Struct
    {
        public override int GetStructType()
        {
            throw new System.NotImplementedException();
        }

        public override int GetSizeWidth()
        {
            throw new System.NotImplementedException();
        }

        public override int GetPackWidth()
        {
            throw new System.NotImplementedException();
        }

        public override void Read(IDecoder dec)
        {
            throw new System.NotImplementedException();
        }

        public override void Write(IEncoder enc)
        {
            throw new System.NotImplementedException();
        }

        public override Dictionary<string, object> Fields
        {
            get { throw new System.NotImplementedException(); }
        }
    }
}
