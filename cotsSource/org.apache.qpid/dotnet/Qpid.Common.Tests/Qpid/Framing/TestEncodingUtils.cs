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
using NUnit.Framework;
using Apache.Qpid.Buffer;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Framing.Tests
{
    [TestFixture]
    public class TestEncodingUtils
    {
       [Test]
       public void CanReadLongAsShortString()
       {
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          EncodingUtils.WriteShortStringBytes(buffer, "98878122");
          buffer.Flip();
          long value = EncodingUtils.ReadLongAsShortString(buffer);
          Assert.AreEqual(98878122, value);
       }
       [Test]
       public void CanReadLongAsShortStringNegative()
       {
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          EncodingUtils.WriteShortStringBytes(buffer, "-98878122");
          buffer.Flip();
          long value = EncodingUtils.ReadLongAsShortString(buffer);
          Assert.AreEqual(-98878122, value);
       }
       [Test]
       public void CanReadLongAsShortStringEmpty()
       {
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          EncodingUtils.WriteShortStringBytes(buffer, "");
          buffer.Flip();
          long value = EncodingUtils.ReadLongAsShortString(buffer);
          Assert.AreEqual(0, value);
       }

    }
}
