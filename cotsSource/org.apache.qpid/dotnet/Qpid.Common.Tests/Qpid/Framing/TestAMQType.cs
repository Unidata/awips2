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
    public class TestAMQType
    {

       #region LONG_STRING tests
       [Test]
       public void LONG_STRING_ReadWrite()
       {
          AMQType type = AMQType.LONG_STRING;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const string VALUE = "simple string 1";

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       #endregion // LONG_STRING tests

       #region UINT32 tests
       [Test]
       public void UINT32_CanGetEncodingSize()
       {
          AMQType type = AMQType.UINT32;
          Assert.AreEqual(4, type.GetEncodingSize(1234443));
       }

       [Test]
       public void UINT32_ToNativeValue()
       {
          AMQType type = AMQType.UINT32;
          Assert.AreEqual(1, type.ToNativeValue(1));
          Assert.AreEqual(1, type.ToNativeValue((short)1));
          Assert.AreEqual(1, type.ToNativeValue((byte)1));
          Assert.AreEqual(1, type.ToNativeValue("1"));

          try
          {
             Assert.AreEqual(1, type.ToNativeValue("adasdads"));
             Assert.Fail("Invalid format allowed");
          } catch ( FormatException )
          {
          }
       }

       [Test]
       public void UINT32_ReadWrite()
       {
          AMQType type = AMQType.UINT32;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const uint VALUE = 0xFFEEDDCC;

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       #endregion // UINT32 Tests

       #region VOID Tests
       [Test]
       public void VOID_CanGetEncodingSize()
       {
          AMQType type = AMQType.VOID;
          Assert.AreEqual(0, type.GetEncodingSize(null));
       }

       [Test]
       public void VOID_ToNativeValue()
       {
          AMQType type = AMQType.VOID;
          Assert.IsNull(type.ToNativeValue(null));

          try
          {
             type.ToNativeValue("asdasd");
             Assert.Fail("converted invalid value");
          } catch (FormatException)
          {
          }
       }

       [Test]
       public void VOID_ReadWrite()
       {
          AMQType type = AMQType.VOID;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);

          type.WriteToBuffer(null, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(null, value.Value);
       }

       #endregion // VOID Tests

       #region BOOLEAN Tests
       [Test]
       public void BOOLEAN_CanGetEncodingSize()
       {
          AMQType type = AMQType.BOOLEAN;
          Assert.AreEqual(1, type.GetEncodingSize(true));
       }

       [Test]
       public void BOOLEAN_ToNativeValue()
       {
          AMQType type = AMQType.BOOLEAN;
          Assert.AreEqual(true, type.ToNativeValue(true));
          Assert.AreEqual(false, type.ToNativeValue("false"));

          try
          {
             type.ToNativeValue("asdasd");
             Assert.Fail("converted invalid value");
          } catch ( FormatException )
          {
          }
       }

       [Test]
       public void BOOLEAN_ReadWrite()
       {
          AMQType type = AMQType.BOOLEAN;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);

          type.WriteToBuffer(true, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(true, value.Value);
       }
       #endregion // BOOLEAN Tests

       #region INT16 tests
       [Test]
       public void INT16_ReadWrite()
       {
          AMQType type = AMQType.INT16;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const short VALUE = -32765;

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       //public void UINT16_ReadWrite()
       //{
       //   AMQType type = AMQType.UINT16;
       //   ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
       //   const ushort VALUE = 64321;

       //   type.WriteToBuffer(VALUE, buffer);
       //   buffer.Flip();
       //   buffer.Rewind();
       //   AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
       //   Assert.AreEqual(VALUE, value.Value);
       //}
       #endregion // INT16 Tests

       #region INT32 tests
       [Test]
       public void INT32_ReadWrite()
       {
          AMQType type = AMQType.INT32;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const int VALUE = -39273563;

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       #endregion // INT32 Tests

       #region INT64 tests
       [Test]
       public void INT64_ReadWrite()
       {
          AMQType type = AMQType.INT64;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const long VALUE = -(2^43+1233123);

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       [Test]
       public void UINT64_ReadWrite()
       {
          AMQType type = AMQType.UINT64;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const ulong VALUE = (2 ^ 61 + 1233123);

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       #endregion // INT64 Tests

       #region FLOAT tests
       [Test]
       public void FLOAT_ReadWrite()
       {
          AMQType type = AMQType.FLOAT;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const float VALUE = 1.2345000E-035f;

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       #endregion // FLOAT Tests

       #region DOUBLE tests
       [Test]
       public void DOUBLE_ReadWrite()
       {
          AMQType type = AMQType.DOUBLE;
          ByteBuffer buffer = ByteBuffer.Allocate(0x1000);
          const double VALUE = 1.2345000E-045;

          type.WriteToBuffer(VALUE, buffer);
          buffer.Flip();
          buffer.Rewind();
          AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
          Assert.AreEqual(VALUE, value.Value);
       }
       #endregion // FLOAT Tests
    }
}
