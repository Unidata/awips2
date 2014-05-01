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

using NUnit.Framework;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Buffer.Tests
{
   /// <summary>
   /// Tests for the SimpleByteBuffer class
   /// </summary>
   [TestFixture]
   public class SimpleByteBufferTests
   {
      [Test]
      public void CanCreateNewBuffer()
      {
         const int size = 10;
         ByteBuffer buffer = ByteBuffer.Allocate(size);
         Assert.AreEqual(size, buffer.Capacity);
         Assert.AreEqual(0, buffer.Position);
         Assert.AreEqual(size, buffer.Remaining);
         Assert.AreEqual(true, buffer.HasRemaining);
      }

      [Test]
      public void CanWrapArray()
      {
         byte[] array = new byte[10];
         for ( int i=0; i < array.Length; i++ )
         {
            array[i] = (byte) i;
         }
         ByteBuffer buffer = ByteBuffer.Wrap(array);
         // the buffer should be the same size, 
         // and positioned at the end
         Assert.AreEqual(array.Length, buffer.Capacity);
         Assert.AreEqual(array.Length, buffer.Position);
         Assert.AreEqual(array.Length, buffer.Limit);
      }

      #region Base Read/Write tests
      //
      // Base Read/Write tests
      //
      [Test]
      public void CanReadWriteBytes()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer.Rewind();
         Assert.AreEqual(0x01, buffer.GetByte());
         Assert.AreEqual(0x02, buffer.GetByte());
         Assert.AreEqual(0x03, buffer.GetByte());
      }

      [Test]
      [ExpectedException(typeof(BufferUnderflowException))]
      public void ThrowOnReadByteWithNoSpace()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(1);
         buffer.Put((byte)0x01);
         buffer.GetByte();
      }

      [Test]
      [ExpectedException(typeof(BufferOverflowException))]
      public void ThrowOnWriteByteWithNoSpace()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(1);
         buffer.Put((byte)0x01).Put((byte)0x02);
      }

      #endregion Base Read/Write tests

      #region Other Buffer Operations
      //
      // Other Buffer Operations
      //
      
      [Test]
      public void CanFlipBuffer()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer.Flip();
         Assert.AreEqual(10, buffer.Capacity);
         Assert.AreEqual(3, buffer.Limit);
         Assert.AreEqual(0, buffer.Position);
         Assert.AreEqual(3, buffer.Remaining);
      }

      [Test]
      public void CanCompactBuffer()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer.Flip();
         buffer.Position = 1;
         buffer.Compact();
         Assert.AreEqual(10, buffer.Capacity);
         Assert.AreEqual(10, buffer.Limit);
         Assert.AreEqual(2, buffer.Position);
         Assert.AreEqual(8, buffer.Remaining);
         buffer.Rewind();
         Assert.AreEqual((byte)0x02, buffer.GetByte());
         Assert.AreEqual((byte)0x03, buffer.GetByte());
      }

      [Test]
      public void CanClearBuffer()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer.Flip();
         buffer.Position = 2;
         buffer.Clear();
         Assert.AreEqual(10, buffer.Capacity);
         Assert.AreEqual(10, buffer.Limit);
         Assert.AreEqual(0, buffer.Position);
         Assert.AreEqual(10, buffer.Remaining);
      }

      [Test]
      public void CanExpandBuffer()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer.Flip();
         buffer.Position = 2;
         int pos = buffer.Position;
         buffer.Expand(20);

         Assert.AreEqual(pos, buffer.Position);
         Assert.IsTrue(buffer.Remaining >= 20);
         buffer.Rewind();
         Assert.AreEqual(0x01, buffer.GetByte());
      }

      [Test]
      public void CanAutoExpand()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(2);
         buffer.IsAutoExpand = true;
         // should cause autoexpand
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         Assert.IsTrue(buffer.Capacity > 2);
      }

      [Test]
      public void CanGetArray()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer.Flip();

         byte[] array = buffer.Array;
         for ( int i=0; i < buffer.Limit; i++ )
         {
            Assert.AreEqual(buffer.GetByte(), array[i]);
         }
      }

      [Test]
      public void CanSkip()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Skip(4);
         Assert.AreEqual(4, buffer.Position);
      }

      #endregion // Base Read/Write tests

      #region Typed Accessors
      //
      // Typed Accessors
      //
      [Test]
      public void CanReadWriteSByte()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         sbyte value = -12;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetSByte());
      }
      [Test]
      public void CanReadWriteUInt16()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         ushort value = 41233;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetUInt16());
      }
      [Test]
      public void CanReadWriteInt16()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         short value = -21233;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetInt16());
      }
      [Test]
      public void CanReadWriteUInt32()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         uint value = 41233211;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetUInt32());
      }
      [Test]
      public void CanReadWriteInt32()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         int value = -22221233;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetInt32());
      }
      [Test]
      public void CanReadWriteUInt64()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         ulong value = 41233218871;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetUInt64());
      }
      [Test]
      public void CanReadWriteInt64()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         long value = -9887335411;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetInt64());
      }
      [Test]
      public void CanReadWriteFloat()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         float value = -1.2331f;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetFloat());
      }

      [Test]
      public void CanReadWriteDouble()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         double value = -1.2331E12;
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetDouble());
      }

      [Test]
      public void CanReadWriteChar()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         char value = 'H';
         buffer.Put(value);
         buffer.Flip();
         Assert.AreEqual(value, buffer.GetChar());
      }

      [Test]
      public void CanReadWriteByteArray()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put(new byte[] { 0x01, 0x02, 0x03});
         buffer.Flip();
         byte[] data = new byte[3];
         buffer.GetBytes(data);
         Assert.AreEqual(0x01, data[0]);
         Assert.AreEqual(0x02, data[1]);
         Assert.AreEqual(0x03, data[2]);
      }

      [Test]
      public void CanReadWriteByteArrayWithOffset()
      {
         ByteBuffer buffer = ByteBuffer.Allocate(10);
         buffer.Put(new byte[] { 0x01, 0x02, 0x03, 0x04, 0x05 }, 1, 4);
         buffer.Flip();
         byte[] data = new byte[3];
         buffer.GetBytes(data, 2, 1);
         Assert.AreEqual(0x00, data[0]);
         Assert.AreEqual(0x00, data[1]);
         Assert.AreEqual(0x02, data[2]);
      }

      [Test]
      public void CanWriteByteBuffer()
      {
         ByteBuffer buffer1 = ByteBuffer.Allocate(10);
         buffer1.Put((byte)0x01).Put((byte)0x02).Put((byte)0x03);
         buffer1.Flip();

         ByteBuffer buffer2 = ByteBuffer.Allocate(10);
         buffer2.Put(buffer1);
         buffer2.Flip();
         Assert.AreEqual(buffer1.Limit, buffer2.Limit);
         Assert.AreEqual(0x01, buffer2.GetByte());
      }
      #endregion // Typed Accessors

   } // class SimpleByteBufferTests
}


