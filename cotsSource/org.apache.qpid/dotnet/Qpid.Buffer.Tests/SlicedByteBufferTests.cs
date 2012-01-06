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

namespace Apache.Qpid.Buffer.Tests
{
   /// <summary>
   /// Tests for the SlicedByteBuffer class
   /// </summary>
   [TestFixture]
   public class SlicedByteBufferTests
   {
      private ByteBuffer _baseBuffer;

      [SetUp]
      public void Setup()
      {
         const int size = 50;
         _baseBuffer = ByteBuffer.Allocate(size);
         for ( byte b = 0; b < 10; b++ )
         {
            _baseBuffer.Put(b);
         }
         _baseBuffer.Flip();
      }

      [Test]
      public void CanSliceBuffer()
      {
         _baseBuffer.Position = 5;

         ByteBuffer slice = _baseBuffer.Slice();
         Assert.AreEqual(5, slice.Capacity);
         Assert.AreEqual(0, slice.Position);
         Assert.AreEqual(5, slice.Remaining);
         Assert.AreEqual(5, slice.Limit);
      }

      [Test]
      public void CanReadWriteSlice()
      {
         _baseBuffer.Position = 5;

         ByteBuffer slice = _baseBuffer.Slice();
         slice.Put((byte) 0xFF).Put((byte) 0xF0).Put((byte) 0xA0);
         slice.Flip();

         Assert.AreEqual(3, slice.Limit);
         Assert.AreEqual(0xFF, slice.GetByte());
         Assert.AreEqual(0xF0, slice.GetByte());
         Assert.AreEqual(0xA0, slice.GetByte());
      }

      [Test]
      public void WriteModifiesBaseBufferOnCorrectPosition()
      {
         _baseBuffer.Position = 5;

         ByteBuffer slice = _baseBuffer.Slice();
         slice.Put((byte) 0xFF);
         slice.Flip();
         // reading the _baseBuffer at position 5 should yield 0xFF
         _baseBuffer.Position = 5;
         Assert.AreEqual(0xFF, _baseBuffer.GetByte());
         
      }

      [Test]
      public void CanReadWriteByteArray()
      {
         _baseBuffer.Position = 5;

         ByteBuffer slice = _baseBuffer.Slice();
         byte[] data = {0xFF, 0xF0, 0xF2, 0xEE, 0x23};
         slice.Put(data, 2, 2);
         slice.Flip();

         Assert.AreEqual(2, slice.Limit);
         Assert.AreEqual(0xF2, slice.GetByte());
         Assert.AreEqual(0xEE, slice.GetByte());
      }

      [Test]
      [ExpectedException(typeof(BufferOverflowException))]
      public void ThrowWhenWritePastLimit()
      {
         _baseBuffer.Position = 5;

         ByteBuffer slice = _baseBuffer.Slice();
         slice.Put(0x01).Put(0x02);
      }


      [Test]
      [ExpectedException(typeof(NotSupportedException))]
      public void ThrowOnCompact()
      {
         // we don't support compacting
         ByteBuffer slice = _baseBuffer.Slice();
         slice.Compact();
      }

      [Test]
      [ExpectedException(typeof(NotSupportedException))]
      public void ThrowOnResize()
      {
         // we don't support resizing
         ByteBuffer slice = _baseBuffer.Slice();
         slice.Expand(50);
      }
   } // class SlicedByteBufferTests
}
