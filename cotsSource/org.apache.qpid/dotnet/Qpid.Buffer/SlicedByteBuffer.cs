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

namespace Apache.Qpid.Buffer
{
   internal sealed class SlicedByteBuffer : ByteBuffer
   {
      private ByteBuffer _buffer;
      private int _capacity;
      private int _startPos;

      public override int Capacity
      {
         get { return _capacity; }
      }

      public override byte[] Array
      {
         get { return _buffer.Array; }
      }

      /// <summary>
      /// Initialize a new instance
      /// </summary>
      /// <param name="buffer">Underlying byte buffer</param>
      internal SlicedByteBuffer(ByteBuffer buffer)
      {
         _buffer = buffer;
         _startPos = buffer.Position;
         Position = 0;
         _capacity = buffer.Remaining;
         Limit = Capacity;
         // cannot autoexpand
         IsAutoExpand = false;
      }

      protected override void DoWrite(int position, byte value)
      {
         _buffer.Put(_startPos + position, value);
      }

      protected override void DoWrite(int position, byte[] src, int offset, int length)
      {
         _buffer.Put(_startPos + position, src, offset, length);
      }

      protected override byte DoReadByte(int position)
      {
         return _buffer.GetByte(_startPos + position);
      }

      protected override void DoReadBytes(int position, byte[] dest, int offset, int length)
      {
         _buffer.GetBytes(_startPos + position, dest, offset, length);
      }

      protected override void DoCompact()
      {
         throw new NotSupportedException();
      }

      protected override void DoResize(int newSize)
      {
         throw new NotSupportedException();
      }
   } // class SlicedByteBuffer
}
