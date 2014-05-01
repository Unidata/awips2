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
   internal sealed class SimpleByteBuffer : ByteBuffer
   {
      private byte[] _buffer;

      public override int Capacity
      {
         get { return _buffer.Length; }
      }

      public override byte[] Array
      {
         get { return _buffer; }
      }

      /// <summary>
      /// Initialize a new instance with the desired size
      /// </summary>
      /// <param name="desiredSize">Initial Length of the array</param>
      internal SimpleByteBuffer(int desiredSize)
      {
         _buffer = new byte[desiredSize];
         Position = 0;
         Limit = Capacity;
      }

      /// <summary>
      /// Initialize a new instance with the data from
      /// an underlying array
      /// </summary>
      /// <param name="buffer">Initial data</param>
      /// <remarks>The original array is copied during construction and is not modified</remarks>
      internal SimpleByteBuffer(byte[] buffer)
      {
         _buffer = (byte[])buffer.Clone();
         // position at end
         Position = Limit = Capacity;
      }

      protected override void DoWrite(int position, byte value)
      {
         // available space is already handled by base class
         _buffer[position] = value;
      }

      protected override void DoWrite(int position, byte[] src, int offset, int length)
      {
         // available space is already handled by base class
         for ( int i = 0; i < length; i++ )
         {
            _buffer[position+i] = src[offset+i];
         }
      }

      protected override byte DoReadByte(int position)
      {
         return _buffer[position];
      }

      protected override void DoReadBytes(int position, byte[] dest, int offset, int length)
      {
         System.Array.Copy(_buffer, position, dest, offset, length);
      }

      protected override void DoCompact()
      {
         if ( Remaining > 0 )
         {
            if ( Position > 0 )
            {
               System.Array.Copy(_buffer, Position, _buffer, 0, Remaining);
            }
            Position = Remaining;
         } else
         {
            Position = 0;
         }
         Limit = Capacity;
      }

      protected override void DoResize(int newSize)
      {
         if ( newSize < Capacity )
            throw new NotSupportedException("Cannot resize a buffer to make it smaller");

         int newCapacity = 1;
         while ( newCapacity < newSize )
         {
            newCapacity <<= 1;
         }

         byte[] newBuffer = new byte[newCapacity];
         System.Array.Copy(_buffer, newBuffer, _buffer.Length);
         _buffer = newBuffer;
      }
   } // class SimpleByteBuffer
}
