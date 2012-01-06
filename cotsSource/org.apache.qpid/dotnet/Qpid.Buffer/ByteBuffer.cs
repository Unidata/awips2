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
   /// <summary>
   /// Abstract class implementing a byte buffer
   /// </summary>
   public abstract class ByteBuffer
   {
      private int _position;
      private int _limit;
      private bool _isAutoExpand;
      private static IByteBufferAllocator _allocator = 
         new SimpleByteBufferAllocator();

      #region Properties
      //
      // Properties
      //

      /// <summary>
      /// The maximum number of bytes the buffer can hold
      /// </summary>
      public abstract int Capacity
      {
         get;
      }

      /// <summary>
      /// Return the backing array of this buffer
      /// </summary>
      public abstract byte[] Array
      { 
         get;
      }
      
      /// <summary>
      /// The current position inside this buffer
      /// </summary>
      public int Position
      {
         get { return _position; }
         set { Seek(value); }
      }

      /// <summary>
      /// Index of the first element that should not be read or written. 
      /// A buffer's limit is never negative and is never greater than the its capacity.
      /// </summary>
      public int Limit
      {
         get { return _limit; }
         set { SetLimit(value);  }
      }

      /// <summary>
      /// Number of bytes remaining in the buffer from the current position
      /// </summary>
      public int Remaining
      {
         get { return Limit - Position; }
      }

      /// <summary>
      /// True if there are bytes remaining in the buffer
      /// </summary>
      public bool HasRemaining
      {
         get { return Remaining > 0; }
      }

      /// <summary>
      /// If true, the buffer will be resized as necessary
      /// to allow space for writing. By default is false.
      /// </summary>
      public bool IsAutoExpand
      {
         get { return _isAutoExpand; }
         set { _isAutoExpand = value; }
      }

      #endregion // Properties

      #region Buffer Manipulation
      //
      // Buffer Manipulation
      //

      /// <summary>
      /// Move the buffer to Position 0
      /// </summary>
      /// <returns>This instance</returns>
      public ByteBuffer Rewind()
      {
         Seek(0);
         return this;
      }

      /// <summary>
      /// Prepare the buffer to read back what's been written
      /// </summary>
      /// <returns>This instance</returns>
      public ByteBuffer Flip()
      {
         Limit = Position;
         Position = 0;
         return this;
      }

      /// <summary>
      /// Compact this buffer.
      /// </summary>
      /// <returns>This instance</returns>
      /// <remarks>
      /// The bytes between the buffer's current position and its limit, if any, 
      /// are copied to the beginning of the buffer.
      /// </remarks>
      public ByteBuffer Compact()
      {
         DoCompact();
         return this;
      }

      /// <summary>
      /// Clears this buffer. The position is set to zero, the limit is set to the capacity
      /// </summary>
      /// <returns>This instance</returns>
      public ByteBuffer Clear()
      {
         Limit = Capacity;
         Position = 0;
         return this;
      }

      /// <summary>
      /// Expands this buffer's capacity so that 
      /// Remaining == expectedRemaining
      /// </summary>
      /// <param name="expectedRemaining">Number of bytes that should be accessable after resizing</param>
      /// <returns>This instance</returns>
      public ByteBuffer Expand(int expectedRemaining)
      {
         return Expand(Position, expectedRemaining);
      }

      /// <summary>
      /// Expands this buffer's capacity so that 
      /// Remaining == expectedRemaining
      /// </summary>
      /// <param name="position">Position from which to start the resize</param>
      /// <param name="expectedRemaining">Number of bytes that should be accessable after resizing</param>
      /// <returns>This instance</returns>
      public ByteBuffer Expand(int position, int expectedRemaining)
      {
         if ( expectedRemaining <= 0 )
            throw new ArgumentException("expectedRemaining must be greater than 0");

         int end = position + expectedRemaining;
         if ( end > Capacity )
         {
            DoResize(end);
         }
         if ( end > Limit )
            Limit = end;
         return this;
      }

      /// <summary>
      /// Creates a new byte buffer whose content is a shared 
      /// subsequence of this buffer's content.
      /// </summary>
      /// <remarks>
      ///  The content of the new buffer will start at this buffer's current position. 
      /// Changes to this buffer's content will be visible in the new buffer, 
      /// and vice versa; the two buffers' position and limit values will be independent.
      /// <para>
      /// The new buffer's position will be zero, its capacity and its limit will 
      /// be the number of bytes remaining in this buffer.
      /// </para>
      /// </remarks>
      /// <returns>A view on top of this instance</returns>
      public ByteBuffer Slice()
      {
         return new SlicedByteBuffer(this);
      }

      /// <summary>
      /// Skip the specified number of bytes
      /// </summary>
      /// <param name="numBytes">Number of bytes to move forward by</param>
      /// <returns>This instance</returns>
      public ByteBuffer Skip(int numBytes)
      {
         Position += numBytes;
         return this;
      }

      /// <summary>
      /// Acquire this buffer to keep it alive.
      /// </summary>
      public virtual void Acquire()
      {
         // override in subclass if supported
      }

      /// <summary>
      /// Release this buffer instance
      /// </summary>
      public virtual void Release()
      {
         // override in subclass if supported
      }

      /// <summary>
      /// Return a string with a Hex Dump of this buffer's contents
      /// </summary>
      /// <returns>The hex dump</returns>
      public string GetHexDump()
      {
         return ByteBufferHexDumper.GetHexDump(this);
      }

      public override string ToString()
      {
         return GetHexDump();
      }
      #endregion // Buffer Manipulation

      #region Static Operations
      //
      // Static Operations
      //
      /// <summary>
      /// Replaces the default allocator with your own implementation
      /// </summary>
      /// <param name="allocator">New allocator</param>
      public static void SetAllocator(IByteBufferAllocator allocator)
      {
         if ( allocator == null )
            throw new ArgumentNullException("allocator");
         _allocator = allocator;
      }

      /// <summary>
      /// Allocate a new buffer with the specified capacity
      /// using the default allocator
      /// </summary>
      /// <param name="capacity">Desired capacity</param>
      /// <returns>The new buffer</returns>
      public static ByteBuffer Allocate(int capacity)
      {
         return _allocator.Allocate(capacity);
      }

      /// <summary>
      /// Wraps the specified arrat into a new buffer
      /// </summary>
      /// <param name="buffer"></param>
      /// <returns></returns>
      public static ByteBuffer Wrap(byte[] buffer)
      {
         return _allocator.Wrap(buffer);
      }
      #endregion // Static Operations

      #region Data Accessors
      //
      // Data Accessors
      //
      
      // Byte Stuff

      /// <summary>
      /// Read the next byte in the buffer
      /// </summary>
      /// <returns>The next byte available</returns>
      public byte GetByte()
      {
         byte value = GetByte(Position);
         Position += 1;
         return value;
      }
      /// <summary>
      /// Read the byte at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public byte GetByte(int position)
      {
         CheckSpaceForReading(position, 1);
         return ReadByte(position);
      }
      /// <summary>
      /// Write a byte at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(byte value)
      {
         Put(Position, value);
         Position++;
         return this;
      }
      /// <summary>
      /// Write a byte at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, byte value)
      {
         CheckSpaceForWriting(position, 1);
         Write(position, value);
         return this;
      }

      // SByte Stuff

      /// <summary>
      /// Read the next signed byte in the buffer
      /// </summary>
      /// <returns>The next signed byte available</returns>
      public sbyte GetSByte()
      {
         sbyte value = GetSByte(Position);
         Position += 1;
         return value;
      }
      /// <summary>
      /// Read the signed byte at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public sbyte GetSByte(int position)
      {
         CheckSpaceForReading(position, 1);
         return (sbyte)ReadByte(position);
      }

      /// <summary>
      /// Write a signed byte at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(sbyte value)
      {
         Put(Position, value);
         Position += 1;
         return this;
      }

      /// <summary>
      /// Write a signed byte at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, sbyte value)
      {
         CheckSpaceForWriting(position, 1);
         Write(position, (byte)value);
         return this;
      }

      // UInt16 Stuff

      /// <summary>
      /// Read the next uint16 in the buffer
      /// </summary>
      /// <returns>The next uint16 available</returns>
      public ushort GetUInt16()
      {
         ushort value = GetUInt16(Position);
         Position += 2;
         return value;
      }
      /// <summary>
      /// Read the uint16 at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public ushort GetUInt16(int position)
      {
         CheckSpaceForReading(position, 2);
         byte upper = ReadByte(position);
         byte lower = ReadByte(position+1);
         return (ushort)(((ushort)upper << 8) + lower);
      }

      /// <summary>
      /// Write a uint16 at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(ushort value)
      {
         Put(Position, value);
         Position += 2;
         return this;
      }

      /// <summary>
      /// Write a uint16 at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, ushort value)
      {
         CheckSpaceForWriting(position, 2);
         Write(position, (byte)(value >> 8));
         Write(position+1, (byte)(value));
         return this;
      }

      // Int16 Stuff

      /// <summary>
      /// Read the next int16 in the buffer
      /// </summary>
      /// <returns>The next int16 available</returns>
      public short GetInt16()
      {
         return (short) GetUInt16();
      }
      /// <summary>
      /// Read the int16 at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public short GetInt16(int position)
      {
         return (short)GetUInt16(position);
      }

      /// <summary>
      /// Write a int16 at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(short value)
      {
         return Put((ushort) value);
      }

      /// <summary>
      /// Write a int16 at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, short value)
      {
         return Put(position, (ushort)value);
      }


      // UInt32 Stuff

      /// <summary>
      /// Read the next uint32 in the buffer
      /// </summary>
      /// <returns>The next uint32 available</returns>
      public uint GetUInt32()
      {
         uint value = GetUInt32(Position);
         Position += 4;
         return value;
      }
      /// <summary>
      /// Read the uint32 at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public uint GetUInt32(int position)
      {
         CheckSpaceForReading(position, 4);
         byte b1 = ReadByte(position);
         byte b2 = ReadByte(position + 1);
         byte b3 = ReadByte(position + 2);
         byte b4 = ReadByte(position + 3);
         return (uint)((b1 << 24) + (b2 << 16) + (b3 << 8) + b4);
      }

      /// <summary>
      /// Write a uint32 at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(uint value)
      {
         Put(Position, value);
         Position += 4;
         return this;
      }

      /// <summary>
      /// Write a uint32 at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, uint value)
      {
         CheckSpaceForWriting(position, 4);
         Write(position, (byte)(value >> 24));
         Write(position + 1, (byte)(value >> 16));
         Write(position + 2, (byte)(value >> 8));
         Write(position + 3, (byte)(value));
         return this;
      }

      // Int32 Stuff

      /// <summary>
      /// Read the next int32 in the buffer
      /// </summary>
      /// <returns>The next int32 available</returns>
      public int GetInt32()
      {
         return (int)GetUInt32();
      }
      /// <summary>
      /// Read the int32 at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public int GetInt32(int position)
      {
         return (int)GetUInt32(position);
      }

      /// <summary>
      /// Write a int32 at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int value)
      {
         return Put((uint)value);
      }

      /// <summary>
      /// Write a int32 at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, int value)
      {
         return Put(position, (uint)value);
      }

      // UInt64 Stuff

      /// <summary>
      /// Read the next uint64 in the buffer
      /// </summary>
      /// <returns>The next uint64 available</returns>
      public ulong GetUInt64()
      {
         ulong value = GetUInt64(Position);
         Position += 8;
         return value;
      }
      /// <summary>
      /// Read the uint64 at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public ulong GetUInt64(int position)
      {
         CheckSpaceForReading(position, 8);
         byte b1 = ReadByte(position);
         byte b2 = ReadByte(position + 1);
         byte b3 = ReadByte(position + 2);
         byte b4 = ReadByte(position + 3);
         byte b5 = ReadByte(position + 4);
         byte b6 = ReadByte(position + 5);
         byte b7 = ReadByte(position + 6);
         byte b8 = ReadByte(position + 7);
         // all the casts necessary because otherwise each subexpression
         // only gets promoted to uint and cause incorrect results
         return (((ulong)b1 << 56) + ((ulong)b2 << 48) + ((ulong)b3 << 40) +
            ((ulong)b4 << 32) + ((ulong)b5 << 24) +
            ((ulong)b6 << 16) + ((ulong)b7 << 8) + b8);
      }

      /// <summary>
      /// Write a uint64 at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(ulong value)
      {
         Put(Position, value);
         Position += 8;
         return this;
      }

      /// <summary>
      /// Write a uint64 at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, ulong value)
      {
         CheckSpaceForWriting(position, 8);
         Write(position, (byte)(value >> 56));
         Write(position + 1, (byte)(value >> 48));
         Write(position + 2, (byte)(value >> 40));
         Write(position + 3, (byte)(value >> 32));
         Write(position + 4, (byte)(value >> 24));
         Write(position + 5, (byte)(value >> 16));
         Write(position + 6, (byte)(value >> 8));
         Write(position + 7, (byte)(value));
         return this;
      }

      // Int64 Stuff

      /// <summary>
      /// Read the next int64 in the buffer
      /// </summary>
      /// <returns>The next int64 available</returns>
      public long GetInt64()
      {
         return (long)GetUInt64();
      }
      /// <summary>
      /// Read the int64 at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public long GetInt64(int position)
      {
         return (long)GetUInt64(position);
      }

      /// <summary>
      /// Write a int64 at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(long value)
      {
         return Put((ulong)value);
      }

      /// <summary>
      /// Write a int64 at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, long value)
      {
         return Put(position, (ulong)value);
      }


      // Float Stuff

      /// <summary>
      /// Read the next float in the buffer
      /// </summary>
      /// <returns>The next float available</returns>
      public float GetFloat()
      {
         unsafe
         {
            uint val = GetUInt32();
            return *((float*)&val);
         }
      }
      /// <summary>
      /// Read the float at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public float GetFloat(int position)
      {
         unsafe
         {
            uint val = GetUInt32(position);
            return *((float*)&val);
         }
      }

      /// <summary>
      /// Write a float at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(float value)
      {
         unsafe
         {
            uint val = *((uint*)&value);
            return Put(val);
         }
      }

      /// <summary>
      /// Write a float at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, float value)
      {
         unsafe
         {
            uint val = *((uint*)&value);
            return Put(position, val);
         }
      }

      // Double Stuff

      /// <summary>
      /// Read the next double in the buffer
      /// </summary>
      /// <returns>The next double available</returns>
      public double GetDouble()
      {
         unsafe
         {
            ulong val = GetUInt64();
            return *((double*)&val);
         }
      }
      /// <summary>
      /// Read the double at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public double GetDouble(int position)
      {
         unsafe
         {
            ulong val = GetUInt64(position);
            return *((double*)&val);
         }
      }

      /// <summary>
      /// Write a double at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(double value)
      {
         unsafe
         {
            ulong val = *((ulong*)&value);
            return Put(val);
         }
      }

      /// <summary>
      /// Write a double at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, double value)
      {
         unsafe
         {
            ulong val = *((ulong*)&value);
            return Put(position, val);
         }
      }

      // Char Stuff

      /// <summary>
      /// Read the next char in the buffer
      /// </summary>
      /// <returns>The next char available</returns>
      public char GetChar()
      {
         return (char)GetUInt16();
      }
      /// <summary>
      /// Read the char at the specified position
      /// </summary>
      /// <param name="position">Position to read from</param>
      /// <returns>The value at the position</returns>
      public char GetChar(int position)
      {
         return (char)GetUInt16(position);
      }

      /// <summary>
      /// Write a char at the current position
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(char value)
      {
         return Put((ushort) value);
      }

      /// <summary>
      /// Write a char at the specified position
      /// </summary>
      /// <param name="position">Position to write to</param>
      /// <param name="value">Value to write</param>
      /// <returns>This instance</returns>
      public ByteBuffer Put(int position, char value)
      {
         return Put(position, (ushort)value);
      }

      // Byte[] stuff

      public void GetBytes(byte[] buffer)
      {
         GetBytes(buffer, 0, buffer.Length);
      }

      public void GetBytes(byte[] buffer, int offset, int length)
      {
         GetBytes(Position, buffer, offset, length);
         Position += length;
      }
      public void GetBytes(int position, byte[] buffer, int offset, int length)
      {
         CheckSpaceForReading(position, length);
         if ( offset + length > buffer.Length )
            throw new ArgumentException("Invalid offset +  length");
         ReadBytes(position, buffer, offset, length);
      }

      public ByteBuffer Put(byte[] buffer)
      {
         return Put(buffer, 0, buffer.Length);
      }

      public ByteBuffer Put(byte[] buffer, int offset, int length)
      {
         Put(Position, buffer, offset, length);
         Position += length;
         return this;
      }

      public ByteBuffer Put(int position, byte[] buffer, int offset, int length)
      {
         CheckSpaceForWriting(position, length);
         if ( offset + length > buffer.Length )
            throw new ArgumentException("Invalid offset +  length");

         Write(position, buffer, offset, length);
         return this;
      }

      public ByteBuffer Put(ByteBuffer data)
      {
         Put(Position, data);
         Position += data.Remaining;
         return this;
      }

      public ByteBuffer Put(int position, ByteBuffer data)
      {
         CheckSpaceForWriting(position, data.Remaining);
         Write(position, data.Array, data.Position, data.Remaining);
         return this;
      }

      #endregion // Data Accessors

      #region Core Overrides
      //
      // Core Overrides
      //

      protected abstract void DoWrite(int position, byte value);
      protected abstract void DoWrite(int position, byte[] src, int offset, int length);
      protected abstract byte DoReadByte(int position);
      protected abstract void DoReadBytes(int position, byte[] dest, int offset, int length);
      protected abstract void DoCompact();
      protected abstract void DoResize(int newSize);

      #endregion // Core Overrides

      #region Private Methods
      //
      // Private Methods
      //

      private void Seek(int offset)
      {
         if ( offset > Capacity )
            throw new ArgumentException("Cannot position beyond end of buffer");
         _position = offset;
         AdjustLimit();
      }

      private void SetLimit(int newLimit)
      {
         if ( newLimit < 0 )
            throw new ArgumentOutOfRangeException("The new limit must be a positive value");
         if ( newLimit > Capacity )
            throw new ArgumentOutOfRangeException("The new limit must not be greater than the capacity");
         _limit = newLimit;
         if ( _position > newLimit )
            _position = newLimit;
      }

      private void AdjustLimit()
      {
         if ( _limit < _position )
            _limit = _position;
      }

      private void CheckSpaceForReading(int position, int length)
      {
         if ( position + length > Limit )
         {
            throw new BufferUnderflowException("Attempt to read " + length + " byte(s) to buffer where position is " + position +
                                               " and limit is " + Limit);
         }
      }
      
      private void CheckSpaceForWriting(int position, int length)
      {
         if ( IsAutoExpand )
         {
            Expand(position, length);
         }
         if ( position + length > Limit )
         {
            throw new BufferOverflowException("Attempt to write " + length + " byte(s) to buffer where position is " + position +
                                              " and limit is " + Limit);
         }
      }

      private void Write(int position, byte value)
      {
         DoWrite(position, value);
      }
      private void Write(int position, byte[] src, int offset, int length)
      {
         DoWrite(position, src, offset, length);
      }
      private byte ReadByte(int position)
      {
         return DoReadByte(position);
      }
      private void ReadBytes(int position, byte[] dest, int offset, int length)
      {
         DoReadBytes(position, dest, offset, length);
      }

      #endregion // Private Methods

   } // class ByteBuffer
}
