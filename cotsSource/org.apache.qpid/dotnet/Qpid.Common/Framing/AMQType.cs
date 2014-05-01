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
using System.Text;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Framing
{
   /// <summary>
   /// Base class for the Field Table Type system.
   /// Ported over from the Java AMQType enumeration
   /// </summary>
   public abstract class AMQType
   {
      private byte _identifier;

      /// <summary>
      /// Type code identifier for this type
      /// </summary>
      public byte Identifier
      {
         get { return _identifier; }
      }

      protected AMQType(char identifier)
      {
         _identifier = (byte)identifier;
      }

      /// <summary>
      /// Create a new <see cref="AMQTypedValue"/> instance
      /// </summary>
      /// <param name="value">Value to initialize with</param>
      /// <returns>A new typed value instance</returns>
      public AMQTypedValue AsTypedValue(object value)
      {
         return new AMQTypedValue(this, ToNativeValue(value));
      }

      /// <summary>
      /// Write the specified value to the buffer using the encoding
      /// specified for this type
      /// </summary>
      /// <param name="value">Value to write</param>
      /// <param name="buffer">Buffer to write to</param>
      public void WriteToBuffer(object value, ByteBuffer buffer)
      {
         buffer.Put(Identifier);
         WriteValueImpl(value, buffer);
      }

      public override string ToString()
      {
         return ((Char) Identifier).ToString();
      }

      /// <summary>
      /// Get the encoding size for the specified value in this type format
      /// </summary>
      /// <param name="value">Value to find encoded size for</param>
      /// <returns>The encoded size</returns>
      public abstract uint GetEncodingSize(object value);
      /// <summary>
      /// Convert the specified value to this type
      /// </summary>
      /// <param name="value">Value to convert</param>
      /// <returns>The converted value</returns>
      public abstract object ToNativeValue(object value);

      /// <summary>
      /// Read a value from the specified buffer using the encoding for
      /// this type
      /// </summary>
      /// <param name="buffer">Buffer to read from</param>
      /// <returns>The value read</returns>
      public abstract object ReadValueFromBuffer(ByteBuffer buffer);

      protected abstract void WriteValueImpl(Object value, ByteBuffer buffer);


      #region Known Types
      //
      // Known Types
      //

      // long string is not defined in the proposed specification,
      // and the 'S' discriminator is left for unsigned short (16-bit) values
      public static readonly AMQType LONG_STRING = new AMQLongStringType();
      public static readonly AMQType UINT32 = new AMQUInt32Type();
      public static readonly AMQType DECIMAL = new AMQDecimalType();
      public static readonly AMQType TIMESTAMP = new AMQTimeStampType();
      public static readonly AMQType FIELD_TABLE = new AMQFieldTableType();
      public static readonly AMQType VOID = new AMQVoidType();
      public static readonly AMQType BINARY = new AMQBinaryType();
      public static readonly AMQType ASCII_STRING = new AMQAsciiStringType();
      public static readonly AMQType WIDE_STRING = new AMQWideStringType();
      public static readonly AMQType BOOLEAN = new AMQBooleanType();
      public static readonly AMQType ASCII_CHARACTER = new AMQAsciiCharType();
      public static readonly AMQType BYTE = new AMQByteType();
      public static readonly AMQType SBYTE = new AMQSByteType();
      public static readonly AMQType INT16 = new AMQInt16Type();
      public static readonly AMQType UINT16 = new AMQUInt16Type();
      public static readonly AMQType INT32 = new AMQInt32Type();
      public static readonly AMQType INT64 = new AMQInt64Type();
      public static readonly AMQType UINT64 = new AMQUInt64Type();
      public static readonly AMQType FLOAT = new AMQFloatType();
      public static readonly AMQType DOUBLE = new AMQDoubleType();

      #endregion // Known Types

      #region Type Implementation
      //
      // Type Implementation
      //

      sealed class AMQLongStringType : AMQType
      {
         public AMQLongStringType() : base('S')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedLongStringLength((string) value);
         }

         public override object ToNativeValue(object value)
         {
            if ( value == null )
               throw new ArgumentNullException("value");
            return value.ToString();
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadLongString(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteLongStringBytes(buffer, (string) value);
         }

      }

      sealed class AMQUInt32Type : AMQType
      {
         public AMQUInt32Type() : base('i')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.UnsignedIntegerLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToUInt32(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadUnsignedInteger(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteUnsignedInteger(buffer, (uint) value);
         }

      }

      sealed class AMQDecimalType : AMQType
      {
         public AMQDecimalType() : base('D')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            throw new NotImplementedException();
         }

         public override object ToNativeValue(object value)
         {
            throw new NotImplementedException();
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            throw new NotImplementedException();
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            throw new NotImplementedException();
         }
      }

      sealed class AMQTimeStampType : AMQType
      {
         public AMQTimeStampType() : base('T')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            throw new NotImplementedException();
         }

         public override object ToNativeValue(object value)
         {
            throw new NotImplementedException();
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            throw new NotImplementedException();
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            throw new NotImplementedException();
         }
      }

      sealed class AMQFieldTableType : AMQType
      {
         public AMQFieldTableType() : base('F')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            throw new NotImplementedException();
         }

         public override object ToNativeValue(object value)
         {
            throw new NotImplementedException();
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            throw new NotImplementedException();
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            throw new NotImplementedException();
         }
      }

      sealed class AMQVoidType : AMQType
      {
         public AMQVoidType() : base('V')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return 0;
         }

         public override object ToNativeValue(object value)
         {
            if ( value != null )
               throw new FormatException(string.Format("Cannot convert {0} to VOID type", value));
            return null;
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return null;
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
         }
      }

      // Extended Types

      sealed class AMQBinaryType : AMQType
      {
         public AMQBinaryType() : base('x')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedLongstrLength((byte[]) value);
         }

         public override object ToNativeValue(object value)
         {
            if ( value is byte[] || value == null )
            {
               return value;
            }
            throw new ArgumentException("Value cannot be converted to byte[]");
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadLongstr(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteLongstr(buffer, (byte[])value);
         }
      }
      
      sealed class AMQAsciiStringType : AMQType
      {
         public AMQAsciiStringType() : base('c')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedAsciiStringLength((string)value);
         }

         public override object ToNativeValue(object value)
         {
            if ( value == null )
               throw new ArgumentNullException("value");
            return value.ToString();
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadAsciiString(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteAsciiString(buffer, (string)value);
         }
      }

      sealed class AMQWideStringType : AMQType
      {
         // todo: Change encoding to UTF16 (java code still uses default 
         // ascii encoding for wide strings
         private static readonly Encoding ENCODING = Encoding.ASCII;

         public AMQWideStringType()
            : base('C')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedLongStringLength((string)value, ENCODING);
         }

         public override object ToNativeValue(object value)
         {
            if ( value == null )
               throw new ArgumentNullException("value");
            return value.ToString();
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadLongString(buffer, ENCODING);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteLongStringBytes(buffer, (string)value, ENCODING);
         }
      }

      sealed class AMQBooleanType : AMQType
      {
         public AMQBooleanType() : base('t')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedBooleanLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToBoolean(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadBoolean(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteBoolean(buffer, (bool)value);
         }
      }

      sealed class AMQAsciiCharType : AMQType
      {
         public AMQAsciiCharType() : base('k')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedCharLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToChar(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadChar(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteChar(buffer, (char)value);
         }
      }

      sealed class AMQByteType : AMQType
      {
         public AMQByteType() : base('B')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedByteLength();
         }

         public override object ToNativeValue(object value)
         {
            return  Convert.ToByte(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadByte(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteByte(buffer, (byte)value);
         }
      }

      sealed class AMQSByteType : AMQType
      {
         public AMQSByteType()
            : base('b')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedSByteLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToSByte(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadSByte(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteSByte(buffer, (sbyte)value);
         }
      }

      sealed class AMQInt16Type : AMQType
      {
         public AMQInt16Type() : base('s')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedShortLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToInt16(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadShort(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteShort(buffer, (short)value);
         }
      }

      sealed class AMQUInt16Type : AMQType
      {
         public AMQUInt16Type()
            : base('S')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedUnsignedShortLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToUInt16(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadUnsignedShort(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteUnsignedShort(buffer, (ushort)value);
         }
      }

      sealed class AMQInt32Type : AMQType
      {
         public AMQInt32Type() : base('I')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedIntegerLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToInt32(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadInteger(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteInteger(buffer, (int)value);
         }
      }

      sealed class AMQInt64Type : AMQType
      {
         public AMQInt64Type() : base('l')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedLongLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToInt64(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadLong(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteLong(buffer, (long)value);
         }
      }

      sealed class AMQUInt64Type : AMQType
      {
         public AMQUInt64Type()
            : base('L')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedUnsignedLongLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToUInt64(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadUnsignedLong(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteUnsignedLong(buffer, (ulong)value);
         }
      }

      sealed class AMQFloatType : AMQType
      {
         public AMQFloatType() : base('f')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedFloatLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToSingle(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadFloat(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteFloat(buffer, (float)value);
         }
      }

      sealed class AMQDoubleType : AMQType
      {
         public AMQDoubleType() : base('d')
         {
         }

         public override uint GetEncodingSize(object value)
         {
            return EncodingUtils.EncodedDoubleLength();
         }

         public override object ToNativeValue(object value)
         {
            return Convert.ToDouble(value);
         }

         public override object ReadValueFromBuffer(ByteBuffer buffer)
         {
            return EncodingUtils.ReadDouble(buffer);
         }

         protected override void WriteValueImpl(object value, ByteBuffer buffer)
         {
            EncodingUtils.WriteDouble(buffer, (double)value);
         }
      }      

      #endregion // Type Implementation

   } // class AMQType
}
