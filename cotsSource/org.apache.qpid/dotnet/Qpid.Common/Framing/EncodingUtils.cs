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
using System.Globalization;
using System.Text;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Framing
{
   public class EncodingUtils
   {
      private static readonly Encoding DEFAULT_ENCODER = Encoding.ASCII;

      // SHORT STRING
      public static ushort EncodedShortStringLength(string s)
      {
         if ( s == null )
         {
            return 1;
         } else
         {
            return (ushort)(1 + s.Length);
         }
      }
      public static void WriteShortStringBytes(ByteBuffer buffer, string s)
      {
         if ( s != null )
         {
            //try
            //{
            //final byte[] encodedString = s.getBytes(STRING_ENCODING);
            byte[] encodedString;
            lock ( DEFAULT_ENCODER )
            {
               encodedString = DEFAULT_ENCODER.GetBytes(s);
            }
            // TODO: check length fits in an unsigned byte
            buffer.Put((byte)encodedString.Length);
            buffer.Put(encodedString);

         } else
         {
            // really writing out unsigned byte
            buffer.Put((byte)0);
         }
      }

      // ASCII STRINGS
      public static uint EncodedAsciiStringLength(string s)
      {
         // TODO: move this to 2-byte length once the proposed encodings
         // have been approved. Also, validate length!
         if ( s == null )
            return 4;
         else 
            return (uint) (4 + s.Length);
      }
      public static string ReadAsciiString(ByteBuffer buffer)
      {
         return ReadLongString(buffer, DEFAULT_ENCODER);
      }
      public static void WriteAsciiString(ByteBuffer buffer, string s)
      {
         WriteLongStringBytes(buffer, s, DEFAULT_ENCODER);
      }

      // LONG STRING
      public static uint EncodedLongStringLength(string s)
      {
         return EncodedLongStringLength(s, DEFAULT_ENCODER);
      }

      public static uint EncodedLongStringLength(string s, Encoding encoding)
      {
         if ( s == null )
         {
            return 4;
         } else
         {
            return (uint)(4 + encoding.GetByteCount(s));
         }
      }
      public static string ReadLongString(ByteBuffer buffer)
      {
         return ReadLongString(buffer, DEFAULT_ENCODER);
      }
      public static string ReadLongString(ByteBuffer buffer, Encoding encoding)
      {
         uint length = buffer.GetUInt32();
         if ( length == 0 )
         {
            return null;
         } else
         {
            byte[] data = new byte[length];
            buffer.GetBytes(data);
            lock ( encoding )
            {
               return encoding.GetString(data);
            }
         }
      }
      public static void WriteLongStringBytes(ByteBuffer buffer, string s)
      {
         WriteLongStringBytes(buffer, s, DEFAULT_ENCODER);
      }

      public static void WriteLongStringBytes(ByteBuffer buffer, string s, Encoding encoding)
      {
         if ( !(s == null || s.Length <= 0xFFFE) )
         {
            throw new ArgumentException("String too long");
         }
         if ( s != null )
         {
            lock ( encoding )
            {
               byte[] encodedString = encoding.GetBytes(s);
               buffer.Put((uint)encodedString.Length);
               buffer.Put(encodedString);
            }
         } else
         {
            buffer.Put((uint)0);
         }
      }

      // BINARY
      public static uint EncodedLongstrLength(byte[] bytes)
      {
         if ( bytes == null )
         {
            return 4;
         } else
         {
            return (uint)(4 + bytes.Length);
         }
      }
      public static byte[] ReadLongstr(ByteBuffer buffer)
      {
         uint length = buffer.GetUInt32();
         if ( length == 0 )
         {
            return null;
         } else
         {
            byte[] result = new byte[length];
            buffer.GetBytes(result);
            return result;
         }
      }
      public static void WriteLongstr(ByteBuffer buffer, byte[] data)
      {
         if ( data != null )
         {
            buffer.Put((uint)data.Length);
            buffer.Put(data);
         } else
         {
            buffer.Put((uint)0);
         }
      }

      // BOOLEANS
      public static bool[] ReadBooleans(ByteBuffer buffer)
      {
         byte packedValue = buffer.GetByte();
         bool[] result = new bool[8];

         for ( int i = 0; i < 8; i++ )
         {
            result[i] = ((packedValue & (1 << i)) != 0);
         }
         return result;
      }
      public static void WriteBooleans(ByteBuffer buffer, bool[] values)
      {
         byte packedValue = 0;
         for ( int i = 0; i < values.Length; i++ )
         {
            if ( values[i] )
            {
               packedValue = (byte)(packedValue | (1 << i));
            }
         }

         buffer.Put(packedValue);
      }

      // FIELD TABLES
      public static uint EncodedFieldTableLength(FieldTable table)
      {
         if ( table == null )
         {
            // size is encoded as 4 octets
            return 4;
         } else
         {
            // size of the table plus 4 octets for the size
            return table.EncodedSize + 4;
         }
      }
      /// <summary>
      /// Reads the field table using the data in the specified buffer
      /// </summary>
      /// <param name="buffer">The buffer to read from.</param>
      /// <returns>a populated field table</returns>
      /// <exception cref="AMQFrameDecodingException">if the buffer does not contain a decodable field table</exception>
      public static FieldTable ReadFieldTable(ByteBuffer buffer)
      {
         uint length = buffer.GetUInt32();
         if ( length == 0 )
         {
            return null;
         } else
         {
            return new FieldTable(buffer, length);
         }
      }
      public static void WriteFieldTableBytes(ByteBuffer buffer, FieldTable table)
      {
         if ( table != null )
         {
            table.WriteToBuffer(buffer);
         } else
         {
            buffer.Put((uint)0);
         }
      }


      /// <summary>
      /// Read a short string from the buffer
      /// </summary>
      /// <param name="buffer">The buffer to read from.</param>
      /// <returns>a string</returns>
      /// <exception cref="AMQFrameDecodingException">if the buffer does not contain a decodable short string</exception>
      public static string ReadShortString(ByteBuffer buffer)
      {
         byte length = buffer.GetByte();
         if ( length == 0 )
         {
            return null;
         } else
         {
            byte[] data = new byte[length];
            buffer.GetBytes(data);

            lock ( DEFAULT_ENCODER )
            {
               return DEFAULT_ENCODER.GetString(data);
            }
         }
      }



      // BOOLEAN
      public static uint EncodedBooleanLength()
      {
         return 1;
      }
      public static bool ReadBoolean(ByteBuffer buffer)
      {
         byte packedValue = buffer.GetByte();
         return (packedValue == 1);
      }
      public static void WriteBoolean(ByteBuffer buffer, bool value)
      {
         buffer.Put((byte)(value ? 1 : 0));
      }


      // CHAR
      public static uint EncodedCharLength()
      {
         return EncodedByteLength();
      }
      public static char ReadChar(ByteBuffer buffer)
      {
         return (char)buffer.GetByte();
      }
      public static void WriteChar(ByteBuffer buffer, char value)
      {
         buffer.Put((byte)value);
      }

      // BYTE
      public static uint EncodedByteLength()
      {
         return 1;
      }
      public static byte ReadByte(ByteBuffer buffer)
      {
         return buffer.GetByte();
      }
      public static void WriteByte(ByteBuffer buffer, byte value)
      {
         buffer.Put(value);
      }

      // SBYTE
      public static uint EncodedSByteLength()
      {
         return 1;
      }
      public static sbyte ReadSByte(ByteBuffer buffer)
      {
         return buffer.GetSByte();
      }
      public static void WriteSByte(ByteBuffer buffer, sbyte value)
      {
         buffer.Put(value);
      }

      // INT16
      public static uint EncodedShortLength()
      {
         return 2;
      }

      public static short ReadShort(ByteBuffer buffer)
      {
         return buffer.GetInt16();
      }
      public static void WriteShort(ByteBuffer buffer, short value)
      {
         buffer.Put(value);
      }

      // UINT16
      public static uint EncodedUnsignedShortLength()
      {
         return 2;
      }

      public static ushort ReadUnsignedShort(ByteBuffer buffer)
      {
         return buffer.GetUInt16();
      }
      public static void WriteUnsignedShort(ByteBuffer buffer, ushort value)
      {
         buffer.Put(value);
      }


      // INT32
      public static uint EncodedIntegerLength()
      {
         return 4;
      }
      public static int ReadInteger(ByteBuffer buffer)
      {
         return buffer.GetInt32();
      }
      public static void WriteInteger(ByteBuffer buffer, int value)
      {
         buffer.Put(value);
      }

      // UINT32
      public static uint UnsignedIntegerLength()
      {
         return 4;
      }
      public static void WriteUnsignedInteger(ByteBuffer buffer, uint value)
      {
         buffer.Put(value);
      }
      public static uint ReadUnsignedInteger(ByteBuffer buffer)
      {
         return buffer.GetUInt32();
      }

      // INT64
      public static uint EncodedUnsignedLongLength()
      {
         return 8;
      }
      public static ulong ReadUnsignedLong(ByteBuffer buffer)
      {
         return buffer.GetUInt64();
      }
      public static void WriteUnsignedLong(ByteBuffer buffer, ulong value)
      {
         buffer.Put(value);
      }
      
      // UINT64
      public static uint EncodedLongLength()
      {
         return 8;
      }
      public static long ReadLong(ByteBuffer buffer)
      {
         return buffer.GetInt64();
      }
      public static void WriteLong(ByteBuffer buffer, long value)
      {
         buffer.Put(value);
      }

      // FLOAT
      public static uint EncodedFloatLength()
      {
         return 4;
      }
      public static void WriteFloat(ByteBuffer buffer, float value)
      {
         buffer.Put(value);
      }
      public static float ReadFloat(ByteBuffer buffer)
      {
         return buffer.GetFloat();
      }

      // DOUBLE
      public static uint EncodedDoubleLength()
      {
         return 8;
      }
      public static void WriteDouble(ByteBuffer buffer, double value)
      {
         buffer.Put(value);
      }
      public static double ReadDouble(ByteBuffer buffer)
      {
         return buffer.GetDouble();
      }

      // OTHER
      public static long ReadLongAsShortString(ByteBuffer buffer)
      {
         string value = ReadShortString(buffer);
         if ( value == null || value.Length == 0 )
            return 0L;
         return Convert.ToInt64(value, CultureInfo.InvariantCulture);
      }

   }

}
