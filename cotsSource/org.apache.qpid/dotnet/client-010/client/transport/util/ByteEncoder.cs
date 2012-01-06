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

namespace org.apache.qpid.transport.util
{
    public static class ByteEncoder
    {
        #region Endian conversion helper routines
        /// <summary>
        /// Returns the value encoded in Big Endian (PPC, XDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Big-endian encoded value.</returns>
        public static Int32 GetBigEndian(Int32 value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return SwapByteOrder(value);
            }
            return value;
        }

        /// <summary>
        /// Returns the value encoded in Big Endian (PPC, XDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Big-endian encoded value.</returns>
        public static UInt16 GetBigEndian(UInt16 value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return SwapByteOrder(value);
            }
            return value;
        }

        /// <summary>
        /// Returns the value encoded in Big Endian (PPC, XDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Big-endian encoded value.</returns>
        public static UInt32 GetBigEndian(UInt32 value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return SwapByteOrder(value);
            }
            return value;
        }

        /// <summary>
        /// Returns the value encoded in Big Endian (PPC, XDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Big-endian encoded value.</returns>
        public static long GetBigEndian(long value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return SwapByteOrder(value);
            }
            return value;
        }
        
        public static double GetBigEndian(double value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return SwapByteOrder(value);
            }
            return value;
        }        

        /// <summary>
        /// Returns the value encoded in Little Endian (x86, NDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Little-endian encoded value.</returns>
        public static Int32 GetLittleEndian(Int32 value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return value;
            }
            return SwapByteOrder(value);
        }

        /// <summary>
        /// Returns the value encoded in Little Endian (x86, NDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Little-endian encoded value.</returns>
        public static UInt32 GetLittleEndian(UInt32 value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return value;
            }
            return SwapByteOrder(value);
        }

        /// <summary>
        /// Returns the value encoded in Little Endian (x86, NDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Little-endian encoded value.</returns>
        public static UInt16 GetLittleEndian(UInt16 value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return value;
            }
            return SwapByteOrder(value);
        }

        /// <summary>
        /// Returns the value encoded in Little Endian (x86, NDR) format.
        /// </summary>
        /// <param name="value">Value to encode.</param>
        /// <returns>Little-endian encoded value.</returns>
        public static long GetLittleEndian(long value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return value;
            }
            return SwapByteOrder(value);
        }
        
        public static double GetLittleEndian(double value)
        {
            if (BitConverter.IsLittleEndian)
            {
                return value;
            }
            return SwapByteOrder(value);
        }        

        /// <summary>
        /// Swaps the Byte order of an <see cref="Int32"/>.
        /// </summary>
        /// <param name="value"><see cref="Int32"/> to swap the bytes of.</param>
        /// <returns>Byte order swapped <see cref="Int32"/>.</returns>
        private static Int32 SwapByteOrder(Int32 value)
        {
            Int32 swapped = (Int32)((0x000000FF) & (value >> 24)
                                     | (0x0000FF00) & (value >> 8)
                                     | (0x00FF0000) & (value << 8)
                                     | (0xFF000000) & (value << 24));
            return swapped;
        }

        /// <summary>
        /// Swaps the byte order of a <see cref="UInt16"/>.
        /// </summary>
        /// <param name="value"><see cref="UInt16"/> to swap the bytes of.</param>
        /// <returns>Byte order swapped <see cref="UInt16"/>.</returns>
        private static UInt16 SwapByteOrder(UInt16 value)
        {
            return (UInt16)((0x00FF & (value >> 8))
                             | (0xFF00 & (value << 8)));
        }

        /// <summary>
        /// Swaps the byte order of a <see cref="UInt32"/>.
        /// </summary>
        /// <param name="value"><see cref="UInt32"/> to swap the bytes of.</param>
        /// <returns>Byte order swapped <see cref="UInt32"/>.</returns>
        private static UInt32 SwapByteOrder(UInt32 value)
        {
            UInt32 swapped = ((0x000000FF) & (value >> 24)
                             | (0x0000FF00) & (value >> 8)
                             | (0x00FF0000) & (value << 8)
                             | (0xFF000000) & (value << 24));
            return swapped;
        }

        /// <summary>
        /// Swaps the byte order of a <see cref="Double"/> (double precision IEEE 754)
        /// </summary>
        /// <param name="value"><see cref="Double"/> to swap.</param>
        /// <returns>Byte order swapped <see cref="Double"/> value.</returns>
        private static long SwapByteOrder(long value)
        {
            Byte[] buffer = BitConverter.GetBytes(value);
            Array.Reverse(buffer, 0, buffer.Length);
            return BitConverter.ToInt64(buffer, 0);
        }
        
        private static double SwapByteOrder(double value)
        {
            Byte[] buffer = BitConverter.GetBytes(value);
            Array.Reverse(buffer, 0, buffer.Length);
            return BitConverter.ToDouble(buffer,0) ;
        }        
        #endregion
    }

}
