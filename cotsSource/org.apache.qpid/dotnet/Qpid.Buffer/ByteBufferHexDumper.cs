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
using System.Text;

namespace Apache.Qpid.Buffer
{
    public class ByteBufferHexDumper
    {
        private static byte[] highDigits;
        
        private static byte[] lowDigits;
        
        static ByteBufferHexDumper()
        {
            byte[] digits = { (byte)'0', (byte)'1', (byte)'2', (byte)'3', (byte)'4', (byte)'5', (byte)'6',
                              (byte)'7', (byte)'8', (byte)'9', (byte)'A', (byte)'B', (byte)'C', (byte)'D', 
                              (byte)'E', (byte)'F' };
            int i;
            byte[] high = new byte[256];
            byte[] low = new byte[256];
            
            for (i = 0; i < 256; i++)
            {
                high[i] = digits[i >> 4];
                low[i] = digits[i & 0x0F];                
            }
            
            highDigits = high;
            lowDigits = low;
        }
        
        public static string GetHexDump(ByteBuffer input)
        {
            int size = input.Remaining;
            if (size == 0)
            {
                return "empty";
            }
            
            StringBuilder output = new StringBuilder(size * 3 - 1);

            byte[] data = input.Array;
            int byteValue = data[0] & 0xFF;
            output.Append((char) highDigits[byteValue]);
            output.Append((char) lowDigits[byteValue]);            
            
            for (int i = 1 ; i < size; i++)
            {
                output.Append(' ');
                byteValue = data[i] & 0xFF;
                output.Append((char) highDigits[byteValue]);
                output.Append((char) lowDigits[byteValue]);
            }
            
            return output.ToString();
        }
    }
}



