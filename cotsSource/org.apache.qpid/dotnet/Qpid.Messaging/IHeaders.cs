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
namespace Apache.Qpid.Messaging
{
    /// <summary>
    /// IHeaders represents the header fields of an AMQ message and provides methods to access those fields. There are accessor methods to
    /// get and set each header field for each supported header field data type.
    /// 
    /// <para/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th>Responsibilities</th></tr>
    /// <tr><td>Provide accessors for all supported header field types.</td></tr>
    /// <tr><td>Check if a set of headers contains a named property.</td></tr>
    /// </table>
    /// 
    /// </summary>
    public interface IHeaders
    {
        bool Contains(string name);

        object this[string name] { get; set; }
        
        bool GetBoolean(string name);
        void SetBoolean(string name, bool value);

        byte GetByte(string name);
        void SetByte(string name, byte value);

        //sbyte GetSByte(string name);
        //void SetSByte(string name, sbyte value);

        short GetShort(string name);
        void SetShort(string name, short value);

        int GetInt(string name);
        void SetInt(string name, int value);

        long GetLong(string name);
        void SetLong(string name, long value);

        float GetFloat(string name);
        void SetFloat(string name, float value);

        double GetDouble(string name);
        void SetDouble(string name, double value);

        string GetString(string name);
        void SetString(string name, string value);
    }
}
