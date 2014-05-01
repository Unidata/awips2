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
    public interface IBytesMessage : IMessage
    {
        long BodyLength { get; }

        bool ReadBoolean();
        void WriteBoolean(bool value);
        
        byte ReadByte();
        int ReadBytes(byte[] array);
        int ReadBytes(byte[] array, int length);
        void WriteByte(byte value);
        void WriteBytes(byte[] value);
        void WriteBytes(byte[] value, int offset, int length);

        char ReadChar();
        void WriteChar(char value);
        
        double ReadDouble();
        void WriteDouble(double value);

        float ReadFloat();
        void WriteFloat(float value);

        int ReadInt();
        void WriteInt(int value);

        long ReadLong();
        void WriteLong(long value);

        short ReadShort();
        void WriteShort(short value);

        short ReadSignedByte();
        void WriteSignedByte(short value);

        string ReadUTF();
        void WriteUTF(string value);

        void Reset();
    }
}
