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
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Framing
{
   public class AMQTypedValue
   {
      private readonly AMQType _type;
      private readonly object _value;

      public AMQType Type
      {
         get { return _type; }
      }

      public object Value
      {
         get { return _value; }
      }

      public uint EncodingLength
      {
         get { return _type.GetEncodingSize(_value); }
      }

      public AMQTypedValue(AMQType type, object value)
      {
         if ( type == null )
            throw new ArgumentNullException("type");
         _type = type;
         _value = type.ToNativeValue(value);
      }

      public AMQTypedValue(AMQType type, ByteBuffer buffer)
      {
         _type = type;
         _value = type.ReadValueFromBuffer(buffer);
      }

      public void WriteToBuffer(ByteBuffer buffer)
      {
         _type.WriteToBuffer(_value, buffer);
      }
      
      public static AMQTypedValue ReadFromBuffer(ByteBuffer buffer)
      {
         AMQType type = AMQTypeMap.GetType(buffer.GetByte());
         return new AMQTypedValue(type, buffer);
      }

      public override string ToString()
      {
         return string.Format("{0}: {1}", Type, Value);
      }
   }
}
