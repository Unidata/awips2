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
using System.Collections;

namespace Apache.Qpid.Framing
{
   public sealed class AMQTypeMap
   {
      private static Hashtable _reverseTypeMap;

      private AMQTypeMap()
      {
      }

      static AMQTypeMap()
      {
         _reverseTypeMap = Hashtable.Synchronized(new Hashtable());

         Add(AMQType.LONG_STRING);
         Add(AMQType.BOOLEAN);
         Add(AMQType.BYTE);
         Add(AMQType.SBYTE);
         Add(AMQType.INT16);
         // not supported for now as type code conflicts
         // with LONG_STRING
         //Add(AMQType.UINT16);
         Add(AMQType.INT32);
         Add(AMQType.UINT32);
         Add(AMQType.INT64);
         Add(AMQType.UINT64);
         Add(AMQType.FLOAT);
         Add(AMQType.DOUBLE);
         Add(AMQType.DECIMAL);
         Add(AMQType.BINARY);
         Add(AMQType.ASCII_STRING);
         Add(AMQType.WIDE_STRING);
         Add(AMQType.ASCII_CHARACTER);
         Add(AMQType.TIMESTAMP);
         Add(AMQType.FIELD_TABLE);
         Add(AMQType.VOID);
      }

      public static AMQType GetType(byte identifier)
      {
         AMQType type = (AMQType)_reverseTypeMap[identifier];
         if ( type == null )
            throw new ArgumentOutOfRangeException(string.Format("No such type code: {0:x}", identifier));
         return type;
      }

      private static void Add(AMQType type)
      {
         _reverseTypeMap.Add(type.Identifier, type);
      }
   }
}
