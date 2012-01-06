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
using System.Text;
using Apache.Qpid.Framing;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Client.Message
{
   internal class QpidHeaders : IHeaders
   {
      private FieldTable _headers;

      public QpidHeaders(FieldTable headers)
      {
         if ( headers == null )
            throw new ArgumentNullException("headers");
         _headers = headers;
      }

      public bool Contains(string name)
      {
         CheckPropertyName(name);
         return _headers.Contains(name);
      }

      public void Clear()
      {
         _headers.Clear();
      }

      public object this[string name]
      {
         get { return GetObject(name); }
         set { SetObject(name, value); }
      }

      public bool GetBoolean(string name)
      {
         CheckPropertyName(name);
         if ( Contains(name) )
            return _headers.GetBoolean(name);
         return false;
      }

      public void SetBoolean(string name, bool b)
      {
         CheckPropertyName(name);
         _headers.SetBoolean(name, b);
      }

      public byte GetByte(string propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetByte(propertyName);
         return 0;
      }

      public void SetByte(string propertyName, byte b)
      {
         CheckPropertyName(propertyName);
         _headers.SetByte(propertyName, b);
      }

      //  we have sbyte overloads to interoperate with java
      // because the Java client/server uses signed bytes
      // by default, while C#'s is unsigned
      public sbyte GetSByte(string propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetSByte(propertyName);
         return 0;
      }

      public void SetSByte(string propertyName, sbyte b)
      {
         CheckPropertyName(propertyName);
         _headers.SetSByte(propertyName, b);
      }

      public short GetShort(string propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetInt16(propertyName);
         return 0;
      }

      public void SetShort(string propertyName, short i)
      {
         CheckPropertyName(propertyName);
         _headers.SetInt16(propertyName, i);
      }

      public int GetInt(string propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetInt32(propertyName);
         return 0;
      }

      public void SetInt(string propertyName, int i)
      {
         CheckPropertyName(propertyName);
         _headers.SetInt32(propertyName, i);
      }

      public long GetLong(string propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetInt64(propertyName);
         return 0;
      }

      public void SetLong(string propertyName, long l)
      {
         CheckPropertyName(propertyName);
         _headers.SetInt64(propertyName, l);
      }

      public float GetFloat(String propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetFloat(propertyName);
         return 0f;
      }

      public void SetFloat(string propertyName, float f)
      {
         CheckPropertyName(propertyName);
         _headers.SetFloat(propertyName, f);
      }

      public double GetDouble(string propertyName)
      {
         CheckPropertyName(propertyName);
         if ( Contains(propertyName) )
            return _headers.GetDouble(propertyName);
         return 0;
      }

      public void SetDouble(string propertyName, double v)
      {
         CheckPropertyName(propertyName);
         _headers.SetDouble(propertyName, v);
      }

      public string GetString(string propertyName)
      {
         CheckPropertyName(propertyName);
         return _headers.GetString(propertyName);
      }

      public void SetString(string propertyName, string value)
      {
         CheckPropertyName(propertyName);
         _headers.SetString(propertyName, value);
      }

      public object GetObject(string propertyName)
      {
         CheckPropertyName(propertyName);
         return _headers[propertyName];
      }

      public void SetObject(string propertyName, object value)
      {
         CheckPropertyName(propertyName);
         _headers[propertyName] = value;
      }

      private static void CheckPropertyName(string propertyName)
      {
         if ( propertyName == null )
         {
            throw new ArgumentException("Property name must not be null");
         } else if ( "".Equals(propertyName) )
         {
            throw new ArgumentException("Property name must not be the empty string");
         }
      }

      public override string ToString()
      {
         StringBuilder buf = new StringBuilder("{");
         int i = 0;
         foreach ( DictionaryEntry entry in _headers )
         {
            ++i;
            if ( i > 1 )
            {
               buf.Append(", ");
            }
            string propertyName = (string)entry.Key;
            if ( propertyName == null )
            {
               buf.Append("\nInternal error: Property with NULL key defined");
            } else
            {
               buf.Append(propertyName);
               buf.Append(" = ").Append(entry.Value);
            }
         }
         buf.Append("}");
         return buf.ToString();
      }

   }
}
