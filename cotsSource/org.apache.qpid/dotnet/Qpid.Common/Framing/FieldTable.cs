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
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Collections;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Framing
{
    public class FieldTable : IFieldTable, IEnumerable
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(FieldTable));

        IDictionary _properties;
        private ByteBuffer _encodedForm;
        private object _syncLock;
        private uint _encodedSize;

        public FieldTable()
        {
           _syncLock = new object();
        }
        
        /// <summary>
        /// Construct a new field table.
        /// </summary>
        /// <param name="buffer">the buffer from which to read data. The length byte must be read already</param>
        /// <param name="length">the length of the field table. Must be > 0.</param>
        public FieldTable(ByteBuffer buffer, uint length) : this()
        {
           _encodedForm = buffer.Slice();
           _encodedForm.Limit = (int)length;
           _encodedSize = length;
           buffer.Skip((int)length);
        }

        /// <summary>
        /// The set of all property names
        /// </summary>
        public ICollection Keys
        {
           get 
           { 
              InitMapIfNecessary();
              return _properties.Keys;  
           }
        }

        /// <summary>
        /// Calculated size of this field table once encoded
        /// </summary>
        public uint EncodedSize
        {
           get { return _encodedSize; }
        }

        /// <summary>
        /// Number of properties in the field table
        /// </summary>
        public int Count
        {
           get
           {
              InitMapIfNecessary();
              return _properties.Count;
           }
        }

        /// <summary>
        /// Gets or sets the specified property.
        /// </summary>
        /// <param name="key">Property name</param>
        /// <returns>The specified property value</returns>
        public object this[string key]
        { 
           get { return GetObject(key); }
           set { SetObject(key, value); }
        }

        #region Typed Setters and Getters
        //
        // Typed Setters and Getters
        //
        public bool GetBoolean(string key)
        {
           return (bool)this[key];
        }
        public void SetBoolean(string key, bool value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.BOOLEAN.AsTypedValue(value));
        }
        public byte GetByte(string key)
        {
           return (byte)this[key];
        }
        public void SetByte(string key, byte value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.BYTE.AsTypedValue(value));
        }
        public sbyte GetSByte(string key)
        {
           return (sbyte)this[key];
        }
        public void SetSByte(string key, sbyte value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.SBYTE.AsTypedValue(value));
        }
        public short GetInt16(string key)
        {
           return (short)this[key];
        }
        public void SetInt16(string key, short value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.INT16.AsTypedValue(value));
        }
        public int GetInt32(string key)
        {
           return (int)this[key];
        }
        public void SetInt32(string key, int value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.INT32.AsTypedValue(value));
        }
        public long GetInt64(string key)
        {
           return (long)this[key];
        }
        public void SetInt64(string key, long value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.INT64.AsTypedValue(value));
        }
        public char GetChar(string key)
        {
           return (char)this[key];
        }
        public void SetChar(string key, char value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.ASCII_CHARACTER.AsTypedValue(value));
        }
        public float GetFloat(string key)
        {
           return (float)this[key];
        }
        public void SetFloat(string key, float value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.FLOAT.AsTypedValue(value));
        }
        public double GetDouble(string key)
        {
           return (double)this[key];
        }
        public void SetDouble(string key, double value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.DOUBLE.AsTypedValue(value));
        }
        public decimal GetDecimal(string key)
        {
           return (decimal)this[key];
        }
        public void SetDecimal(string key, decimal value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.DECIMAL.AsTypedValue(value));
        }
        public string GetString(string key)
        {
           return (string)this[key];
        }
        public void SetString(string key, string value)
        {
           CheckPropertyName(key);
           if ( value == null )
              SetProperty(key, AMQType.VOID.AsTypedValue(null));
           else
              SetProperty(key, AMQType.LONG_STRING.AsTypedValue(value));
        }
        public byte[] GetBytes(string key)
        {
           return (byte[])this[key];
        }
        public void SetBytes(string key, byte[] value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.BINARY.AsTypedValue(value));
        }
        public ushort GetUInt16(string key)
        {
           return (ushort)this[key];
        }
        public void SetUInt16(string key, ushort value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.UINT16.AsTypedValue(value));
        }
        public uint GetUInt32(string key)
        {
           return (uint)this[key];
        }
        public void SetUInt32(string key, uint value)
        { 
           CheckPropertyName(key);
           SetProperty(key, AMQType.UINT32.AsTypedValue(value));
        }
        public ulong GetUInt64(string key)
        {
           return (ulong)this[key];
        }
        public void SetUInt64(string key, ulong value)
        {
           CheckPropertyName(key);
           SetProperty(key, AMQType.UINT64.AsTypedValue(value));
        } 

        #endregion // Typed Setters and Getters

        #region Public Methods
        //
        // Public Methods
        //

        /// <summary>
        /// Removes the property with the specified name
        /// </summary>
        /// <param name="key">The name of the property to remove</param>
        /// <returns>The previous value of the property or null</returns>
        public AMQTypedValue RemoveKey(string key)
        {
           InitMapIfNecessary();
           _encodedForm = null;
           AMQTypedValue value = (AMQTypedValue)_properties[key];
           if ( value != null )
           {
              _properties.Remove(key);
              _encodedSize -= EncodingUtils.EncodedShortStringLength(key);
              _encodedSize--;
              _encodedSize -= value.EncodingLength;

           }
           return value;
        }
        

        /// <summary>
        /// Remove the property with the specified name
        /// </summary>
        /// <param name="key">The name of the property to remove</param>
        public void Remove(string key)
        {
           RemoveKey(key);
        }

        /// <summary>
        /// Remove all properties from the table
        /// </summary>
        public void Clear()
        {
           InitMapIfNecessary();
           _encodedForm = null;
           _properties.Clear();
           _encodedSize = 0;
        }

        /// <summary>
        /// Adds all the items from one field table in this one. Will overwrite any items in the current table
        /// with the same key.
        /// </summary>
        /// <param name="ft">the source field table</param>
        public void AddAll(IFieldTable ft)
        {
           foreach ( DictionaryEntry dictionaryEntry in ft )
           {
              this[(string)dictionaryEntry.Key] = dictionaryEntry.Value;
           }
        }

        /// <summary>
        /// Get a enumerator over the internal property set.
        /// Notice the enumerator will DictionaryEntry objects with 
        /// a string as the Key and an <see cref="AMQTypedValue"/> instance as the value
        /// </summary>
        /// <returns>The enumerator object</returns>
        public IEnumerator GetEnumerator()
        {
           InitMapIfNecessary();
           return _properties.GetEnumerator();
        }

        /// <summary>
        /// Indicates if a property with the given name exists
        /// </summary>
        /// <param name="s">Property name to check</param>
        /// <returns>True if the property exists</returns>
        public bool Contains(string s)
        {
           InitMapIfNecessary();
           return _properties.Contains(s);
        }
        
        /// <summary>
        /// Returns a dictionary mapping Property Names to the corresponding
        /// <see cref="AMQTypedValue"/> value
        /// </summary>
        /// <returns>The internal dictionary</returns>
        public IDictionary AsDictionary()
        {
           InitMapIfNecessary();
           return _properties;
        }

        /// <summary>
        /// Returns a string representation of this field table
        /// </summary>
        /// <returns>A string</returns>
        public override string ToString()
        {
           StringBuilder sb = new StringBuilder("FieldTable {");

           bool first = true;
           InitMapIfNecessary();
           foreach ( DictionaryEntry entry in _properties )
           {
              if ( !first )
              {
                 sb.Append(", ");
              }
              first = false;
              sb.Append(entry.Key).Append(" => ").Append(entry.Value);
           }

           sb.Append("}");
           return sb.ToString();
        }
        
        /// <summary>
        /// Serializes this instance to the specified <see cref="ByteBuffer"/>.
        /// </summary>
        /// <param name="buffer">The buffer to write to</param>
        public void WriteToBuffer(ByteBuffer buffer)
        {
            if ( _log.IsDebugEnabled )
            {
               _log.Debug("FieldTable::writeToBuffer: Writing encoded length of " + EncodedSize + "...");
            }

           EncodingUtils.WriteUnsignedInteger(buffer, EncodedSize);
           WritePayload(buffer);
        }

        /// <summary>
        /// Returns a byte array with the serialized representation
        /// of this field table
        /// </summary>
        /// <returns>An array of bytes</returns>
        public byte[] GetDataAsBytes()
        {
           ByteBuffer buffer = ByteBuffer.Allocate((int)_encodedSize);
           WritePayload(buffer);
           byte[] result = new byte[_encodedSize];
           buffer.Flip();
           buffer.GetBytes(result);
           //buffer.Release();
           return result;
        }

        #endregion // Public Methods

        #region Private Methods
        //
        // Private Methods
        //

        private static void CheckPropertyName(string propertyName)
        {
           if ( propertyName == null || propertyName.Length == 0 )
              throw new ArgumentNullException("propertyName");
           CheckIdentifierFormat(propertyName);
        }

        private static void CheckIdentifierFormat(string propertyName)
        {
           //        AMQP Spec: 4.2.5.5 Field Tables
           //        Guidelines for implementers:
           //           * Field names MUST start with a letter, '$' or '#' and may continue with
           //             letters, '$' or '#', digits, or underlines, to a maximum length of 128
           //             characters.
           //           * The server SHOULD validate field names and upon receiving an invalid
           //             field name, it SHOULD signal a connection exception with reply code
           //             503 (syntax error). Conformance test: amq_wlp_table_01.
           //           * A peer MUST handle duplicate fields by using only the first instance.


           // AMQP length limit
           if ( propertyName.Length > 128 )
           {
              throw new ArgumentException("AMQP limits property names to 128 characters");
           }

           // AMQ start character
           if ( !(Char.IsLetter(propertyName[0])
                 || propertyName[0] == '$'
                 || propertyName[0] == '#'
                 || propertyName[0] == '_' ) )// Not official AMQP added for JMS.
           {
              throw new ArgumentException("Identifier '" + propertyName + "' does not start with a valid AMQP start character");
           }
        }

        private object GetObject(string key)
        {
           AMQTypedValue value = GetProperty(key);
           return value != null ? value.Value : null;
        }
        
        private void SetObject(string key, object value)
        {
           if ( value is bool )
           {
              SetBoolean(key, (bool)value);
           } else if ( value is byte )
           {
              SetByte(key, (byte)value);
           } else if ( value is sbyte )
           {
              SetSByte(key, (sbyte)value);
           } else if ( value is short )
           {
              SetInt16(key, (short)value);
           } else if ( value is ushort )
           {
              SetUInt16(key, (ushort)value);
           } else if ( value is int )
           {
              SetInt32(key, (int) value);
           } else if ( value is uint )
           {
              SetUInt32(key, (uint)value);
           } else if ( value is long )
           {
              SetInt64(key, (long) value);
           } else if ( value is ulong )
           {
              SetUInt64(key, (ulong)value);
           } else if ( value is char )
           {
              SetChar(key, (char) value);
           } else if ( value is float )
           {
              SetFloat(key, (float) value);
           } else if ( value is double )
           {
              SetDouble(key, (double) value);
           } else if ( value is decimal )
           {
              SetDecimal(key, (decimal) value);
           } else if ( value is string )
           {
              SetString(key, (string) value);
           } else if ( value is byte[] )
           {
              SetBytes(key, (byte[])value);
           } else
           {
              throw new ArgumentException("Data type not supported yet");
           }
        }
        
        private AMQTypedValue GetProperty(string name)
        {
           InitMapIfNecessary();
           return (AMQTypedValue) _properties[name];
        }

        private void PopulateFromBuffer()
        {
           try
           {
              ByteBuffer buffer = _encodedForm;
              _encodedForm = null;
              if ( buffer != null )
                 SetFromBuffer(buffer, _encodedSize);
           } catch ( AMQFrameDecodingException e )
           {
              _log.Error("Error decoding FieldTable in deferred decoding mode ", e);
              throw;
           }
        }

        private void SetFromBuffer(ByteBuffer buffer, uint length)
        {
           bool trace = _log.IsDebugEnabled;
           if ( length > 0 )
           {
              int expectedRemaining = buffer.Remaining - (int)length;
              _properties = new LinkedHashtable();

              do
              {
                 string key = EncodingUtils.ReadShortString(buffer);
                 AMQTypedValue value = AMQTypedValue.ReadFromBuffer(buffer);
                 if ( trace )
                 {
                    _log.Debug(string.Format("FieldTable::PropFieldTable(buffer,{0}): Read type '{1}', key '{2}', value '{3}'", length, value.Type, key, value.Value));
                 }
                 _properties.Add(key, value);

              } while ( buffer.Remaining > expectedRemaining );
              _encodedSize = length;
           }
           if ( trace )
           {
              _log.Debug("FieldTable::FieldTable(buffer," + length + "): Done.");
           }
        }

        private void InitMapIfNecessary()
        {
           lock ( _syncLock )
           {
              if ( _properties == null )
              {
                 if ( _encodedForm == null )
                 {
                    _properties = new LinkedHashtable();
                 } else
                 {
                    PopulateFromBuffer();
                 }
              }
           }
        }

        private AMQTypedValue SetProperty(string key, AMQTypedValue value)
        {
           InitMapIfNecessary();
           _encodedForm = null;
           if ( value == null )
           {
              RemoveKey(key);
           }
           AMQTypedValue oldVal = (AMQTypedValue)_properties[key];
           _properties.Add(key, value);
           if ( oldVal != null )
           {
              _encodedSize -= oldVal.EncodingLength;
           } else
           {
              _encodedSize += EncodingUtils.EncodedShortStringLength(key) + (uint)1;
           }
           if ( value != null )
           {
              _encodedSize += value.EncodingLength;
           }

           return oldVal;
        }

        public void WritePayload(ByteBuffer buffer)
        {
           if ( _encodedForm != null )
           {
              lock ( _syncLock )
              {
                 buffer.Put(_encodedForm);
                 _encodedForm.Flip();
              }
           } else if ( _properties != null )
           {
              foreach ( DictionaryEntry de in _properties )
              {
                 string key = (string)de.Key;
                 AMQTypedValue value = (AMQTypedValue)de.Value;
                 try
                 {
                    if ( _log.IsDebugEnabled )
                    {
                       _log.Debug("Writing Property:" + key +
                                  " Type:" + value.Type +
                                  " Value:" + value.Value);
                       _log.Debug("Buffer Position:" + buffer.Position +
                                  " Remaining:" + buffer.Remaining);
                    }
                    //Write the actual parameter name
                    EncodingUtils.WriteShortStringBytes(buffer, key);
                    value.WriteToBuffer(buffer);
                 } catch ( Exception ex )
                 {
                    if ( _log.IsDebugEnabled )
                    {
                       _log.Debug("Exception thrown:" + ex);
                       _log.Debug("Writing Property:" + key +
                                  " Type:" + value.Type +
                                  " Value:" + value.Value);
                       _log.Debug("Buffer Position:" + buffer.Position +
                                  " Remaining:" + buffer.Remaining);
                    }
                    throw;
                 }
              }
           }
        }
        #endregion // Private Methods
    }
}
