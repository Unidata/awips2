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
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.AMQPInvalidClassException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

// extends FieldTable
public class FieldTable
{
    private static final Logger _logger = LoggerFactory.getLogger(FieldTable.class);
    private static final String STRICT_AMQP = "STRICT_AMQP";
    private final boolean _strictAMQP = Boolean.valueOf(System.getProperty(STRICT_AMQP, "false"));

    private ByteBuffer _encodedForm;
    private LinkedHashMap<AMQShortString, AMQTypedValue> _properties;
    private long _encodedSize;
    private static final int INITIAL_HASHMAP_CAPACITY = 16;
    private static final int INITIAL_ENCODED_FORM_SIZE = 256;

    public FieldTable()
    {
        super();
        // _encodedForm = ByteBuffer.allocate(INITIAL_ENCODED_FORM_SIZE);
        // _encodedForm.setAutoExpand(true);
        // _encodedForm.limit(0);
    }

    /**
     * Construct a new field table.
     *
     * @param buffer the buffer from which to read data. The length byte must be read already
     * @param length the length of the field table. Must be > 0.
     *
     * @throws AMQFrameDecodingException if there is an error decoding the table
     */
    public FieldTable(ByteBuffer buffer, long length) throws AMQFrameDecodingException
    {
        this();
        _encodedForm = buffer.slice();
        _encodedForm.limit((int) length);
        _encodedSize = length;
        buffer.skip((int) length);
    }

    public AMQTypedValue getProperty(AMQShortString string)
    {
        checkPropertyName(string);

        synchronized (this)
        {
            if (_properties == null)
            {
                if (_encodedForm == null)
                {
                    return null;
                }
                else
                {
                    populateFromBuffer();
                }
            }
        }

        if (_properties == null)
        {
            return null;
        }
        else
        {
            return _properties.get(string);
        }
    }

    private void populateFromBuffer()
    {
        try
        {
            setFromBuffer(_encodedForm, _encodedSize);
        }
        catch (AMQFrameDecodingException e)
        {
            _logger.error("Error decoding FieldTable in deferred decoding mode ", e);
            throw new IllegalArgumentException(e);
        }
    }

    private AMQTypedValue setProperty(AMQShortString key, AMQTypedValue val)
    {
        checkPropertyName(key);
        initMapIfNecessary();
        if (_properties.containsKey(key))
        {
            _encodedForm = null;

            if (val == null)
            {
                return removeKey(key);
            }
        }
        else if ((_encodedForm != null) && (val != null))
        {
            // We have updated data to store in the buffer
            // So clear the _encodedForm to allow it to be rebuilt later
            // this is safer than simply appending to any existing buffer.
            _encodedForm = null;
        }
        else if (val == null)
        {
            return null;
        }

        AMQTypedValue oldVal = _properties.put(key, val);
        if (oldVal != null)
        {
            _encodedSize -= oldVal.getEncodingSize();
        }
        else
        {
            _encodedSize += EncodingUtils.encodedShortStringLength(key) + 1;
        }

        _encodedSize += val.getEncodingSize();

        return oldVal;
    }

    private void initMapIfNecessary()
    {
        synchronized (this)
        {
            if (_properties == null)
            {
                if ((_encodedForm == null) || (_encodedSize == 0))
                {
                    _properties = new LinkedHashMap<AMQShortString, AMQTypedValue>();
                }
                else
                {
                    populateFromBuffer();
                }
            }

        }
    }

    public Boolean getBoolean(String string)
    {
        return getBoolean(new AMQShortString(string));
    }

    public Boolean getBoolean(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.BOOLEAN))
        {
            return (Boolean) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Byte getByte(String string)
    {
        return getByte(new AMQShortString(string));
    }

    public Byte getByte(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.BYTE))
        {
            return (Byte) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Short getShort(String string)
    {
        return getShort(new AMQShortString(string));
    }

    public Short getShort(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.SHORT))
        {
            return (Short) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Integer getInteger(String string)
    {
        return getInteger(new AMQShortString(string));
    }

    public Integer getInteger(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.INT))
        {
            return (Integer) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Long getLong(String string)
    {
        return getLong(new AMQShortString(string));
    }

    public Long getLong(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.LONG))
        {
            return (Long) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Float getFloat(String string)
    {
        return getFloat(new AMQShortString(string));
    }

    public Float getFloat(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.FLOAT))
        {
            return (Float) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Double getDouble(String string)
    {
        return getDouble(new AMQShortString(string));
    }

    public Double getDouble(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.DOUBLE))
        {
            return (Double) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public String getString(String string)
    {
        return getString(new AMQShortString(string));
    }

    public String getString(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && ((value.getType() == AMQType.WIDE_STRING) || (value.getType() == AMQType.ASCII_STRING)))
        {
            return (String) value.getValue();
        }
        else if ((value != null) && (value.getValue() != null) && !(value.getValue() instanceof byte[]))
        {
            return String.valueOf(value.getValue());
        }
        else
        {
            return null;
        }

    }

    public Character getCharacter(String string)
    {
        return getCharacter(new AMQShortString(string));
    }

    public Character getCharacter(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.ASCII_CHARACTER))
        {
            return (Character) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public byte[] getBytes(String string)
    {
        return getBytes(new AMQShortString(string));
    }

    public byte[] getBytes(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if ((value != null) && (value.getType() == AMQType.BINARY))
        {
            return (byte[]) value.getValue();
        }
        else
        {
            return null;
        }
    }

    /**
     * Extracts a value from the field table that is itself a FieldTable associated with the specified parameter name.
     *
     * @param string The name of the parameter to get the associated FieldTable value for.
     *
     * @return The associated FieldTable value, or <tt>null</tt> if the associated value is not of FieldTable type or
     *         not present in the field table at all.
     */
    public FieldTable getFieldTable(String string)
    {
        return getFieldTable(new AMQShortString(string));
    }

    /**
     * Extracts a value from the field table that is itself a FieldTable associated with the specified parameter name.
     *
     * @param string The name of the parameter to get the associated FieldTable value for.
     *
     * @return The associated FieldTable value, or <tt>null</tt> if the associated value is not of FieldTable type or
     *         not present in the field table at all.
     */
    public FieldTable getFieldTable(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);

        if ((value != null) && (value.getType() == AMQType.FIELD_TABLE))
        {
            return (FieldTable) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public Object getObject(String string)
    {
        return getObject(new AMQShortString(string));
    }

    public Object getObject(AMQShortString string)
    {
        AMQTypedValue value = getProperty(string);
        if (value != null)
        {
            return value.getValue();
        }
        else
        {
            return value;
        }

    }

    public Long getTimestamp(AMQShortString name)
    {
        AMQTypedValue value = getProperty(name);
        if ((value != null) && (value.getType() == AMQType.TIMESTAMP))
        {
            return (Long) value.getValue();
        }
        else
        {
            return null;
        }
    }

    public BigDecimal getDecimal(AMQShortString propertyName)
    {
        AMQTypedValue value = getProperty(propertyName);
        if ((value != null) && (value.getType() == AMQType.DECIMAL))
        {
            return (BigDecimal) value.getValue();
        }
        else
        {
            return null;
        }
    }

    // ************  Setters
    public Object setBoolean(String string, Boolean b)
    {
        return setBoolean(new AMQShortString(string), b);
    }

    public Object setBoolean(AMQShortString string, Boolean b)
    {
        return setProperty(string, AMQType.BOOLEAN.asTypedValue(b));
    }

    public Object setByte(String string, Byte b)
    {
        return setByte(new AMQShortString(string), b);
    }

    public Object setByte(AMQShortString string, Byte b)
    {
        return setProperty(string, AMQType.BYTE.asTypedValue(b));
    }

    public Object setShort(String string, Short i)
    {
        return setShort(new AMQShortString(string), i);
    }

    public Object setShort(AMQShortString string, Short i)
    {
        return setProperty(string, AMQType.SHORT.asTypedValue(i));
    }

    public Object setInteger(String string, Integer i)
    {
        return setInteger(new AMQShortString(string), i);
    }

    public Object setInteger(AMQShortString string, Integer i)
    {
        return setProperty(string, AMQType.INT.asTypedValue(i));
    }

    public Object setLong(String string, Long l)
    {
        return setLong(new AMQShortString(string), l);
    }

    public Object setLong(AMQShortString string, Long l)
    {
        return setProperty(string, AMQType.LONG.asTypedValue(l));
    }

    public Object setFloat(String string, Float f)
    {
        return setFloat(new AMQShortString(string), f);
    }

    public Object setFloat(AMQShortString string, Float v)
    {
        return setProperty(string, AMQType.FLOAT.asTypedValue(v));
    }

    public Object setDouble(String string, Double d)
    {
        return setDouble(new AMQShortString(string), d);
    }

    public Object setDouble(AMQShortString string, Double v)
    {
        return setProperty(string, AMQType.DOUBLE.asTypedValue(v));
    }

    public Object setString(String string, String s)
    {
        return setString(new AMQShortString(string), s);
    }

    public Object setAsciiString(AMQShortString string, String value)
    {
        if (value == null)
        {
            return setProperty(string, AMQType.VOID.asTypedValue(null));
        }
        else
        {
            return setProperty(string, AMQType.ASCII_STRING.asTypedValue(value));
        }
    }

    public Object setString(AMQShortString string, String value)
    {
        if (value == null)
        {
            return setProperty(string, AMQType.VOID.asTypedValue(null));
        }
        else
        {
            return setProperty(string, AMQType.LONG_STRING.asTypedValue(value));
        }
    }

    public Object setChar(String string, char c)
    {
        return setChar(new AMQShortString(string), c);
    }

    public Object setChar(AMQShortString string, char c)
    {
        return setProperty(string, AMQType.ASCII_CHARACTER.asTypedValue(c));
    }

    public Object setBytes(String string, byte[] b)
    {
        return setBytes(new AMQShortString(string), b);
    }

    public Object setBytes(AMQShortString string, byte[] bytes)
    {
        return setProperty(string, AMQType.BINARY.asTypedValue(bytes));
    }

    public Object setBytes(String string, byte[] bytes, int start, int length)
    {
        return setBytes(new AMQShortString(string), bytes, start, length);
    }

    public Object setBytes(AMQShortString string, byte[] bytes, int start, int length)
    {
        byte[] newBytes = new byte[length];
        System.arraycopy(bytes, start, newBytes, 0, length);

        return setBytes(string, bytes);
    }

    public Object setObject(String string, Object o)
    {
        return setObject(new AMQShortString(string), o);
    }

    public Object setTimestamp(AMQShortString string, long datetime)
    {
        return setProperty(string, AMQType.TIMESTAMP.asTypedValue(datetime));
    }

    public Object setDecimal(AMQShortString string, BigDecimal decimal)
    {
        if (decimal.longValue() > Integer.MAX_VALUE)
        {
            throw new UnsupportedOperationException("AMQP doesnot support decimals larger than " + Integer.MAX_VALUE);
        }

        if (decimal.scale() > Byte.MAX_VALUE)
        {
            throw new UnsupportedOperationException("AMQP doesnot support decimal scales larger than " + Byte.MAX_VALUE);
        }

        return setProperty(string, AMQType.DECIMAL.asTypedValue(decimal));
    }

    public Object setVoid(AMQShortString string)
    {
        return setProperty(string, AMQType.VOID.asTypedValue(null));
    }

    /**
     * Associates a nested field table with the specified parameter name.
     *
     * @param string  The name of the parameter to store in the table.
     * @param ftValue The field table value to associate with the parameter name.
     *
     * @return The stored value.
     */
    public Object setFieldTable(String string, FieldTable ftValue)
    {
        return setFieldTable(new AMQShortString(string), ftValue);
    }

    /**
     * Associates a nested field table with the specified parameter name.
     *
     * @param string  The name of the parameter to store in the table.
     * @param ftValue The field table value to associate with the parameter name.
     *
     * @return The stored value.
     */
    public Object setFieldTable(AMQShortString string, FieldTable ftValue)
    {
        return setProperty(string, AMQType.FIELD_TABLE.asTypedValue(ftValue));
    }

    public Object setObject(AMQShortString string, Object object)
    {
        if (object instanceof Boolean)
        {
            return setBoolean(string, (Boolean) object);
        }
        else if (object instanceof Byte)
        {
            return setByte(string, (Byte) object);
        }
        else if (object instanceof Short)
        {
            return setShort(string, (Short) object);
        }
        else if (object instanceof Integer)
        {
            return setInteger(string, (Integer) object);
        }
        else if (object instanceof Long)
        {
            return setLong(string, (Long) object);
        }
        else if (object instanceof Float)
        {
            return setFloat(string, (Float) object);
        }
        else if (object instanceof Double)
        {
            return setDouble(string, (Double) object);
        }
        else if (object instanceof String)
        {
            return setString(string, (String) object);
        }
        else if (object instanceof Character)
        {
            return setChar(string, (Character) object);
        }
        else if (object instanceof byte[])
        {
            return setBytes(string, (byte[]) object);
        }

        throw new AMQPInvalidClassException("Only Primatives objects allowed Object is:" + object.getClass());
    }

    public boolean isNullStringValue(String name)
    {
        AMQTypedValue value = getProperty(new AMQShortString(name));

        return (value != null) && (value.getType() == AMQType.VOID);
    }

    // ***** Methods

    public Enumeration getPropertyNames()
    {
        return Collections.enumeration(keys());
    }

    public boolean propertyExists(AMQShortString propertyName)
    {
        return itemExists(propertyName);
    }

    public boolean propertyExists(String propertyName)
    {
        return itemExists(propertyName);
    }

    public boolean itemExists(AMQShortString propertyName)
    {
        checkPropertyName(propertyName);
        initMapIfNecessary();

        return _properties.containsKey(propertyName);
    }

    public boolean itemExists(String string)
    {
        return itemExists(new AMQShortString(string));
    }

    public String toString()
    {
        initMapIfNecessary();

        return _properties.toString();
    }

    private void checkPropertyName(AMQShortString propertyName)
    {
        if (propertyName == null)
        {
            throw new IllegalArgumentException("Property name must not be null");
        }
        else if (propertyName.length() == 0)
        {
            throw new IllegalArgumentException("Property name must not be the empty string");
        }

        if (_strictAMQP)
        {
            checkIdentiferFormat(propertyName);
        }
    }

    protected static void checkIdentiferFormat(AMQShortString propertyName)
    {
        // AMQP Spec: 4.2.5.5 Field Tables
        // Guidelines for implementers:
        // * Field names MUST start with a letter, '$' or '#' and may continue with
        // letters, '$' or '#', digits, or underlines, to a maximum length of 128
        // characters.
        // * The server SHOULD validate field names and upon receiving an invalid
        // field name, it SHOULD signal a connection exception with reply code
        // 503 (syntax error). Conformance test: amq_wlp_table_01.
        // * A peer MUST handle duplicate fields by using only the first instance.

        // AMQP length limit
        if (propertyName.length() > 128)
        {
            throw new IllegalArgumentException("AMQP limits property names to 128 characters");
        }

        // AMQ start character
        if (!(Character.isLetter(propertyName.charAt(0)) || (propertyName.charAt(0) == '$')
                    || (propertyName.charAt(0) == '#') || (propertyName.charAt(0) == '_'))) // Not official AMQP added for JMS.
        {
            throw new IllegalArgumentException("Identifier '" + propertyName
                + "' does not start with a valid AMQP start character");
        }
    }

    // *************************  Byte Buffer Processing

    public void writeToBuffer(ByteBuffer buffer)
    {
        final boolean trace = _logger.isDebugEnabled();

        if (trace)
        {
            _logger.debug("FieldTable::writeToBuffer: Writing encoded length of " + getEncodedSize() + "...");
            if (_properties != null)
            {
                _logger.debug(_properties.toString());
            }
        }

        EncodingUtils.writeUnsignedInteger(buffer, getEncodedSize());

        putDataInBuffer(buffer);
    }

    public byte[] getDataAsBytes()
    {
        final int encodedSize = (int) getEncodedSize();
        final ByteBuffer buffer = ByteBuffer.allocate(encodedSize); // FIXME XXX: Is cast a problem?

        putDataInBuffer(buffer);

        final byte[] result = new byte[encodedSize];
        buffer.flip();
        buffer.get(result);
        buffer.release();

        return result;
    }

    public long getEncodedSize()
    {
        return _encodedSize;
    }

    private void recalculateEncodedSize()
    {

        int encodedSize = 0;
        if (_properties != null)
        {
            for (Map.Entry<AMQShortString, AMQTypedValue> e : _properties.entrySet())
            {
                encodedSize += EncodingUtils.encodedShortStringLength(e.getKey());
                encodedSize++; // the byte for the encoding Type
                encodedSize += e.getValue().getEncodingSize();

            }
        }

        _encodedSize = encodedSize;
    }

    public void addAll(FieldTable fieldTable)
    {
        initMapIfNecessary();
        _encodedForm = null;
        _properties.putAll(fieldTable._properties);
        recalculateEncodedSize();
    }


    public static interface FieldTableElementProcessor
    {
        public boolean processElement(String propertyName, AMQTypedValue value);

        public Object getResult();
    }

    public Object processOverElements(FieldTableElementProcessor processor)
    {
        initMapIfNecessary();
        if (_properties != null)
        {
            for (Map.Entry<AMQShortString, AMQTypedValue> e : _properties.entrySet())
            {
                boolean result = processor.processElement(e.getKey().toString(), e.getValue());
                if (!result)
                {
                    break;
                }
            }
        }

        return processor.getResult();

    }

    public int size()
    {
        initMapIfNecessary();

        return _properties.size();

    }

    public boolean isEmpty()
    {
        return size() == 0;
    }

    public boolean containsKey(AMQShortString key)
    {
        initMapIfNecessary();

        return _properties.containsKey(key);
    }

    public boolean containsKey(String key)
    {
        return containsKey(new AMQShortString(key));
    }

    public Set<String> keys()
    {
        initMapIfNecessary();
        Set<String> keys = new LinkedHashSet<String>();
        for (AMQShortString key : _properties.keySet())
        {
            keys.add(key.toString());
        }

        return keys;
    }

    public Iterator<Map.Entry<AMQShortString, AMQTypedValue>> iterator()
    {
        if(_encodedForm != null)
        {
            return new FieldTableIterator(_encodedForm.duplicate().rewind(),(int)_encodedSize);
        }
        else
        {
            initMapIfNecessary();
            return _properties.entrySet().iterator();
        }
    }

    public Object get(String key)
    {
        return get(new AMQShortString(key));
    }

    public Object get(AMQShortString key)
    {
        return getObject(key);
    }

    public Object put(AMQShortString key, Object value)
    {
        return setObject(key, value);
    }

    public Object remove(String key)
    {

        return remove(new AMQShortString(key));

    }

    public Object remove(AMQShortString key)
    {
        AMQTypedValue val = removeKey(key);

        return (val == null) ? null : val.getValue();

    }

    public AMQTypedValue removeKey(AMQShortString key)
    {
        initMapIfNecessary();
        _encodedForm = null;
        AMQTypedValue value = _properties.remove(key);
        if (value == null)
        {
            return null;
        }
        else
        {
            _encodedSize -= EncodingUtils.encodedShortStringLength(key);
            _encodedSize--;
            _encodedSize -= value.getEncodingSize();

            return value;
        }

    }

    public void clear()
    {
        initMapIfNecessary();
        _encodedForm = null;
        _properties.clear();
        _encodedSize = 0;
    }

    public Set<AMQShortString> keySet()
    {
        initMapIfNecessary();

        return _properties.keySet();
    }

    private void putDataInBuffer(ByteBuffer buffer)
    {

        if (_encodedForm != null)
        {
            if(buffer.isDirect() || buffer.isReadOnly())
            {
                ByteBuffer encodedForm = _encodedForm.duplicate();

                if (encodedForm.position() != 0)
                {
                    encodedForm.flip();
                }

                buffer.put(encodedForm);
            }
            else
            {
                buffer.put(_encodedForm.array(),_encodedForm.arrayOffset(),(int)_encodedSize);
            }
        }
        else if (_properties != null)
        {
            final Iterator<Map.Entry<AMQShortString, AMQTypedValue>> it = _properties.entrySet().iterator();

            // If there are values then write out the encoded Size... could check _encodedSize != 0
            // write out the total length, which we have kept up to date as data is added

            while (it.hasNext())
            {
                final Map.Entry<AMQShortString, AMQTypedValue> me = it.next();
                try
                {
                    if (_logger.isDebugEnabled())
                    {
                        _logger.debug("Writing Property:" + me.getKey() + " Type:" + me.getValue().getType() + " Value:"
                            + me.getValue().getValue());
                        _logger.debug("Buffer Position:" + buffer.position() + " Remaining:" + buffer.remaining());
                    }

                    // Write the actual parameter name
                    EncodingUtils.writeShortStringBytes(buffer, me.getKey());
                    me.getValue().writeToBuffer(buffer);
                }
                catch (Exception e)
                {
                    if (_logger.isDebugEnabled())
                    {
                        _logger.debug("Exception thrown:" + e);
                        _logger.debug("Writing Property:" + me.getKey() + " Type:" + me.getValue().getType() + " Value:"
                            + me.getValue().getValue());
                        _logger.debug("Buffer Position:" + buffer.position() + " Remaining:" + buffer.remaining());
                    }

                    throw new RuntimeException(e);
                }
            }
        }
    }

    private void setFromBuffer(ByteBuffer buffer, long length) throws AMQFrameDecodingException
    {

        final boolean trace = _logger.isDebugEnabled();
        if (length > 0)
        {

            final int expectedRemaining = buffer.remaining() - (int) length;

            _properties = new LinkedHashMap<AMQShortString, AMQTypedValue>(INITIAL_HASHMAP_CAPACITY);

            do
            {

                final AMQShortString key = EncodingUtils.readAMQShortString(buffer);
                AMQTypedValue value = AMQTypedValue.readFromBuffer(buffer);

                if (trace)
                {
                    _logger.debug("FieldTable::PropFieldTable(buffer," + length + "): Read type '" + value.getType()
                        + "', key '" + key + "', value '" + value.getValue() + "'");
                }

                _properties.put(key, value);

            }
            while (buffer.remaining() > expectedRemaining);

        }

        _encodedSize = length;

        if (trace)
        {
            _logger.debug("FieldTable::FieldTable(buffer," + length + "): Done.");
        }
    }

    private static final class FieldTableEntry implements Map.Entry<AMQShortString, AMQTypedValue>
    {
        private final AMQTypedValue _value;
        private final AMQShortString _key;

        public FieldTableEntry(final AMQShortString key, final AMQTypedValue value)
        {
            _key = key;
            _value = value;
        }

        public AMQShortString getKey()
        {
            return _key;
        }

        public AMQTypedValue getValue()
        {
            return _value;
        }

        public AMQTypedValue setValue(final AMQTypedValue value)
        {
            throw new UnsupportedOperationException();
        }

        public boolean equals(Object o)
        {
            if(o instanceof FieldTableEntry)
            {
                FieldTableEntry other = (FieldTableEntry) o;
                return (_key == null ? other._key == null : _key.equals(other._key))
                       && (_value == null ? other._value == null : _value.equals(other._value));
            }
            else
            {
                return false;
            }
        }

        public int hashCode()
        {
            return (getKey()==null   ? 0 : getKey().hashCode())
                   ^ (getValue()==null ? 0 : getValue().hashCode());
        }

    }


    private static final class FieldTableIterator implements Iterator<Map.Entry<AMQShortString, AMQTypedValue>>
    {

        private final ByteBuffer _buffer;
        private int _expectedRemaining;

        public FieldTableIterator(ByteBuffer buffer, int length)
        {
            _buffer = buffer;
            _expectedRemaining = buffer.remaining() - length;
        }

        public boolean hasNext()
        {
            return (_buffer.remaining() > _expectedRemaining);
        }

        public Map.Entry<AMQShortString, AMQTypedValue> next()
        {
            if(hasNext())
            {
                final AMQShortString key = EncodingUtils.readAMQShortString(_buffer);
                AMQTypedValue value = AMQTypedValue.readFromBuffer(_buffer);
                return new FieldTableEntry(key, value);
            }
            else
            {
                return null;
            }
        }

        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }




    public int hashCode()
    {
        initMapIfNecessary();

        return _properties.hashCode();
    }

    public boolean equals(Object o)
    {
        if (o == this)
        {
            return true;
        }

        if (o == null)
        {
            return false;
        }

        if (!(o instanceof FieldTable))
        {
            return false;
        }

        initMapIfNecessary();

        FieldTable f = (FieldTable) o;
        f.initMapIfNecessary();

        return _properties.equals(f._properties);
    }

    public static FieldTable convertToFieldTable(Map<String, Object> map)
    {
        if (map != null)
        {
            FieldTable table = new FieldTable();
            for(Map.Entry<String,Object> entry : map.entrySet())
            {
                table.put(new AMQShortString(entry.getKey()), entry.getValue());
            }

            return table;
        }
        else
        {
            return null;
        }
    }


}
