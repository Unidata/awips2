/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.client.message;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.BasicContentHeaderProperties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.JMSException;
import javax.jms.MessageFormatException;

import java.nio.charset.CharacterCodingException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

public class JMSMapMessage extends AbstractBytesTypedMessage implements javax.jms.MapMessage
{
    private static final Logger _logger = LoggerFactory.getLogger(JMSMapMessage.class);

    public static final String MIME_TYPE = "jms/map-message";


    private Map<String, Object> _map = new HashMap<String, Object>();

    public JMSMapMessage(AMQMessageDelegateFactory delegateFactory) throws JMSException
    {
        this(delegateFactory, null);
    }

    JMSMapMessage(AMQMessageDelegateFactory delegateFactory, ByteBuffer data) throws JMSException
    {

        super(delegateFactory, data); // this instantiates a content header
        if(data != null)
        {
            populateMapFromData();
        }

    }

    JMSMapMessage(AMQMessageDelegate delegate, ByteBuffer data) throws AMQException
    {

        super(delegate, data);
        try
        {
            populateMapFromData();
        }
        catch (JMSException je)
        {
            throw new AMQException(null, "Error populating MapMessage from ByteBuffer", je);

        }

    }


    public String toBodyString() throws JMSException
    {
        return _map == null ? "" : _map.toString();
    }

    protected String getMimeType()
    {
        return MIME_TYPE;
    }

    public ByteBuffer getData()
    {
        // What if _data is null?
        writeMapToData();

        return super.getData();
    }

    @Override
    public void clearBodyImpl() throws JMSException
    {
        super.clearBodyImpl();
        _map.clear();
    }

    public boolean getBoolean(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Boolean)
        {
            return ((Boolean) value).booleanValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Boolean.valueOf((String) value);
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to boolean.");
        }

    }

    public byte getByte(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Byte)
        {
            return ((Byte) value).byteValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Byte.valueOf((String) value).byteValue();
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to byte.");
        }
    }

    public short getShort(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Short)
        {
            return ((Short) value).shortValue();
        }
        else if (value instanceof Byte)
        {
            return ((Byte) value).shortValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Short.valueOf((String) value).shortValue();
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to short.");
        }

    }

    public int getInt(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Integer)
        {
            return ((Integer) value).intValue();
        }
        else if (value instanceof Short)
        {
            return ((Short) value).intValue();
        }
        else if (value instanceof Byte)
        {
            return ((Byte) value).intValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Integer.valueOf((String) value).intValue();
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to int.");
        }

    }

    public long getLong(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Long)
        {
            return ((Long) value).longValue();
        }
        else if (value instanceof Integer)
        {
            return ((Integer) value).longValue();
        }

        if (value instanceof Short)
        {
            return ((Short) value).longValue();
        }

        if (value instanceof Byte)
        {
            return ((Byte) value).longValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Long.valueOf((String) value).longValue();
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to long.");
        }

    }

    public char getChar(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (!_map.containsKey(propName))
        {
            throw new MessageFormatException("Property " + propName + " not present");
        }
        else if (value instanceof Character)
        {
            return ((Character) value).charValue();
        }
        else if (value == null)
        {
            throw new NullPointerException("Property " + propName + " has null value and therefore cannot "
                + "be converted to char.");
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to boolan.");
        }

    }

    public float getFloat(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Float)
        {
            return ((Float) value).floatValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Float.valueOf((String) value).floatValue();
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to float.");
        }
    }

    public double getDouble(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (value instanceof Double)
        {
            return ((Double) value).doubleValue();
        }
        else if (value instanceof Float)
        {
            return ((Float) value).doubleValue();
        }
        else if ((value instanceof String) || (value == null))
        {
            return Double.valueOf((String) value).doubleValue();
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to double.");
        }
    }

    public String getString(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if ((value instanceof String) || (value == null))
        {
            return (String) value;
        }
        else if (value instanceof byte[])
        {
            throw new MessageFormatException("Property " + propName + " of type byte[] " + "cannot be converted to String.");
        }
        else
        {
            return value.toString();
        }

    }

    public byte[] getBytes(String propName) throws JMSException
    {
        Object value = _map.get(propName);

        if (!_map.containsKey(propName))
        {
            throw new MessageFormatException("Property " + propName + " not present");
        }
        else if ((value instanceof byte[]) || (value == null))
        {
            return (byte[]) value;
        }
        else
        {
            throw new MessageFormatException("Property " + propName + " of type " + value.getClass().getName()
                + " cannot be converted to byte[].");
        }
    }

    public Object getObject(String propName) throws JMSException
    {
        return _map.get(propName);
    }

    public Enumeration getMapNames() throws JMSException
    {
        return Collections.enumeration(_map.keySet());
    }

    public void setBoolean(String propName, boolean b) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, b);
    }

    public void setByte(String propName, byte b) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, b);
    }

    public void setShort(String propName, short i) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, i);
    }

    public void setChar(String propName, char c) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, c);
    }

    public void setInt(String propName, int i) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, i);
    }

    public void setLong(String propName, long l) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, l);
    }

    public void setFloat(String propName, float v) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, v);
    }

    public void setDouble(String propName, double v) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, v);
    }

    public void setString(String propName, String string1) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, string1);
    }

    public void setBytes(String propName, byte[] bytes) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        _map.put(propName, bytes);
    }

    public void setBytes(String propName, byte[] bytes, int offset, int length) throws JMSException
    {
        if ((offset == 0) && (length == bytes.length))
        {
            setBytes(propName, bytes);
        }
        else
        {
            byte[] newBytes = new byte[length];
            System.arraycopy(bytes, offset, newBytes, 0, length);
            setBytes(propName, newBytes);
        }
    }

    public void setObject(String propName, Object value) throws JMSException
    {
        checkWritable();
        checkPropertyName(propName);
        if ((value instanceof Boolean) || (value instanceof Byte) || (value instanceof Short) || (value instanceof Integer)
                || (value instanceof Long) || (value instanceof Character) || (value instanceof Float)
                || (value instanceof Double) || (value instanceof String) || (value instanceof byte[]) || (value == null))
        {
            _map.put(propName, value);
        }
        else
        {
            throw new MessageFormatException("Cannot set property " + propName + " to value " + value + "of type "
                + value.getClass().getName() + ".");
        }
    }

    private void checkPropertyName(String propName)
    {
        if ((propName == null) || propName.equals(""))
        {
            throw new IllegalArgumentException("Property name cannot be null, or the empty String.");
        }
    }

    public boolean itemExists(String propName) throws JMSException
    {
        return _map.containsKey(propName);
    }

    private void populateMapFromData() throws JMSException
    {
        if (_data != null)
        {
            _data.rewind();

            final int entries = readIntImpl();
            for (int i = 0; i < entries; i++)
            {
                String propName = readStringImpl();
                Object value = readObject();
                _map.put(propName, value);
            }
        }
        else
        {
            _map.clear();
        }
    }

    private void writeMapToData()
    {
        allocateInitialBuffer();
        final int size = _map.size();
        writeIntImpl(size);
        for (Map.Entry<String, Object> entry : _map.entrySet())
        {
            try
            {
                writeStringImpl(entry.getKey());
            }
            catch (CharacterCodingException e)
            {
                throw new IllegalArgumentException("Cannot encode property key name " + entry.getKey(), e);

            }

            try
            {
                writeObject(entry.getValue());
            }
            catch (JMSException e)
            {
                Object value = entry.getValue();
                throw new IllegalArgumentException("Cannot encode property key name " + entry.getKey() + " value : " + value
                    + " (type: " + value.getClass().getName() + ").", e);
            }
        }

    }

}
