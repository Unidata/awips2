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
package org.apache.qpid.test.unit.message;

import java.util.Enumeration;
import java.util.Hashtable;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;

public class NonQpidMessage implements Message
{
    private String _JMSMessageID;
    private long _JMSTimestamp;
    private byte[] _JMSCorrelationIDAsBytes;
    private String _JMSCorrelationID;
    private Destination _JMSReplyTo;
    private Destination _JMSDestination;
    private int _JMSDeliveryMode;
    private boolean _JMSRedelivered;
    private String _JMSType;
    private long _JMSExpiration;
    private int _JMSPriority;
    private Hashtable _properties;

    public NonQpidMessage()
    {
        _properties = new Hashtable();
        _JMSPriority = javax.jms.Message.DEFAULT_PRIORITY;
        _JMSDeliveryMode = javax.jms.Message.DEFAULT_DELIVERY_MODE;
    }

    public String getJMSMessageID() throws JMSException
    {
        return _JMSMessageID;
    }

    public void setJMSMessageID(String string) throws JMSException
    {
        _JMSMessageID = string;
    }

    public long getJMSTimestamp() throws JMSException
    {
        return _JMSTimestamp;
    }

    public void setJMSTimestamp(long l) throws JMSException
    {
        _JMSTimestamp = l;
    }

    public byte[] getJMSCorrelationIDAsBytes() throws JMSException
    {
        return _JMSCorrelationIDAsBytes;
    }

    public void setJMSCorrelationIDAsBytes(byte[] bytes) throws JMSException
    {
        _JMSCorrelationIDAsBytes = bytes;
    }

    public void setJMSCorrelationID(String string) throws JMSException
    {
        _JMSCorrelationID = string;
    }

    public String getJMSCorrelationID() throws JMSException
    {
        return _JMSCorrelationID;
    }

    public Destination getJMSReplyTo() throws JMSException
    {
        return _JMSReplyTo;
    }

    public void setJMSReplyTo(Destination destination) throws JMSException
    {
        _JMSReplyTo = destination;
    }

    public Destination getJMSDestination() throws JMSException
    {
        return _JMSDestination;
    }

    public void setJMSDestination(Destination destination) throws JMSException
    {
        _JMSDestination = destination;
    }

    public int getJMSDeliveryMode() throws JMSException
    {
        return _JMSDeliveryMode;
    }

    public void setJMSDeliveryMode(int i) throws JMSException
    {
        _JMSDeliveryMode = i;
    }

    public boolean getJMSRedelivered() throws JMSException
    {
        return _JMSRedelivered;
    }

    public void setJMSRedelivered(boolean b) throws JMSException
    {
        _JMSRedelivered = b;
    }

    public String getJMSType() throws JMSException
    {
        return _JMSType;
    }

    public void setJMSType(String string) throws JMSException
    {
        _JMSType = string;
    }

    public long getJMSExpiration() throws JMSException
    {
        return _JMSExpiration;
    }

    public void setJMSExpiration(long l) throws JMSException
    {
        _JMSExpiration = l;
    }

    public int getJMSPriority() throws JMSException
    {
        return _JMSPriority;
    }

    public void setJMSPriority(int i) throws JMSException
    {
        _JMSPriority = i;
    }

    public void clearProperties() throws JMSException
    {
        _properties.clear();
    }

    public boolean propertyExists(String string) throws JMSException
    {
        return _properties.containsKey(string);
    }

    public boolean getBooleanProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Boolean)
            {
                return (Boolean) o;
            }
            else
            {
                return Boolean.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public byte getByteProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Byte)
            {
                return (Byte) o;
            }
            else
            {
                return Byte.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public short getShortProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Short)
            {
                return (Short) o;
            }
            else
            {
                return Short.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public int getIntProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Integer)
            {
                return (Integer) o;
            }
            else
            {
                return Integer.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public long getLongProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Long)
            {
                return (Long) o;
            }
            else
            {
                return Long.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public float getFloatProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Float)
            {
                return (Float) o;
            }
            else
            {
                return Float.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public double getDoubleProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Double)
            {
                return (Double) o;
            }
            else
            {
                return Double.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public String getStringProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof String)
            {
                return (String) o;
            }
            else
            {
                return null;
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public Object getObjectProperty(String string) throws JMSException
    {
        if (propertyExists(string))
        {
            Object o = _properties.get(string);
            if (o instanceof Boolean)
            {
                return (Boolean) o;
            }
            else
            {
                return Boolean.valueOf(null);
            }
        }
        else
        {
            throw new JMSException("property does not exist: " + string);
        }
    }

    public Enumeration getPropertyNames() throws JMSException
    {
        return _properties.keys();
    }

    public void setBooleanProperty(String string, boolean b) throws JMSException
    {
        _properties.put(string, b);
    }

    public void setByteProperty(String string, byte b) throws JMSException
    {
        _properties.put(string, b);
    }

    public void setShortProperty(String string, short i) throws JMSException
    {
        _properties.put(string, i);
    }

    public void setIntProperty(String string, int i) throws JMSException
    {
        _properties.put(string, i);
    }

    public void setLongProperty(String string, long l) throws JMSException
    {
        _properties.put(string, l);
    }

    public void setFloatProperty(String string, float v) throws JMSException
    {
        _properties.put(string, v);
    }

    public void setDoubleProperty(String string, double v) throws JMSException
    {
        _properties.put(string, v);
    }

    public void setStringProperty(String string, String string1) throws JMSException
    {
        _properties.put(string, string1);
    }

    public void setObjectProperty(String string, Object object) throws JMSException
    {
        _properties.put(string, object);
    }

    public void acknowledge() throws JMSException
    {

    }

    public void clearBody() throws JMSException
    {

    }
}
