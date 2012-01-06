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
package org.apache.qpid.client.message;

import java.util.Enumeration;

import javax.jms.JMSException;
import javax.jms.MessageFormatException;

import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.AMQPInvalidClassException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;


public final class JMSHeaderAdapter
{
    private final FieldTable _headers;

    public JMSHeaderAdapter(FieldTable headers)
    {
        _headers = headers;
    }


    public FieldTable getHeaders()
    {
        return _headers;
    }

    public boolean getBoolean(String string) throws JMSException
    {
        checkPropertyName(string);
        Boolean b = getHeaders().getBoolean(string);

        if (b == null)
        {
            if (getHeaders().containsKey(string))
            {
                Object str = getHeaders().getObject(string);

                if (str == null || !(str instanceof String))
                {
                    throw new MessageFormatException("getBoolean can't use " + string + " item.");
                }
                else
                {
                    return Boolean.valueOf((String) str);
                }
            }
            else
            {
                b = Boolean.valueOf(null);
            }
        }

        return b;
    }

    public boolean getBoolean(AMQShortString string) throws JMSException
    {
        checkPropertyName(string);
        Boolean b = getHeaders().getBoolean(string);

        if (b == null)
        {
            if (getHeaders().containsKey(string))
            {
                Object str = getHeaders().getObject(string);

                if (str == null || !(str instanceof String))
                {
                    throw new MessageFormatException("getBoolean can't use " + string + " item.");
                }
                else
                {
                    return Boolean.valueOf((String) str);
                }
            }
            else
            {
                b = Boolean.valueOf(null);
            }
        }

        return b;
    }

    public char getCharacter(String string) throws JMSException
    {
        checkPropertyName(string);
        Character c = getHeaders().getCharacter(string);

        if (c == null)
        {
            if (getHeaders().isNullStringValue(string))
            {
                throw new NullPointerException("Cannot convert null char");
            }
            else
            {
                throw new MessageFormatException("getChar can't use " + string + " item.");
            }
        }
        else
        {
            return (char) c;
        }
    }

    public byte[] getBytes(String string) throws JMSException
    {
        return getBytes(new AMQShortString(string));
    }

    public byte[] getBytes(AMQShortString string) throws JMSException
    {
        checkPropertyName(string);

        byte[] bs = getHeaders().getBytes(string);

        if (bs == null)
        {
            throw new MessageFormatException("getBytes can't use " + string + " item.");
        }
        else
        {
            return bs;
        }
    }

    public byte getByte(String string) throws JMSException
    {
        checkPropertyName(string);
        Byte b = getHeaders().getByte(string);
        if (b == null)
        {
            if (getHeaders().containsKey(string))
            {
                Object str = getHeaders().getObject(string);

                if (str == null || !(str instanceof String))
                {
                    throw new MessageFormatException("getByte can't use " + string + " item.");
                }
                else
                {
                    return Byte.valueOf((String) str);
                }
            }
            else
            {
                b = Byte.valueOf(null);
            }
        }

        return b;
    }

    public short getShort(String string) throws JMSException
    {
        checkPropertyName(string);
        Short s = getHeaders().getShort(string);

        if (s == null)
        {
            s = Short.valueOf(getByte(string));
        }

        return s;
    }

    public int getInteger(String string) throws JMSException
    {
        checkPropertyName(string);
        Integer i = getHeaders().getInteger(string);

        if (i == null)
        {
            i = Integer.valueOf(getShort(string));
        }

        return i;
    }

    public long getLong(String string) throws JMSException
    {
        checkPropertyName(string);
        Long l = getHeaders().getLong(string);

        if (l == null)
        {
            l = Long.valueOf(getInteger(string));
        }

        return l;
    }

    public float getFloat(String string) throws JMSException
    {
        checkPropertyName(string);
        Float f = getHeaders().getFloat(string);

        if (f == null)
        {
            if (getHeaders().containsKey(string))
            {
                Object str = getHeaders().getObject(string);

                if (str == null || !(str instanceof String))
                {
                    throw new MessageFormatException("getFloat can't use " + string + " item.");
                }
                else
                {
                    return Float.valueOf((String) str);
                }
            }
            else
            {
                f = Float.valueOf(null);
            }

        }

        return f;
    }

    public double getDouble(String string) throws JMSException
    {
        checkPropertyName(string);
        Double d = getHeaders().getDouble(string);

        if (d == null)
        {
            d = Double.valueOf(getFloat(string));
        }

        return d;
    }

    public String getString(String string) throws JMSException
    {
        checkPropertyName(string);
        String s = getHeaders().getString(string);

        if (s == null)
        {
            if (getHeaders().containsKey(string))
            {
                Object o = getHeaders().getObject(string);
                if (o instanceof byte[])
                {
                    throw new MessageFormatException("getObject couldn't find " + string + " item.");
                }
                else
                {
                    if (o == null)
                    {
                        return null;
                    }
                    else
                    {
                        s = String.valueOf(o);
                    }
                }
            }//else return s // null; 
        }

        return s;
    }

    public Object getObject(String string) throws JMSException
    {
        checkPropertyName(string);
        return getHeaders().getObject(string);
    }

    public void setBoolean(AMQShortString string, boolean b) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setBoolean(string, b);
    }

    public void setBoolean(String string, boolean b) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setBoolean(string, b);
    }

    public void setChar(String string, char c) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setChar(string, c);
    }

    public Object setBytes(AMQShortString string, byte[] bytes)
    {
        checkPropertyName(string);
        return getHeaders().setBytes(string, bytes);
    }

    public Object setBytes(String string, byte[] bytes)
    {
        checkPropertyName(string);
        return getHeaders().setBytes(string, bytes);
    }

    public Object setBytes(String string, byte[] bytes, int start, int length)
    {
        checkPropertyName(string);
        return getHeaders().setBytes(string, bytes, start, length);
    }

    public void setByte(String string, byte b) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setByte(string, b);
    }

    public void setByte(AMQShortString string, byte b) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setByte(string, b);
    }


    public void setShort(String string, short i) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setShort(string, i);
    }

    public void setInteger(String string, int i) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setInteger(string, i);
    }

    public void setInteger(AMQShortString string, int i) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setInteger(string, i);
    }

    public void setLong(String string, long l) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setLong(string, l);
    }

    public void setFloat(String string, float v) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setFloat(string, v);
    }

    public void setDouble(String string, double v) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setDouble(string, v);
    }

    public void setString(String string, String string1) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setString(string, string1);
    }

    public void setString(AMQShortString string, String string1) throws JMSException
    {
        checkPropertyName(string);
        getHeaders().setString(string, string1);
    }

    public void setObject(String string, Object object) throws JMSException
    {
        checkPropertyName(string);
        try
        {
            getHeaders().setObject(string, object);
        }
        catch (AMQPInvalidClassException aice)
        {
            MessageFormatException mfe = new MessageFormatException("Only primatives are allowed object is:" + object.getClass());
            mfe.setLinkedException(aice);
            throw mfe;
        }
    }

    public boolean itemExists(String string) throws JMSException
    {
        checkPropertyName(string);
        return getHeaders().containsKey(string);
    }

    public Enumeration getPropertyNames()
    {
        return getHeaders().getPropertyNames();
    }

    public void clear()
    {
        getHeaders().clear();
    }

    public boolean propertyExists(AMQShortString propertyName)
    {
        checkPropertyName(propertyName);
        return getHeaders().propertyExists(propertyName);
    }

    public boolean propertyExists(String propertyName)
    {
        checkPropertyName(propertyName);
        return getHeaders().propertyExists(propertyName);
    }

    public Object put(Object key, Object value)
    {
        checkPropertyName(key.toString());
        return getHeaders().setObject(key.toString(), value);
    }

    public Object remove(AMQShortString propertyName)
    {
        checkPropertyName(propertyName);
        return getHeaders().remove(propertyName);
    }

    public Object remove(String propertyName)
    {
        checkPropertyName(propertyName);
        return getHeaders().remove(propertyName);
    }

    public boolean isEmpty()
    {
        return getHeaders().isEmpty();
    }

    public void writeToBuffer(ByteBuffer data)
    {
        getHeaders().writeToBuffer(data);
    }

    public Enumeration getMapNames()
    {
        return getPropertyNames();
    }

    protected void checkPropertyName(CharSequence propertyName)
    {
        if (propertyName == null)
        {
            throw new IllegalArgumentException("Property name must not be null");
        }
        else if (propertyName.length() == 0)
        {
            throw new IllegalArgumentException("Property name must not be the empty string");
        }

        checkIdentiferFormat(propertyName);
    }

    protected void checkIdentiferFormat(CharSequence propertyName)
    {
//        JMS requirements 3.5.1 Property Names
//        Identifiers:
//        - An identifier is an unlimited-length character sequence that must begin
//          with a Java identifier start character; all following characters must be Java
//          identifier part characters. An identifier start character is any character for
//          which the method Character.isJavaIdentifierStart returns true. This includes
//          '_' and '$'. An identifier part character is any character for which the
//          method Character.isJavaIdentifierPart returns true.
//        - Identifiers cannot be the names NULL, TRUE, or FALSE.
//          Identifiers cannot be NOT, AND, OR, BETWEEN, LIKE, IN, IS, or
//          ESCAPE.
//          Identifiers are either header field references or property references. The
//          type of a property value in a message selector corresponds to the type
//          used to set the property. If a property that does not exist in a message is
//          referenced, its value is NULL. The semantics of evaluating NULL values
//          in a selector are described in Section 3.8.1.2, Null Values.
//          The conversions that apply to the get methods for properties do not
//          apply when a property is used in a message selector expression. For
//          example, suppose you set a property as a string value, as in the
//          following:
//              myMessage.setStringProperty("NumberOfOrders", "2");
//          The following expression in a message selector would evaluate to false,
//          because a string cannot be used in an arithmetic expression:
//          "NumberOfOrders > 1"
//          Identifiers are case sensitive.
//          Message header field references are restricted to JMSDeliveryMode,
//          JMSPriority, JMSMessageID, JMSTimestamp, JMSCorrelationID, and
//          JMSType. JMSMessageID, JMSCorrelationID, and JMSType values may be
//          null and if so are treated as a NULL value.

        if (Boolean.getBoolean("strict-jms"))
        {
            // JMS start character
            if (!(Character.isJavaIdentifierStart(propertyName.charAt(0))))
            {
                throw new IllegalArgumentException("Identifier '" + propertyName + "' does not start with a valid JMS identifier start character");
            }

            // JMS part character
            int length = propertyName.length();
            for (int c = 1; c < length; c++)
            {
                if (!(Character.isJavaIdentifierPart(propertyName.charAt(c))))
                {
                    throw new IllegalArgumentException("Identifier '" + propertyName + "' contains an invalid JMS identifier character");
                }
            }

            // JMS invalid names
            if ((propertyName.equals("NULL")
                 || propertyName.equals("TRUE")
                 || propertyName.equals("FALSE")
                 || propertyName.equals("NOT")
                 || propertyName.equals("AND")
                 || propertyName.equals("OR")
                 || propertyName.equals("BETWEEN")
                 || propertyName.equals("LIKE")
                 || propertyName.equals("IN")
                 || propertyName.equals("IS")
                 || propertyName.equals("ESCAPE")))
            {
                throw new IllegalArgumentException("Identifier '" + propertyName + "' is not allowed in JMS");
            }
        }

    }
}
