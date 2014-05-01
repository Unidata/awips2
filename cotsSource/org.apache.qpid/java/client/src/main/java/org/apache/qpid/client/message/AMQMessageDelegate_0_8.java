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

import org.apache.commons.collections.map.ReferenceMap;
import org.apache.mina.common.ByteBuffer;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.CustomJMSXProperty;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.client.AMQUndefinedDestination;
import org.apache.qpid.client.JMSAMQException;
import org.apache.qpid.framing.ContentHeaderProperties;
import org.apache.qpid.framing.BasicContentHeaderProperties;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.AMQException;
import org.apache.qpid.url.BindingURL;
import org.apache.qpid.url.AMQBindingURL;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageNotWriteableException;
import java.util.Map;
import java.util.Collections;
import java.util.Enumeration;
import java.util.UUID;
import java.net.URISyntaxException;

public class AMQMessageDelegate_0_8 extends AbstractAMQMessageDelegate
{
    private static final Map _destinationCache = Collections.synchronizedMap(new ReferenceMap());

    public static final String JMS_TYPE = "x-jms-type";


    private boolean _readableProperties = false;

    private Destination _destination;
    private JMSHeaderAdapter _headerAdapter;
    private static final boolean STRICT_AMQP_COMPLIANCE =
            Boolean.parseBoolean(System.getProperties().getProperty(AMQSession.STRICT_AMQP, AMQSession.STRICT_AMQP_DEFAULT));

    private ContentHeaderProperties _contentHeaderProperties;
    /** If the acknowledge mode is CLIENT_ACKNOWLEDGE the session is required */
    private AMQSession _session;
    private final long _deliveryTag;

    // The base set of items that needs to be set. 
    private AMQMessageDelegate_0_8(BasicContentHeaderProperties properties, long deliveryTag)
    {
        _contentHeaderProperties = properties;
        _deliveryTag = deliveryTag;
        _readableProperties = (_contentHeaderProperties != null);
        _headerAdapter = new JMSHeaderAdapter(((BasicContentHeaderProperties) _contentHeaderProperties).getHeaders());
    }

    // Used for the creation of new messages
    protected AMQMessageDelegate_0_8()
    {
        this(new BasicContentHeaderProperties(), -1);
        _readableProperties = false;
        _headerAdapter = new JMSHeaderAdapter(((BasicContentHeaderProperties) _contentHeaderProperties).getHeaders());

    }

    // Used when generating a received message object
    protected AMQMessageDelegate_0_8(long deliveryTag, BasicContentHeaderProperties contentHeader, AMQShortString exchange,
                                     AMQShortString routingKey) 
    {
        this(contentHeader, deliveryTag);

        Integer type = contentHeader.getHeaders().getInteger(CustomJMSXProperty.JMS_QPID_DESTTYPE.getShortStringName());

        AMQDestination dest = null;

        // If we have a type set the attempt to use that.
        if (type != null)
        {
            switch (type.intValue())
            {
                case AMQDestination.QUEUE_TYPE:
                    dest = new AMQQueue(exchange, routingKey, routingKey);
                    break;
                case AMQDestination.TOPIC_TYPE:
                    dest = new AMQTopic(exchange, routingKey, null);
                    break;
                default:
                    // Use the generateDestination method
                    dest = null;
            }
        }

        if (dest == null)
        {
            dest = generateDestination(exchange, routingKey);
        }

        setJMSDestination(dest);
    }



    public String getJMSMessageID() throws JMSException
    {
        return getContentHeaderProperties().getMessageIdAsString();
    }

    public void setJMSMessageID(String messageId) throws JMSException
    {
        if (messageId != null)
        {
            getContentHeaderProperties().setMessageId(messageId);
        }
    }

    public void setJMSMessageID(UUID messageId) throws JMSException
    {
        if (messageId != null)
        {
            getContentHeaderProperties().setMessageId("ID:" + messageId);
        }
    }


    public long getJMSTimestamp() throws JMSException
    {
        return getContentHeaderProperties().getTimestamp();
    }

    public void setJMSTimestamp(long timestamp) throws JMSException
    {
        getContentHeaderProperties().setTimestamp(timestamp);
    }

    public byte[] getJMSCorrelationIDAsBytes() throws JMSException
    {
        return getContentHeaderProperties().getCorrelationIdAsString().getBytes();
    }

    public void setJMSCorrelationIDAsBytes(byte[] bytes) throws JMSException
    {
        getContentHeaderProperties().setCorrelationId(new String(bytes));
    }

    public void setJMSCorrelationID(String correlationId) throws JMSException
    {
        getContentHeaderProperties().setCorrelationId(correlationId);
    }

    public String getJMSCorrelationID() throws JMSException
    {
        return getContentHeaderProperties().getCorrelationIdAsString();
    }

    public Destination getJMSReplyTo() throws JMSException
    {
        String replyToEncoding = getContentHeaderProperties().getReplyToAsString();
        if (replyToEncoding == null)
        {
            return null;
        }
        else
        {
            Destination dest = (Destination) _destinationCache.get(replyToEncoding);
            if (dest == null)
            {
                try
                {
                    BindingURL binding = new AMQBindingURL(replyToEncoding);
                    dest = AMQDestination.createDestination(binding);
                }
                catch (URISyntaxException e)
                {
                    throw new JMSAMQException("Illegal value in JMS_ReplyTo property: " + replyToEncoding, e);
                }

                _destinationCache.put(replyToEncoding, dest);
            }

            return dest;
        }
    }

    public void setJMSReplyTo(Destination destination) throws JMSException
    {
        if (destination == null)
        {
            getContentHeaderProperties().setReplyTo((String) null);
            return; // We're done here
        }

        if (!(destination instanceof AMQDestination))
        {
            throw new IllegalArgumentException(
                "ReplyTo destination may only be an AMQDestination - passed argument was type " + destination.getClass());
        }

        final AMQDestination amqd = (AMQDestination) destination;

        final AMQShortString encodedDestination = amqd.getEncodedName();
        _destinationCache.put(encodedDestination, destination);
        getContentHeaderProperties().setReplyTo(encodedDestination);
    }

    public Destination getJMSDestination() throws JMSException
    {
        return _destination;
    }

    public void setJMSDestination(Destination destination)
    {
        _destination = destination;
    }

    public void setContentType(String contentType)
    {
        getContentHeaderProperties().setContentType(contentType);
    }

    public String getContentType()
    {
        return getContentHeaderProperties().getContentTypeAsString();
    }

    public void setEncoding(String encoding)
    {
        getContentHeaderProperties().setEncoding(encoding);
    }

    public String getEncoding()
    {
        return getContentHeaderProperties().getEncodingAsString();
    }

    public String getReplyToString()
    {
        return getContentHeaderProperties().getReplyToAsString();
    }

    public int getJMSDeliveryMode() throws JMSException
    {
        return getContentHeaderProperties().getDeliveryMode();
    }

    public void setJMSDeliveryMode(int i) throws JMSException
    {
        getContentHeaderProperties().setDeliveryMode((byte) i);
    }

    public BasicContentHeaderProperties getContentHeaderProperties()
    {
        return (BasicContentHeaderProperties) _contentHeaderProperties;
    }


    public String getJMSType() throws JMSException
    {
        return getContentHeaderProperties().getTypeAsString();
    }

    public void setJMSType(String string) throws JMSException
    {
        getContentHeaderProperties().setType(string);
    }

    public long getJMSExpiration() throws JMSException
    {
        return getContentHeaderProperties().getExpiration();
    }

    public void setJMSExpiration(long l) throws JMSException
    {
        getContentHeaderProperties().setExpiration(l);
    }



    public boolean propertyExists(String propertyName) throws JMSException
    {
        return getJmsHeaders().propertyExists(propertyName);
    }

    public boolean getBooleanProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getBoolean(propertyName);
    }

    public byte getByteProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getByte(propertyName);
    }

    public short getShortProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getShort(propertyName);
    }

    public int getIntProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getInteger(propertyName);
    }

    public long getLongProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getLong(propertyName);
    }

    public float getFloatProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getFloat(propertyName);
    }

    public double getDoubleProperty(String propertyName) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        return getJmsHeaders().getDouble(propertyName);
    }

    public String getStringProperty(String propertyName) throws JMSException
    {
        //NOTE: if the JMSX Property is a non AMQP property then we must check _strictAMQP and throw as below.
        if (propertyName.equals(CustomJMSXProperty.JMSXUserID.toString()))
        {
            return ((BasicContentHeaderProperties) _contentHeaderProperties).getUserIdAsString();
        }
        else
        {
            if (STRICT_AMQP_COMPLIANCE)
            {
                throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
            }

            return getJmsHeaders().getString(propertyName);
        }
    }

    public Object getObjectProperty(String propertyName) throws JMSException
    {
        return getJmsHeaders().getObject(propertyName);
    }

    public Enumeration getPropertyNames() throws JMSException
    {
        return getJmsHeaders().getPropertyNames();
    }

    public void setBooleanProperty(String propertyName, boolean b) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        checkWritableProperties();
        getJmsHeaders().setBoolean(propertyName, b);
    }

    public void setByteProperty(String propertyName, byte b) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        checkWritableProperties();
        getJmsHeaders().setByte(propertyName, new Byte(b));
    }

    public void setShortProperty(String propertyName, short i) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        checkWritableProperties();
        getJmsHeaders().setShort(propertyName, new Short(i));
    }

    public void setIntProperty(String propertyName, int i) throws JMSException
    {
        checkWritableProperties();
        getJmsHeaders().setInteger(propertyName, new Integer(i));
    }

    public void setLongProperty(String propertyName, long l) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        checkWritableProperties();
        getJmsHeaders().setLong(propertyName, new Long(l));
    }

    public void setFloatProperty(String propertyName, float f) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        checkWritableProperties();
        getJmsHeaders().setFloat(propertyName, new Float(f));
    }

    public void setDoubleProperty(String propertyName, double v) throws JMSException
    {
        if (STRICT_AMQP_COMPLIANCE)
        {
            throw new UnsupportedOperationException("JMS Proprerties not supported in AMQP");
        }

        checkWritableProperties();
        getJmsHeaders().setDouble(propertyName, new Double(v));
    }

    public void setStringProperty(String propertyName, String value) throws JMSException
    {
        checkWritableProperties();
        getJmsHeaders().setString(propertyName, value);
    }

    public void setObjectProperty(String propertyName, Object object) throws JMSException
    {
        checkWritableProperties();
        getJmsHeaders().setObject(propertyName, object);
    }

    public void removeProperty(String propertyName) throws JMSException
    {
        getJmsHeaders().remove(propertyName);
    }


    private JMSHeaderAdapter getJmsHeaders()
    {
        return _headerAdapter;
    }

    protected void checkWritableProperties() throws MessageNotWriteableException
    {
        if (_readableProperties)
        {
            throw new MessageNotWriteableException("You need to call clearProperties() to make the message writable");
        }
        _contentHeaderProperties.updated();
    }


    public int getJMSPriority() throws JMSException
    {
        return getContentHeaderProperties().getPriority();
    }

    public void setJMSPriority(int i) throws JMSException
    {
        getContentHeaderProperties().setPriority((byte) i);
    }

    public void clearProperties() throws JMSException
    {
        getJmsHeaders().clear();

        _readableProperties = false;
    }


    public void acknowledgeThis() throws JMSException
    {
        // the JMS 1.1 spec says in section 3.6 that calls to acknowledge are ignored when client acknowledge
        // is not specified. In our case, we only set the session field where client acknowledge mode is specified.
        if (_session != null)
        {
            if (_session.getAMQConnection().isClosed())
            {
                throw new javax.jms.IllegalStateException("Connection is already closed");
            }

            // we set multiple to true here since acknowledgement implies acknowledge of all previous messages
            // received on the session
            _session.acknowledgeMessage(_deliveryTag, true);
        }
    }

    public void acknowledge() throws JMSException
    {
        if (_session != null)
        {
            _session.acknowledge();
        }
    }
    

     /**
     * The session is set when CLIENT_ACKNOWLEDGE mode is used so that the CHANNEL ACK can be sent when the user calls
     * acknowledge()
     *
     * @param s the AMQ session that delivered this message
     */
    public void setAMQSession(AMQSession s)
    {
        _session = s;
    }

    public AMQSession getAMQSession()
    {
        return _session;
    }

    /**
     * Get the AMQ message number assigned to this message
     *
     * @return the message number
     */
    public long getDeliveryTag()
    {
        return _deliveryTag;
    }


}
