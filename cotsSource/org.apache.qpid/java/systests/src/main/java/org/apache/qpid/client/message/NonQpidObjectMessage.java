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

import java.io.Serializable;
import java.util.Enumeration;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.ObjectMessage;
import javax.jms.Session;

public class NonQpidObjectMessage implements ObjectMessage {

    private ObjectMessage _realMessage;
    private String _contentString;

    /**
     * Allows us to construct a JMS message which
     * does not inherit from the Qpid message superclasses
     * and expand our unit testing of MessageConverter et al
     * @param session
     */
    public NonQpidObjectMessage(Session session) throws JMSException
    {
        _realMessage = session.createObjectMessage();
    }

    public String getJMSMessageID() throws JMSException {
        return _realMessage.getJMSMessageID();
    }

    public void setJMSMessageID(String string) throws JMSException {
        _realMessage.setJMSMessageID(string);
    }

    public long getJMSTimestamp() throws JMSException {
        return _realMessage.getJMSTimestamp();
    }

    public void setJMSTimestamp(long l) throws JMSException {
        _realMessage.setJMSTimestamp(l);
    }

    public byte[] getJMSCorrelationIDAsBytes() throws JMSException {
        return _realMessage.getJMSCorrelationIDAsBytes();
    }

    public void setJMSCorrelationIDAsBytes(byte[] bytes) throws JMSException {
        _realMessage.setJMSCorrelationIDAsBytes(bytes);
    }

    public void setJMSCorrelationID(String string) throws JMSException {
        _realMessage.setJMSCorrelationID(string);
    }

    public String getJMSCorrelationID() throws JMSException {
        return _realMessage.getJMSCorrelationID();
    }

    public Destination getJMSReplyTo() throws JMSException {
        return _realMessage.getJMSReplyTo();
    }

    public void setJMSReplyTo(Destination destination) throws JMSException {
        _realMessage.setJMSReplyTo(destination);
    }

    public Destination getJMSDestination() throws JMSException {
        return _realMessage.getJMSDestination();
    }

    public void setJMSDestination(Destination destination) throws JMSException {
        _realMessage.setJMSDestination(destination);
    }

    public int getJMSDeliveryMode() throws JMSException {
        return _realMessage.getJMSDeliveryMode();
    }

    public void setJMSDeliveryMode(int i) throws JMSException {
        _realMessage.setJMSDeliveryMode(i);
    }

    public boolean getJMSRedelivered() throws JMSException {
        return _realMessage.getJMSRedelivered();
    }

    public void setJMSRedelivered(boolean b) throws JMSException {
        _realMessage.setJMSRedelivered(b);
    }

    public String getJMSType() throws JMSException {
        return _realMessage.getJMSType();
    }

    public void setJMSType(String string) throws JMSException {
        _realMessage.setJMSType(string);
    }

    public long getJMSExpiration() throws JMSException {
        return _realMessage.getJMSExpiration();
    }

    public void setJMSExpiration(long l) throws JMSException {
        _realMessage.setJMSExpiration(l);
    }

    public int getJMSPriority() throws JMSException {
        return _realMessage.getJMSPriority();
    }

    public void setJMSPriority(int i) throws JMSException {
        _realMessage.setJMSPriority(i);
    }

    public void clearProperties() throws JMSException {
        _realMessage.clearProperties();
    }

    public boolean propertyExists(String string) throws JMSException {
        return _realMessage.propertyExists(string);
    }

    public boolean getBooleanProperty(String string) throws JMSException {
        return _realMessage.getBooleanProperty(string);
    }

    public byte getByteProperty(String string) throws JMSException {
        return _realMessage.getByteProperty(string);
    }

    public short getShortProperty(String string) throws JMSException {
        return _realMessage.getShortProperty(string);
    }

    public int getIntProperty(String string) throws JMSException {
        return _realMessage.getIntProperty(string);
    }

    public long getLongProperty(String string) throws JMSException {
        return _realMessage.getLongProperty(string);
    }

    public float getFloatProperty(String string) throws JMSException {
        return _realMessage.getFloatProperty(string);
    }

    public double getDoubleProperty(String string) throws JMSException {
        return _realMessage.getDoubleProperty(string);
    }

    public String getStringProperty(String string) throws JMSException {
        return _realMessage.getStringProperty(string);
    }

    public Object getObjectProperty(String string) throws JMSException {
        return _realMessage.getObjectProperty(string);
    }

    public Enumeration getPropertyNames() throws JMSException {
        return _realMessage.getPropertyNames();
    }

    public void setBooleanProperty(String string, boolean b) throws JMSException {
        _realMessage.setBooleanProperty(string,b);
    }

    public void setByteProperty(String string, byte b) throws JMSException {
        _realMessage.setByteProperty(string,b);
    }

    public void setShortProperty(String string, short i) throws JMSException {
        _realMessage.setShortProperty(string,i);
    }

    public void setIntProperty(String string, int i) throws JMSException {
        _realMessage.setIntProperty(string,i);
    }

    public void setLongProperty(String string, long l) throws JMSException {
        _realMessage.setLongProperty(string,l);
    }

    public void setFloatProperty(String string, float v) throws JMSException {
        _realMessage.setFloatProperty(string,v);
    }

    public void setDoubleProperty(String string, double v) throws JMSException {
        _realMessage.setDoubleProperty(string,v);
    }

    public void setStringProperty(String string, String string1) throws JMSException {
        _realMessage.setStringProperty(string,string1);
    }

    public void setObjectProperty(String string, Object object) throws JMSException {
        _realMessage.setObjectProperty(string,object);
    }

    public void acknowledge() throws JMSException {
        _realMessage.acknowledge();
    }

    public void clearBody() throws JMSException {
        _realMessage.clearBody();
    }

    public void setObject(Serializable serializable) throws JMSException {
        if (serializable instanceof String)
        {
            _contentString = (String)serializable;
        }
    }

    public Serializable getObject() throws JMSException {
        return _contentString;      }
}
