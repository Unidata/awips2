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

import org.apache.qpid.client.AMQSession;

import javax.jms.Destination;
import javax.jms.JMSException;
import java.util.Enumeration;
import java.util.UUID;

public interface AMQMessageDelegate
{
    void acknowledgeThis() throws JMSException;

    String getJMSMessageID() throws JMSException;

    void setJMSMessageID(String string) throws JMSException;

    long getJMSTimestamp() throws JMSException;

    void setJMSTimestamp(long l) throws JMSException;

    byte[] getJMSCorrelationIDAsBytes() throws JMSException;

    void setJMSCorrelationIDAsBytes(byte[] bytes) throws JMSException;

    void setJMSCorrelationID(String string) throws JMSException;

    String getJMSCorrelationID() throws JMSException;

    Destination getJMSReplyTo() throws JMSException;

    void setJMSReplyTo(Destination destination) throws JMSException;

    Destination getJMSDestination() throws JMSException;

    int getJMSDeliveryMode() throws JMSException;

    void setJMSDeliveryMode(int i) throws JMSException;

    String getJMSType() throws JMSException;

    void setJMSType(String string) throws JMSException;

    long getJMSExpiration() throws JMSException;

    void setJMSExpiration(long l) throws JMSException;

    int getJMSPriority() throws JMSException;

    void setJMSPriority(int i) throws JMSException;

    void clearProperties() throws JMSException;

    boolean propertyExists(String string) throws JMSException;

    boolean getBooleanProperty(String string) throws JMSException;

    byte getByteProperty(String string) throws JMSException;

    short getShortProperty(String string) throws JMSException;

    int getIntProperty(String string) throws JMSException;

    long getLongProperty(String string) throws JMSException;

    float getFloatProperty(String string) throws JMSException;

    double getDoubleProperty(String string) throws JMSException;

    String getStringProperty(String string) throws JMSException;

    Object getObjectProperty(String string) throws JMSException;

    Enumeration getPropertyNames() throws JMSException;

    void setBooleanProperty(String string, boolean b) throws JMSException;

    void setByteProperty(String string, byte b) throws JMSException;

    void setShortProperty(String string, short i) throws JMSException;

    void setIntProperty(String string, int i) throws JMSException;

    void setLongProperty(String string, long l) throws JMSException;

    void setFloatProperty(String string, float v) throws JMSException;

    void setDoubleProperty(String string, double v) throws JMSException;

    void setStringProperty(String string, String string1) throws JMSException;

    void setObjectProperty(String string, Object object) throws JMSException;

    void acknowledge() throws JMSException;

    public void setJMSDestination(Destination destination);

    public void setContentType(String contentType);
    public String getContentType();

    public void setEncoding(String encoding);
    public String getEncoding();


    String getReplyToString();

    void removeProperty(final String propertyName) throws JMSException;

    void setAMQSession(final AMQSession s);

    AMQSession getAMQSession();

    long getDeliveryTag();

    void setJMSMessageID(final UUID messageId) throws JMSException;
}
