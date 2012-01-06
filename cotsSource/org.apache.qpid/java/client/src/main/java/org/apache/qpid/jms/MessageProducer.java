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
package org.apache.qpid.jms;

import java.io.UnsupportedEncodingException;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;

/**
 */
public interface MessageProducer extends javax.jms.MessageProducer
{
    /**
     * Set the default MIME type for messages produced by this producer. This reduces the overhead of each message.
     * @param mimeType
     */
    void setMimeType(String mimeType) throws JMSException;

    /**
     * Set the default encoding for messages produced by this producer. This reduces the overhead of each message.
     * @param encoding the encoding as understood by XXXX how do I specify this?? RG
     * @throws UnsupportedEncodingException if the encoding is not understood
     */
    void setEncoding(String encoding) throws UnsupportedEncodingException, JMSException;
    
    void send(Destination destination, Message message, int deliveryMode,
                     int priority, long timeToLive, boolean immediate)
            throws JMSException;

    void send(Destination destination, Message message, int deliveryMode,
                     int priority, long timeToLive, boolean mandatory, boolean immediate)
            throws JMSException;

    void send(Destination destination, Message message, int deliveryMode, int priority, long timeToLive,
                     boolean mandatory, boolean immediate, boolean waitUntilSent) throws JMSException;

}
