package org.apache.qpid.tools;
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


import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;
import javax.jms.TextMessage;

public class MessageFactory
{
    public static Message createBytesMessage(Session ssn, int size) throws JMSException
    {
        BytesMessage msg = ssn.createBytesMessage();
        msg.writeBytes(createMessagePayload(size).getBytes());
        return msg;
    }

    public static Message createTextMessage(Session ssn, int size) throws JMSException
    {
        TextMessage msg = ssn.createTextMessage();
        msg.setText(createMessagePayload(size));
        return msg;
    }

    public static String createMessagePayload(int size)
    {
        String msgData = "Qpid Test Message";

        StringBuffer buf = new StringBuffer(size);
        int count = 0;
        while (count <= (size - msgData.length()))
        {
            buf.append(msgData);
            count += msgData.length();
        }
        if (count < size)
        {
            buf.append(msgData, 0, size - count);
        }

        return buf.toString();
    }
}
