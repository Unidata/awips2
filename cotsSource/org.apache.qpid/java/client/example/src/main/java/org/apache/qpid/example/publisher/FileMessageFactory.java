/*
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
 */

package org.apache.qpid.example.publisher;

import org.apache.qpid.util.FileUtils;
import org.apache.qpid.example.shared.Statics;

import java.io.*;
import javax.jms.*;

public class FileMessageFactory
{
    protected final Session _session;
    protected final String _payload;
    protected final String _filename;

    /**
     * Contructs and instance using a filename from which content will be used to create message
     * @param session
     * @param filename
     * @throws MessageFactoryException
     */
    public FileMessageFactory(Session session, String filename) throws MessageFactoryException
    {
        try
        {
            _filename = filename;
            _payload = FileUtils.readFileAsString(filename);
            _session = session;
        }
        catch (Exception e)
        {
            MessageFactoryException mfe = new MessageFactoryException(e.toString(), e);
            throw mfe;
        }
    }

    /**
     * Creates a text message and sets filename property on it
     * The filename property is purely intended to provide visibility
     * of file content passing trhough the broker using example classes
     * @return  Message - a TextMessage with content from file
     * @throws JMSException
     */
    public Message createEventMessage() throws JMSException
    {
        TextMessage msg = _session.createTextMessage();
        msg.setText(_payload);
        msg.setStringProperty(Statics.FILENAME_PROPERTY, new File(_filename).getName());

        return msg;
    }

    /**
     * Creates message from a string for use by the monitor
     * @param session
     * @param textMsg - message content
     * @return Message - TextMessage with content from String
     * @throws JMSException
     */
    public static Message createSimpleEventMessage(Session session, String textMsg) throws JMSException
    {
        TextMessage msg = session.createTextMessage();
        msg.setText(textMsg);

        return msg;
    }

    public Message createShutdownMessage() throws JMSException
    {
        return _session.createTextMessage("SHUTDOWN");
    }

    public Message createReportRequestMessage() throws JMSException
    {
        return _session.createTextMessage("REPORT");
    }

    public Message createReportResponseMessage(String msg) throws JMSException
    {
        return _session.createTextMessage(msg);
    }

    public boolean isShutdown(Message m)
    {
        return checkText(m, "SHUTDOWN");
    }

    public boolean isReport(Message m)
    {
        return checkText(m, "REPORT");
    }

    public Object getReport(Message m)
    {
        try
        {
            return ((TextMessage) m).getText();
        }
        catch (JMSException e)
        {
            e.printStackTrace(System.out);

            return e.toString();
        }
    }

    private static boolean checkText(Message m, String s)
    {
        try
        {
            return (m instanceof TextMessage) && ((TextMessage) m).getText().equals(s);
        }
        catch (JMSException e)
        {
            e.printStackTrace(System.out);

            return false;
        }
    }
}
