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
package org.apache.qpid.test.unit.message;

import javax.jms.*;

import junit.framework.TestCase;

import org.apache.qpid.client.*;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.client.message.*;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.AMQException;

import java.util.Map;


public class MessageConverterTest extends TestCase
{

    public static final String JMS_CORR_ID = "QPIDID_01";
    public static final int JMS_DELIV_MODE = 1;
    public static final String JMS_TYPE = "test.jms.type";
    public static final Destination JMS_REPLY_TO = new AMQQueue(ExchangeDefaults.DIRECT_EXCHANGE_NAME,"my.replyto");

    protected JMSTextMessage testTextMessage;

    protected JMSMapMessage testMapMessage;
    private AMQSession _session = new TestAMQSession();


    protected void setUp() throws Exception
    {
        super.setUp();
        testTextMessage = new JMSTextMessage(AMQMessageDelegateFactory.FACTORY_0_8);

        //Set Message Text
        testTextMessage.setText("testTextMessage text");
        setMessageProperties(testTextMessage);

        testMapMessage = new JMSMapMessage(AMQMessageDelegateFactory.FACTORY_0_8);
        testMapMessage.setString("testMapString", "testMapStringValue");
        testMapMessage.setDouble("testMapDouble", Double.MAX_VALUE);
    }

    public void testSetProperties() throws Exception
    {
        AbstractJMSMessage newMessage = new MessageConverter(_session, (TextMessage) testTextMessage).getConvertedMessage();
        mesagePropertiesTest(testTextMessage, newMessage);
    }

    public void testJMSTextMessageConversion() throws Exception
    {
        AbstractJMSMessage newMessage = new MessageConverter(_session, (TextMessage) testTextMessage).getConvertedMessage();
        assertEquals("Converted message text mismatch", ((JMSTextMessage) newMessage).getText(), testTextMessage.getText());
    }

    public void testJMSMapMessageConversion() throws Exception
    {
        AbstractJMSMessage newMessage = new MessageConverter(_session, (MapMessage) testMapMessage).getConvertedMessage();
        assertEquals("Converted map message String mismatch", ((JMSMapMessage) newMessage).getString("testMapString"),
                     testMapMessage.getString("testMapString"));
        assertEquals("Converted map message Double mismatch", ((JMSMapMessage) newMessage).getDouble("testMapDouble"),
                     testMapMessage.getDouble("testMapDouble"));

    }

    public void testMessageConversion() throws Exception
    {
        Message newMessage = new NonQpidMessage();
        setMessageProperties(newMessage);
        mesagePropertiesTest(testTextMessage, newMessage);
    }

    private void setMessageProperties(Message message) throws JMSException
    {
        message.setJMSCorrelationID(JMS_CORR_ID);
        message.setJMSDeliveryMode(JMS_DELIV_MODE);
        message.setJMSType(JMS_TYPE);
        message.setJMSReplyTo(JMS_REPLY_TO);

        //Add non-JMS properties
        message.setStringProperty("testProp1", "testValue1");
        message.setDoubleProperty("testProp2", Double.MIN_VALUE);
    }


    private void mesagePropertiesTest(Message expectedMessage, Message actualMessage)
    {
        try
        {
            //check JMS prop values on newMessage match
            assertEquals("JMS Correlation ID mismatch", expectedMessage.getJMSCorrelationID(), actualMessage.getJMSCorrelationID());
            assertEquals("JMS Delivery mode mismatch", expectedMessage.getJMSDeliveryMode(), actualMessage.getJMSDeliveryMode());
            assertEquals("JMS Type mismatch", expectedMessage.getJMSType(), actualMessage.getJMSType());
            assertEquals("JMS Reply To mismatch", expectedMessage.getJMSReplyTo(), actualMessage.getJMSReplyTo());

            //check non-JMS standard props ok too
            assertEquals("Test String prop value mismatch", expectedMessage.getStringProperty("testProp1"),
                         actualMessage.getStringProperty("testProp1"));

            assertEquals("Test Double prop value mismatch", expectedMessage.getDoubleProperty("testProp2"),
                         actualMessage.getDoubleProperty("testProp2"));
        }
        catch (JMSException e)
        {
            fail("An error occured testing the property values" + e.getCause());
            e.printStackTrace();
        }
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
        testTextMessage = null;
    }


}
