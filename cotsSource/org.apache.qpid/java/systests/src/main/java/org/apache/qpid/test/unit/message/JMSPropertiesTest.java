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

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.message.NonQpidObjectMessage;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Destination;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.Session;
import java.util.Enumeration;

/**
 * @author Apache Software Foundation
 */
public class JMSPropertiesTest extends QpidTestCase
{

    private static final Logger _logger = LoggerFactory.getLogger(JMSPropertiesTest.class);

    public String _connectionString = "vm://:1";

    public static final String JMS_CORR_ID = "QPIDID_01";
    public static final int JMS_DELIV_MODE = 1;
    public static final String JMS_TYPE = "test.jms.type";

    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testJMSProperties() throws Exception
    {
        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQSession consumerSession = (AMQSession) con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        Queue queue =
            new AMQQueue(con.getDefaultQueueExchangeName(), new AMQShortString("someQ"), new AMQShortString("someQ"), false,
                true);
        MessageConsumer consumer = consumerSession.createConsumer(queue);

        AMQConnection con2 = (AMQConnection) getConnection("guest", "guest");
        Session producerSession = con2.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        MessageProducer producer = producerSession.createProducer(queue);
        Destination JMS_REPLY_TO = new AMQQueue(con2, "my.replyto");
        // create a test message to send
        ObjectMessage sentMsg = new NonQpidObjectMessage(producerSession);
        sentMsg.setJMSCorrelationID(JMS_CORR_ID);
        sentMsg.setJMSDeliveryMode(JMS_DELIV_MODE);
        sentMsg.setJMSType(JMS_TYPE);
        sentMsg.setJMSReplyTo(JMS_REPLY_TO);

        String JMSXGroupID_VALUE = "group";
        sentMsg.setStringProperty("JMSXGroupID", JMSXGroupID_VALUE);
        
        int JMSXGroupSeq_VALUE = 1;
        sentMsg.setIntProperty("JMSXGroupSeq", JMSXGroupSeq_VALUE);

        // send it
        producer.send(sentMsg);

        con2.close();

        con.start();

        // get message and check JMS properties
        ObjectMessage rm = (ObjectMessage) consumer.receive(2000);
        assertNotNull(rm);

        assertEquals("JMS Correlation ID mismatch", sentMsg.getJMSCorrelationID(), rm.getJMSCorrelationID());
        // TODO: Commented out as always overwritten by send delivery mode value - prob should not set in conversion
        // assertEquals("JMS Delivery Mode mismatch",sentMsg.getJMSDeliveryMode(),rm.getJMSDeliveryMode());
        assertEquals("JMS Type mismatch", sentMsg.getJMSType(), rm.getJMSType());
        assertEquals("JMS Reply To mismatch", sentMsg.getJMSReplyTo(), rm.getJMSReplyTo());
        assertTrue("JMSMessageID Does not start ID:", rm.getJMSMessageID().startsWith("ID:"));
        assertEquals("JMS Default priority should be 4",Message.DEFAULT_PRIORITY,rm.getJMSPriority());   
        
        //Validate that the JMSX values are correct
        assertEquals("JMSXGroupID is not as expected:", JMSXGroupID_VALUE, rm.getStringProperty("JMSXGroupID"));
        assertEquals("JMSXGroupSeq is not as expected:", JMSXGroupSeq_VALUE, rm.getIntProperty("JMSXGroupSeq"));

        boolean JMSXGroupID_Available = false;
        boolean JMSXGroupSeq_Available = false;
        Enumeration props = con.getMetaData().getJMSXPropertyNames();
        while (props.hasMoreElements())
        {
            String name = (String) props.nextElement();
            if (name.equals("JMSXGroupID"))
            {
                JMSXGroupID_Available = true;
            }
            if (name.equals("JMSXGroupSeq"))
            {
                JMSXGroupSeq_Available = true;
            }
        }

        assertTrue("JMSXGroupID not available.",JMSXGroupID_Available);
        assertTrue("JMSXGroupSeq not available.",JMSXGroupSeq_Available);

        con.close();
    }

}
