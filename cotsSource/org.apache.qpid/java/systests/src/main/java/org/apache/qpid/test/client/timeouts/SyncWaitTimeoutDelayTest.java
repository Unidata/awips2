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
package org.apache.qpid.test.client.timeouts;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.qpid.AMQTimeoutException;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageProducer;

/** This tests that when the syncWrite timeout is set that it will timeout on that time rather than the default 30s. */
public class SyncWaitTimeoutDelayTest extends SyncWaitDelayTest
{
    protected static final Logger _logger = Logger.getLogger(SyncWaitTimeoutDelayTest.class);

    public void setUp() throws Exception
    {
        POST_COMMIT_DELAY = 1000L;

        //Set the syncWrite timeout to be less than the COMMIT Delay so we can validate that it is being applied
        SYNC_WRITE_TIMEOUT = 500L;

        super.setUp();
    }

    @Override
    public void test() throws JMSException
    {
        MessageProducer producer = _session.createProducer(_queue);

        Message message = _session.createTextMessage("Message");

        producer.send(message);

        _logger.info("Calling Commit");

        long start = System.nanoTime();
        try
        {
            _session.commit();
            fail("Commit occured even though syncWait timeout is shorter than delay in commit");
        }
        catch (JMSException e)
        {
            assertTrue("Wrong exception type received.", e.getLinkedException() instanceof AMQTimeoutException);
            assertTrue("Wrong message received on exception.", e.getMessage().startsWith("Failed to commit"));
            // As we are using Nano time ensure to multiply up the millis.            
            assertTrue("Timeout was more than 30s default", (System.nanoTime() - start) < (1000000L * 1000 * 30));
        }

    }
}
