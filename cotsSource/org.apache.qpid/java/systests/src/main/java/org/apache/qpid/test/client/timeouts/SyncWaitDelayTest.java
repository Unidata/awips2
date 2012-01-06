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

import java.io.File;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;

import org.apache.commons.configuration.XMLConfiguration;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.ConfigurationFileApplicationRegistry;
import org.apache.qpid.test.utils.QpidTestCase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This tests that when the commit takes a long time(due to POST_COMMIT_DELAY) that the commit does not timeout
 * This test must be run in conjunction with SyncWaiteTimeoutDelay or be run with POST_COMMIT_DELAY > 30s to ensure
 * that the default value is being replaced.
 */
public class SyncWaitDelayTest extends QpidTestCase
{
    protected static final Logger _logger = LoggerFactory.getLogger(SyncWaitDelayTest.class);

    private String VIRTUALHOST = "test";
    protected long POST_COMMIT_DELAY = 1000L;
    protected long SYNC_WRITE_TIMEOUT = POST_COMMIT_DELAY + 1000;

    protected Connection _connection;
    protected Session _session;
    protected Queue _queue;
    protected MessageConsumer _consumer;

    public void setUp() throws Exception
    {

        setConfigurationProperty("virtualhosts.virtualhost." + VIRTUALHOST+".store.class", "org.apache.qpid.server.store.SlowMessageStore");
        setConfigurationProperty("virtualhosts.virtualhost." + VIRTUALHOST+".store.delays.commitTran.post", String.valueOf(POST_COMMIT_DELAY));
        setConfigurationProperty("management.enabled", "false");

        
        super.setUp();

        //Set the syncWrite timeout to be just larger than the delay on the commitTran.
        setSystemProperty("amqj.default_syncwrite_timeout", String.valueOf(SYNC_WRITE_TIMEOUT));

        _connection = getConnection();

        //Create Queue        
        _queue = (Queue) getInitialContext().lookup("queue");

        //Create Consumer
        _session = _connection.createSession(true, Session.SESSION_TRANSACTED);

        //Ensure Queue exists
        _session.createConsumer(_queue).close();
    }


    public void test() throws JMSException
    {
        MessageProducer producer = _session.createProducer(_queue);

        Message message = _session.createTextMessage("Message");

        producer.send(message);

        long start = System.nanoTime();

        _logger.info("Calling Commit");

        try
        {
            _session.commit();
            long end = System.nanoTime();
            long time = (end - start);
            // As we are using Nano time ensure to multiply up the millis.
            assertTrue("Commit was quickier than the built in delay:" + time, time > 1000000L * POST_COMMIT_DELAY);
            assertFalse("Commit was slower than the built in default", time > 1000000L * 1000 * 30);
        }
        catch (JMSException e)
        {
            fail(e.getMessage());
        }

    }

}
