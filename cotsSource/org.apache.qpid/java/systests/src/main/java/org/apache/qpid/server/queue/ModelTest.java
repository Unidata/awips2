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
package org.apache.qpid.server.queue;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.management.common.mbeans.ManagedBroker;
import org.apache.qpid.management.common.mbeans.ManagedQueue;
import org.apache.qpid.test.utils.JMXTestUtils;
import org.apache.qpid.test.utils.QpidTestCase;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Session;
import javax.management.JMException;
import javax.management.MBeanException;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * This Test validates the Queue Model on the broker.
 * Currently it has some basic queue creation / deletion tests.
 * However, it should be expanded to include other tests that relate to the
 * model. i.e.
 *
 * The Create and Delete tests should ensure that the requisite logging is
 * performed.
 *
 * Additions to this suite would be to complete testing of creations, validating
 * fields such as owner/exclusive, autodelete and priority are correctly set.
 *
 * Currently this test uses the JMX interface to validate that the queue has
 * been declared as expected so these tests cannot run against a CPP broker.
 *
 *
 * Tests should ensure that they clean up after themselves.
 * e,g. Durable queue creation test should perform a queue delete.
 */
public class ModelTest extends QpidTestCase
{

    private static final String USER = "admin";
    private JMXTestUtils _jmxUtils;
    private static final String VIRTUALHOST_NAME = "test";

    @Override
    public void setUp() throws Exception
    {
        // Create a JMX Helper
        _jmxUtils = new JMXTestUtils(this, USER, USER);
        _jmxUtils.setUp();
        super.setUp();

        // Open the JMX Connection
        _jmxUtils.open();
    }

    @Override
    public void tearDown() throws Exception
    {
        // Close the JMX Connection
        _jmxUtils.close();
        super.tearDown();
    }

    /**
     * Test that a transient queue can be created via AMQP.
     *
     * @throws Exception On unexpected error
     */
    public void testQueueCreationTransientViaAMQP() throws Exception
    {
        Connection connection = getConnection();

        String queueName = getTestQueueName();
        boolean durable = false;
        boolean autoDelete = false;
        boolean exclusive = false;

        createViaAMQPandValidateViaJMX(connection, queueName, durable,
                                       autoDelete, exclusive);
    }

    /**
     * Test that a durable queue can be created via AMQP.
     *
     * @throws Exception On unexpected error
     */

    public void testQueueCreationDurableViaAMQP() throws Exception
    {
        Connection connection = getConnection();

        String queueName = getTestQueueName();
        boolean durable = true;
        boolean autoDelete = false;
        boolean exclusive = false;

        createViaAMQPandValidateViaJMX(connection, queueName, durable,
                                       autoDelete, exclusive);

        // Clean up
        ManagedBroker managedBroker =
                _jmxUtils.getManagedBroker(VIRTUALHOST_NAME);
        managedBroker.deleteQueue(queueName);
    }

    /**
     * Test that a transient queue can be created via JMX.
     *
     * @throws IOException                  if there is a problem via the JMX connection
     * @throws javax.management.JMException if there is a problem with the JMX command
     */
    public void testCreationTransientViaJMX() throws IOException, JMException
    {
        String name = getName();
        String owner = null;
        boolean durable = false;

        createViaJMXandValidateViaJMX(name, owner, durable, durable);
    }

    /**
     * Test that a durable queue can be created via JMX.
     *
     * @throws IOException                  if there is a problem via the JMX connection
     * @throws javax.management.JMException if there is a problem with the JMX command
     */
    public void testCreationDurableViaJMX() throws IOException, JMException
    {
        String name = getName();
        String owner = null;
        boolean durable = true;

        createViaJMXandValidateViaJMX(name, owner, durable, durable);

        // Clean up
        ManagedBroker managedBroker =
                _jmxUtils.getManagedBroker(VIRTUALHOST_NAME);
        managedBroker.deleteQueue(name);
    }

    /**
     * Test that a transient queue can be deleted via JMX.
     *
     * @throws IOException                  if there is a problem via the JMX connection
     * @throws javax.management.JMException if there is a problem with the JMX command
     */
    public void testDeletionTransientViaJMX() throws IOException, JMException
    {
        String name = getName();

        _jmxUtils.createQueue(VIRTUALHOST_NAME, name, null, false);

        ManagedBroker managedBroker = _jmxUtils.
                getManagedBroker(VIRTUALHOST_NAME);

        try
        {
            managedBroker.deleteQueue(name);
        }
        catch (UndeclaredThrowableException e)
        {
            fail(((MBeanException) ((InvocationTargetException)
                    e.getUndeclaredThrowable()).getTargetException()).getTargetException().getMessage());
        }
    }

    /**
     * Test that a durable queue can be created via JMX.
     *
     * @throws IOException                  if there is a problem via the JMX connection
     * @throws javax.management.JMException if there is a problem with the JMX command
     */
    public void testDeletionDurableViaJMX() throws IOException, JMException
    {
        String name = getName();

        _jmxUtils.createQueue(VIRTUALHOST_NAME, name, null, true);

        ManagedBroker managedBroker = _jmxUtils.
                getManagedBroker(VIRTUALHOST_NAME);

        try
        {
            managedBroker.deleteQueue(name);
        }
        catch (UndeclaredThrowableException e)
        {
            fail(((MBeanException) ((InvocationTargetException)
                    e.getUndeclaredThrowable()).getTargetException()).getTargetException().getMessage());
        }
    }

    /*
     * Helper Methods
     */

    /**
     * Using the provided JMS Connection create a queue using the AMQP extension
     * with the given properties and then validate it was created correctly via
     * the JMX Connection
     *
     * @param connection Qpid JMS Connection
     * @param queueName  String the desired QueueName
     * @param durable    boolean if the queue should be durable
     * @param autoDelete boolean if the queue is an autoDelete queue
     * @param exclusive  boolean if the queue is exclusive
     *
     * @throws AMQException if there is a problem with the createQueue call
     * @throws JMException  if there is a problem with the JMX validatation
     * @throws IOException  if there is a problem with the JMX connection
     * @throws JMSException if there is a problem creating the JMS Session
     */
    private void createViaAMQPandValidateViaJMX(Connection connection,
                                                String queueName,
                                                boolean durable,
                                                boolean autoDelete,
                                                boolean exclusive)
            throws AMQException, JMException, IOException, JMSException
    {
        AMQSession session = (AMQSession) connection.createSession(false,
                                                                   Session.AUTO_ACKNOWLEDGE);

        session.createQueue(new AMQShortString(queueName),
                            autoDelete, durable, exclusive);

        validateQueueViaJMX(queueName, exclusive ? ((AMQConnection) connection).
                getUsername() : null, durable, autoDelete);
    }

    /**
     * Use the JMX Helper to create a queue with the given properties and then
     * validate it was created correctly via the JMX Connection
     *
     * @param queueName  String the desired QueueName
     * @param owner      String the owner value that should be set
     * @param durable    boolean if the queue should be durable
     * @param autoDelete boolean if the queue is an autoDelete queue
     *
     * @throws JMException if there is a problem with the JMX validatation
     * @throws IOException if there is a problem with the JMX connection
     */
    private void createViaJMXandValidateViaJMX(String queueName, String owner,
                                               boolean durable, boolean autoDelete)
            throws JMException, IOException
    {
        _jmxUtils.createQueue(VIRTUALHOST_NAME, queueName, owner, durable);

        validateQueueViaJMX(queueName, owner, durable, autoDelete);
    }

    /**
     * Validate that a queue with the given properties exists on the broker
     *
     * @param queueName  String the desired QueueName
     * @param owner      String the owner value that should be set
     * @param durable    boolean if the queue should be durable
     * @param autoDelete boolean if the queue is an autoDelete queue
     *
     * @throws JMException if there is a problem with the JMX validatation
     * @throws IOException if there is a problem with the JMX connection
     */
    private void validateQueueViaJMX(String queueName, String owner, boolean durable, boolean autoDelete)
            throws JMException, IOException
    {
        ManagedQueue managedQueue = _jmxUtils.
                getManagedObject(ManagedQueue.class,
                                 _jmxUtils.getQueueObjectName(VIRTUALHOST_NAME,
                                                              queueName));

        assertEquals(queueName, managedQueue.getName());
        assertEquals(String.valueOf(owner), managedQueue.getOwner());
        assertEquals(durable, managedQueue.isDurable());
        assertEquals(autoDelete, managedQueue.isAutoDelete());
    }

}
