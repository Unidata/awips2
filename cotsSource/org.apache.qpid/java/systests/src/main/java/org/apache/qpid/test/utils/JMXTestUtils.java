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
package org.apache.qpid.test.utils;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.commands.objects.AllObjects;
import org.apache.qpid.management.common.JMXConnnectionFactory;
import org.apache.qpid.management.common.mbeans.ManagedBroker;
import org.apache.qpid.management.common.mbeans.ManagedExchange;

import javax.management.JMException;
import javax.management.MBeanException;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import java.io.IOException;
import java.util.Set;

/**
 * 
 */
public class JMXTestUtils
{
    QpidTestCase _test;
    MBeanServerConnection _mbsc;
    JMXConnector _jmxc;

    private String USER;
    private String PASSWORD;

    public JMXTestUtils(QpidTestCase test, String user, String password)
    {
        _test = test;
        USER = user;
        PASSWORD = password;
    }

    public void setUp() throws IOException, ConfigurationException, Exception
    {
        _test.setConfigurationProperty("management.enabled", "true");       
    }

    public void open() throws Exception
    {
        _jmxc = JMXConnnectionFactory.getJMXConnection(
                5000, "127.0.0.1",
                _test.getManagementPort(_test.getPort()), USER, PASSWORD);

        _mbsc = _jmxc.getMBeanServerConnection();
    }

    public void close() throws IOException
    {
        _jmxc.close();
    }

    /**
     * Create a non-durable test exchange with the current test name
     *
     * @throws javax.management.JMException - is thrown if a exchange with this testName already exists
     * @throws java.io.IOException          - if there is a problem with the JMX Connection
     * @throws javax.management.MBeanException
     *                                      - if there is another problem creating the exchange
     */
    public void createExchange(String virtualHostName, String name, String type, boolean durable)
            throws JMException, IOException, MBeanException
    {
        ManagedBroker managedBroker = getManagedBroker(virtualHostName);

        managedBroker.createNewExchange(name, type, durable);
    }

    /**
     * Create a non-durable queue (with no owner) that is named after the
     * creating test.
     *
     * @throws JMException - is thrown if a queue with this testName already exists
     * @throws IOException - if there is a problem with the JMX Connection
     */
    public void createQueue(String virtualHostName, String name, String owner, boolean durable)
            throws JMException, IOException
    {
        ManagedBroker managedBroker = getManagedBroker(virtualHostName);

        managedBroker.createNewQueue(name, owner, durable);
    }

    /**
     * Retrive the ObjectName for the test Virtualhost.
     *
     * This is then use to create aproxy to the ManagedBroker MBean.
     *
     * @return the ObjectName for the 'test' VirtualHost.
     */
    public ObjectName getVirtualHostManagerObjectName(String vhostName)
    {
        // Get the name of the test manager
        AllObjects allObject = new AllObjects(_mbsc);
        allObject.querystring = "org.apache.qpid:type=VirtualHost.VirtualHostManager,VirtualHost=" + vhostName + ",*";

        Set<ObjectName> objectNames = allObject.returnObjects();

        _test.assertNotNull("Null ObjectName Set returned", objectNames);
        _test.assertEquals("Incorrect number test vhosts returned", 1, objectNames.size());

        // We have verified we have only one value in objectNames so return it
        return objectNames.iterator().next();
    }

    /**
     * Retrive the ObjectName for the given Exchange on the test Virtualhost.
     *
     * This is then use to create aproxy to the ManagedExchange MBean.
     *
     * @param queue The exchange to retireve e.g. 'direct'
     *
     * @return the ObjectName for the given exchange on the test VirtualHost.
     */
    public ObjectName getQueueObjectName(String virtualHostName, String queue)
    {
        // Get the name of the test manager
        AllObjects allObject = new AllObjects(_mbsc);
        allObject.querystring = "org.apache.qpid:type=VirtualHost.Queue,VirtualHost=" + virtualHostName + ",name=" + queue + ",*";

        Set<ObjectName> objectNames = allObject.returnObjects();

        _test.assertNotNull("Null ObjectName Set returned", objectNames);
        _test.assertEquals("Incorrect number of queues with name '" + allObject.querystring +
                           "' returned", 1, objectNames.size());

        // We have verified we have only one value in objectNames so return it
        return objectNames.iterator().next();
    }

    /**
     * Retrive the ObjectName for the given Exchange on the test Virtualhost.
     *
     * This is then use to create aproxy to the ManagedExchange MBean.
     *
     * @param virtualHostName
     * @param exchange        The exchange to retireve e.g. 'direct'
     *
     * @return the ObjectName for the given exchange on the test VirtualHost.
     */
    public ObjectName getExchangeObjectName(String virtualHostName, String exchange)
    {
        // Get the name of the test manager
        AllObjects allObject = new AllObjects(_mbsc);
        allObject.querystring = "org.apache.qpid:type=VirtualHost.Exchange,VirtualHost=" + virtualHostName + ",name=" + exchange + ",*";

        Set<ObjectName> objectNames = allObject.returnObjects();

        _test.assertNotNull("Null ObjectName Set returned", objectNames);
        _test.assertEquals("Incorrect number of exchange with name '" + exchange +
                           "' returned", 1, objectNames.size());

        // We have verified we have only one value in objectNames so return it
        return objectNames.iterator().next();
    }

    public <T> T getManagedObject(Class<T> managedClass, String queryString)
    {
        AllObjects allObject = new AllObjects(_mbsc);
        allObject.querystring = queryString;

        Set<ObjectName> objectNames = allObject.returnObjects();

        _test.assertNotNull("Null ObjectName Set returned", objectNames);
        _test.assertEquals("More than one " + managedClass + " returned", 1, objectNames.size());

        ObjectName objectName = objectNames.iterator().next();

        return getManagedObject(managedClass, objectName);
    }

    public <T> T getManagedObject(Class<T> managedClass, ObjectName objectName)
    {
        return MBeanServerInvocationHandler.
                newProxyInstance(_mbsc, objectName, managedClass, false);
    }

    public ManagedBroker getManagedBroker(String virtualHost)
    {
        return getManagedObject(ManagedBroker.class, getVirtualHostManagerObjectName(virtualHost).toString());
    }

    public ManagedExchange getManagedExchange(String exchangeName)
    {
        return MBeanServerInvocationHandler.
                newProxyInstance(_mbsc, getExchangeObjectName("test", exchangeName),
                                 ManagedExchange.class, false);
    }
}
