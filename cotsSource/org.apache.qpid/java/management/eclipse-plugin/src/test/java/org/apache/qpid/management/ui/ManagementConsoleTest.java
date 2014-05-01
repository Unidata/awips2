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
package org.apache.qpid.management.ui;

import junit.framework.TestCase;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.server.exchange.DirectExchange;
import org.apache.qpid.server.management.AMQManagedObject;
import org.apache.qpid.server.queue.AMQQueueMBean;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.AMQQueueFactory;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;

import javax.management.MBeanFeatureInfo;
import javax.management.MBeanInfo;
import java.util.ArrayList;
import java.util.List;


/**
 * Test class to test if any change in the broker JMX code is affesting the management console
 * There are some hardcoding of management feature names and parameter names to create a customized
 * look in the console.
 */
public class ManagementConsoleTest extends TestCase
{
    private VirtualHost _virtualHost;

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        IApplicationRegistry applicationRegistry = ApplicationRegistry.getInstance();
        _virtualHost = applicationRegistry.getVirtualHostRegistry().getVirtualHost("test");
    }

    @Override
    protected void tearDown() throws Exception
    {
        // Correctly Close the AR that we created above
        ApplicationRegistry.remove();
    }

    /**
     * Test for AMQQueueMBean attribute and operation names, which are used in the management console
     * @throws Exception
     */
    public void testAMQQueueMBeanInfo() throws Exception
    {
        // If this test fails due to changes in the broker code,
        // then the constants in the Constants.java shoule be updated accordingly
        AMQQueue queue = AMQQueueFactory.createAMQQueueImpl(new AMQShortString("testQueueForManagement"), false, null, false, _virtualHost,
                                                            null);
        AMQManagedObject mbean = new AMQQueueMBean(queue);
        MBeanInfo mbeanInfo = mbean.getMBeanInfo();

        List<String> operationNames = getNamesList(mbeanInfo.getOperations());
        assertTrue(operationNames.contains(Constants.OPERATION_MOVE_MESSAGES));

        List<String> attributesList = getNamesList(mbeanInfo.getAttributes());
        assertTrue(attributesList.contains(Constants.ATTRIBUTE_QUEUE_CONSUMERCOUNT));
        assertTrue(attributesList.contains(Constants.ATTRIBUTE_QUEUE_DEPTH));
    }

    /**
     * Test for Exchange MBean attribute and operation names, which are used in the management console
     * @throws Exception
     */
    public void testExchangeMBeanInfo() throws Exception
    {
        // If this test fails due to changes in the broker code,
        // then the constants in the Constants.java shoule be updated accordingly 
        DirectExchange exchange = new DirectExchange();
        exchange.initialise(_virtualHost, ExchangeDefaults.DIRECT_EXCHANGE_NAME, false, 0, true);
        AMQManagedObject mbean = (AMQManagedObject)exchange.getManagedObject();
        MBeanInfo mbeanInfo = mbean.getMBeanInfo();

        // Check for the Exchange Type property in the ObjectName
        assertNotNull(mbean.getObjectName().getKeyProperty(Constants.EXCHANGE_TYPE));

        // Check for operation names
        List<String> operationNames = getNamesList(mbeanInfo.getOperations());
        assertTrue(operationNames.contains(Constants.OPERATION_CREATE_BINDING));
    }

    /**
     * Test for VirtualHostManagerMBean features used in Management console for customizing the GUI
     * @throws Exception
     */
    public void testVirtualHostManagerMBeanInfo() throws Exception
    {
        AMQManagedObject mbean = (AMQManagedObject)_virtualHost.getManagedObject();
        assertTrue(mbean.getType().equals(Constants.VIRTUAL_HOST));
    }

    private List<String> getNamesList(MBeanFeatureInfo[] features)
    {
        List<String> names = new ArrayList<String>();
        for (MBeanFeatureInfo feature : features)
        {
            names.add(feature.getName());
        }

        return names;
    }
}
