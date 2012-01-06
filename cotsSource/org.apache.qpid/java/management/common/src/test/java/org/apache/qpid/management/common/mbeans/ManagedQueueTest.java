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
package org.apache.qpid.management.common.mbeans;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;

import javax.management.MBeanAttributeInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;

import junit.framework.TestCase;

public class ManagedQueueTest extends TestCase
{
    public void testAttributesContants()
    {
        //Construct a test MBeanInfo that matches what we would get from a real 
        //MBean using the ManagedQueue management interface. Use this to test
        //that all attributes have a listing in the attribute array constant.

        StubInvocationHandler stubIH = new StubInvocationHandler();
        Class<ManagedQueue> mq = ManagedQueue.class;
        
        ManagedQueue impl = mq.cast(Proxy.newProxyInstance(mq.getClassLoader(), new Class<?>[] {mq}, stubIH));
        try
        {
            StandardMBean mbean = new StandardMBean(impl, ManagedQueue.class);
            
            List<String> attributeList = new ArrayList<String>();
            for(String attr : ManagedQueue.QUEUE_ATTRIBUTES)
            {
                attributeList.add(attr);
            }
            
            //retrieve the attributes from the constructed MBeanInfo
            MBeanAttributeInfo[] attributes = mbean.getMBeanInfo().getAttributes();

            for(MBeanAttributeInfo info : attributes)
            {
                if(!attributeList.contains(info.getName()))
                {
                    fail(mq.getSimpleName() + " attributes constant array does not include the attribute: " + info.getName());
                }
            }
        }
        catch (NotCompliantMBeanException e)
        {
            fail("Unable to create the test proxy mbean to generate the MBeanInfo");
        }

    }
    
    private static class StubInvocationHandler implements InvocationHandler
    {
        //invocation handler used to present a stub implementation when generating the StandardMBean
        public Object invoke(Object proxy, Method method, Object[] args)
        {
            return null;
        }
    }

}
