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
package org.apache.qpid.server.failover;

import junit.framework.TestCase;
import org.apache.qpid.AMQDisconnectedException;
import org.apache.qpid.AMQException;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.client.vmbroker.AMQVMBrokerCreationException;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import java.util.concurrent.CountDownLatch;

public class FailoverMethodTest extends TestCase implements ExceptionListener
{
    private CountDownLatch _failoverComplete = new CountDownLatch(1);

    public void setUp() throws AMQVMBrokerCreationException
    {
        ApplicationRegistry.getInstance();
        TransportConnection.createVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
    }

    public void tearDown()
    {
        TransportConnection.killVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
        ApplicationRegistry.remove();
    }

    /**
     * Test that the round robin method has the correct delays.
     * The first connection to vm://:1 will work but the localhost connection should fail but the duration it takes
     * to report the failure is what is being tested.
     *
     * @throws URLSyntaxException
     * @throws InterruptedException
     * @throws JMSException
     */
    public void testFailoverRoundRobinDelay() throws URLSyntaxException, InterruptedException, JMSException
    {
        //note: The VM broker has no connect delay and the default 1 retry
        //        while the tcp:localhost broker has 3 retries with a 2s connect delay
        String connectionString = "amqp://guest:guest@/test?brokerlist=" +
                                  "'vm://:" + ApplicationRegistry.DEFAULT_INSTANCE +
                                  ";tcp://localhost:5670?connectdelay='2000',retries='3''";

        AMQConnectionURL url = new AMQConnectionURL(connectionString);

        try
        {
            long start = System.currentTimeMillis();
            AMQConnection connection = new AMQConnection(url, null);

            connection.setExceptionListener(this);

            TransportConnection.killVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
            ApplicationRegistry.remove();

            _failoverComplete.await();

            long end = System.currentTimeMillis();

            long duration = (end - start);

            //Failover should take more that 6 seconds.
            // 3 Retires
            // so VM Broker NoDelay 0 (Connect) NoDelay 0
            // then TCP NoDelay 0 Delay 1 Delay 2 Delay  3
            // so 3 delays of 2s in total for connection
            // as this is a tcp connection it will take 1second per connection to fail
            // so max time is 6seconds of delay plus 4 seconds of TCP Delay + 1 second of runtime. == 11 seconds 

            // Ensure we actually had the delay
            assertTrue("Failover took less than 6 seconds", duration > 6000);

            // Ensure we don't have delays before initial connection and reconnection.
            // We allow 1 second for initial connection and failover logic on top of 6s of sleep.
            assertTrue("Failover took more than 11 seconds:(" + duration + ")", duration < 11000);
        }
        catch (AMQException e)
        {
            fail(e.getMessage());
        }
    }

    public void testFailoverSingleDelay() throws URLSyntaxException, AMQVMBrokerCreationException,
                                                 InterruptedException, JMSException
    {
        String connectionString = "amqp://guest:guest@/test?brokerlist='vm://:1?connectdelay='2000',retries='3''";

        AMQConnectionURL url = new AMQConnectionURL(connectionString);

        try
        {
            long start = System.currentTimeMillis();
            AMQConnection connection = new AMQConnection(url, null);

            connection.setExceptionListener(this);

            TransportConnection.killVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
            ApplicationRegistry.remove();

            _failoverComplete.await();

            long end = System.currentTimeMillis();

            long duration = (end - start);

            //Failover should take more that 6 seconds.
            // 3 Retires
            // so NoDelay 0 (Connect) NoDelay 0 Delay 1 Delay 2 Delay  3
            // so 3 delays of 2s in total for connection
            // so max time is 6 seconds of delay + 1 second of runtime. == 7 seconds

            // Ensure we actually had the delay
            assertTrue("Failover took less than 6 seconds", duration > 6000);

            // Ensure we don't have delays before initial connection and reconnection.
            // We allow 1 second for initial connection and failover logic on top of 6s of sleep.
            assertTrue("Failover took more than 7 seconds:(" + duration + ")", duration < 7000);
        }
        catch (AMQException e)
        {
            fail(e.getMessage());
        }
    }

    public void onException(JMSException e)
    {
        if (e.getLinkedException() instanceof AMQDisconnectedException)
        {
            _failoverComplete.countDown();
        }
    }

    public void testNoFailover() throws URLSyntaxException, AMQVMBrokerCreationException,
                                        InterruptedException, JMSException
    {
        String connectionString = "amqp://guest:guest@/test?brokerlist='vm://:1?connectdelay='500',retries='3'',failover='nofailover'";

        AMQConnectionURL url = new AMQConnectionURL(connectionString);

        try
        {
            //Kill initial broker
            TransportConnection.killVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
            ApplicationRegistry.remove();

            //Create a thread to start the broker asynchronously
            Thread brokerStart = new Thread(new Runnable()
            {
                public void run()
                {
                    try
                    {
                        //Wait before starting broker
                        // The wait should allow atleast 1 retries to fail before broker is ready
                        Thread.sleep(750);
                        ApplicationRegistry.getInstance();
                        TransportConnection.createVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
                    }
                    catch (Exception e)
                    {
                        System.err.println(e.getMessage());
                        e.printStackTrace();
                    }
                }
            });


            brokerStart.start();
            long start = System.currentTimeMillis();


            //Start the connection so it will use the retries
            AMQConnection connection = new AMQConnection(url, null);

            long end = System.currentTimeMillis();

            long duration = (end - start);

            // Check that we actually had a delay had a delay in connection
            assertTrue("Initial connection should be longer than 1 delay : 500 <:(" + duration + ")", duration > 500);


            connection.setExceptionListener(this);

            //Ensure we collect the brokerStart thread
            brokerStart.join();

            start = System.currentTimeMillis();

            //Kill connection
            TransportConnection.killVMBroker(ApplicationRegistry.DEFAULT_INSTANCE);
            ApplicationRegistry.remove();

            _failoverComplete.await();

            end = System.currentTimeMillis();

            duration = (end - start);

            // Notification of the connection failure should be very quick as we are denying the ability to failover.
            // It may not be as quick for Java profile tests so lets just make sure it is less than the connectiondelay
            assertTrue("Notification of the connection failure took was : 100 >:(" + duration + ")", duration < 500);
        }
        catch (AMQException e)
        {
            fail(e.getMessage());
        }
    }

}
