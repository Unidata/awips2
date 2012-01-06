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

package org.apache.qpid.server.failure;

import junit.framework.TestCase;
import org.apache.qpid.test.utils.QpidClientConnectionHelper;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;
import org.apache.log4j.Logger;

import javax.jms.JMSException;
import javax.jms.DeliveryMode;
import java.io.IOException;


/** Test Case provided by client Non-functional Test NF101: heap exhaustion behaviour */
public class HeapExhaustion extends TestCase
{
    private static final Logger _logger = Logger.getLogger(HeapExhaustion.class);

    protected QpidClientConnectionHelper conn;
    protected final String BROKER = "localhost";
    protected final String vhost = "/test";
    protected final String queue = "direct://amq.direct//queue";

    protected String hundredK;
    protected String megabyte;

    protected String generatePayloadOfSize(Integer numBytes)
    {
        return new String(new byte[numBytes]);
    }

    protected void setUp() throws Exception
    {
        conn = new QpidClientConnectionHelper(BROKER);
        conn.setVirtualHost(vhost);

        try
        {
            conn.connect();
        } catch (JMSException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        // clear queue
        _logger.debug("setup: clearing test queue");
        conn.consume(queue, 2000);

        hundredK = generatePayloadOfSize(1024 * 100);
        megabyte = generatePayloadOfSize(1024 * 1024);
    }

    protected void tearDown() throws Exception
    {
        conn.disconnect();
    }


    /**
     * PUT at maximum rate (although we commit after each PUT) until failure
     *
     * @throws Exception on error
     */
    public void testUntilFailureTransient() throws Exception
    {
        int copies = 0;
        int total = 0;
        String payload = hundredK;
        int size = payload.getBytes().length;
        while (true)
        {
            conn.put(queue, payload, 1, DeliveryMode.NON_PERSISTENT);
            copies++;
            total += size;
            System.out.println("put copy " + copies + " OK for total bytes: " + total);
        }
    }

    /**
     * PUT at lower rate (5 per second) until failure
     *
     * @throws Exception on error
     */
    public void testUntilFailureWithDelaysTransient() throws Exception
    {
        int copies = 0;
        int total = 0;
        String payload = hundredK;
        int size = payload.getBytes().length;
        while (true)
        {
            conn.put(queue, payload, 1, DeliveryMode.NON_PERSISTENT);
            copies++;
            total += size;
            System.out.println("put copy " + copies + " OK for total bytes: " + total);
            Thread.sleep(200);
        }
    }

    public static void noDelay()
    {
        HeapExhaustion he = new HeapExhaustion();

        try
        {
            he.setUp();
        }
        catch (Exception e)
        {
            _logger.info("Unable to connect");
            System.exit(0);
        }

        try
        {
            _logger.info("Running testUntilFailure");
            try
            {
                he.testUntilFailureTransient();
            }
            catch (FailoverException fe)
            {
                _logger.error("Caught failover:" + fe);
            }
            _logger.info("Finishing Connection ");

            try
            {
                he.tearDown();
            }
            catch (JMSException jmse)
            {
                if (((AMQException) jmse.getLinkedException()).getErrorCode() == AMQConstant.REQUEST_TIMEOUT)
                {
                    _logger.info("Successful test of testUntilFailure");
                }
                else
                {
                    _logger.error("Test Failed due to:" + jmse);
                }
            }
        }
        catch (Exception e)
        {
            _logger.error("Test Failed due to:" + e);
        }
    }

    public static void withDelay()
    {
        HeapExhaustion he = new HeapExhaustion();

        try
        {
            he.setUp();
        }
        catch (Exception e)
        {
            _logger.info("Unable to connect");
            System.exit(0);
        }

        try
        {
            _logger.info("Running testUntilFailure");
            try
            {
                he.testUntilFailureWithDelaysTransient();
            }
            catch (FailoverException fe)
            {
                _logger.error("Caught failover:" + fe);
            }
            _logger.info("Finishing Connection ");

            try
            {
                he.tearDown();
            }
            catch (JMSException jmse)
            {
                if (((AMQException) jmse.getLinkedException()).getErrorCode() == AMQConstant.REQUEST_TIMEOUT)
                {
                    _logger.info("Successful test of testUntilFailure");
                }
                else
                {
                    _logger.error("Test Failed due to:" + jmse);
                }
            }
        }
        catch (Exception e)
        {
            _logger.error("Test Failed due to:" + e);
        }
    }

    public static void main(String args[])
    {
        noDelay();


        try
        {
            System.out.println("Restart failed broker now to retest broker with delays in send.");
            System.in.read();
        }
        catch (IOException e)
        {
            _logger.info("Continuing");
        }

        withDelay();
    }
}
