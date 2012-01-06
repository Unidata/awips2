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
package org.apache.qpid.tools;

import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import javax.jms.BytesMessage;
import javax.jms.DeliveryMode;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.TextMessage;

import org.apache.qpid.thread.Threading;

/**
 * Latency test sends an x number of messages in warmup mode and wait for a confirmation
 * from the consumer that it has successfully consumed them and ready to start the
 * test. It will start sending y number of messages and each message will contain a time
 * stamp. This will be used at the receiving end to measure the latency.
 *
 * It is important to have a sufficiently large number for the warmup count to
 * ensure the system is in steady state before the test is started.
 *
 * If you plan to plot the latencies then msg_count should be a smaller number (ex 500 or 1000)
 * You also need to specify a file name using -Dfile=/home/rajith/latency.log.1
 *
 * The idea is to get a latency sample for the system once it achieves steady state.
 *
 */

public class LatencyTest extends PerfBase implements MessageListener
{
    MessageProducer producer;
    MessageConsumer consumer;
    Message msg;
    byte[] payload;
    long maxLatency = 0;
    long minLatency = Long.MAX_VALUE;
    long totalLatency = 0;  // to calculate avg latency.
    int rcvdMsgCount = 0;
    double stdDev = 0;
    double avgLatency = 0;
    boolean warmup_mode = true;
    boolean transacted = false;
    int transSize = 0;

    final List<Long> latencies;
    final Lock lock = new ReentrantLock();
    final Condition warmedUp;
    final Condition testCompleted;

    public LatencyTest()
    {
        super();
        warmedUp = lock.newCondition();
        testCompleted = lock.newCondition();
        // Storing the following two for efficiency
        transacted = params.isTransacted();
        transSize = params.getTransactionSize();
        latencies = new ArrayList <Long>(params.getMsgCount());
    }

    public void setUp() throws Exception
    {
        super.setUp();
        consumer = session.createConsumer(dest);
        consumer.setMessageListener(this);

        // if message caching is enabled we pre create the message
        // else we pre create the payload
        if (params.isCacheMessage())
        {
            msg = MessageFactory.createBytesMessage(session, params.getMsgSize());
            msg.setJMSDeliveryMode(params.isDurable()?
                                   DeliveryMode.PERSISTENT :
                                   DeliveryMode.NON_PERSISTENT
                                  );
        }
        else
        {
            payload = MessageFactory.createMessagePayload(params.getMsgSize()).getBytes();
        }

        producer = session.createProducer(dest);
        producer.setDisableMessageID(params.isDisableMessageID());
        producer.setDisableMessageTimestamp(params.isDisableTimestamp());
    }

    protected Message getNextMessage() throws Exception
    {
        if (params.isCacheMessage())
        {
            return msg;
        }
        else
        {
            msg = session.createBytesMessage();
            ((BytesMessage)msg).writeBytes(payload);
            return msg;
        }
    }

    public void warmup()throws Exception
    {
        System.out.println("Warming up......");
        int count = params.getWarmupCount();
        for (int i=0; i < count; i++)
        {
            producer.send(getNextMessage());
        }
        Message msg = session.createTextMessage("End");
        producer.send(msg);

        if (params.isTransacted())
        {
            session.commit();
        }

        try
        {
            lock.lock();
            warmedUp.await();
        }
        finally
        {
            lock.unlock();
        }
    }

    public void onMessage(Message msg)
    {
        try
        {
            if (msg instanceof TextMessage && ((TextMessage)msg).getText().equals("End"))
            {
                if (warmup_mode)
                {
                    warmup_mode = false;
                    try
                    {
                        lock.lock();
                        warmedUp.signal();
                    }
                    finally
                    {
                        lock.unlock();
                    }
                }
                else
                {
                    computeStats();
                }
            }
            else if (!warmup_mode)
            {
                long time = System.currentTimeMillis();
                rcvdMsgCount ++;

                if (transacted && (rcvdMsgCount % transSize == 0))
                {
                    session.commit();
                }

                long latency = time - msg.getJMSTimestamp();
                latencies.add(latency);
                totalLatency = totalLatency + latency;
            }

        }
        catch(Exception e)
        {
            handleError(e,"Error when receiving messages");
        }

    }

    private void computeStats()
    {
        avgLatency = (double)totalLatency/(double)rcvdMsgCount;
        double sigma = 0;

        for (long latency: latencies)
        {
            maxLatency = Math.max(maxLatency, latency);
            minLatency = Math.min(minLatency, latency);
            sigma = sigma + Math.pow(latency - avgLatency,2);
        }

        stdDev = Math.sqrt(sigma/(rcvdMsgCount -1));

        try
        {
            lock.lock();
            testCompleted.signal();
        }
        finally
        {
            lock.unlock();
        }
    }

    public void writeToFile() throws Exception
    {
        String fileName = System.getProperty("file");
        PrintWriter writer = new PrintWriter(new FileOutputStream(fileName));
        for (long latency: latencies)
        {
            writer.println(String.valueOf(latency));
        }
        writer.flush();
        writer.close();
    }

    public void printToConsole()
    {
        System.out.println(new StringBuilder("Total Msgs Received : ").append(rcvdMsgCount).toString());
        System.out.println(new StringBuilder("Standard Deviation  : ").
                           append(df.format(stdDev)).
                           append(" ms").toString());
        System.out.println(new StringBuilder("Avg Latency         : ").
                           append(df.format(avgLatency)).
                           append(" ms").toString());
        System.out.println(new StringBuilder("Min Latency         : ").
                           append(minLatency).
                           append(" ms").toString());
        System.out.println(new StringBuilder("Max Latency         : ").
                           append(maxLatency).
                           append(" ms").toString());
        System.out.println("Completed the test......\n");
    }

    public void startTest() throws Exception
    {
        System.out.println("Starting test......");
        int count = params.getMsgCount();

        for(int i=0; i < count; i++ )
        {
            Message msg = getNextMessage();
            msg.setJMSTimestamp(System.currentTimeMillis());
            producer.send(msg);
            if ( transacted && ((i+1) % transSize == 0))
            {
                session.commit();
            }
        }
        Message msg = session.createTextMessage("End");
        producer.send(msg);
        if (params.isTransacted())
        {
            session.commit();
        }
    }

    public void tearDown() throws Exception
    {
        try
        {
            lock.lock();
            testCompleted.await();
        }
        finally
        {
            lock.unlock();
        }

        producer.close();
        consumer.close();
        session.close();
        con.close();
    }

    public void test()
    {
        try
        {
            setUp();
            warmup();
            startTest();
            tearDown();
        }
        catch(Exception e)
        {
            handleError(e,"Error when running test");
        }
    }


    public static void main(String[] args)
    {
        final LatencyTest latencyTest = new LatencyTest();        
        Runnable r = new Runnable()
        {
            public void run()
            {
                latencyTest.test();
                latencyTest.printToConsole();
                if (System.getProperty("file") != null)
                {
                    try
                    {
                        latencyTest.writeToFile();
                    }
                    catch(Exception e)
                    {
                        e.printStackTrace();
                    }
                }
            }
        };
        
        Thread t;
        try
        {
            t = Threading.getThreadFactory().createThread(r);                      
        }
        catch(Exception e)
        {
            throw new Error("Error creating latency test thread",e);
        }
        t.start(); 
    }
}