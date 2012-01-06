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

import javax.jms.Destination;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.TextMessage;

import org.apache.qpid.thread.Threading;

/**
 * PerfConsumer will receive x no of messages in warmup mode.
 * Once it receives the Start message it will then signal the PerfProducer.
 * It will start recording stats from the first message it receives after
 * the warmup mode is done.
 *
 * The following calculations are done.
 * The important numbers to look at is
 * a) Avg Latency
 * b) System throughput.
 *
 * Latency.
 * =========
 * Currently this test is written with the assumption that either
 * a) The Perf Producer and Consumer are on the same machine
 * b) They are on separate machines that have their time synced via a Time Server
 *
 * In order to calculate latency the producer inserts a timestamp
 * hen the message is sent. The consumer will note the current time the message is
 * received and will calculate the latency as follows
 * latency = rcvdTime - msg.getJMSTimestamp()
 *
 * Through out the test it will keep track of the max and min latency to show the
 * variance in latencies.
 *
 * Avg latency is measured by adding all latencies and dividing by the total msgs.
 * You can also compute this by (rcvdTime - testStartTime)/rcvdMsgCount
 *
 * Throughput
 * ===========
 * System throughput is calculated as follows
 * rcvdMsgCount/(rcvdTime - testStartTime)
 *
 * Consumer rate is calculated as
 * rcvdMsgCount/(rcvdTime - startTime)
 *
 * Note that the testStartTime referes to when the producer sent the first message
 * and startTime is when the consumer first received a message.
 *
 * rcvdTime keeps track of when the last message is received.
 *
 * All throughput rates are given as msg/sec so the rates are multiplied by 1000.
 *
 */

public class PerfConsumer extends PerfBase implements MessageListener
{
    MessageConsumer consumer;
    long maxLatency = 0;
    long minLatency = Long.MAX_VALUE;
    long totalLatency = 0;  // to calculate avg latency.
    int rcvdMsgCount = 0;
    long testStartTime = 0; // to measure system throughput
    long startTime = 0;     // to measure consumer throughput
    long rcvdTime = 0;
    boolean transacted = false;
    int transSize = 0;

    final Object lock = new Object();

    public PerfConsumer()
    {
        super();
    }

    public void setUp() throws Exception
    {
        super.setUp();
        consumer = session.createConsumer(dest);

        // Storing the following two for efficiency
        transacted = params.isTransacted();
        transSize = params.getTransactionSize();
    }

    public void warmup()throws Exception
    {
        System.out.println("Warming up......");

        boolean start = false;
        while (!start)
        {
            Message msg = consumer.receive();
            if (msg instanceof TextMessage)
            {
                if (((TextMessage)msg).getText().equals("End"))
                {
                    start = true;
                    MessageProducer temp = session.createProducer(msg.getJMSReplyTo());
                    temp.send(session.createMessage());
                    if (params.isTransacted())
                    {
                        session.commit();
                    }
                    temp.close();
                }
            }
        }
    }

    public void startTest() throws Exception
    {
        System.out.println("Starting test......");
        consumer.setMessageListener(this);
    }

    public void printResults() throws Exception
    {
        synchronized (lock)
        {
            lock.wait();
        }

        double avgLatency = (double)totalLatency/(double)rcvdMsgCount;
        double throughput = ((double)rcvdMsgCount/(double)(rcvdTime - testStartTime))*1000;
        double consRate   = ((double)rcvdMsgCount/(double)(rcvdTime - startTime))*1000;
        System.out.println(new StringBuilder("Total Msgs Received : ").append(rcvdMsgCount).toString());
        System.out.println(new StringBuilder("Consumer rate       : ").
                           append(df.format(consRate)).
                           append(" msg/sec").toString());
        System.out.println(new StringBuilder("System Throughput   : ").
                           append(df.format(throughput)).
                           append(" msg/sec").toString());
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

    public void notifyCompletion(Destination replyTo) throws Exception
    {
        MessageProducer tmp = session.createProducer(replyTo);
        Message endMsg = session.createMessage();
        tmp.send(endMsg);
        if (params.isTransacted())
        {
            session.commit();
        }
        tmp.close();
    }

    public void tearDown() throws Exception
    {
        consumer.close();
        session.close();
        con.close();
    }

    public void onMessage(Message msg)
    {
        try
        {
            if (msg instanceof TextMessage && ((TextMessage)msg).getText().equals("End"))
            {
                notifyCompletion(msg.getJMSReplyTo());

                synchronized (lock)
                {
                   lock.notifyAll();
                }
            }
            else
            {
                rcvdTime = System.currentTimeMillis();
                rcvdMsgCount ++;

                if (rcvdMsgCount == 1)
                {
                    startTime = rcvdTime;
                    testStartTime = msg.getJMSTimestamp();
                }

                if (transacted && (rcvdMsgCount % transSize == 0))
                {
                    session.commit();
                }

                long latency = rcvdTime - msg.getJMSTimestamp();
                maxLatency = Math.max(maxLatency, latency);
                minLatency = Math.min(minLatency, latency);
                totalLatency = totalLatency + latency;
            }

        }
        catch(Exception e)
        {
            handleError(e,"Error when receiving messages");
        }

    }

    public void test()
    {
        try
        {
            setUp();
            warmup();
            startTest();
            printResults();
            tearDown();
        }
        catch(Exception e)
        {
            handleError(e,"Error when running test");
        }
    }

    public static void main(String[] args)
    {
        final PerfConsumer cons = new PerfConsumer();
        Runnable r = new Runnable()
        {
            public void run()
            {
                cons.test();
            }
        };
        
        Thread t;
        try
        {
            t = Threading.getThreadFactory().createThread(r);                      
        }
        catch(Exception e)
        {
            throw new Error("Error creating consumer thread",e);
        }
        t.start(); 
    }
}