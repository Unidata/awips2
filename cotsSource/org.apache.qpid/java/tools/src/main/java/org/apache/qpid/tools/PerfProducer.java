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

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.jms.BytesMessage;
import javax.jms.DeliveryMode;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;

import org.apache.qpid.thread.Threading;

/**
 * PerfProducer sends an x no of messages in warmup mode and wait for a confirmation
 * from the consumer that it has successfully consumed them and ready to start the
 * test. It will start sending y no of messages and each message will contain a time
 * stamp. This will be used at the receiving end to measure the latency.
 *
 * This is done with the assumption that both consumer and producer are running on
 * the same machine or different machines which have time synced using a time server.
 *
 * This test also calculates the producer rate as follows.
 * rate = msg_count/(time_before_sending_msgs - time_after_sending_msgs)
 *
 * All throughput rates are given as msg/sec so the rates are multiplied by 1000.
 *
 * Rajith - Producer rate is not an accurate perf metric IMO.
 * It is heavily inlfuenced by any in memory buffering.
 * System throughput and latencies calculated by the PerfConsumer are more realistic
 * numbers.
 *
 */
public class PerfProducer extends PerfBase
{
    MessageProducer producer;
    Message msg;
    byte[] payload;
    List<byte[]> payloads;
    boolean cacheMsg = false;
    boolean randomMsgSize = false;
    boolean durable = false;
    Random random;
    int msgSizeRange = 1024;
    
    public PerfProducer()
    {
        super();
    }

    public void setUp() throws Exception
    {
        super.setUp();
        feedbackDest = session.createTemporaryQueue();

        durable = params.isDurable();
        
        // if message caching is enabled we pre create the message
        // else we pre create the payload
        if (params.isCacheMessage())
        {
            cacheMsg = true;
            
            msg = MessageFactory.createBytesMessage(session, params.getMsgSize());
            msg.setJMSDeliveryMode(durable?
                                   DeliveryMode.PERSISTENT :
                                   DeliveryMode.NON_PERSISTENT
                                  );
        }
        else if (params.isRandomMsgSize())
        {
            random = new Random(20080921);
            randomMsgSize = true;
            msgSizeRange = params.getMsgSize();
            payloads = new ArrayList<byte[]>(msgSizeRange);
            
            for (int i=0; i < msgSizeRange; i++)
            {
                payloads.add(MessageFactory.createMessagePayload(i).getBytes());
            }
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
        if (cacheMsg)
        {
            return msg;
        }
        else
        {            
            msg = session.createBytesMessage();
            
            if (!randomMsgSize)
            {
                ((BytesMessage)msg).writeBytes(payload);
            }
            else
            {
                ((BytesMessage)msg).writeBytes(payloads.get(random.nextInt(msgSizeRange)));
            }
            msg.setJMSDeliveryMode(durable?
                    DeliveryMode.PERSISTENT :
                    DeliveryMode.NON_PERSISTENT
                   );
            return msg;
        }
    }

    public void warmup()throws Exception
    {
        System.out.println("Warming up......");
        MessageConsumer tmp = session.createConsumer(feedbackDest);

        for (int i=0; i < params.getWarmupCount() -1; i++)
        {
            producer.send(getNextMessage());
        }
        Message msg = session.createTextMessage("End");
        msg.setJMSReplyTo(feedbackDest);
        producer.send(msg);

        if (params.isTransacted())
        {
            session.commit();
        }

        tmp.receive();

        if (params.isTransacted())
        {
            session.commit();
        }

        tmp.close();
    }

    public void startTest() throws Exception
    {
        System.out.println("Starting test......");
        int count = params.getMsgCount();
        boolean transacted = params.isTransacted();
        int tranSize =  params.getTransactionSize();

        long start = System.currentTimeMillis();
        for(int i=0; i < count; i++ )
        {
            Message msg = getNextMessage();
            msg.setJMSTimestamp(System.currentTimeMillis());
            producer.send(msg);
            if ( transacted && ((i+1) % tranSize == 0))
            {
                session.commit();
            }
        }
        long time = System.currentTimeMillis() - start;
        double rate = ((double)count/(double)time)*1000;
        System.out.println(new StringBuilder("Producer rate: ").
                               append(df.format(rate)).
                               append(" msg/sec").
                               toString());
    }

    public void waitForCompletion() throws Exception
    {
        MessageConsumer tmp = session.createConsumer(feedbackDest);
        Message msg = session.createTextMessage("End");
        msg.setJMSReplyTo(feedbackDest);
        producer.send(msg);

        if (params.isTransacted())
        {
            session.commit();
        }

        tmp.receive();

        if (params.isTransacted())
        {
            session.commit();
        }

        tmp.close();
        System.out.println("Consumer has completed the test......");
    }

    public void tearDown() throws Exception
    {
        producer.close();
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
            waitForCompletion();
            tearDown();
        }
        catch(Exception e)
        {
            handleError(e,"Error when running test");
        }
    }


    public static void main(String[] args)
    {
        final PerfProducer prod = new PerfProducer();
        Runnable r = new Runnable()
        {
            public void run()
            {
                prod.test();
            }
        };
        
        Thread t;
        try
        {
            t = Threading.getThreadFactory().createThread(r);                      
        }
        catch(Exception e)
        {
            throw new Error("Error creating producer thread",e);
        }
        t.start();            
    }
}