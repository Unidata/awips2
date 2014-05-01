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
package org.apache.qpid.oldtopic;

import javax.jms.*;

public class Publisher implements MessageListener
{
    private final Object _lock = new Object();
    private final Connection _connection;
    private final Session _session;
    private final MessageFactory _factory;
    private final MessageProducer _publisher;
    private int _count;

    Publisher(Connection connection, int size, int ackMode, boolean persistent) throws Exception
    {
        _connection = connection;
        _session = _connection.createSession(false, ackMode);
        _factory = new MessageFactory(_session, size);
        _publisher = _factory.createTopicPublisher();
        _publisher.setDeliveryMode(persistent ? DeliveryMode.PERSISTENT : DeliveryMode.NON_PERSISTENT);
        System.out.println("Publishing " + (persistent ? "persistent" : "non-persistent") + " messages of " + size + " bytes, " + Config.getAckModeDescription(ackMode) + ".");
    }

    private void test(Config config) throws Exception
    {
        test(config.getBatch(), config.getDelay(), config.getMessages(), config.getClients(), config.getWarmup());
    }

    private void test(int batches, long delay, int msgCount, int consumerCount, int warmup) throws Exception
    {
        _factory.createControlConsumer().setMessageListener(this);
        _connection.start();

        if(warmup > 0)
        {
            System.out.println("Runing warmup (" + warmup + " msgs)");
            long time = batch(warmup, consumerCount);
            System.out.println("Warmup completed in " + time + "ms");
        }

        long[] times = new long[batches];
        for(int i = 0; i < batches; i++)
        {
            if(i > 0) Thread.sleep(delay*1000);
            times[i] = batch(msgCount, consumerCount);
            System.out.println("Batch " + (i+1) + " of " + batches + " completed in " + times[i] + " ms.");
        }

        long min = min(times);
        long max = max(times);
        System.out.println("min: " + min + ", max: " + max + " avg: " + avg(times, min, max));

        //request shutdown
        _publisher.send(_factory.createShutdownMessage());

        _connection.stop();
        _connection.close();
    }

    private long batch(int msgCount, int consumerCount) throws Exception
    {
        _count = consumerCount;
        long start = System.currentTimeMillis();
        publish(msgCount);
        waitForCompletion(consumerCount);
        return System.currentTimeMillis() - start;
    }

    private void publish(int count) throws Exception
    {

        //send events
        for (int i = 0; i < count; i++)
        {
            _publisher.send(_factory.createEventMessage());
            if ((i + 1) % 100 == 0)
            {
                System.out.println("Sent " + (i + 1) + " messages");
            }
        }

        //request report
        _publisher.send(_factory.createReportRequestMessage());
    }

    private void waitForCompletion(int consumers) throws Exception
    {
        System.out.println("Waiting for completion...");
        synchronized (_lock)
        {
            while (_count > 0)
            {
                _lock.wait();
            }
        }
    }


    public void onMessage(Message message)
    {
        System.out.println("Received report " + _factory.getReport(message) + " " + --_count + " remaining");
        if (_count == 0)
        {
            synchronized (_lock)
            {
                _lock.notify();
            }
        }
    }

    static long min(long[] times)
    {
        long min = times.length > 0 ? times[0] : 0;
        for(int i = 0; i < times.length; i++)
        {
            min = Math.min(min, times[i]);
        }
        return min;
    }

    static long max(long[] times)
    {
        long max = times.length > 0 ? times[0] : 0;
        for(int i = 0; i < times.length; i++)
        {
            max = Math.max(max, times[i]);
        }
        return max;
    }

    static long avg(long[] times, long min, long max)
    {
        long sum = 0;
        for(int i = 0; i < times.length; i++)
        {
            sum += times[i];
        }
        sum -= min;
        sum -= max;

        return (sum / (times.length - 2));
    }

    public static void main(String[] argv) throws Exception
    {
        Config config = new Config();
        config.setOptions(argv);

        Connection con = config.createConnection();
        int size = config.getPayload();
        int ackMode = config.getAckMode();
        boolean persistent = config.usePersistentMessages();
        new Publisher(con, size, ackMode, persistent).test(config);
    }
}
