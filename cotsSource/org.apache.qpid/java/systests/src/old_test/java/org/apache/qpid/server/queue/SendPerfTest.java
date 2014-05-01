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
import org.apache.qpid.codec.AMQCodecFactory;
import org.apache.qpid.framing.BasicPublishBody;
import org.apache.qpid.framing.ContentBody;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.server.AMQChannel;
import org.apache.qpid.server.RequiredDeliveryException;
import org.apache.qpid.server.txn.TransactionalContext;
import org.apache.qpid.server.txn.NonTransactionalContext;
import org.apache.qpid.server.exchange.AbstractExchange;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.handler.OnCurrentThreadExecutor;
import org.apache.qpid.server.protocol.AMQMinaProtocolSession;
import org.apache.qpid.server.protocol.AMQProtocolSession;
import org.apache.qpid.server.protocol.MockIoSession;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.SkeletonMessageStore;
import org.apache.qpid.server.util.AveragedRun;
import org.apache.qpid.server.util.TestApplicationRegistry;
import org.apache.qpid.server.util.TimedRun;

import java.util.ArrayList;
import java.util.List;
import java.util.LinkedList;

public class SendPerfTest extends TimedRun
{
    private int _messages = 1000;
    private int _clients = 10;
    private List<AMQQueue> _queues;

    public SendPerfTest(int clients, int messages)
    {
        super("SendPerfTest, msgs=" + messages + ", clients=" + clients);
        _messages = messages;
        _clients = clients;
    }

    protected void setup() throws Exception
    {
        _queues = initQueues(_clients);
        System.gc();
    }

    protected void teardown() throws Exception
    {
        System.gc();
    }

    protected void run() throws Exception
    {
        deliver(_messages, _queues);
    }

    //have a dummy AMQProtocolSession that does nothing on the writeFrame()
    //set up x number of queues
    //create necessary bits and pieces to deliver a message
    //deliver y messages to each queue

    public static void main(String[] argv) throws Exception
    {
        ApplicationRegistry.initialise(new TestApplicationRegistry());
        int clients = Integer.parseInt(argv[0]);
        int messages = Integer.parseInt(argv[1]);
        int iterations = Integer.parseInt(argv[2]);
        AveragedRun test = new AveragedRun(new SendPerfTest(clients, messages), iterations);
        test.run();
    }

    /**
     * Delivers messages to a number of queues.
     * @param count the number of messages to deliver
     * @param queues the list of queues
     * @throws NoConsumersException
     */
    static void deliver(int count, List<AMQQueue> queues) throws AMQException
    {
        BasicPublishBody publish = new BasicPublishBody();
        publish.exchange = new NullExchange().getName();
        ContentHeaderBody header = new ContentHeaderBody();
        List<ContentBody> body = new ArrayList<ContentBody>();
        MessageStore messageStore = new SkeletonMessageStore();
        // channel can be null since it is only used in ack processing which does not apply to this test
        TransactionalContext txContext = new NonTransactionalContext(messageStore, null,
                                                                     new LinkedList<RequiredDeliveryException>());
        body.add(new ContentBody());
        MessageHandleFactory factory = new MessageHandleFactory();
        for (int i = 0; i < count; i++)
        {
            // this routes and delivers the message
            AMQMessage msg = new AMQMessage(i, publish, txContext, header, queues, body, messageStore,
                                            factory);
        }
    }

    static List<AMQQueue> initQueues(int number) throws AMQException
    {
        Exchange exchange = new NullExchange();
        List<AMQQueue> queues = new ArrayList<AMQQueue>(number);
        for (int i = 0; i < number; i++)
        {
            AMQQueue q = createQueue("Queue" + (i + 1));
            q.bind("routingKey", exchange);
            try
            {
                q.registerProtocolSession(createSession(), 1, "1", false);
            }
            catch (Exception e)
            {
                throw new AMQException("Error creating protocol session: " + e, e);
            }
            queues.add(q);
        }
        return queues;
    }

    static AMQQueue createQueue(String name) throws AMQException
    {
        return new AMQQueue(name, false, null, false, ApplicationRegistry.getInstance().getQueueRegistry(),
                new OnCurrentThreadExecutor());
    }

    static AMQProtocolSession createSession() throws Exception
    {
        IApplicationRegistry reg = ApplicationRegistry.getInstance();
        AMQCodecFactory codecFactory = new AMQCodecFactory(true);
        AMQMinaProtocolSession result = new AMQMinaProtocolSession(new MockIoSession(), reg.getQueueRegistry(), reg.getExchangeRegistry(), codecFactory);
        result.addChannel(new AMQChannel(1, null, null));
        return result;
    }

    static class NullExchange extends AbstractExchange
    {
        public String getName()
        {
            return "NullExchange";
        }

        protected ExchangeMBean createMBean()
        {
            return null;
        }

        public void registerQueue(String routingKey, AMQQueue queue, FieldTable args) throws AMQException
        {
        }

        public void deregisterQueue(String routingKey, AMQQueue queue) throws AMQException
        {
        }

        public void route(AMQMessage payload) throws AMQException
        {
        }
    }
}
