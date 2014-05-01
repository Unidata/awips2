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
package org.apache.qpid;

import org.apache.qpid.transport.*;
import org.apache.qpid.transport.network.mina.MinaHandler;

import static org.apache.qpid.transport.util.Functions.str;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;


/**
 * ToyBroker
 *
 * @author Rafael H. Schloming
 */

class ToyBroker extends SessionDelegate
{

    private ToyExchange exchange;
    private Map<String,Consumer> consumers = new ConcurrentHashMap<String,Consumer>();

    public ToyBroker(ToyExchange exchange)
    {
        this.exchange = exchange;
    }

    public void messageAcquire(Session context, MessageAcquire struct)
    {
        System.out.println("\n==================> messageAcquire " );
        context.executionResult((int) struct.getId(), new Acquired(struct.getTransfers()));
    }

    @Override public void queueDeclare(Session ssn, QueueDeclare qd)
    {
        exchange.createQueue(qd.getQueue());
        System.out.println("\n==================> declared queue: " + qd.getQueue() + "\n");
    }

    @Override public void exchangeBind(Session ssn, ExchangeBind qb)
    {
        exchange.bindQueue(qb.getExchange(), qb.getBindingKey(),qb.getQueue());
        System.out.println("\n==================> bound queue: " + qb.getQueue() + " with binding key " + qb.getBindingKey() + "\n");
    }

    @Override public void queueQuery(Session ssn, QueueQuery qq)
    {
        QueueQueryResult result = new QueueQueryResult().queue(qq.getQueue());
        ssn.executionResult((int) qq.getId(), result);
    }

    @Override public void messageSubscribe(Session ssn, MessageSubscribe ms)
    {
        Consumer c = new Consumer();
        c._queueName = ms.getQueue();
        consumers.put(ms.getDestination(),c);
        System.out.println("\n==================> message subscribe : " + ms.getDestination() + " queue: " + ms.getQueue()  + "\n");
    }

    @Override public void messageFlow(Session ssn,MessageFlow struct)
    {
        Consumer c = consumers.get(struct.getDestination());
        c._credit = struct.getValue();
        System.out.println("\n==================> message flow : " + struct.getDestination() + " credit: " + struct.getValue()  + "\n");
    }

    @Override public void messageFlush(Session ssn,MessageFlush struct)
    {
        System.out.println("\n==================> message flush for consumer : " + struct.getDestination() + "\n");
        checkAndSendMessagesToConsumer(ssn,struct.getDestination());
    }

    @Override public void messageTransfer(Session ssn, MessageTransfer xfr)
    {
        String dest = xfr.getDestination();
        System.out.println("received transfer " + dest);
        Header header = xfr.getHeader();
        DeliveryProperties props = header.get(DeliveryProperties.class);
        if (props != null)
        {
            System.out.println("received headers routing_key " + props.getRoutingKey());
        }
        MessageProperties mp = header.get(MessageProperties.class);
        System.out.println("MP: " + mp);
        if (mp != null)
        {
            System.out.println(mp.getApplicationHeaders());
        }

        if (exchange.route(dest,props.getRoutingKey(),xfr))
        {
            System.out.println("queued " + xfr);
            dispatchMessages(ssn);
        }
        else
        {

            if (props == null || !props.getDiscardUnroutable())
            {
                RangeSet ranges = new RangeSet();
                ranges.add(xfr.getId());
                ssn.messageReject(ranges, MessageRejectCode.UNROUTABLE,
                                  "no such destination");
            }
        }
        ssn.processed(xfr);
    }

    private void transferMessageToPeer(Session ssn,String dest, MessageTransfer m)
    {
        System.out.println("\n==================> Transfering message to: " +dest + "\n");
        ssn.messageTransfer(m.getDestination(), MessageAcceptMode.EXPLICIT,
                            MessageAcquireMode.PRE_ACQUIRED,
                            m.getHeader(), m.getBody());
    }

    private void dispatchMessages(Session ssn)
    {
        for (String dest: consumers.keySet())
        {
            checkAndSendMessagesToConsumer(ssn,dest);
        }
    }

    private void checkAndSendMessagesToConsumer(Session ssn,String dest)
    {
        Consumer c = consumers.get(dest);
        LinkedBlockingQueue<MessageTransfer> queue = exchange.getQueue(c._queueName);
        MessageTransfer m = queue.poll();
        while (m != null && c._credit>0)
        {
            transferMessageToPeer(ssn,dest,m);
            c._credit--;
            m = queue.poll();
        }
    }

    // ugly, but who cares :)
    // assumes unit is always no of messages, not bytes
    // assumes it's credit mode and not window
    private class Consumer
    {
        long _credit;
        String _queueName;
    }

    public static final void main(String[] args) throws IOException
    {
        final ToyExchange exchange = new ToyExchange();
        ConnectionDelegate delegate = new ServerDelegate()
        {
            public SessionDelegate getSessionDelegate()
            {
                return new ToyBroker(exchange);
            }
        };

        MinaHandler.accept("0.0.0.0", 5672, delegate);
    }

}
