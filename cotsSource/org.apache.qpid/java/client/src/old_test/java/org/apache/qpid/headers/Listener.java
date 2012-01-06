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
package org.apache.qpid.headers;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.jms.Session;
//import org.apache.qpid.testutil.Config;

import javax.jms.MessageListener;
import javax.jms.Message;
import javax.jms.Destination;
import javax.jms.MessageProducer;
import javax.jms.JMSException;

public class Listener //implements MessageListener
{
/*    private final AMQConnection _connection;
    private final MessageProducer _controller;
    private final AMQSession _session;
    private final MessageFactory _factory;
    private int count;
    private long start;

    Listener(AMQConnection connection, Destination exchange) throws Exception
    {
        _connection = connection;
        _session = (AMQSession) connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        _factory = new MessageFactory(_session, 0, 19);

        //register for events
        _factory.createConsumer(exchange).setMessageListener(this);
        _connection.start();

        _controller = _session.createProducer(exchange);
    }

    private void shutdown()
    {
        try
        {
            _session.close();
            _connection.stop();
            _connection.close();
        }
        catch(Exception e)
        {
            e.printStackTrace(System.out);
        }
    }

    private void report()
    {
        try
        {
            String msg = getReport();
            _controller.send(_factory.createReportResponseMessage(msg));
            System.out.println("Sent report: " + msg);
        }
        catch(Exception e)
        {
            e.printStackTrace(System.out);
        }
    }

    private String getReport() throws JMSException
    {
        long time = (System.currentTimeMillis() - start);
        return "Received " + count + " in " + time + "ms";
    }

    public void onMessage(Message message)
    {
        if(count == 0) start = System.currentTimeMillis();

        if(_factory.isShutdown(message))
        {
            shutdown();
        }
        else if(_factory.isReport(message))
        {
            //send a report:
            report();
        }
        else if (++count % 100 == 0)
        {
            System.out.println("Received " + count + " messages.");
        }
    }

    public static void main(String[] argv) throws Exception
    {
        Config config = new Config();
        config.setType(Config.HEADERS);
        config.setName("test_headers_exchange");
        config.setOptions(argv);
        new Listener((AMQConnection) config.getConnection(), config.getDestination());
    }*/
}
