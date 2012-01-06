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
import org.apache.log4j.*;
import javax.jms.Connection;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;

public class Listener implements MessageListener
{
    private final Connection _connection;
    private final MessageProducer _controller;
    private final javax.jms.Session _session;
    private final MessageFactory _factory;
    private boolean init;
    private int count;
    private long start;

    Listener(Connection connection, int ackMode) throws Exception
    {
        this(connection, ackMode, null);
    }

    Listener(Connection connection, int ackMode, String name) throws Exception
    {
        _connection = connection;
        _session = connection.createSession(false, ackMode);
        _factory = new MessageFactory(_session);

        //register for events
        if(name == null)
        {
            _factory.createTopicConsumer().setMessageListener(this);
        }
        else
        {
            _factory.createDurableTopicConsumer(name).setMessageListener(this);
        }

        _connection.start();

        _controller = _factory.createControlPublisher();
        System.out.println("Waiting for messages " +
                Config.getAckModeDescription(ackMode)
                + (name == null ? "" : " (subscribed with name " + name + " and client id " + connection.getClientID() + ")")
                + "...");

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

    private String getReport()
    {
        long time = (System.currentTimeMillis() - start);
        return "Received " + count + " in " + time + "ms";
    }

    public void onMessage(Message message)
    {
        if(!init)
        {
            start = System.currentTimeMillis();
            count = 0;
            init = true;
        }

        if(_factory.isShutdown(message))
        {
            shutdown();
        }
        else if(_factory.isReport(message))
        {
            //send a report:
            report();
            init = false;
        }
        else if (++count % 100 == 0)
        {
            System.out.println("Received " + count + " messages.");
        }
    }

    public static void main(String[] argv) throws Exception
    {
        Config config = new Config();
        config.setOptions(argv);

        Connection con = config.createConnection();
        if(config.getClientId() != null)
        {
            con.setClientID(config.getClientId());
        }
        new Listener(con, config.getAckMode(), config.getSubscriptionId());
    }
}
