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

package org.apache.qpid.example.transport;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.channels.SocketChannel;
import java.util.UUID;

/**
 * This is a simple application that demonstrates how you can use the Qpid AMQP interfaces to use existing sockets as
 * the transport for the Client API.
 *
 * The Demo here runs twice:
 * 1. Just to show a simple publish and receive.
 * 2. To demonstrate how to use existing sockets and utilise the underlying client failover mechnaism.
 */
public class ExistingSocketConnectorDemo implements ConnectionListener
{
    private static boolean DEMO_FAILOVER = false;

    public static void main(String[] args) throws IOException, URLSyntaxException, AMQException, JMSException
    {
        System.out.println("Testing socket connection to localhost:5672.");

        new ExistingSocketConnectorDemo();

        System.out.println("Testing socket connection failover between localhost:5672 and localhost:5673.");

        DEMO_FAILOVER = true;

        new ExistingSocketConnectorDemo();
    }

    Connection _connection;
    MessageProducer _producer;
    Session _session;

    String Socket1_ID = UUID.randomUUID().toString();
    String Socket2_ID = UUID.randomUUID().toString();



    /** Here we can see the broker we are connecting to is set to be 'socket:///' signifying we will provide the socket. */
    public final String CONNECTION = "amqp://guest:guest@id/test?brokerlist='socket://" + Socket1_ID + ";socket://" + Socket2_ID + "'";


    public ExistingSocketConnectorDemo() throws IOException, URLSyntaxException, AMQException, JMSException
    {

        Socket socket = SocketChannel.open().socket();
        socket.connect(new InetSocketAddress("localhost", 5672));

        TransportConnection.registerOpenSocket(Socket1_ID, socket);


        _connection = new AMQConnection(CONNECTION);

        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        MessageConsumer consumer = _session.createConsumer(_session.createQueue("Queue"));

        _producer = _session.createProducer(_session.createQueue("Queue"));

        _connection.start();

        if (!DEMO_FAILOVER)
        {
            _producer.send(_session.createTextMessage("Simple Test"));
        }
        else
        {
            // Using the Qpid interfaces we can set a listener that allows us to demonstrate failover
            ((AMQConnection) _connection).setConnectionListener(this);

            System.out.println("Testing failover: Please ensure second broker running on localhost:5673 and shutdown broker on 5672.");
        }

        //We do a blocking receive here so that we can demonstrate failover.
        Message message = consumer.receive();

        System.out.println("Recevied :" + message);

        _connection.close();
    }

    // ConnectionListener Interface

    public void bytesSent(long count)
    {
        //not used in this example
    }
    public void bytesReceived(long count)
    {
        //not used in this example
    }

    public boolean preFailover(boolean redirect)
    {
        /**
         * This method is called before the underlying client library starts to reconnect. This gives us the opportunity
         * to set a new socket for the failover to occur on.
         */
        try
        {
            Socket socket = SocketChannel.open().socket();

            socket.connect(new InetSocketAddress("localhost", 5673));

            // This is the new method to pass in an open socket for the connection to use.
            TransportConnection.registerOpenSocket(Socket2_ID, socket);
        }
        catch (IOException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public boolean preResubscribe()
    {
        //not used in this example - but must return true to allow the resubscription of existing clients.
        return true;
    }

    public void failoverComplete()
    {
        // Now that failover has completed we can send a message that the receiving thread will pick up
        try
        {
            _producer.send(_session.createTextMessage("Simple Failover Test"));
        }
        catch (JMSException e)
        {
            e.printStackTrace();
        }
    }
}
