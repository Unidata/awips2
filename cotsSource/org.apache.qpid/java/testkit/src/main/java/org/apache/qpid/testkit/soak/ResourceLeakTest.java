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
package org.apache.qpid.testkit.soak;


import java.util.Random;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.BasicMessageConsumer;
import org.apache.qpid.client.BasicMessageProducer;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.testkit.TestLauncher;
import org.apache.qpid.thread.Threading;

/**
 * Test Description
 * ================
 * This test will open x number of connections where each
 * connection will create a session and a producer/consumer pair,
 * and then a randomly selected set of connections (about 1/4th)
 * will send a configurable no of messages and try to receive them.
 * It will then sleep for configurable time interval and
 * tear down the connections/sessions/consumers.
 * It will then repeat the process again until the test is stopped.
 *
 * Purpose of the test
 * ===================
 * To find if the broker has leaks when cleaning resources.
 * To find if the client has leaks with resources.
 */
public class ResourceLeakTest extends TestLauncher
{
  /*  protected long connection_idle_time = 5000;    
    protected Random rand = new Random();
    
    public ResourceLeakTest()
    {
        super();        
    }

    public void test()
    {
        try
        {

            AMQConnection[] cons = new AMQConnection[connection_count];
            Session[] sessions = new Session[connection_count];
            MessageConsumer[] msgCons = new MessageConsumer[connection_count];
            MessageProducer [] msgProds = new MessageProducer[connection_count];
            
            while (true)
            {
                for (int i = 0; i < connection_count; i++)
                {
                    AMQConnection con = new AMQConnection(url);
                    con.start();
                    cons[i] = con;
                    Session ssn = con.createSession(false, Session.AUTO_ACKNOWLEDGE);
                    sessions[i] = ssn;
                    Destination dest = new AMQQueue(new AMQShortString(exchange_name),
                            new AMQShortString(routing_key + i),
                            new AMQShortString(queue_name + i),
                            true, //exclusive
                            true  // auto delete
                            );
                    MessageConsumer msgCon = ssn.createConsumer(dest);
                    msgCons[i] = msgCon;
                    MessageProducer msgProd = ssn.createProducer(dest);
                    msgProds[i] = msgProd;
                }
                
                // Select some connections randomly and send/recv messages
                // Exercise around quarter of the connections
                for (int i=0; i < connection_count/4; i++)
                {    
                    int k = rand.nextInt(connection_count);
                    
                    BytesMessage msg = sessions[k].createBytesMessage();
                    msg.writeBytes("Test Msg".getBytes());

                    for (int j = 0; j < msg_count;j++)
                    {
                        msgProds[k].send(msg);
                    }

                    int j = 0;
                    while (j < msg_count)
                    {
                      msgCons[k].receive();
                      j++;
                    }
                }
                System.out.println(df.format(System.currentTimeMillis()));
                Thread.sleep(connection_idle_time);

                try
                {
                    for (int i = 0; i < connection_count; i++)
                    {
                        if (!((BasicMessageConsumer)msgCons[i]).isClosed())
                        {
                            msgCons[i].close();
                        }
                        
                        if (!((BasicMessageProducer)msgProds[i]).isClosed())
                        {
                            msgProds[i].close();
                        }
                        
                        if (!((AMQSession)sessions[i]).isClosed())
                        {
                            sessions[i].close();
                        }
                        if (!((AMQConnection)cons[i]).isClosed())
                        {
                            cons[i].close();
                        }
                    }
                }
                catch (Exception e)
                {
                    handleError(e,"Exception closing resources");
                }
            }
        }
        catch (Exception e)
        {
            handleError(e,"Exception in setting up the test");
        }

    }

    public static void main(String[] args)
    {
        final ResourceLeakTest test = new ResourceLeakTest();
        Runnable r = new Runnable(){    
            public void run()
            {
                test.test();
            }
        };    
        
        Thread t;
        try
        {
            t = Threading.getThreadFactory().createThread(r);                      
        }
        catch(Exception e)
        {
            throw new Error("Error creating test thread",e);
        }
    }*/

}
