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
package org.apache.qpid.testkit;


import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.thread.Threading;

/**
 * A basic test case class that could launch a Sender/Receiver
 * or both, each on it's own separate thread.
 * 
 * If con_count == ssn_count, then each entity created will have
 * it's own Connection. Else if con_count < ssn_count, then
 * a connection will be shared by ssn_count/con_count # of entities.
 * 
 * The if both sender and receiver options are set, it will
 * share a connection.   
 *
 * The following options are available as jvm args
 * host, port
 * con_count,ssn_count
 * con_idle_time -  which determines heartbeat
 * sender, receiver - booleans which indicate which entity to create.
 * Setting them both is also a valid option.
 */
public class TestLauncher implements ErrorHandler
{
    protected String host = "127.0.0.1";
    protected int port = 5672;
    protected int session_count = 1;
    protected int connection_count = 1;
    protected long connection_idle_time = 5000;
    protected boolean sender = false;
    protected boolean receiver = false;
    protected String url;

    protected String queue_name =  "message_queue";
    protected String exchange_name =  "amq.direct";
    protected String routing_key =  "routing_key";
    protected boolean uniqueDests = false;
    protected boolean durable = false;
    protected String failover = "";
    protected AMQConnection controlCon;
    protected Destination controlDest = null;
    protected Session controlSession = null;
    protected MessageProducer statusSender;
    protected List<AMQConnection> clients = new ArrayList<AMQConnection>();
    protected DateFormat df = new SimpleDateFormat("yyyy.MM.dd 'at' HH:mm:ss");
    protected NumberFormat nf = new DecimalFormat("##.00");
    protected String testName;    
        
    public TestLauncher()
    {
       testName = System.getProperty("test_name","UNKNOWN");
       host = System.getProperty("host", "127.0.0.1");
       port = Integer.getInteger("port", 5672);
       session_count = Integer.getInteger("ssn_count", 1);
       connection_count = Integer.getInteger("con_count", 1);
       connection_idle_time = Long.getLong("con_idle_time", 5000);
       sender = Boolean.getBoolean("sender");
       receiver = Boolean.getBoolean("receiver");
       
       queue_name = System.getProperty("queue_name", "message_queue");
       exchange_name = System.getProperty("exchange_name", "amq.direct");
       routing_key = System.getProperty("routing_key", "routing_key");
       failover = System.getProperty("failover", "");
       uniqueDests = Boolean.getBoolean("unique_dests");
       durable = Boolean.getBoolean("durable");
       
       url = "amqp://username:password@topicClientid/test?brokerlist='tcp://"
				+ host + ":" + port + "?idle_timeout=" + connection_idle_time
				+ "'";
       
       if (failover.equalsIgnoreCase("failover_exchange"))
       {
    	   url += "&failover='failover_exchange'";
    	   
    	   System.out.println("Failover exchange " + url );
       }
       
       configureLogging();
    }

    protected void configureLogging()
    {
    	PatternLayout layout = new PatternLayout();
    	layout.setConversionPattern("%t %d %p [%c{4}] %m%n");
    	BasicConfigurator.configure(new ConsoleAppender(layout));
    	
    	String logLevel = System.getProperty("log.level","warn");
    	String logComponent = System.getProperty("log.comp","org.apache.qpid");
    	
    	Logger logger = Logger.getLogger(logComponent);
    	logger.setLevel(Level.toLevel(logLevel, Level.WARN));
    	
    	System.out.println("Level " + logger.getLevel());
    	
    }
    
    public void setUpControlChannel()
    {
        try
        {
            controlCon = new AMQConnection(url);
            controlCon.start();
            
            controlDest = new AMQQueue(new AMQShortString(""),
                                     new AMQShortString("control"),
                                     new AMQShortString("control"),
                                     false, //exclusive
                                     false, //auto-delete
                                     false); // durable

            // Create the session to setup the messages
            controlSession = controlCon.createSession(false, Session.AUTO_ACKNOWLEDGE);
            statusSender = controlSession.createProducer(controlDest);

        }
        catch (Exception e)
        {
            handleError("Error while setting up the test",e);
        }
    }
    
    public void cleanup()
    {
    	try
    	{
    		controlSession.close();
    		controlCon.close();
    		for (AMQConnection con : clients)
    		{
    			con.close();
    		}
    	}
	    catch (Exception e)
	    {
	        handleError("Error while tearing down the test",e);
	    }
    }
        
    public void start()
    {
        try
        {
           
        	int ssn_per_con = session_count;
        	if (connection_count < session_count)
        	{
        		ssn_per_con = session_count/connection_count;
        	}
        	
        	for (int i = 0; i< connection_count; i++)
        	{
        		AMQConnection con = new AMQConnection(url);
        		con.start();
        		clients.add(con);        		
        		for (int j = 0; j< ssn_per_con; j++)
            	{
        			String prefix = createPrefix(i,j);
        			Destination dest = createDest(prefix);
        			if (sender)
        			{
        				createSender(prefix,con,dest,this);
        			}
        			
        			if (receiver)
        			{
        				createReceiver(prefix,con,dest,this);
        			}
            	}
        	}
        }
        catch (Exception e)
        {
            handleError("Exception while setting up the test",e);
        }

    }
    
    protected void createReceiver(String index,final AMQConnection con, final Destination dest, final ErrorHandler h)
    {
    	Runnable r = new Runnable()
        {
            public void run()
            {
               try 
               {
            	   Receiver rcv = new Receiver(con,dest);
				   rcv.setErrorHandler(h);
				   rcv.run();
				}
	            catch (Exception e) 
	            {
					h.handleError("Error Starting Receiver", e);
				}
            }
        };
        
        Thread t = null;
        try
        {
            t = Threading.getThreadFactory().createThread(r);                      
        }
        catch(Exception e)
        {
            handleError("Error creating Receive thread",e);
        }
        
        t.setName("ReceiverThread-" + index);
        t.start();
    }
    
    protected void createSender(String index,final AMQConnection con, final Destination dest, final ErrorHandler h)
    {
    	Runnable r = new Runnable()
        {
            public void run()
            {
               try 
               {
            	   Sender sender = new Sender(con, dest);
            	   sender.setErrorHandler(h);
            	   sender.run();
				}
	            catch (Exception e) 
	            {
					h.handleError("Error Starting Sender", e);
				}
            }
        };
        
        Thread t = null;
        try
        {
            t = Threading.getThreadFactory().createThread(r);                      
        }
        catch(Exception e)
        {
            handleError("Error creating Sender thread",e);
        }
        
        t.setName("SenderThread-" + index);
        t.start();
    }

    public void handleError(String msg,Exception e)
    {
    	// In case sending the message fails
        StringBuilder sb = new StringBuilder();
        sb.append(msg);
        sb.append(" @ ");
        sb.append(df.format(new Date(System.currentTimeMillis())));
        sb.append(" ");
        sb.append(e.getMessage());
        System.err.println(sb.toString());
        e.printStackTrace();
        
        try 
        {
			TextMessage errorMsg = controlSession.createTextMessage();
			errorMsg.setStringProperty("status", "error");
			errorMsg.setStringProperty("desc", msg);
			errorMsg.setStringProperty("time", df.format(new Date(System.currentTimeMillis())));        
			errorMsg.setStringProperty("exception-trace", serializeStackTrace(e));
			synchronized (this)
			{
				statusSender.send(errorMsg);
			}
		} catch (JMSException e1) {
			e1.printStackTrace();
		}       
    }
    
    private String serializeStackTrace(Exception e)
    {
    	ByteArrayOutputStream bOut = new ByteArrayOutputStream();
    	PrintStream printStream = new PrintStream(bOut);
    	e.printStackTrace(printStream);
    	printStream.close();
    	return bOut.toString();
    }
    
    private String createPrefix(int i, int j)
    {
    	return String.valueOf(i).concat(String.valueOf(j));
    }
    
    /**
     * The following are supported.
     * 
     * 1. A producer/consumer pair on a topic or a queue
     * 2. A single producer with multiple consumers on topic/queue
     * 
     * Multiple consumers on a topic will result in a private queue
     * for each consumers.
     * 
     * We want to avoid multiple producers on the same topic/queue
     * as the queues will fill up in no time.
     */
    private Destination createDest(String prefix)
    {
    	Destination dest = null;
        if (exchange_name.equals("amq.topic"))
        {
            dest = new AMQTopic(
            		 new AMQShortString(exchange_name),
                     new AMQShortString(uniqueDests ? prefix + routing_key :
							                                   routing_key),
                     false,   //auto-delete
                     null,   //queue name
                     durable);
        }
        else
        {
            dest = new AMQQueue(
            		new AMQShortString(exchange_name),
            		new AMQShortString(uniqueDests ? prefix + routing_key :
                                                              routing_key),
                    new AMQShortString(uniqueDests ? prefix + queue_name :
                    	                                      queue_name),
                    false, //exclusive
                    false, //auto-delete
                    durable);
        }
        return dest;
    }
    
    public static void main(String[] args)
    {
    	final TestLauncher test = new TestLauncher();
    	test.setUpControlChannel();
    	test.start();
    	Runtime.getRuntime().addShutdownHook(new Thread() {
    	    public void run() { test.cleanup(); }
    	});

    }
}
