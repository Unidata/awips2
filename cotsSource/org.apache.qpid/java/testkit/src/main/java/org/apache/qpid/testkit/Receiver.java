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


import java.util.ArrayList;
import java.util.List;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.TextMessage;

import org.apache.qpid.client.AMQConnection;

/**
 * A generic receiver which consumers a stream of messages
 * from a given address in a broker (host/port) 
 * until told to stop by killing it.
 * 
 * It participates in a feedback loop to ensure the producer
 * doesn't fill up the queue. If it receives an "End" msg
 * it sends a reply to the replyTo address in that msg.
 * 
 * It doesn't check for correctness or measure anything
 * leaving those concerns to another entity. 
 * However it prints a timestamp every x secs(-Dreport_frequency)
 * as checkpoint to figure out how far the test has progressed if
 * a failure occurred.
 * 
 * It also takes in an optional Error handler to
 * pass out any error in addition to writing them to std err. 
 * 
 * This is intended more as building block to create
 * more complex test cases. However there is a main method
 * provided to use this standalone.
 *
 * The following options are available and configurable 
 * via jvm args.
 * 
 * sync_rcv - Whether to consume sync (instead of using a listener).
 * report_frequency - how often a timestamp is printed
 * durable
 * transacted
 * tx_size - size of transaction batch in # msgs. 
 */
public class Receiver extends Client implements MessageListener
{
	// Until addressing is properly supported.
	protected enum Reliability {
		AT_MOST_ONCE, AT_LEAST_ONCE, EXACTLY_ONCE;
		
		Reliability getReliability(String s)
		{
			if (s.equalsIgnoreCase("at_most_once"))
			{
				return AT_MOST_ONCE;
			}
			else if (s.equalsIgnoreCase("at_least_once"))
			{
				return AT_LEAST_ONCE;
			}
			else
			{
				return EXACTLY_ONCE;
			}
		}
	};
	
	long msg_count = 0;
	int sequence = 0;
	boolean sync_rcv = Boolean.getBoolean("sync_rcv");
	boolean uniqueDests = Boolean.getBoolean("unique_dests");
	Reliability reliability = Reliability.EXACTLY_ONCE;
	MessageConsumer consumer;
    List<Integer> duplicateMessages = new ArrayList<Integer>();
    
    public Receiver(Connection con,Destination dest) throws Exception
    {
    	super(con);
    	reliability = reliability.getReliability(System.getProperty("reliability","exactly_once"));
    	ssn = con.createSession(transacted,ack_mode);
    	consumer = ssn.createConsumer(dest);
    	if (!sync_rcv)
    	{
    		consumer.setMessageListener(this);
    	}
    	
    	System.out.println("Operating in mode : " + reliability);
    	System.out.println("Receiving messages from : " + dest);
    }

    public void onMessage(Message msg)
    {    	
    	handleMessage(msg);
    }
    
    public void run() throws Exception
    {
    	while(true)
    	{
    		if(sync_rcv)
    		{
    			Message msg = consumer.receive();
    			handleMessage(msg);
    		}
    		Thread.sleep(reportFrequency);
    		System.out.println(df.format(System.currentTimeMillis())
    				+ " - messages received : " + msg_count);
    	}
    }
    
    private void handleMessage(Message m)
    {
    	try
        {   
            if (m instanceof TextMessage && ((TextMessage) m).getText().equals("End"))
            {
                MessageProducer temp = ssn.createProducer(m.getJMSReplyTo());
                Message controlMsg = ssn.createTextMessage();
                temp.send(controlMsg);
                if (transacted)
                {
                    ssn.commit();
                }
                temp.close();
            }
            else
            {   
            	
            	int seq = m.getIntProperty("sequence");   
            	if (uniqueDests)
            	{
            		if (seq == 0)
	                {
            			sequence = 0; // wrap around for each iteration
	                }
            		
	                if (seq < sequence)
	                {                    
	                    duplicateMessages.add(seq);
	                    if (reliability == Reliability.EXACTLY_ONCE)
	                    {
	                    	throw new Exception(": Received a duplicate message (expected="
	                    			+ sequence  + ",received=" + seq + ")" ); 
	                    }                    
	                }
	                else if (seq == sequence)
	                {
	                	sequence++;
	                	msg_count ++;
	                }
	                else
	                {  
	                	// Multiple publishers are not allowed in this test case.
	                	// So out of order messages are not allowed.
	                	throw new Exception(": Received an out of order message (expected="
	                			+ sequence  + ",received=" + seq + ")" ); 
	                }
            	}
                // Please note that this test case doesn't expect duplicates
                // When testing for transactions.
            	if (transacted && msg_count % txSize == 0)
            	{
            		ssn.commit();
            	}
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        	handleError("Exception receiving messages",e);
        }   	
    }

    // Receiver host port address
    public static void main(String[] args) throws Exception
    {
    	String host = "127.0.0.1";
    	int port = 5672;
    	
    	if (args.length > 0)
    	{
    		host = args[0];
    	}
    	if (args.length > 1)
    	{
    		port = Integer.parseInt(args[1]);
    	}    	
    	// #3rd argument should be an address
        // Any other properties is best configured via jvm args    	
        
    	AMQConnection con = new AMQConnection(
				"amqp://username:password@topicClientid/test?brokerlist='tcp://"
						+ host + ":" + port + "'");
        
        // FIXME Need to add support for the new address format
        // Then it's trivial to add destination for that.
        Receiver rcv = new Receiver(con,null);
        rcv.run();
    }

}
