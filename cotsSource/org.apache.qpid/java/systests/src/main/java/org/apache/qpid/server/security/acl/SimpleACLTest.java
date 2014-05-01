/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
*  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */

package org.apache.qpid.server.security.acl;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQConnectionFailureException;
import org.apache.qpid.client.AMQAuthenticationException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.jms.ConnectionListener;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.Connection;
import javax.jms.DeliveryMode;
import javax.jms.ExceptionListener;
import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.naming.NamingException;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class SimpleACLTest extends QpidTestCase implements ConnectionListener
{
    public void setUp() throws Exception
    {
    	//Performing setUp here would result in a broker with the default ACL test config
    	
    	//Each test now calls the private setUpACLTest to allow them to make 
    	//individual customisations to the base ACL settings
    }


    public void tearDown() throws Exception
    {
        try
        {
            super.tearDown();
        }
        catch (JMSException e)
        {
            //we're throwing this away as it can happen in this test as the state manager remembers exceptions
            //that we provoked with authentication failures, where the test passes - we can ignore on con close
        }
    }
    
    private void setUpACLTest() throws Exception
    {
        final String QPID_HOME = System.getProperty("QPID_HOME");

        if (QPID_HOME == null)
        {
            fail("QPID_HOME not set");
        }

        // Initialise ACLs.
        _configFile = new File(QPID_HOME, "etc/config-systests-acl.xml");

        super.setUp();
    }

    public String createConnectionString(String username, String password)
    {

        return "amqp://" + username + ":" + password + "@clientid/test?brokerlist='" + getBroker() + "?retries='0''";
    }

    public void testAccessAuthorized() throws AMQException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            Session sesh = conn.createSession(true, Session.SESSION_TRANSACTED);

            conn.start();

            //Do something to show connection is active.
            sesh.rollback();

            conn.close();
        }
        catch (Exception e)
        {
            fail("Connection was not created due to:" + e);
        }
    }
    
    public void testAccessVhostAuthorisedGuest() throws IOException, Exception
    {
        //The 'guest' user has no access to the 'test' vhost, as tested below in testAccessNoRights(), and so
        //is unable to perform actions such as connecting (and by extension, creating a queue, and consuming 
        //from a queue etc). In order to test the vhost-wide 'access' ACL right, the 'guest' user has been given 
        //this right in the 'test2' vhost.

        setUpACLTest();
        
        try
        {
            //get a connection to the 'test2' vhost using the guest user and perform various actions.
            Connection conn = getConnection(new AMQConnectionURL(
                    "amqp://username:password@clientid/test2?brokerlist='" + getBroker() + "'"));
            
            ((AMQConnection) conn).setConnectionListener(this);

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            //create Queues and consumers for each
            Queue namedQueue = sesh.createQueue("vhostAccessCreatedQueue" + getTestQueueName());
            Queue tempQueue = sesh.createTemporaryQueue();
            MessageConsumer consumer = sesh.createConsumer(namedQueue);
            MessageConsumer tempConsumer = sesh.createConsumer(tempQueue);

            //send a message to each queue (also causing an exchange declare)
            MessageProducer sender = ((AMQSession)sesh).createProducer(null);
            ((org.apache.qpid.jms.MessageProducer) sender).send(namedQueue, sesh.createTextMessage("test"),
                                                                DeliveryMode.NON_PERSISTENT, 0, 0L, false, false, true);
            ((org.apache.qpid.jms.MessageProducer) sender).send(tempQueue, sesh.createTextMessage("test"),
                                                                DeliveryMode.NON_PERSISTENT, 0, 0L, false, false, true);

            //consume the messages from the queues
            consumer.receive(2000);
            tempConsumer.receive(2000);

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test failed due to:" + e.getMessage());
        }
    }
    
    public void testAccessNoRights() throws Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("guest", "guest");

            //Attempt to do do things to test connection.
            Session sesh = conn.createSession(true, Session.SESSION_TRANSACTED);
            conn.start();
            sesh.rollback();

            fail("Connection was created.");
        }
        catch (JMSException jmse)
        {
            Throwable linkedException = jmse.getLinkedException();
            assertNotNull("Cause was null", linkedException);

            assertEquals("Linked Exception was wrong type", AMQConnectionFailureException.class, linkedException.getClass());

            Throwable cause = linkedException.getCause();
            assertEquals("Cause was wrong type", AMQAuthenticationException.class, cause.getClass());
            assertEquals("Incorrect error code thrown", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());
        }
    }

    public void testClientConsumeFromTempQueueValid() throws AMQException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            sesh.createConsumer(sesh.createTemporaryQueue());

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test failed due to:" + e.getMessage());
        }
    }

    public void testClientConsumeFromNamedQueueInvalid() throws NamingException, Exception
    {
    	setUpACLTest();
    	
    	//QPID-2081: use a latch to sync on exception causing connection close, to work 
    	//around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            //Prevent Failover
            ((AMQConnection) conn).setConnectionListener(this);

            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });
            
            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            sesh.createConsumer(sesh.createQueue("IllegalQueue"));
            fail("Test failed as consumer was created.");
            //conn will be automatically closed
        }
        catch (JMSException e)
        {
            Throwable cause = e.getLinkedException();

            assertNotNull("There was no liked exception", cause);
            assertEquals("Wrong linked exception type", AMQAuthenticationException.class, cause.getClass());
            assertEquals("Incorrect error code received", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());
        
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    public void testClientCreateTemporaryQueue() throws JMSException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            //Create Temporary Queue  - can't use the createTempQueue as QueueName is null.
            ((AMQSession) sesh).createQueue(new AMQShortString("doesnt_matter_as_autodelete_means_tmp"),
                                            true, false, false);

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test failed due to:" + e.getMessage());
        }
    }

    public void testClientCreateNamedQueue() throws NamingException, JMSException, AMQException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });
            
            //Create a Named Queue
            ((AMQSession) sesh).createQueue(new AMQShortString("IllegalQueue"), false, false, false);

            fail("Test failed as Queue creation succeded.");
            //conn will be automatically closed
        }
        catch (AMQAuthenticationException amqe)
        {
            amqe.printStackTrace();
            assertEquals("Incorrect error code thrown", 403, ((AMQAuthenticationException) amqe).getErrorCode().getCode());
        
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    public void testClientPublishUsingTransactionSuccess() throws AMQException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            ((AMQConnection) conn).setConnectionListener(this);

            Session sesh = conn.createSession(true, Session.SESSION_TRANSACTED);

            conn.start();

            MessageProducer sender = sesh.createProducer(sesh.createQueue("example.RequestQueue"));

            sender.send(sesh.createTextMessage("test"));

            //Send the message using a transaction as this will allow us to retrieve any errors that occur on the broker.
            sesh.commit();

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test publish failed:" + e);
        }
    }

    public void testClientPublishValidQueueSuccess() throws AMQException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            ((AMQConnection) conn).setConnectionListener(this);

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            MessageProducer sender = ((AMQSession) sesh).createProducer(null);

            Queue queue = sesh.createQueue("example.RequestQueue");

            // Send a message that we will wait to be sent, this should give the broker time to process the msg
            // before we finish this test. Message is set !immed !mand as the queue is invalid so want to test ACLs not
            // queue existence.
            ((org.apache.qpid.jms.MessageProducer) sender).send(queue, sesh.createTextMessage("test"),
                                                                DeliveryMode.NON_PERSISTENT, 0, 0L, false, false, true);

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test publish failed:" + e);
        }
    }

    public void testClientPublishInvalidQueueSuccess() throws AMQException, URLSyntaxException, JMSException, NamingException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            ((AMQConnection) conn).setConnectionListener(this);
            
            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });

            Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            MessageProducer sender = ((AMQSession) session).createProducer(null);

            Queue queue = session.createQueue("Invalid");

            // Send a message that we will wait to be sent, this should give the broker time to close the connection
            // before we finish this test. Message is set !immed !mand as the queue is invalid so want to test ACLs not
            // queue existence.
            ((org.apache.qpid.jms.MessageProducer) sender).send(queue, session.createTextMessage("test"),
                                                                DeliveryMode.NON_PERSISTENT, 0, 0L, false, false, true);

            // Test the connection with a valid consumer
            // This may fail as the session may be closed before the queue or the consumer created.
            Queue temp = session.createTemporaryQueue();

            session.createConsumer(temp).close();

            //Connection should now be closed and will throw the exception caused by the above send
            conn.close();

            fail("Close is not expected to succeed.");
        }
        catch (JMSException e)
        {
            Throwable cause = e.getLinkedException();
            if (!(cause instanceof AMQAuthenticationException))
            {
                e.printStackTrace();
            }
            assertEquals("Incorrect exception", AMQAuthenticationException.class, cause.getClass());
            assertEquals("Incorrect error code thrown", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());
            
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    public void testServerConsumeFromNamedQueueValid() throws AMQException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("server", "guest");

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            sesh.createConsumer(sesh.createQueue("example.RequestQueue"));

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test failed due to:" + e.getMessage());
        }
    }

    public void testServerConsumeFromNamedQueueInvalid() throws AMQException, URLSyntaxException, NamingException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("client", "guest");

            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });
            
            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            sesh.createConsumer(sesh.createQueue("Invalid"));

            fail("Test failed as consumer was created.");
            //conn will be automatically closed
        }
        catch (JMSException e)
        {
            Throwable cause = e.getLinkedException();

            assertNotNull("There was no liked exception", cause);
            assertEquals("Wrong linked exception type", AMQAuthenticationException.class, cause.getClass());
            assertEquals("Incorrect error code received", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());
 
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    public void testServerConsumeFromTemporaryQueue() throws AMQException, URLSyntaxException, NamingException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("server", "guest");

            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });
            
            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            sesh.createConsumer(sesh.createTemporaryQueue());
            fail("Test failed as consumer was created.");
            //conn will be automatically closed
        }
        catch (JMSException e)
        {
            Throwable cause = e.getLinkedException();

            assertNotNull("There was no liked exception", cause);
            assertEquals("Wrong linked exception type", AMQAuthenticationException.class, cause.getClass());
            assertEquals("Incorrect error code received", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());
        
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    @Override
    public Connection getConnection(String username, String password) throws NamingException, JMSException
    {
        AMQConnection connection = (AMQConnection) super.getConnection(username, password);

        //Prevent Failover
        connection.setConnectionListener(this);

        return (Connection) connection;
    }

    public void testServerCreateNamedQueueValid() throws JMSException, URLSyntaxException, Exception
    {
    	setUpACLTest();
    	
        try
        {
            Connection conn = getConnection("server", "guest");

            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            //Create Temporary Queue
            ((AMQSession) sesh).createQueue(new AMQShortString("example.RequestQueue"), false, false, false);

            conn.close();
        }
        catch (Exception e)
        {
            fail("Test failed due to:" + e.getMessage());
        }
    }

    public void testServerCreateNamedQueueInvalid() throws JMSException, URLSyntaxException, AMQException, NamingException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("server", "guest");

            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });
            
            Session sesh = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            //Create a Named Queue
            ((AMQSession) sesh).createQueue(new AMQShortString("IllegalQueue"), false, false, false);

            fail("Test failed as creation succeded.");
            //conn will be automatically closed
        }
        catch (AMQAuthenticationException amqe)
        {
            assertEquals("Incorrect error code thrown", 403, amqe.getErrorCode().getCode());
            
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    public void testServerCreateTemporaryQueueInvalid() throws NamingException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);

        Connection conn = getConnection("server", "guest");
        try
        {

            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });

            Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            session.createTemporaryQueue();

            fail("Test failed as creation succeded.");
        }
        catch (JMSException e)
        {
            Throwable cause = e.getLinkedException();

            assertNotNull("There was no liked exception", cause);
            assertEquals("Wrong linked exception type", AMQAuthenticationException.class, cause.getClass());
            assertEquals("Incorrect error code received", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());

            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
        finally
        {
        	try
        	{
        		conn.close();
        	}
        	catch (Exception e)
        	{
        		// This normally fails because we are denied
        	}
        }
    }

    public void testServerCreateAutoDeleteQueueInvalid() throws NamingException, JMSException, AMQException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        Connection connection = null;
        try
        {
            connection = getConnection("server", "guest");

            connection.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });
            
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            connection.start();

            ((AMQSession) session).createQueue(new AMQShortString("again_ensure_auto_delete_queue_for_temporary"),
                                               true, false, false);

            fail("Test failed as creation succeded.");
            //connection will be automatically closed
        }
        catch (AMQAuthenticationException amqe)
        {
            assertEquals("Incorrect error code thrown", 403, amqe.getErrorCode().getCode());
        
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    /**
     * This test uses both the cilent and sender to validate that the Server is able to publish to a temporary queue.
     * The reason the client must be in volved is that the Serve is unable to create its own Temporary Queues.
     *
     * @throws AMQException
     * @throws URLSyntaxException
     * @throws JMSException
     */
    public void testServerPublishUsingTransactionSuccess() throws AMQException, URLSyntaxException, JMSException, NamingException, Exception
    {
    	setUpACLTest();
    	
        //Set up the Server
        Connection serverConnection = getConnection("server", "guest");

        ((AMQConnection) serverConnection).setConnectionListener(this);

        Session serverSession = serverConnection.createSession(true, Session.SESSION_TRANSACTED);

        Queue requestQueue = serverSession.createQueue("example.RequestQueue");

        MessageConsumer server = serverSession.createConsumer(requestQueue);

        serverConnection.start();

        //Set up the consumer
        Connection clientConnection = getConnection("client", "guest");

        //Send a test mesage
        Session clientSession = clientConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Queue responseQueue = clientSession.createTemporaryQueue();

        MessageConsumer clientResponse = clientSession.createConsumer(responseQueue);

        clientConnection.start();

        Message request = clientSession.createTextMessage("Request");

        assertNotNull("Response Queue is null", responseQueue);

        request.setJMSReplyTo(responseQueue);

        clientSession.createProducer(requestQueue).send(request);

        try
        {
            Message msg = null;

            msg = server.receive(2000);

            while (msg != null && !((TextMessage) msg).getText().equals("Request"))
            {
                msg = server.receive(2000);
            }

            assertNotNull("Message not received", msg);

            assertNotNull("Reply-To is Null", msg.getJMSReplyTo());

            MessageProducer sender = serverSession.createProducer(msg.getJMSReplyTo());

            sender.send(serverSession.createTextMessage("Response"));

            //Send the message using a transaction as this will allow us to retrieve any errors that occur on the broker.
            serverSession.commit();

            //Ensure Response is received.
            Message clientResponseMsg = clientResponse.receive(2000);
            assertNotNull("Client did not receive response message,", clientResponseMsg);
            assertEquals("Incorrect message received", "Response", ((TextMessage) clientResponseMsg).getText());

        }
        catch (Exception e)
        {
            fail("Test publish failed:" + e);
        }
        finally
        {
            try
            {
                serverConnection.close();
            }
            finally
            {
                clientConnection.close();
            }
        }
    }

    public void testServerPublishInvalidQueueSuccess() throws AMQException, URLSyntaxException, JMSException, NamingException, Exception
    {
    	setUpACLTest();
    	
        //QPID-2081: use a latch to sync on exception causing connection close, to work 
        //around the connection close race during tearDown() causing sporadic failures
    	final CountDownLatch exceptionReceived = new CountDownLatch(1);
    	
        try
        {
            Connection conn = getConnection("server", "guest");
            
            conn.setExceptionListener(new ExceptionListener()
            {
                public void onException(JMSException e)
                {
                    exceptionReceived.countDown();
                }
            });

            ((AMQConnection) conn).setConnectionListener(this);

            Session session = conn.createSession(false, Session.AUTO_ACKNOWLEDGE);

            conn.start();

            MessageProducer sender = ((AMQSession) session).createProducer(null);

            Queue queue = session.createQueue("Invalid");

            // Send a message that we will wait to be sent, this should give the broker time to close the connection
            // before we finish this test. Message is set !immed !mand as the queue is invalid so want to test ACLs not
            // queue existence.
            ((org.apache.qpid.jms.MessageProducer) sender).send(queue, session.createTextMessage("test"),
                                                                DeliveryMode.NON_PERSISTENT, 0, 0L, false, false, true);

            // Test the connection with a valid consumer
            // This may not work as the session may be closed before the queue or consumer creation can occur.
            // The correct JMSexception with linked error will only occur when the close method is recevied whilst in
            // the failover safe block
            session.createConsumer(session.createQueue("example.RequestQueue")).close();

            //Connection should now be closed and will throw the exception caused by the above send
            conn.close();

            fail("Close is not expected to succeed.");
        }
        catch (JMSException e)
        {
            Throwable cause = e.getLinkedException();

            if (cause == null)
            {
                e.printStackTrace(System.out);
                fail("JMS Exception did not have cause");
            }
            else if (!(cause instanceof AMQAuthenticationException))
            {
                cause.printStackTrace(System.out);
                assertEquals("Incorrect exception", IllegalStateException.class, cause.getClass());
                System.out.println("QPID-1204 : Session became closed and we got that error rather than the authentication error.");
            }
            else
            {
                assertEquals("Incorrect exception", AMQAuthenticationException.class, cause.getClass());
                assertEquals("Incorrect error code thrown", 403, ((AMQAuthenticationException) cause).getErrorCode().getCode());
            }
            
            //use the latch to ensure the control thread waits long enough for the exception thread 
            //to have done enough to mark the connection closed before teardown commences
            assertTrue("Timed out waiting for conneciton to report close",
            		exceptionReceived.await(2, TimeUnit.SECONDS));
        }
    }

    // Connection Listener Interface - Used here to block failover

    public void bytesSent(long count)
    {
    }

    public void bytesReceived(long count)
    {
    }

    public boolean preFailover(boolean redirect)
    {
        //Prevent failover.
        return false;
    }

    public boolean preResubscribe()
    {
        return false;
    }

    public void failoverComplete()
    {
    }
}
