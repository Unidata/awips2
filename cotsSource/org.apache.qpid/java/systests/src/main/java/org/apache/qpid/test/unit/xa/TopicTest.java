/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid.test.unit.xa;

import javax.jms.*;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;
import javax.transaction.xa.XAException;

import junit.framework.TestSuite;

import java.util.concurrent.atomic.AtomicBoolean;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 *
 */
public class TopicTest extends AbstractXATestCase
{
    /* this clas logger */
    private static final Logger _logger = LoggerFactory.getLogger(TopicTest.class);

    /**
     * the topic use by all the tests
     */
    private static Topic _topic = null;

    /**
     * the topic connection factory used by all tests
     */
    private static XATopicConnectionFactory _topicFactory = null;

    /**
     * standard topic connection
     */
    private static XATopicConnection _topicConnection = null;

    /**
     * standard topic session created from the standard connection
     */
    private static XATopicSession _session = null;

    private static TopicSession _nonXASession = null;

    /**
     * the topic name
     */
    private static final String TOPICNAME = "xaTopic";

    /**
     * Indicate that a listenere has failed
     */
    private static boolean _failure = false;

    /** -------------------------------------------------------------------------------------- **/
    /** ----------------------------- JUnit support  ----------------------------------------- **/
    /** -------------------------------------------------------------------------------------- **/

    /**
     * Gets the test suite tests
     *
     * @return the test suite tests
     */
    public static TestSuite getSuite()
    {
        return new TestSuite(TopicTest.class);
    }

    /**
     * Run the test suite.
     *
     * @param args Any command line arguments specified to this class.
     */
    public static void main(String args[])
    {
        junit.textui.TestRunner.run(getSuite());
    }

    public void tearDown() throws Exception
    {
        if (!isBroker08())
        {
            try
            {
                _topicConnection.stop();
                _topicConnection.close();
            }
            catch (Exception e)
            {
                fail("Exception thrown when cleaning standard connection: " + e.getStackTrace());
            }
        }
        super.tearDown();
    }

    /**
     * Initialize standard actors
     */
    public void init()
    {
        if (!isBroker08())
        {
            // lookup test queue
            try
            {
                _topic = (Topic) getInitialContext().lookup(TOPICNAME);
            }
            catch (Exception e)
            {
                fail("cannot lookup test topic " + e.getMessage());
            }
            // lookup connection factory
            try
            {
                _topicFactory = getConnectionFactory();
            }
            catch (Exception e)
            {
                fail("enable to lookup connection factory ");
            }
            // create standard connection
            try
            {
                _topicConnection = getNewTopicXAConnection();
            }
            catch (JMSException e)
            {
                fail("cannot create queue connection: " + e.getMessage());
            }
            // create standard session
            try
            {
                _session = _topicConnection.createXATopicSession();
            }
            catch (JMSException e)
            {
                fail("cannot create queue session: " + e.getMessage());
            }
            // create a standard session
            try
            {
                _nonXASession = _topicConnection.createTopicSession(true, Session.AUTO_ACKNOWLEDGE);
            }
            catch (JMSException e)
            {
                e.printStackTrace();  //To change body of catch statement use Options | File Templates.
            }
            init(_session, _topic);
        }
    }

    /** -------------------------------------------------------------------------------------- **/
    /** ----------------------------- Test Suite  -------------------------------------------- **/
    /** -------------------------------------------------------------------------------------- **/


    /**
     * Uses two transactions respectively with xid1 and xid2 that are use to send a message
     * within xid1 and xid2. xid2 is committed and xid1 is used to receive the message that was sent within xid2.
     * Xid is then committed and a standard transaction is used to receive the message that was sent within xid1.
     */
    public void testProducer()
    {
        if (!isBroker08())
        {
            _logger.debug("testProducer");
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            try
            {
                Session nonXASession = _nonXASession;
                MessageConsumer nonXAConsumer = nonXASession.createConsumer(_topic);
                _producer.setDeliveryMode(DeliveryMode.PERSISTENT);
                // start the xaResource for xid1
                try
                {
                    _logger.debug("starting tx branch xid1");
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                }
                catch (XAException e)
                {
                    e.printStackTrace();
                    fail("cannot start the transaction with xid1: " + e.getMessage());
                }
                try
                {
                    // start the connection
                    _topicConnection.start();
                    _logger.debug("produce a message with sequence number 1");
                    _message.setLongProperty(_sequenceNumberPropertyName, 1);
                    _producer.send(_message);
                }
                catch (JMSException e)
                {
                    fail(" cannot send persistent message: " + e.getMessage());
                }
                _logger.debug("suspend the transaction branch xid1");
                try
                {
                    _xaResource.end(xid1, XAResource.TMSUSPEND);
                }
                catch (XAException e)
                {
                    fail("Cannot end the transaction with xid1: " + e.getMessage());
                }
                _logger.debug("start the xaResource for xid2");
                try
                {
                    _xaResource.start(xid2, XAResource.TMNOFLAGS);
                }
                catch (XAException e)
                {
                    fail("cannot start the transaction with xid2: " + e.getMessage());
                }
                try
                {
                    _logger.debug("produce a message");
                    _message.setLongProperty(_sequenceNumberPropertyName, 2);
                    _producer.send(_message);
                }
                catch (JMSException e)
                {
                    fail(" cannot send second persistent message: " + e.getMessage());
                }
                _logger.debug("end xid2 and start xid1");
                try
                {
                    _xaResource.end(xid2, XAResource.TMSUCCESS);
                    _xaResource.start(xid1, XAResource.TMRESUME);
                }
                catch (XAException e)
                {
                    fail("Exception when ending and starting transactions: " + e.getMessage());
                }
                _logger.debug("two phases commit transaction with xid2");
                try
                {
                    int resPrepare = _xaResource.prepare(xid2);
                    if (resPrepare != XAResource.XA_OK)
                    {
                        fail("prepare returned: " + resPrepare);
                    }
                    _xaResource.commit(xid2, false);
                }
                catch (XAException e)
                {
                    fail("Exception thrown when preparing transaction with xid2: " + e.getMessage());
                }
                _logger.debug("receiving a message from topic test we expect it to be the second one");
                try
                {
                    TextMessage message = (TextMessage) _consumer.receive(1000);
                    if (message == null)
                    {
                        fail("did not receive second message as expected ");
                    }
                    else
                    {
                        if (message.getLongProperty(_sequenceNumberPropertyName) != 2)
                        {
                            fail("receive wrong message its sequence number is: " + message
                                    .getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                }
                catch (JMSException e)
                {
                    fail("Exception when receiving second message: " + e.getMessage());
                }
                _logger.debug("end and one phase commit the first transaction");
                try
                {
                    _xaResource.end(xid1, XAResource.TMSUCCESS);
                    _xaResource.commit(xid1, true);
                }
                catch (XAException e)
                {
                    fail("Exception thrown when commiting transaction with xid1");
                }
                _logger.debug("We should now be able to receive the first and second message");
                try
                {
                    TextMessage message1 = (TextMessage) nonXAConsumer.receive(1000);
                    if (message1 == null)
                    {
                        fail("did not receive first message as expected ");
                    }
                    else
                    {
                        if (message1.getLongProperty(_sequenceNumberPropertyName) != 2)
                        {
                            fail("receive wrong message its sequence number is: " + message1
                                    .getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    message1 = (TextMessage) nonXAConsumer.receive(1000);
                    if (message1 == null)
                    {
                        fail("did not receive first message as expected ");
                    }
                    else
                    {
                        if (message1.getLongProperty(_sequenceNumberPropertyName) != 1)
                        {
                            fail("receive wrong message its sequence number is: " + message1
                                    .getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _logger.debug("commit transacted session");
                    nonXASession.commit();
                    _logger.debug("Test that the topic is now empty");
                    message1 = (TextMessage) nonXAConsumer.receive(1000);
                    if (message1 != null)
                    {
                        fail("receive an unexpected message ");
                    }
                }
                catch (JMSException e)
                {
                    fail("Exception thrown when emptying the queue: " + e.getMessage());
                }
            }
            catch (JMSException e)
            {
                fail("cannot create standard consumer: " + e.getMessage());
            }
        }
    }


    /**
     * strategy: Produce a message within Tx1 and commit tx1.  consume this message within tx2 and abort tx2.
     * Consume the same message within tx3 and commit it. Check that no more message is available.
     */
    public void testDurSub()
    {
        if (!isBroker08())
        {
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            Xid xid3 = getNewXid();
            Xid xid4 = getNewXid();
            String durSubName = "xaSubDurable";
            try
            {
                TopicSubscriber xaDurSub = _session.createDurableSubscriber(_topic, durSubName);
                try
                {
                    _topicConnection.start();
                    _logger.debug("start xid1");
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                    // start the connection
                    _topicConnection.start();
                    _logger.debug("produce a message with sequence number 1");
                    _message.setLongProperty(_sequenceNumberPropertyName, 1);
                    _producer.send(_message);
                    _logger.debug("2 phases commit xid1");
                    _xaResource.end(xid1, XAResource.TMSUCCESS);
                    if (_xaResource.prepare(xid1) != XAResource.XA_OK)
                    {
                        fail("Problem when preparing tx1 ");
                    }
                    _xaResource.commit(xid1, false);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid1: " + e.getMessage());
                }
                try
                {
                    _logger.debug("start xid2");
                    _xaResource.start(xid2, XAResource.TMNOFLAGS);
                    _logger.debug("receive the previously produced message");
                    TextMessage message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received ");
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 1)
                    {
                        fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                    }
                    _logger.debug("rollback xid2");
                    boolean rollbackOnFailure = false;
                    try
                    {
                        _xaResource.end(xid2, XAResource.TMFAIL);
                    }
                    catch (XAException e)
                    {
                        if (e.errorCode != XAException.XA_RBROLLBACK)
                        {
                            fail("Exception when working with xid2: " + e.getMessage());
                        }
                        rollbackOnFailure = true;
                    }
                    if (!rollbackOnFailure)
                    {
                        if (_xaResource.prepare(xid2) != XAResource.XA_OK)
                        {
                            fail("Problem when preparing tx2 ");
                        }
                        _xaResource.rollback(xid2);
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid2: " + e.getMessage());
                }
                try
                {
                    _logger.debug("start xid3");
                    _xaResource.start(xid3, XAResource.TMNOFLAGS);
                    _logger.debug(" receive the previously aborted consumed message");
                    TextMessage message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received ");
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 1)
                    {
                        fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                    }
                    _logger.debug("commit xid3");
                    _xaResource.end(xid3, XAResource.TMSUCCESS);
                    if (_xaResource.prepare(xid3) != XAResource.XA_OK)
                    {
                        fail("Problem when preparing tx3 ");
                    }
                    _xaResource.commit(xid3, false);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid3: " + e.getMessage());
                }
                try
                {
                    _logger.debug("start xid4");
                    _xaResource.start(xid4, XAResource.TMNOFLAGS);
                    _logger.debug("check that topic is empty");
                    TextMessage message = (TextMessage) xaDurSub.receive(1000);
                    if (message != null)
                    {
                        fail("An unexpected message was received ");
                    }
                    _logger.debug("commit xid4");
                    _xaResource.end(xid4, XAResource.TMSUCCESS);
                    _xaResource.commit(xid4, true);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid4: " + e.getMessage());
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
                fail("problem when creating dur sub: " + e.getMessage());
            }
            finally
            {
                try
                {
                    _session.unsubscribe(durSubName);
                }
                catch (JMSException e)
                {
                    e.printStackTrace();
                    fail("problem when unsubscribing dur sub: " + e.getMessage());
                }
            }
        }
    }

    /**
     * strategy: create a XA durable subscriber dusSub, produce 7 messages with the standard session,
     * consume 2 messages respectively with tx1, tx2 and tx3
     * abort tx2, we now expect to receive messages 3 and 4 first! Receive 3 messages within tx1 i.e. 34 and 7!
     * commit tx3
     * abort tx1: we now expect that only messages 5 and 6 are definitly consumed!
     * start tx4 and consume messages 1 - 4   and 7
     * commit tx4
     * Now the topic should be empty!
     */
    public void testMultiMessagesDurSub()
    {
        if (!isBroker08())
        {
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            Xid xid3 = getNewXid();
            Xid xid4 = getNewXid();
            Xid xid6 = getNewXid();
            String durSubName = "xaSubDurable";
            TextMessage message;
            try
            {
                TopicSubscriber xaDurSub = _session.createDurableSubscriber(_topic, durSubName);
                try
                {
                    Session txSession = _nonXASession;
                    MessageProducer txProducer = txSession.createProducer(_topic);
                    _logger.debug("produce 10 persistent messages");
                    txProducer.setDeliveryMode(DeliveryMode.PERSISTENT);
                    _topicConnection.start();
                    for (int i = 1; i <= 7; i++)
                    {
                        _message.setLongProperty(_sequenceNumberPropertyName, i);
                        txProducer.send(_message);
                    }
                    // commit txSession
                    txSession.commit();
                }
                catch (JMSException e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when producing messages: " + e.getMessage());
                }

                try
                {
                    _logger.debug(" consume 2 messages respectively with tx1, tx2 and tx3");
                    //----- start xid1
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 1; i <= 2; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid1, XAResource.TMSUSPEND);
                    //----- start xid2
                    _xaResource.start(xid2, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 3; i <= 4; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid2, XAResource.TMSUSPEND);
                    //----- start xid3
                    _xaResource.start(xid3, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 5; i <= 6; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid3, XAResource.TMSUCCESS);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when consumming 6 first messages: " + e.getMessage());
                }
                try
                {
                    _logger.debug("abort tx2, we now expect to receive messages 3, 4 and 7");
                    _xaResource.start(xid2, XAResource.TMRESUME);
                    _xaResource.end(xid2, XAResource.TMSUCCESS);
                    _xaResource.prepare(xid2);
                    _xaResource.rollback(xid2);
                    // receive 3 message within tx1: 3, 4 and 7
                    _xaResource.start(xid1, XAResource.TMRESUME);
                    _logger.debug(" 3, 4 and 7");
                    for (int i = 1; i <= 3; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + 3);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) <= 2 || 5 == message
                                .getLongProperty(_sequenceNumberPropertyName) || message
                                .getLongProperty(_sequenceNumberPropertyName) == 6)
                        {
                            fail("wrong sequence number: " + message
                                    .getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when consumming message: 3, 4 and 7:  " + e.getMessage());
                }

                try
                {
                    _xaResource.end(xid1, XAResource.TMSUCCESS);
                    _logger.debug(" commit tx3");
                    _xaResource.commit(xid3, true);
                    _logger.debug("abort tx1");
                    _xaResource.prepare(xid1);
                    _xaResource.rollback(xid1);
                }
                catch (XAException e)
                {
                    e.printStackTrace();
                    fail("XAException thrown when committing tx3 or aborting tx1: " + e.getMessage());
                }

                try
                {
                    // consume messages 1 - 4  + 7
                    //----- start xid1
                    _xaResource.start(xid4, XAResource.TMNOFLAGS);
                    for (int i = 1; i <= 5; i++)
                    {

                        message = (TextMessage) xaDurSub.receive(1000);
                        _logger.debug(" received message: " + message.getLongProperty(_sequenceNumberPropertyName));
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) == 5 || message
                                .getLongProperty(_sequenceNumberPropertyName) == 6)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid4, XAResource.TMSUCCESS);
                    _xaResource.prepare(xid4);
                    _xaResource.commit(xid4, false);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown in last phase: " + e.getMessage());
                }
                // now the topic should be empty!!
                try
                {
                    // start xid6
                    _xaResource.start(xid6, XAResource.TMNOFLAGS);
                    // should now be empty
                    message = (TextMessage) xaDurSub.receive(1000);
                    if (message != null)
                    {
                        fail("An unexpected message was received " + message
                                .getLongProperty(_sequenceNumberPropertyName));
                    }
                    // commit xid6
                    _xaResource.end(xid6, XAResource.TMSUCCESS);
                    _xaResource.commit(xid6, true);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid6: " + e.getMessage());
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
                fail("problem when creating dur sub: " + e.getMessage());
            }
            finally
            {
                try
                {
                    _session.unsubscribe(durSubName);
                }
                catch (JMSException e)
                {
                    e.printStackTrace();
                    fail("problem when unsubscribing dur sub: " + e.getMessage());
                }
            }
        }
    }

    /**
     * strategy: create a XA durable subscriber dusSub, produce 10 messages with the standard session,
     * consume 2 messages respectively with tx1, tx2 and tx3
     * prepare xid2 and xid3
     * crash the server
     * Redo the job for xid1 that has been aborted by server crash
     * abort tx2, we now expect to receive messages 3 and 4 first! Receive 3 messages within tx1 i.e. 34 and 7!
     * commit tx3
     * abort tx1: we now expect that only messages 5 and 6 are definitly consumed!
     * start tx4 and consume messages 1 - 4
     * start tx5 and consume messages 7 - 10
     * abort tx4
     * consume messages 1-4 with tx5
     * commit tx5
     * Now the topic should be empty!
     */
    public void testMultiMessagesDurSubCrash()
    {
        if (!isBroker08())
        {
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            Xid xid3 = getNewXid();
            Xid xid4 = getNewXid();
            Xid xid5 = getNewXid();
            Xid xid6 = getNewXid();
            String durSubName = "xaSubDurable";
            TextMessage message;
            try
            {
                TopicSubscriber xaDurSub = _session.createDurableSubscriber(_topic, durSubName);
                try
                {
                    Session txSession = _nonXASession;
                    MessageProducer txProducer = txSession.createProducer(_topic);
                    // produce 10 persistent messages
                    txProducer.setDeliveryMode(DeliveryMode.PERSISTENT);
                    _topicConnection.start();
                    for (int i = 1; i <= 10; i++)
                    {
                        _message.setLongProperty(_sequenceNumberPropertyName, i);
                        txProducer.send(_message);
                    }
                    // commit txSession
                    txSession.commit();
                }
                catch (JMSException e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when producing messages: " + e.getMessage());
                }
                try
                {
                    // consume 2 messages respectively with tx1, tx2 and tx3
                    //----- start xid1
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 1; i <= 2; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid1, XAResource.TMSUCCESS);
                    //----- start xid2
                    _xaResource.start(xid2, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 3; i <= 4; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid2, XAResource.TMSUCCESS);
                    //----- start xid3
                    _xaResource.start(xid3, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 5; i <= 6; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid3, XAResource.TMSUCCESS);
                    // prepare tx2 and tx3

                    _xaResource.prepare(xid2);
                    _xaResource.prepare(xid3);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when consumming 6 first messages: " + e.getMessage());
                }
                /////// stop the broker now !!
                try
                {
                    restartBroker();
                    init();
                }
                catch (Exception e)
                {
                    fail("Exception when stopping and restarting the server");
                }
                // get the list of in doubt transactions
                try
                {
                    _topicConnection.start();
                    // reconnect to dursub!
                    xaDurSub = _session.createDurableSubscriber(_topic, durSubName);
                    Xid[] inDoubt = _xaResource.recover(XAResource.TMSTARTRSCAN);
                    if (inDoubt == null)
                    {
                        fail("the array of in doubt transactions should not be null ");
                    }
                    // At that point we expect only two indoubt transactions:
                    if (inDoubt.length != 2)
                    {
                        fail("in doubt transaction size is diffenrent than 2, there are " + inDoubt.length + "in doubt transactions");
                    }
                }
                catch (XAException e)
                {
                    e.printStackTrace();
                    fail("exception thrown when recovering transactions " + e.getMessage());
                }
                try
                {
                    // xid1 has been aborted redo the job!
                    // consume 2 messages with tx1
                    //----- start xid1
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                    // receive the 2 first messages
                    for (int i = 1; i <= 2; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid1, XAResource.TMSUSPEND);
                    // abort tx2, we now expect to receive messages 3 and 4 first!
                    _xaResource.rollback(xid2);

                    // receive 3 message within tx1: 3, 4 and 7
                    _xaResource.start(xid1, XAResource.TMRESUME);
                    // receive messages 3, 4 and 7
                    message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received! expected: " + 3);
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 3)
                    {
                        fail("wrong sequence number: " + message
                                .getLongProperty(_sequenceNumberPropertyName) + " 3 was expected");
                    }
                    message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received! expected: " + 4);
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 4)
                    {
                        fail("wrong sequence number: " + message
                                .getLongProperty(_sequenceNumberPropertyName) + " 4 was expected");
                    }
                    message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received! expected: " + 7);
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 7)
                    {
                        fail("wrong sequence number: " + message
                                .getLongProperty(_sequenceNumberPropertyName) + " 7 was expected");
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when consumming message: 3, 4 and 7:  " + e.getMessage());
                }

                try
                {
                    _xaResource.end(xid1, XAResource.TMSUSPEND);
                    // commit tx3
                    _xaResource.commit(xid3, false);
                    // abort tx1
                    _xaResource.prepare(xid1);
                    _xaResource.rollback(xid1);
                }
                catch (XAException e)
                {
                    e.printStackTrace();
                    fail("XAException thrown when committing tx3 or aborting tx1: " + e.getMessage());
                }

                try
                {
                    // consume messages 1 - 4
                    //----- start xid1
                    _xaResource.start(xid4, XAResource.TMNOFLAGS);
                    for (int i = 1; i <= 4; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid4, XAResource.TMSUSPEND);
                    // consume messages 8 - 10
                    _xaResource.start(xid5, XAResource.TMNOFLAGS);
                    for (int i = 7; i <= 10; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid5, XAResource.TMSUSPEND);
                    // abort tx4
                    _xaResource.prepare(xid4);
                    _xaResource.rollback(xid4);
                    // consume messages 1-4 with tx5
                    _xaResource.start(xid5, XAResource.TMRESUME);
                    for (int i = 1; i <= 4; i++)
                    {
                        message = (TextMessage) xaDurSub.receive(1000);
                        if (message == null)
                        {
                            fail("no message received! expected: " + i);
                        }
                        else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                        {
                            fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                        }
                    }
                    _xaResource.end(xid5, XAResource.TMSUSPEND);
                    // commit tx5
                    _xaResource.prepare(xid5);
                    _xaResource.commit(xid5, false);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown in last phase: " + e.getMessage());
                }
                // now the topic should be empty!!
                try
                {
                    // start xid6
                    _xaResource.start(xid6, XAResource.TMNOFLAGS);
                    // should now be empty
                    message = (TextMessage) xaDurSub.receive(1000);
                    if (message != null)
                    {
                        fail("An unexpected message was received " + message
                                .getLongProperty(_sequenceNumberPropertyName));
                    }
                    // commit xid6
                    _xaResource.end(xid6, XAResource.TMSUSPEND);
                    _xaResource.commit(xid6, true);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid6: " + e.getMessage());
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
                fail("problem when creating dur sub: " + e.getMessage());
            }
            finally
            {
                try
                {
                    _session.unsubscribe(durSubName);
                }
                catch (JMSException e)
                {
                    e.printStackTrace();
                    fail("problem when unsubscribing dur sub: " + e.getMessage());
                }
            }
        }
    }


    /**
     * strategy: Produce a message within Tx1 and commit tx1.  a durable subscriber then receives that message within tx2
     * that is then prepared.
     * Shutdown the server and get the list of in doubt transactions:
     * we expect tx2, Tx2 is aborted and the message consumed within tx3 that is committed we then check that the topic is empty.
     */
    public void testDurSubCrash()
    {
        if (!isBroker08())
        {
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            Xid xid3 = getNewXid();
            Xid xid4 = getNewXid();
            String durSubName = "xaSubDurable";
            try
            {
                TopicSubscriber xaDurSub = _session.createDurableSubscriber(_topic, durSubName);
                try
                {
                    _topicConnection.start();
                    //----- start xid1
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                    // start the connection
                    _topicConnection.start();
                    // produce a message with sequence number 1
                    _message.setLongProperty(_sequenceNumberPropertyName, 1);
                    _producer.send(_message);
                    // commit
                    _xaResource.end(xid1, XAResource.TMSUCCESS);
                    if (_xaResource.prepare(xid1) != XAResource.XA_OK)
                    {
                        fail("Problem when preparing tx1 ");
                    }
                    _xaResource.commit(xid1, false);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid1: " + e.getMessage());
                }
                try
                {
                    // start xid2
                    _xaResource.start(xid2, XAResource.TMNOFLAGS);
                    // receive the previously produced message
                    TextMessage message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received ");
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 1)
                    {
                        fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                    }
                    // prepare xid2
                    _xaResource.end(xid2, XAResource.TMSUCCESS);
                    if (_xaResource.prepare(xid2) != XAResource.XA_OK)
                    {
                        fail("Problem when preparing tx2 ");
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid2: " + e.getMessage());
                }

                /////// stop the server now !!
                try
                {
                    restartBroker();
                    init();
                }
                catch (Exception e)
                {
                    fail("Exception when stopping and restarting the server");
                }

                // get the list of in doubt transactions
                try
                {
                    _topicConnection.start();
                    // reconnect to dursub!
                    xaDurSub = _session.createDurableSubscriber(_topic, durSubName);
                    Xid[] inDoubt = _xaResource.recover(XAResource.TMSTARTRSCAN);
                    if (inDoubt == null)
                    {
                        fail("the array of in doubt transactions should not be null ");
                    }
                    // At that point we expect only two indoubt transactions:
                    if (inDoubt.length != 1)
                    {
                        fail("in doubt transaction size is diffenrent than 2, there are " + inDoubt.length + "in doubt transactions");
                    }

                    // commit them
                    for (Xid anInDoubt : inDoubt)
                    {
                        if (anInDoubt.equals(xid2))
                        {
                            System.out.println("aborting xid2 ");
                            try
                            {
                                _xaResource.rollback(anInDoubt);
                            }
                            catch (Exception e)
                            {
                                e.printStackTrace();
                                fail("exception when aborting xid2 ");
                            }
                        }
                        else
                        {
                            System.out.println("XID2 is not in doubt ");
                        }
                    }
                }
                catch (XAException e)
                {
                    e.printStackTrace();
                    fail("exception thrown when recovering transactions " + e.getMessage());
                }

                try
                {
                    // start xid3
                    _xaResource.start(xid3, XAResource.TMNOFLAGS);
                    // receive the previously produced message and aborted
                    TextMessage message = (TextMessage) xaDurSub.receive(1000);
                    if (message == null)
                    {
                        fail("no message received ");
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != 1)
                    {
                        fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                    }
                    // commit xid3
                    _xaResource.end(xid3, XAResource.TMSUCCESS);
                    if (_xaResource.prepare(xid3) != XAResource.XA_OK)
                    {
                        fail("Problem when preparing tx3 ");
                    }
                    _xaResource.commit(xid3, false);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid3: " + e.getMessage());
                }
                try
                {
                    // start xid4
                    _xaResource.start(xid4, XAResource.TMNOFLAGS);
                    // should now be empty
                    TextMessage message = (TextMessage) xaDurSub.receive(1000);
                    if (message != null)
                    {
                        fail("An unexpected message was received " + message
                                .getLongProperty(_sequenceNumberPropertyName));
                    }
                    // commit xid4
                    _xaResource.end(xid4, XAResource.TMSUCCESS);
                    _xaResource.commit(xid4, true);
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception when working with xid4: " + e.getMessage());
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
                fail("problem when creating dur sub: " + e.getMessage());
            }
            finally
            {
                try
                {
                    _session.unsubscribe(durSubName);
                }
                catch (JMSException e)
                {
                    e.printStackTrace();
                    fail("problem when unsubscribing dur sub: " + e.getMessage());
                }
            }
        }
    }

    /**
     * strategy: Produce a message within Tx1 and prepare tx1.  Shutdown the server and get the list of indoubt transactions:
     * we expect tx1, Tx1 is committed  so we expect the test topic not to be empty!
     */
    public void testRecover()
    {
        if (!isBroker08())
        {
            Xid xid1 = getNewXid();
            String durSubName = "test1";
            try
            {
                // create a dummy durable subscriber to be sure that messages are persisted!
                _nonXASession.createDurableSubscriber(_topic, durSubName);
                // start the xaResource for xid1
                try
                {
                    _xaResource.start(xid1, XAResource.TMNOFLAGS);
                }
                catch (XAException e)
                {
                    fail("cannot start the transaction with xid1: " + e.getMessage());
                }
                try
                {
                    // start the connection
                    _topicConnection.start();
                    // produce a message with sequence number 1
                    _message.setLongProperty(_sequenceNumberPropertyName, 1);
                    _producer.send(_message);
                }
                catch (JMSException e)
                {
                    fail(" cannot send persistent message: " + e.getMessage());
                }
                // suspend the transaction
                try
                {
                    _xaResource.end(xid1, XAResource.TMSUCCESS);
                }
                catch (XAException e)
                {
                    fail("Cannot end the transaction with xid1: " + e.getMessage());
                }
                // prepare the transaction with xid1
                try
                {
                    _xaResource.prepare(xid1);
                }
                catch (XAException e)
                {
                    fail("Exception when preparing xid1: " + e.getMessage());
                }

                /////// stop the server now !!
                try
                {
                    restartBroker();
                    init();
                }
                catch (Exception e)
                {
                    fail("Exception when stopping and restarting the server");
                }

                try
                {
                    MessageConsumer nonXAConsumer =  _nonXASession.createDurableSubscriber(_topic, durSubName);
                    _topicConnection.start();
                    // get the list of in doubt transactions
                    try
                    {
                        Xid[] inDoubt = _xaResource.recover(XAResource.TMSTARTRSCAN);
                        if (inDoubt == null)
                        {
                            fail("the array of in doubt transactions should not be null ");
                        }
                        // At that point we expect only two indoubt transactions:
                        if (inDoubt.length != 1)
                        {
                            fail("in doubt transaction size is diffenrent thatn 2, there are " + inDoubt.length + "in doubt transactions");
                        }
                        // commit them
                        for (Xid anInDoubt : inDoubt)
                        {
                            if (anInDoubt.equals(xid1))
                            {
                                _logger.debug("committing xid1 ");
                                try
                                {
                                    _xaResource.commit(anInDoubt, false);
                                }
                                catch (Exception e)
                                {
                                    _logger.debug("PB when aborted xid1");
                                    e.printStackTrace();
                                    fail("exception when committing xid1 ");
                                }
                            }
                            else
                            {
                                _logger.debug("XID1 is not in doubt ");
                            }
                        }
                    }
                    catch (XAException e)
                    {
                        e.printStackTrace();
                        fail("exception thrown when recovering transactions " + e.getMessage());
                    }
                    _logger.debug("the topic should not be empty");
                    TextMessage message1 = (TextMessage) nonXAConsumer.receive(1000);
                    if (message1 == null)
                    {
                        fail("The topic is empty! ");
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    fail("Exception thrown when testin that queue test is empty: " + e.getMessage());
                }
            }
            catch (JMSException e)
            {
                e.printStackTrace();
                fail("cannot create dummy durable subscriber: " + e.getMessage());
            }
            finally
            {
                try
                {
                    // unsubscribe the dummy durable subscriber
                    TopicSession nonXASession = _nonXASession;
                    nonXASession.unsubscribe(durSubName);
                }
                catch (JMSException e)
                {
                    fail("cannot unsubscribe durable subscriber: " + e.getMessage());
                }
            }
        }
    }

    /**
     * strategy:
     * create a standard durable subscriber
     * produce 3 messages
     * consume the first message with that durable subscriber
     * close the standard session that deactivates the durable subscriber
     * migrate the durable subscriber to an xa one
     * consume the second message with that xa durable subscriber
     * close the xa session that deactivates the durable subscriber
     * reconnect to the durable subscriber with a standard session
     * consume the two remaining messages and check that the topic is empty!
     */
    public void testMigrateDurableSubscriber()
    {
        if (!isBroker08())
        {
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            String durSubName = "DurableSubscriberMigrate";
            try
            {
                Session stSession = _nonXASession;
                MessageProducer producer = stSession.createProducer(_topic);
                _logger.debug("Create a standard durable subscriber!");
                TopicSubscriber durSub = stSession.createDurableSubscriber(_topic, durSubName);
                TopicSubscriber durSub1 = stSession.createDurableSubscriber(_topic, durSubName + "_second");
                TextMessage message;
                producer.setDeliveryMode(DeliveryMode.PERSISTENT);
                _topicConnection.start();
                _logger.debug("produce 3 messages");
                for (int i = 1; i <= 3; i++)
                {
                    _message.setLongProperty(_sequenceNumberPropertyName, i);
                    //producer.send( _message );
                    producer.send(_message, DeliveryMode.PERSISTENT, 9 - i, 0);
                    stSession.commit();
                }
                _logger.debug("consume the first message with that durable subscriber");
                message = (TextMessage) durSub.receive(1000);
                if (message == null)
                {
                    fail("no message received ");
                }
                else if (message.getLongProperty(_sequenceNumberPropertyName) != 1)
                {
                    fail("wrong sequence number: " + message.getLongProperty(_sequenceNumberPropertyName));
                }
                // commit the standard session
                stSession.commit();
                _logger.debug("first message consumed ");
                // close the session that deactivates the durable subscriber
                stSession.close();
                _logger.debug("migrate the durable subscriber to an xa one");
                _xaResource.start(xid1, XAResource.TMNOFLAGS);
                durSub = _session.createDurableSubscriber(_topic, durSubName);
                _logger.debug(" consume the second message with that xa durable subscriber and abort it");
                message = (TextMessage) durSub.receive(1000);
                if (message == null)
                {
                    fail("no message received ");
                }
                else if (message.getLongProperty(_sequenceNumberPropertyName) != 2)
                {
                    System.out.println("wrong sequence number, 2 expected, received: " + message
                            .getLongProperty(_sequenceNumberPropertyName));
                }
                _xaResource.end(xid1, XAResource.TMSUCCESS);
                _xaResource.prepare(xid1);
                _xaResource.rollback(xid1);
                _logger.debug("close the session that deactivates the durable subscriber");
                _session.close();
                _logger.debug("create a new standard session");
                stSession = _topicConnection.createTopicSession(true, 1);
                _logger.debug("reconnect to the durable subscriber");
                durSub = stSession.createDurableSubscriber(_topic, durSubName);
                durSub1 = stSession.createDurableSubscriber(_topic, durSubName + "_second");
                _logger.debug("Reconnected to durablse subscribers");
                _logger.debug(" consume the 2 remaining messages");
                message = (TextMessage) durSub.receive(1000);
                if (message == null)
                {
                    fail("no message received ");
                }
                else if (message.getLongProperty(_sequenceNumberPropertyName) != 2)
                {
                    System.out.println("wrong sequence number, 2 expected, received: " + message
                            .getLongProperty(_sequenceNumberPropertyName));
                }
                // consume the third message with that xa durable subscriber
                message = (TextMessage) durSub.receive(1000);
                if (message == null)
                {
                    fail("no message received ");
                }
                else if (message.getLongProperty(_sequenceNumberPropertyName) != 3)
                {
                     System.out.println("wrong sequence number, 3 expected, received: " + message
                            .getLongProperty(_sequenceNumberPropertyName));
                }
                stSession.commit();
                _logger.debug("the topic should be empty now");
                message = (TextMessage) durSub.receive(1000);
                if (message != null)
                {
                    fail("Received unexpected message ");
                }
                stSession.commit();
                _logger.debug(" use dursub1 to receive all the 3 messages");
                for (int i = 1; i <= 3; i++)
                {
                    message = (TextMessage) durSub1.receive(1000);
                    if (message == null)
                    {
                        _logger.debug("no message received ");
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                    {
                        fail("wrong sequence number, " + i + " expected, received: " + message
                                .getLongProperty(_sequenceNumberPropertyName));
                    }
                }
                stSession.commit();
                // send a non persistent message to check that all persistent messages are deleted
                producer = stSession.createProducer(_topic);
                producer.setDeliveryMode(DeliveryMode.NON_PERSISTENT);
                producer.send(_message);
                stSession.commit();
                message = (TextMessage) durSub.receive(1000);
                if (message == null)
                {
                    fail("message not received ");
                }
                message = (TextMessage) durSub1.receive(1000);
                if (message == null)
                {
                    fail("message not received ");
                }
                stSession.commit();
                stSession.close();
                _logger.debug(" now create a standard non transacted session and reconnect to the durable xubscriber");
                TopicConnection stConnection =
                        _topicConnection; //_topicFactory.createTopicConnection("guest", "guest");
                TopicSession autoAclSession = stConnection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
                TopicPublisher publisher = autoAclSession.createPublisher(_topic);
                durSub = autoAclSession.createDurableSubscriber(_topic, durSubName);
                stConnection.start();
                // produce 3 persistent messages
                for (int i = 1; i <= 3; i++)
                {
                    _message.setLongProperty(_sequenceNumberPropertyName, i);
                    //producer.send( _message );
                    publisher.send(_message, DeliveryMode.PERSISTENT, 9 - i, 0);
                }
                _logger.debug(" use dursub to receive all the 3 messages");
                for (int i = 1; i <= 3; i++)
                {
                    message = (TextMessage) durSub.receive(1000);
                    if (message == null)
                    {
                        System.out.println("no message received ");
                    }
                    else if (message.getLongProperty(_sequenceNumberPropertyName) != i)
                    {
                        System.out.println("wrong sequence number, " + i + " expected, received: " + message
                                .getLongProperty(_sequenceNumberPropertyName));
                    }
                }

                _logger.debug("now set a message listener");
                AtomicBoolean lock = new AtomicBoolean(true);
                reset();
                stConnection.stop();
                durSub.setMessageListener(new TopicListener(1, 3, lock));
                _logger.debug(" produce 3 persistent messages");
                for (int i = 1; i <= 3; i++)
                {
                    _message.setLongProperty(_sequenceNumberPropertyName, i);
                    //producer.send( _message );
                    publisher.send(_message, DeliveryMode.PERSISTENT, 9 - i, 0);
                }
                // start the connection
                stConnection.start();
                while (lock.get())
                {
                    synchronized (lock)
                    {
                        lock.wait();
                    }
                }
                if (getFailureStatus())
                {
                    fail("problem with message listener");
                }
                stConnection.stop();
                durSub.setMessageListener(null);
                _logger.debug(" do the same with an xa session");
                // produce 3 persistent messages
                for (int i = 1; i <= 3; i++)
                {
                    _message.setLongProperty(_sequenceNumberPropertyName, i);
                    //producer.send( _message );
                    publisher.send(_message, DeliveryMode.PERSISTENT, 9 - i, 0);
                }
                //stConnection.close();
                autoAclSession.close();
                _logger.debug(" migrate the durable subscriber to an xa one");
                _session = _topicConnection.createXATopicSession();
                _xaResource = _session.getXAResource();
                _xaResource.start(xid2, XAResource.TMNOFLAGS);
                durSub = _session.createDurableSubscriber(_topic, durSubName);
                lock = new AtomicBoolean();
                reset();
                _topicConnection.stop();
                durSub.setMessageListener(new TopicListener(1, 3, lock));
                // start the connection
                _topicConnection.start();
                while (lock.get())
                {
                    synchronized (lock)
                    {
                        lock.wait();
                    }
                }
                if (getFailureStatus())
                {
                    fail("problem with XA message listener");
                }
                _xaResource.end(xid2, XAResource.TMSUCCESS);
                _xaResource.commit(xid2, true);
            }
            catch (Exception e)
            {
                e.printStackTrace();
                fail("Exception thrown: " + e.getMessage());
            }
            finally
            {
                try
                {
                    _topicConnection.createXASession().unsubscribe(durSubName);
                    _topicConnection.createXASession().unsubscribe(durSubName + "_second");
                }
                catch (JMSException e)
                {
                    fail("Exception thrown when unsubscribing durable subscriber  " + e.getMessage());
                }
            }
        }
    }

    /** -------------------------------------------------------------------------------------- **/
    /** ----------------------------- Utility methods  --------------------------------------- **/
    /** -------------------------------------------------------------------------------------- **/

    /**
     * get a new queue connection
     *
     * @return a new queue connection
     * @throws javax.jms.JMSException If the JMS provider fails to create the queue connection
     *                                due to some internal error or in case of authentication failure
     */
    private XATopicConnection getNewTopicXAConnection() throws JMSException
    {
        return _topicFactory.createXATopicConnection("guest", "guest");
    }

    public static void failure()
    {
        _failure = true;
    }

    public static void reset()
    {
        _failure = false;
    }

    public static boolean getFailureStatus()
    {
        return _failure;
    }

    private class TopicListener implements MessageListener
    {
        private long _counter;
        private long _end;
        private final AtomicBoolean _lock;

        public TopicListener(long init, long end, AtomicBoolean lock)
        {
            _counter = init;
            _end = end;
            _lock = lock;
        }

        public void onMessage(Message message)
        {
            long seq = 0;
            try
            {
                seq = message.getLongProperty(TopicTest._sequenceNumberPropertyName);
            }
            catch (JMSException e)
            {
                e.printStackTrace();
                TopicTest.failure();
                _lock.set(false);
                synchronized (_lock)
                {
                    _lock.notifyAll();
                }
            }
            if (seq != _counter)
            {
                System.out.println("received message " + seq + " expected " + _counter);
                TopicTest.failure();
                _lock.set(false);
                synchronized (_lock)
                {
                    _lock.notifyAll();
                }
            }
            _counter++;
            if (_counter > _end)
            {
                _lock.set(false);
                synchronized (_lock)
                {
                    _lock.notifyAll();
                }
            }
        }
    }

}
