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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class QueueTest extends AbstractXATestCase
{
    /* this clas logger */
    private static final Logger _logger = LoggerFactory.getLogger(QueueTest.class);

    /**
     * the queue use by all the tests
     */
    private static Queue _queue = null;
    /**
     * the queue connection factory used by all tests
     */
    private static XAQueueConnectionFactory _queueFactory = null;

    /**
     * standard xa queue connection
     */
    private static XAQueueConnection _xaqueueConnection= null;

    /**
     * standard xa queue connection
     */
    private static QueueConnection _queueConnection=null;


    /**
     * standard queue session created from the standard connection
     */
    private static QueueSession _nonXASession = null;

    /**
     * the queue name
     */
    private static final String QUEUENAME = "xaQueue";

    /** ----------------------------------------------------------------------------------- **/
    /**
     * ----------------------------- JUnit support  ----------------------------------------- *
     */

    /**
     * Gets the test suite tests
     *
     * @return the test suite tests
     */
    public static TestSuite getSuite()
    {
        return new TestSuite(QueueTest.class);
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
                _xaqueueConnection.close();
                _queueConnection.close();
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
                _queue = (Queue) getInitialContext().lookup(QUEUENAME);
            }
            catch (Exception e)
            {
                fail("cannot lookup test queue " + e.getMessage());
            }

            // lookup connection factory
            try
            {
                _queueFactory = getConnectionFactory();
            }
            catch (Exception e)
            {
                fail("enable to lookup connection factory ");
            }
            // create standard connection
            try
            {
                _xaqueueConnection= getNewQueueXAConnection();
            }
            catch (JMSException e)
            {
                fail("cannot create queue connection: " + e.getMessage());
            }
            // create xa session
            XAQueueSession session = null;
            try
            {
                session = _xaqueueConnection.createXAQueueSession();
            }
            catch (JMSException e)
            {
                fail("cannot create queue session: " + e.getMessage());
            }
            // create a standard session
            try
            {
                _queueConnection = _queueFactory.createQueueConnection();
                _nonXASession = _queueConnection.createQueueSession(true, Session.AUTO_ACKNOWLEDGE);
            }
            catch (JMSException e)
            {
                fail("cannot create queue session: " + e.getMessage());
            }
            init(session, _queue);
        }
    }

    /** -------------------------------------------------------------------------------------- **/
    /** ----------------------------- Test Suite  -------------------------------------------- **/
    /** -------------------------------------------------------------------------------------- **/

    /**
     * Uses two transactions respectively with xid1 and xid2 that are used to send a message
     * within xid1 and xid2.  xid2 is committed and xid1 is used to receive the message that was sent within xid2.
     * Xid is then committed and a standard transaction is used to receive the message that was sent within xid1.
     */
    public void testProducer()
    {
        if (!isBroker08())
        {
            _logger.debug("running testProducer");
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
            // start the xaResource for xid1
            try
            {
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
                _xaqueueConnection.start();
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
                _xaResource.end(xid1, XAResource.TMSUSPEND);
            }
            catch (XAException e)
            {
                fail("Cannot end the transaction with xid1: " + e.getMessage());
            }
            // start the xaResource for xid2
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
                // produce a message
                _message.setLongProperty(_sequenceNumberPropertyName, 2);
                _producer.send(_message);
            }
            catch (JMSException e)
            {
                fail(" cannot send second persistent message: " + e.getMessage());
            }
            // end xid2 and start xid1
            try
            {
                _xaResource.end(xid2, XAResource.TMSUCCESS);
                _xaResource.start(xid1, XAResource.TMRESUME);
            }
            catch (XAException e)
            {
                fail("Exception when ending and starting transactions: " + e.getMessage());
            }
            // two phases commit transaction with xid2
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
            // receive a message from queue test we expect it to be the second one
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
            // end and one phase commit the first transaction
            try
            {
                _xaResource.end(xid1, XAResource.TMSUCCESS);
                _xaResource.commit(xid1, true);
            }
            catch (XAException e)
            {
                fail("Exception thrown when commiting transaction with xid1");
            }
            // We should now be able to receive the first message
            try
            {
                _xaqueueConnection.close();
                Session nonXASession = _nonXASession;
                MessageConsumer nonXAConsumer = nonXASession.createConsumer(_queue);
                _queueConnection.start();
                TextMessage message1 = (TextMessage) nonXAConsumer.receive(1000);
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
                // commit that transacted session
                nonXASession.commit();
                // the queue should be now empty
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
    }

    /**
     * strategy: Produce a message within Tx1 and prepare tx1. crash the server then commit tx1 and consume the message
     */
    public void testSendAndRecover()
    {
        if (!isBroker08())
        {
            _logger.debug("running testSendAndRecover");
            Xid xid1 = getNewXid();
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
                _xaqueueConnection.start();
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
                _logger.debug("stopping broker");
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
                        System.out.println("commit xid1 ");
                        try
                        {
                            _xaResource.commit(anInDoubt, false);
                        }
                        catch (Exception e)
                        {
                            System.out.println("PB when aborted xid1");
                        }
                    }
                    else
                    {
                        fail("did not receive right xid ");
                    }
                }
            }
            catch (XAException e)
            {
                e.printStackTrace();
                fail("exception thrown when recovering transactions " + e.getMessage());
            }
            // the queue should contain the first message!
            try
            {
                _xaqueueConnection.close();
                Session nonXASession = _nonXASession;
                MessageConsumer nonXAConsumer = nonXASession.createConsumer(_queue);
                _queueConnection.start();
                TextMessage message1 = (TextMessage) nonXAConsumer.receive(1000);

                if (message1 == null)
                {
                    fail("queue does not contain any message!");
                }
                if (message1.getLongProperty(_sequenceNumberPropertyName) != 1)
                {
                    fail("Wrong message returned! Sequence number is " + message1
                            .getLongProperty(_sequenceNumberPropertyName));
                }
                nonXASession.commit();
            }
            catch (JMSException e)
            {
                fail("Exception thrown when testin that queue test is not empty: " + e.getMessage());
            }
        }
    }

    /**
     * strategy: Produce a message within Tx1 and prepare tx1. Produce a standard message and consume
     * it within tx2 and prepare tx2. Shutdown the server and get the list of in doubt transactions:
     * we expect tx1 and tx2! Then, Tx1 is aborted and tx2 is committed so we expect the test's queue to be empty!
     */
    public void testRecover()
    {
        if (!isBroker08())
        {
            _logger.debug("running testRecover");
            Xid xid1 = getNewXid();
            Xid xid2 = getNewXid();
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
                _xaqueueConnection.start();
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

            // send a message using the standard session
            try
            {
                Session nonXASession = _nonXASession;
                MessageProducer nonXAProducer = nonXASession.createProducer(_queue);
                TextMessage message2 = nonXASession.createTextMessage();
                message2.setText("non XA ");
                message2.setLongProperty(_sequenceNumberPropertyName, 2);
                nonXAProducer.setDeliveryMode(DeliveryMode.PERSISTENT);
                nonXAProducer.send(message2);
                // commit that transacted session
                nonXASession.commit();
            }
            catch (Exception e)
            {
                fail("Exception thrown when emptying the queue: " + e.getMessage());
            }
            // start the xaResource for xid2
            try
            {
                _xaResource.start(xid2, XAResource.TMNOFLAGS);
            }
            catch (XAException e)
            {
                fail("cannot start the transaction with xid1: " + e.getMessage());
            }
            // receive a message from queue test we expect it to be the second one
            try
            {
                TextMessage message = (TextMessage) _consumer.receive(1000);
                if (message == null || message.getLongProperty(_sequenceNumberPropertyName) != 2)
                {
                    fail("did not receive second message as expected ");
                }
            }
            catch (JMSException e)
            {
                fail("Exception when receiving second message: " + e.getMessage());
            }
            // suspend the transaction
            try
            {
                _xaResource.end(xid2, XAResource.TMSUCCESS);
            }
            catch (XAException e)
            {
                fail("Cannot end the transaction with xid2: " + e.getMessage());
            }
            // prepare the transaction with xid1
            try
            {
                _xaResource.prepare(xid2);
            }
            catch (XAException e)
            {
                fail("Exception when preparing xid2: " + e.getMessage());
            }

            /////// stop the server now !!
            try
            {
                _logger.debug("stopping broker");
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
                Xid[] inDoubt = _xaResource.recover(XAResource.TMSTARTRSCAN);
                if (inDoubt == null)
                {
                    fail("the array of in doubt transactions should not be null ");
                }
                // At that point we expect only two indoubt transactions:
                if (inDoubt.length != 2)
                {
                    fail("in doubt transaction size is diffenrent thatn 2, there are " + inDoubt.length + "in doubt transactions");
                }

                // commit them
                for (Xid anInDoubt : inDoubt)
                {
                    if (anInDoubt.equals(xid1))
                    {
                         _logger.debug("rollback xid1 ");
                        try
                        {
                            _xaResource.rollback(anInDoubt);
                        }
                        catch (Exception e)
                        {
                            System.out.println("PB when aborted xid1");
                        }
                    }
                    else if (anInDoubt.equals(xid2))
                    {
                        _logger.debug("commit xid2 ");
                        try
                        {
                            _xaResource.commit(anInDoubt, false);
                        }
                        catch (Exception e)
                        {
                            System.out.println("PB when commiting xid2");
                        }
                    }
                }
            }
            catch (XAException e)
            {
                e.printStackTrace();
                fail("exception thrown when recovering transactions " + e.getMessage());
            }
            // the queue should be empty
            try
            {
                _xaqueueConnection.close();
                Session nonXASession = _nonXASession;
                MessageConsumer nonXAConsumer = nonXASession.createConsumer(_queue);
                _queueConnection.start();
                TextMessage message1 = (TextMessage) nonXAConsumer.receive(1000);
                if (message1 != null)
                {
                    fail("The queue is not empty! ");
                }
            }
            catch (JMSException e)
            {
                fail("Exception thrown when testin that queue test is empty: " + e.getMessage());
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
     * @throws JMSException If the JMS provider fails to create the queue connection
     *                      due to some internal error or in case of authentication failure
     */
    private XAQueueConnection getNewQueueXAConnection() throws JMSException
    {
        return _queueFactory.createXAQueueConnection("guest", "guest");
    }


}
