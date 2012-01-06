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
using System;
using System.Runtime.InteropServices;
using System.Threading;
using log4net;
using NUnit.Framework;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Integration.Tests.interactive
{
    [TestFixture, Category("Interactive")]
    public class FailoverTest : IConnectionListener
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(FailoverTest));

        /// <summary>Specifies the number of times to run the test cycle.</summary>
        const int NUM_MESSAGES = 10;

        /// <summary>Determines how many messages to send within each commit.</summary>
        const int COMMIT_BATCH_SIZE = 1;

        /// <summary>Specifies the duration of the pause to place between each message sent in the test.</summary>
        //const int SLEEP_MILLIS = 1;

        /// <summary>Specified the maximum time in milliseconds to wait for the test to complete.</summary>
        const int TIMEOUT = 10000;

        /// <summary>Defines the number of test messages to send, before prompting the user to fail a broker.</summary>
        const int FAIL_POINT = 5;

        /// <summary>Specified the ack mode to use for the test.</summary>
        AcknowledgeMode _acknowledgeMode = AcknowledgeMode.AutoAcknowledge;

        /// <summary>Determines whether this test runs transactionally or not. </summary>
        bool transacted = false;

        /// <summary>Holds the connection to run the test over.</summary>
        AMQConnection _connection;

        /// <summary>Holds the channel for the test message publisher. </summary>
        IChannel publishingChannel;

        /// <summary>Holds the test message publisher. </summary>
        IMessagePublisher publisher;

        /// <summary>Used to keep count of the number of messages sent. </summary>
        int messagesSent;

        /// <summary>Used to keep count of the number of messages received. </summary>
        int messagesReceived;

        /// <summary>Used to wait for test completion on. </summary>
        private static object testComplete = new Object();

        /// <summary>Used to wait for failover completion on. </summary>
	private static object failoverComplete = new Object();
	
        bool failedOver=false;

        /// <summary>Used to record the extra message count (1) if the message sent right after failover actually made it to the new broker.</summary>
        int _extraMessage = 0;
	
        /// <summary>
        /// Creates the test connection with a fail-over set up, and a producer/consumer pair on that connection.
        /// </summary>
        /// [SetUp]
        public void Init(IConnectionInfo connectionInfo)
        {
	    //log4net.Config.BasicConfigurator.Configure();
            // Reset all counts.
            messagesSent = 0;
            messagesReceived = 0;
            failedOver=false;
            _extraMessage = 0;

	    PromptAndWait("Ensure both brokers are running, then press Enter");	    
	    
            // Create a connection for the test.
            _connection = new AMQConnection(connectionInfo);
            _connection.ConnectionListener = this;

            // Create a consumer to receive the test messages.
            IChannel receivingChannel = _connection.CreateChannel(false, _acknowledgeMode);

            string queueName = receivingChannel.GenerateUniqueName();
            receivingChannel.DeclareQueue(queueName, false, true, true);
            receivingChannel.Bind(queueName, "amq.direct", queueName);

            IMessageConsumer consumer = receivingChannel.CreateConsumerBuilder(queueName)
                .WithPrefetchLow(30)
                .WithPrefetchHigh(60).Create();

            consumer.OnMessage = new MessageReceivedDelegate(OnMessage);
            _connection.Start();

            // Create a publisher to send the test messages.
            publishingChannel = _connection.CreateChannel(transacted, AcknowledgeMode.NoAcknowledge);
            publisher = publishingChannel.CreatePublisherBuilder()
                .WithRoutingKey(queueName)
                .Create();

            _log.Debug("connection = " + _connection);
            _log.Debug("connectionInfo = " + connectionInfo);
            _log.Debug("connection.AsUrl = " + _connection.toURL());
            _log.Debug("AcknowledgeMode is " + _acknowledgeMode);
        }

        /// <summary>
        /// Clean up the test connection.
        /// </summary>
        [TearDown]
        public virtual void Shutdown()
        {
 	    if (!failedOver)
	    {
                 Assert.Fail("The failover callback never occured.");
            }

            Console.WriteLine("Test done shutting down");
	    Thread.Sleep(2000);
            _connection.Close();
        }

        /// <summary>
        /// Runs a failover test, building up the connection information from its component parts. In particular the brokers
        /// to fail between are seperately added into the connection info.
        /// </summary>
        /*[Test]
        public void TestWithBasicInfo()
        {
            _log.Debug("public void TestWithBasicInfo(): called");

            // Manually create the connection parameters.
            QpidConnectionInfo connectionInfo = new QpidConnectionInfo();
            connectionInfo.AddBrokerInfo(new AmqBrokerInfo("amqp", "localhost", 5672, false));
            connectionInfo.AddBrokerInfo(new AmqBrokerInfo("amqp", "localhost", 5673, false));

            Init(connectionInfo);
            DoFailoverTest();
        }*/

        /// <summary>
        /// Runs a failover test, with the failover configuration specified in the Qpid connection URL format.
        /// </summary>
        [Test]
        public void TestWithUrl()
        {
            _log.Debug("public void runTestWithUrl(): called");

            // Parse the connection parameters from a URL.
            String clientId = "failover" + DateTime.Now.Ticks;
            string defaultUrl = "amqp://guest:guest@" + clientId + "/test" +
                "?brokerlist='tcp://localhost:9672;tcp://localhost:9673'&failover='roundrobin'";            
            IConnectionInfo connectionInfo = QpidConnectionInfo.FromUrl(defaultUrl);
            
            Init(connectionInfo);
            DoFailoverTest(0);
        }

        /// <summary>
        /// Send the test messages, prompting at the fail point for the user to cause a broker failure. The test checks that all messages sent
        /// are received within the test time limit.
        /// </summary>
        ///
        /// <param name="connectionInfo">The connection parameters, specifying the brokers to fail between.</param>
        void DoFailoverTest(int delay)
        {
            _log.Debug("void DoFailoverTest(IConnectionInfo connectionInfo): called");

            // Wait for all of the test messages to be received, checking that this occurs within the test time limit.
	    bool withinTimeout = false;

            for (int i = 1; i <= NUM_MESSAGES; ++i)
            {
		SendMessage();

		// Prompt the user to cause a failure if at the fail point.
		if (i == FAIL_POINT)
		{
		    for( int min = delay ; min > 0 ; min--)
		    {
		       Console.WriteLine("Waiting for "+min+" minutes to test connection time bug.");
		       Thread.Sleep(60*1000);
		    }

		    PromptAndWait("Cause a broker failure now, then press return.");
		    Console.WriteLine("NOTE: ensure that the delay between killing the broker and continuing here is less than 20 second");
		    
		    Console.WriteLine("Sending a message to ensure send right after works");

		    SendMessage();

		    Console.WriteLine("Waiting for fail-over to complete before continuing...");


		    lock(failoverComplete)
		    {
			if (!failedOver)
			{
			    withinTimeout = Monitor.Wait(failoverComplete, TIMEOUT);
			}
			else
			{
			    withinTimeout=true;
			}
		    }

		    if (!withinTimeout)
		    {
			PromptAndWait("Failover has not yet occured. Press enter to give up waiting.");
		    }
		}
	    }

            lock(testComplete)
            {
                withinTimeout = Monitor.Wait(testComplete, TIMEOUT);
            }            

            if (!withinTimeout)
            {
                Assert.Fail("Test timed out, before all messages received.");
            }

            _log.Debug("void DoFailoverTest(IConnectionInfo connectionInfo): exiting");
        }

	[Test]
        public void Test5MinuteWait()
	{
	    String clientId = "failover" + DateTime.Now.Ticks;

	    QpidConnectionInfo connectionInfo = new QpidConnectionInfo();
	    connectionInfo.Username = "guest";
	    connectionInfo.Password = "guest";
	    connectionInfo.ClientName = clientId;
	    connectionInfo.VirtualHost = "/test";
	    connectionInfo.AddBrokerInfo(new AmqBrokerInfo("amqp", "localhost", 9672, false));
	    connectionInfo.AddBrokerInfo(new AmqBrokerInfo("amqp", "localhost", 9673, false));
	    
	    Init(connectionInfo);
	    DoFailoverTest(5);
	}

	void SendMessage()
	{
	    ITextMessage msg = publishingChannel.CreateTextMessage("message=" + messagesSent);

	    publisher.Send(msg);
	    messagesSent++;

	    if (transacted)
	    {
		publishingChannel.Commit();
	    }
	    
	    Console.WriteLine("messagesSent = " + messagesSent);
	}
	
        /// <summary>
        /// Receives all of the test messages.
        /// </summary>
        ///
        /// <param name="message">The newly arrived test message.</param>
        public void OnMessage(IMessage message)
        {
            try
            {
                if (_acknowledgeMode == AcknowledgeMode.ClientAcknowledge)
                {
                    message.Acknowledge();
                }

                messagesReceived++;

                _log.Debug("messagesReceived = " + messagesReceived);

                // Check if all of the messages in the test have been received, in which case notify the message producer that the test has 
                // succesfully completed.
                if (messagesReceived == NUM_MESSAGES + _extraMessage)
                {
                    lock (testComplete)
                    {
			failedOver = true;
                        Monitor.Pulse(testComplete);
                    }
                }
            }
            catch (QpidException e)
            {
                _log.Fatal("Exception received. About to stop.", e);
                Stop();
            }
        }

        /// <summary>Prompts the user on stdout and waits for a reply on stdin, using the specified prompt message.</summary>
        ///
        /// <param name="message">The message to prompt the user with.</param>
        private void PromptAndWait(string message)
        {
            Console.WriteLine("\n" + message);
            Console.ReadLine();
        }

        // <summary>Closes the test connection.</summary>
        private void Stop()
        {
            _log.Debug("Stopping...");
            try
            {
                _connection.Close();
            }
            catch (QpidException e)
            {
                _log.Debug("Failed to shutdown: ", e);
            }
        }

        /// <summary>
        /// Called when bytes have been transmitted to the server
        /// </summary>
        ///
        /// <param>count the number of bytes sent in total since the connection was opened</param>     
        public void BytesSent(long count) {}

        /// <summary>
        /// Called when some bytes have been received on a connection
        /// </summary>
        ///
        /// <param>count the number of bytes received in total since the connection was opened</param>         
        public void BytesReceived(long count) {}

        /// <summary>
        /// Called after the infrastructure has detected that failover is required but before attempting failover.
        /// </summary>
        ///
        /// <param>redirect true if the broker requested redirect. false if failover is occurring due to a connection error.</param>
        ///
        /// <return>true to continue failing over, false to veto failover and raise a connection exception</return>         
        public bool PreFailover(bool redirect) 
        {
            _log.Debug("public bool PreFailover(bool redirect): called");
            return true; 
        }

        /// <summary>
        /// Called after connection has been made to another broker after failover has been started but before
        /// any resubscription has been done.
        /// </summary>
        ///
        /// <return> true to continue with resubscription, false to prevent automatic resubscription. This is useful in
        /// cases where the application wants to handle resubscription. Note that in the latter case all sessions, producers
        /// and consumers are invalidated.
        /// </return>
        public bool PreResubscribe() 
        {
            _log.Debug("public bool PreResubscribe(): called");
            return true; 
        }

        /// <summary>
        /// Called once failover has completed successfully. This is called irrespective of whether the client has
        /// vetoed automatic resubscription.
        /// </summary>
        public void FailoverComplete() 
        {
	    failedOver = true;
            _log.Debug("public void FailoverComplete(): called");
	    Console.WriteLine("public void FailoverComplete(): called");
	    lock (failoverComplete) 
	    {
	      Monitor.Pulse(failoverComplete);
	    }
        }
    }
}
