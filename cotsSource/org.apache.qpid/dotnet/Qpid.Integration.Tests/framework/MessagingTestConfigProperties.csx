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
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.Session;

using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// MessagingTestConfigProperties defines a set of property names and default values for specifying a messaging topology,
    /// and test parameters for running a messaging test over that topology. A Properties object holding some of these
    /// properties, superimposed onto the defaults, is used to establish test topologies and control test behaviour.
    ///
    /// <p/>A complete list of the parameters, default values and comments on their usage is provided here:
    ///
    /// <p/><table><caption>Parameters</caption>
    /// <tr><th> Parameter        <th> Default  <th> Comments
    /// <tr><td> messageSize      <td> 0        <td> Message size in bytes. Not including any headers.
    /// <tr><td> destinationName  <td> ping     <td> The root name to use to generate destination names to ping.
    /// <tr><td> persistent       <td> false    <td> Determines whether peristent delivery is used.
    /// <tr><td> transacted       <td> false    <td> Determines whether messages are sent/received in transactions.
    /// <tr><td> broker           <td> tcp://localhost:5672 <td> Determines the broker to connect to.
    /// <tr><td> virtualHost      <td> test     <td> Determines the virtual host to send all ping over.
    /// <tr><td> rate             <td> 0        <td> The maximum rate (in hertz) to send messages at. 0 means no limit.
    /// <tr><td> verbose          <td> false    <td> The verbose flag for debugging. Prints to console on every message.
    /// <tr><td> pubsub           <td> false    <td> Whether to ping topics or queues. Uses p2p by default.
    /// <tr><td> username         <td> guest    <td> The username to access the broker with.
    /// <tr><td> password         <td> guest    <td> The password to access the broker with.
    /// <tr><td> selector         <td> null     <td> Not used. Defines a message selector to filter pings with.
    /// <tr><td> destinationCount <td> 1        <td> The number of receivers listening to the pings.
    /// <tr><td> timeout          <td> 30000    <td> In milliseconds. The timeout to stop waiting for replies.
    /// <tr><td> commitBatchSize  <td> 1        <td> The number of messages per transaction in transactional mode.
    /// <tr><td> uniqueDests      <td> true     <td> Whether each receivers only listens to one ping destination or all.
    /// <tr><td> durableDests     <td> false    <td> Whether or not durable destinations are used.
    /// <tr><td> ackMode          <td> AUTO_ACK <td> The message acknowledgement mode. Possible values are:
    ///                                               0 - SESSION_TRANSACTED
    ///                                               1 - AUTO_ACKNOWLEDGE
    ///                                               2 - CLIENT_ACKNOWLEDGE
    ///                                               3 - DUPS_OK_ACKNOWLEDGE
    ///                                               257 - NO_ACKNOWLEDGE
    ///                                               258 - PRE_ACKNOWLEDGE
    /// <tr><td> maxPending       <td> 0        <td> The maximum size in bytes, of messages sent but not yet received.
    ///                                              Limits the volume of messages currently buffered on the client
    ///                                              or broker. Can help scale test clients by limiting amount of buffered
    ///                                              data to avoid out of memory errors.
    /// </table>
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide the names and defaults of all test parameters.
    /// </table>
    /// </summary>
    ///
    /// <remarks> Put a type-safe wrapper around these properties, but continue to store the parameters as properties. This is
    ///       simply to ensure that it is a simple matter to serialize/deserialize string/string pairs onto messages.</remarks>
    public class MessagingTestConfigProperties extends ParsedProperties
    {
        // ====================== Connection Properties ==================================

        /// <summary> Holds the name of the default connection configuration. </summary>
        public static final string CONNECTION_NAME = "broker";

        /// <summary> Holds the name of the property to get the initial context factory name from. </summary>
        public static final string INITIAL_CONTEXT_FACTORY_PROPNAME = "java.naming.factory.initial";

        /// <summary> Defines the class to use as the initial context factory by default. </summary>
        public static final string INITIAL_CONTEXT_FACTORY_DEFAULT = "org.apache.qpid.jndi.PropertiesFileInitialContextFactory";

        /// <summary> Holds the name of the property to get the test broker url from. </summary>
        public static final string BROKER_PROPNAME = "qpid.test.broker";

        /// <summary> Holds the default broker url for the test. </summary>
        public static final string BROKER_DEFAULT = "vm://:1";

        /// <summary> Holds the name of the property to get the test broker virtual path. </summary>
        public static final string VIRTUAL_HOST_PROPNAME = "virtualHost";

        /// <summary> Holds the default virtual path for the test. </summary>
        public static final string VIRTUAL_HOST_DEFAULT = "";

        /// <summary> Holds the name of the property to get the broker access username from. </summary>
        public static final string USERNAME_PROPNAME = "username";

        /// <summary> Holds the default broker log on username. </summary>
        public static final string USERNAME_DEFAULT = "guest";

        /// <summary> Holds the name of the property to get the broker access password from. </summary>
        public static final string PASSWORD_PROPNAME = "password";

        /// <summary> Holds the default broker log on password. </summary>
        public static final string PASSWORD_DEFAULT = "guest";

        // ====================== Messaging Topology Properties ==========================

        /// <summary> Holds the name of the property to get the bind publisher procuder flag from. </summary>
        public static final string PUBLISHER_PRODUCER_BIND_PROPNAME = "publisherProducerBind";

        /// <summary> Holds the default value of the publisher producer flag. </summary>
        public static final bool PUBLISHER_PRODUCER_BIND_DEFAULT = true;

        /// <summary> Holds the name of the property to get the bind publisher procuder flag from. </summary>
        public static final string PUBLISHER_CONSUMER_BIND_PROPNAME = "publisherConsumerBind";

        /// <summary> Holds the default value of the publisher consumer flag. </summary>
        public static final bool PUBLISHER_CONSUMER_BIND_DEFAULT = false;

        /// <summary> Holds the name of the property to get the bind receivers procuder flag from. </summary>
        public static final string RECEIVER_PRODUCER_BIND_PROPNAME = "receiverProducerBind";

        /// <summary> Holds the default value of the receivers producer flag. </summary>
        public static final bool RECEIVER_PRODUCER_BIND_DEFAULT = false;

        /// <summary> Holds the name of the property to get the bind receivers procuder flag from. </summary>
        public static final string RECEIVER_CONSUMER_BIND_PROPNAME = "receiverConsumerBind";

        /// <summary> Holds the default value of the receivers consumer flag. </summary>
        public static final bool RECEIVER_CONSUMER_BIND_DEFAULT = true;

        /// <summary> Holds the name of the property to get the publishers consumer active flag from. </summary>
        public static final string PUBLISHER_CONSUMER_ACTIVE_PROPNAME = "publisherConsumerActive";

        /// <summary> Holds the default value of the publishers consumer active flag. </summary>
        public static final bool PUBLISHER_CONSUMER_ACTIVE_DEFAULT = true;

        /// <summary> Holds the name of the property to get the receivers consumer active flag from. </summary>
        public static final string RECEIVER_CONSUMER_ACTIVE_PROPNAME = "receiverConsumerActive";

        /// <summary> Holds the default value of the receivers consumer active flag. </summary>
        public static final bool RECEIVER_CONSUMER_ACTIVE_DEFAULT = true;

        /// <summary> Holds the name of the property to get the destination name root from. </summary>
        public static final string SEND_DESTINATION_NAME_ROOT_PROPNAME = "sendDestinationRoot";

        /// <summary> Holds the root of the name of the default destination to send to. </summary>
        public static final string SEND_DESTINATION_NAME_ROOT_DEFAULT = "sendTo";

        /// <summary> Holds the name of the property to get the destination name root from. </summary>
        public static final string RECEIVE_DESTINATION_NAME_ROOT_PROPNAME = "receiveDestinationRoot";

        /// <summary> Holds the root of the name of the default destination to send to. </summary>
        public static final string RECEIVE_DESTINATION_NAME_ROOT_DEFAULT = "receiveFrom";

        /// <summary> Holds the name of the proeprty to get the destination count from. </summary>
        public static final string DESTINATION_COUNT_PROPNAME = "destinationCount";

        /// <summary> Defines the default number of destinations to ping. </summary>
        public static final int DESTINATION_COUNT_DEFAULT = 1;

        /// <summary> Holds the name of the property to get the p2p or pub/sub messaging mode from. </summary>
        public static final string PUBSUB_PROPNAME = "pubsub";

        /// <summary> Holds the pub/sub mode default, true means ping a topic, false means ping a queue. </summary>
        public static final bool PUBSUB_DEFAULT = false;

        // ======================  JMS Options and Flags =================================

        /// <summary> Holds the name of the property to get the test delivery mode from. </summary>
        public static final string PERSISTENT_MODE_PROPNAME = "persistent";

        /// <summary> Holds the message delivery mode to use for the test. </summary>
        public static final bool PERSISTENT_MODE_DEFAULT = false;

        /// <summary> Holds the name of the property to get the test transactional mode from. </summary>
        public static final string TRANSACTED_PUBLISHER_PROPNAME = "transactedPublisher";

        /// <summary> Holds the transactional mode to use for the test. </summary>
        public static final bool TRANSACTED_PUBLISHER_DEFAULT = false;

        /// <summary> Holds the name of the property to get the test transactional mode from. </summary>
        public static final string TRANSACTED_RECEIVER_PROPNAME = "transactedReceiver";

        /// <summary> Holds the transactional mode to use for the test. </summary>
        public static final bool TRANSACTED_RECEIVER_DEFAULT = false;

        /// <summary> Holds the name of the property to set the no local flag from. </summary>
        public static final string NO_LOCAL_PROPNAME = "noLocal";

        /// <summary> Defines the default value of the no local flag to use when consuming messages. </summary>
        public static final bool NO_LOCAL_DEFAULT = false;

        /// <summary> Holds the name of the property to get the message acknowledgement mode from. </summary>
        public static final string ACK_MODE_PROPNAME = "ackMode";

        /// <summary> Defines the default message acknowledgement mode. </summary>
        public static final int ACK_MODE_DEFAULT = Session.AUTO_ACKNOWLEDGE;

        /// <summary> Holds the name of the property to get the durable subscriptions flag from, when doing pub/sub messaging. </summary>
        public static final string DURABLE_SUBSCRIPTION_PROPNAME = "durableSubscription";

        /// <summary> Defines the default value of the durable subscriptions flag. </summary>
        public static final bool DURABLE_SUBSCRIPTION_DEFAULT = false;

        // ======================  Qpid/AMQP Options and Flags ================================

        /// <summary> Holds the name of the property to set the exclusive flag from. </summary>
        public static final string EXCLUSIVE_PROPNAME = "exclusive";

        /// <summary> Defines the default value of the exclusive flag to use when consuming messages. </summary>
        public static final bool EXCLUSIVE_DEFAULT = false;

        /// <summary> Holds the name of the property to set the immediate flag from. </summary>
        public static final string IMMEDIATE_PROPNAME = "immediate";

        /// <summary> Defines the default value of the immediate flag to use when sending messages. </summary>
        public static final bool IMMEDIATE_DEFAULT = false;

        /// <summary> Holds the name of the property to set the mandatory flag from. </summary>
        public static final string MANDATORY_PROPNAME = "mandatory";

        /// <summary> Defines the default value of the mandatory flag to use when sending messages. </summary>
        public static final bool MANDATORY_DEFAULT = false;

        /// <summary> Holds the name of the property to get the durable destinations flag from. </summary>
        public static final string DURABLE_DESTS_PROPNAME = "durableDests";

        /// <summary> Default value for the durable destinations flag. </summary>
        public static final bool DURABLE_DESTS_DEFAULT = false;

        /// <summary> Holds the name of the property to set the prefetch size from. </summary>
        public static final string PREFETCH_PROPNAME = "prefetch";

        /// <summary> Defines the default prefetch size to use when consuming messages. </summary>
        public static final int PREFETCH_DEFAULT = 100;

        // ======================  Common Test Parameters ================================

        /// <summary> Holds the name of the property to get the test message size from. </summary>
        public static final string MESSAGE_SIZE_PROPNAME = "messageSize";

        /// <summary> Used to set up a default message size. </summary>
        public static final int MESSAGE_SIZE_DEAFULT = 0;

        /// <summary> Holds the name of the property to get the message rate from. </summary>
        public static final string RATE_PROPNAME = "rate";

        /// <summary> Defines the default rate (in pings per second) to send pings at. 0 means as fast as possible, no restriction. </summary>
        public static final int RATE_DEFAULT = 0;

        /// <summary> Holds the name of the proeprty to get the. </summary>
        public static final string SELECTOR_PROPNAME = "selector";

        /// <summary> Holds the default message selector. </summary>
        public static final string SELECTOR_DEFAULT = "";

        /// <summary> Holds the name of the property to get the waiting timeout for response messages. </summary>
        public static final string TIMEOUT_PROPNAME = "timeout";

        /// <summary> Default time to wait before assuming that a ping has timed out. </summary>
        public static final long TIMEOUT_DEFAULT = 30000;

        /// <summary> Holds the name of the property to get the commit batch size from. </summary>
        public static final string TX_BATCH_SIZE_PROPNAME = "commitBatchSize";

        /// <summary> Defines the default number of pings to send in each transaction when running transactionally. </summary>
        public static final int TX_BATCH_SIZE_DEFAULT = 1;

        /// <summary> Holds the name of the property to set the maximum amount of pending message data for a producer to hold. </summary>
        public static final string MAX_PENDING_PROPNAME = "maxPending";

        /// <summary> Defines the default maximum quantity of pending message data to allow producers to hold. </summary>
        public static final int MAX_PENDING_DEFAULT = 0;

        /// <summary> Holds the name of the property to get the publisher rollback flag from. </summary>
        public static final string ROLLBACK_PUBLISHER_PROPNAME = "rollbackPublisher";

        /// <summary> Holds the default publisher roll back setting. </summary>
        public static final bool ROLLBACK_PUBLISHER_DEFAULT = false;

        /// <summary> Holds the name of the property to get the publisher rollback flag from. </summary>
        public static final string ROLLBACK_RECEIVER_PROPNAME = "rollbackReceiver";

        /// <summary> Holds the default publisher roll back setting. </summary>
        public static final bool ROLLBACK_RECEIVER_DEFAULT = false;

        // ====================== Options that control the bahviour of the test framework. =========================

        /// <summary> Holds the name of the property to get the behavioural mode of not applicable assertions. </summary>
        public static final string NOT_APPLICABLE_ASSERTION_PROPNAME = "notApplicableAssertion";

        /// <summary> Holds the default behavioral mode of not applicable assertions, which is logging them as a warning. </summary>
        public static final string NOT_APPLICABLE_ASSERTION_DEFAULT = "warn";

        /// <summary> Holds the name of the property to get the verbose mode proeprty from. </summary>
        public static final string VERBOSE_PROPNAME = "verbose";

        /// <summary> Holds the default verbose mode. </summary>
        public static final bool VERBOSE_DEFAULT = false;

        /// <summary> Holds the default configuration properties. </summary>
        public static ParsedProperties defaults = new ParsedProperties();

        static
        {
            defaults.setPropertyIfNull(INITIAL_CONTEXT_FACTORY_PROPNAME, INITIAL_CONTEXT_FACTORY_DEFAULT);
            defaults.setPropertyIfNull(BROKER_PROPNAME, BROKER_DEFAULT);
            defaults.setPropertyIfNull(VIRTUAL_HOST_PROPNAME, VIRTUAL_HOST_DEFAULT);
            defaults.setPropertyIfNull(USERNAME_PROPNAME, USERNAME_DEFAULT);
            defaults.setPropertyIfNull(PASSWORD_PROPNAME, PASSWORD_DEFAULT);

            defaults.setPropertyIfNull(PUBLISHER_PRODUCER_BIND_PROPNAME, PUBLISHER_PRODUCER_BIND_DEFAULT);
            defaults.setPropertyIfNull(PUBLISHER_CONSUMER_BIND_PROPNAME, PUBLISHER_CONSUMER_BIND_DEFAULT);
            defaults.setPropertyIfNull(RECEIVER_PRODUCER_BIND_PROPNAME, RECEIVER_PRODUCER_BIND_DEFAULT);
            defaults.setPropertyIfNull(RECEIVER_CONSUMER_BIND_PROPNAME, RECEIVER_CONSUMER_BIND_DEFAULT);
            defaults.setPropertyIfNull(PUBLISHER_CONSUMER_ACTIVE_PROPNAME, PUBLISHER_CONSUMER_ACTIVE_DEFAULT);
            defaults.setPropertyIfNull(RECEIVER_CONSUMER_ACTIVE_PROPNAME, RECEIVER_CONSUMER_ACTIVE_DEFAULT);
            defaults.setPropertyIfNull(SEND_DESTINATION_NAME_ROOT_PROPNAME, SEND_DESTINATION_NAME_ROOT_DEFAULT);
            defaults.setPropertyIfNull(RECEIVE_DESTINATION_NAME_ROOT_PROPNAME, RECEIVE_DESTINATION_NAME_ROOT_DEFAULT);
            defaults.setPropertyIfNull(DESTINATION_COUNT_PROPNAME, DESTINATION_COUNT_DEFAULT);
            defaults.setPropertyIfNull(PUBSUB_PROPNAME, PUBSUB_DEFAULT);

            defaults.setPropertyIfNull(PERSISTENT_MODE_PROPNAME, PERSISTENT_MODE_DEFAULT);
            defaults.setPropertyIfNull(TRANSACTED_PUBLISHER_PROPNAME, TRANSACTED_PUBLISHER_DEFAULT);
            defaults.setPropertyIfNull(TRANSACTED_RECEIVER_PROPNAME, TRANSACTED_RECEIVER_DEFAULT);
            defaults.setPropertyIfNull(NO_LOCAL_PROPNAME, NO_LOCAL_DEFAULT);
            defaults.setPropertyIfNull(ACK_MODE_PROPNAME, ACK_MODE_DEFAULT);
            defaults.setPropertyIfNull(DURABLE_SUBSCRIPTION_PROPNAME, DURABLE_SUBSCRIPTION_DEFAULT);

            defaults.setPropertyIfNull(EXCLUSIVE_PROPNAME, EXCLUSIVE_DEFAULT);
            defaults.setPropertyIfNull(IMMEDIATE_PROPNAME, IMMEDIATE_DEFAULT);
            defaults.setPropertyIfNull(MANDATORY_PROPNAME, MANDATORY_DEFAULT);
            defaults.setPropertyIfNull(DURABLE_DESTS_PROPNAME, DURABLE_DESTS_DEFAULT);
            defaults.setPropertyIfNull(PREFETCH_PROPNAME, PREFETCH_DEFAULT);

            defaults.setPropertyIfNull(MESSAGE_SIZE_PROPNAME, MESSAGE_SIZE_DEAFULT);
            defaults.setPropertyIfNull(RATE_PROPNAME, RATE_DEFAULT);
            defaults.setPropertyIfNull(SELECTOR_PROPNAME, SELECTOR_DEFAULT);
            defaults.setPropertyIfNull(TIMEOUT_PROPNAME, TIMEOUT_DEFAULT);
            defaults.setPropertyIfNull(TX_BATCH_SIZE_PROPNAME, TX_BATCH_SIZE_DEFAULT);
            defaults.setPropertyIfNull(MAX_PENDING_PROPNAME, MAX_PENDING_DEFAULT);
            defaults.setPropertyIfNull(ROLLBACK_PUBLISHER_PROPNAME, ROLLBACK_PUBLISHER_DEFAULT);
            defaults.setPropertyIfNull(ROLLBACK_RECEIVER_PROPNAME, ROLLBACK_RECEIVER_DEFAULT);

            defaults.setPropertyIfNull(NOT_APPLICABLE_ASSERTION_PROPNAME, NOT_APPLICABLE_ASSERTION_DEFAULT);
            defaults.setPropertyIfNull(VERBOSE_PROPNAME, VERBOSE_DEFAULT);
        }

        /// <summary> Creates a test configuration based on the defaults. </summary>
        public MessagingTestConfigProperties()
        {
            super(defaults);
        }

        /// <summary>
        /// Creates a test configuration based on the supplied properties.
        /// </summary>
        /// <param name="properties"> The test configuration. </param>
        public MessagingTestConfigProperties(Properties properties)
        {
            super(properties);
        }

        /// <summary>
        /// The size of test messages to send.
        /// </summary>
        /// <return> The size of test messages to send. </return>
        public int getMessageSize()
        {
            return getPropertyAsInteger(MESSAGE_SIZE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that the publishing producer should be set up to publish to a destination.
        /// </summary>
        /// <return> Flag to indicate that the publishing producer should be set up to publish to a destination. </return>
        public bool getPublisherProducerBind()
        {
            return getPropertyAsBoolean(PUBLISHER_PRODUCER_BIND_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that the publishing consumer should be set up to receive from a destination.
        /// </summary>
        /// <return> Flag to indicate that the publishing consumer should be set up to receive from a destination. </return>
        public bool getPublisherConsumerBind()
        {
            return getPropertyAsBoolean(PUBLISHER_CONSUMER_BIND_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that the receiving producer should be set up to publish to a destination.
        /// </summary>
        /// <return> Flag to indicate that the receiving producer should be set up to publish to a destination. </return>
        public bool getReceiverProducerBind()
        {
            return getPropertyAsBoolean(RECEIVER_PRODUCER_BIND_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that the receiving consumer should be set up to receive from a destination.
        /// </summary>
        /// <return> Flag to indicate that the receiving consumer should be set up to receive from a destination. </return>
        public bool getReceiverConsumerBind()
        {
            return getPropertyAsBoolean(RECEIVER_CONSUMER_BIND_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that the publishing consumer should be created and actively listening.
        /// </summary>
        /// <return> Flag to indicate that the publishing consumer should be created. </return>
        public bool getPublisherConsumerActive()
        {
            return getPropertyAsBoolean(PUBLISHER_CONSUMER_ACTIVE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that the receiving consumers should be created and actively listening.
        /// </summary>
        /// <return> Flag to indicate that the receiving consumers should be created and actively listening. </return>
        public bool getReceiverConsumerActive()
        {
            return getPropertyAsBoolean(RECEIVER_CONSUMER_ACTIVE_PROPNAME);
        }

        /// <summary>
        /// A root to create all test destination names from.
        /// </summary>
        /// <return> A root to create all test destination names from. </return>
        public string getSendDestinationNameRoot()
        {
            return getProperty(SEND_DESTINATION_NAME_ROOT_PROPNAME);
        }

        /// <summary>
        /// A root to create all receiving destination names from.
        /// </summary>
        /// <return> A root to create all receiving destination names from. </return>
        public string getReceiveDestinationNameRoot()
        {
            return getProperty(RECEIVE_DESTINATION_NAME_ROOT_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that persistent messages should be used.
        /// </summary>
        /// <return> Flag to indicate that persistent messages should be used. </return>
        public bool getPersistentMode()
        {
            return getPropertyAsBoolean(PERSISTENT_MODE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that transactional messages should be sent by the publisher.
        /// </summary>
        /// <return> Flag to indicate that transactional messages should be sent by the publisher. </return>
        public bool getPublisherTransacted()
        {
            return getPropertyAsBoolean(TRANSACTED_PUBLISHER_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that transactional receives should be used by the receiver.
        /// </summary>
        /// <return> Flag to indicate that transactional receives should be used by the receiver. </return>
        public bool getReceiverTransacted()
        {
            return getPropertyAsBoolean(TRANSACTED_PUBLISHER_PROPNAME);
        }

        /// <summary>
        /// The name of the virtual host to run all tests over.
        /// </summary>
        /// <return> The name of the virtual host to run all tests over. </return>
        public string getVirtualHost()
        {
            return getProperty(VIRTUAL_HOST_PROPNAME);
        }

        /// <summary>
        /// Limiting rate for each sender in messages per second, or zero for unlimited.
        /// </summary>
        /// <return> Limiting rate for each sender in messages per second, or zero for unlimited. </return>
        public string getRate()
        {
            return getProperty(RATE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that test messages should be received publish/subscribe style by all receivers.
        /// </summary>
        /// <return> Flag to indicate that test messages should be received publish/subscribe style by all receivers. </return>
        public bool getPubsub()
        {
            return getPropertyAsBoolean(PUBSUB_PROPNAME);
        }

        /// <summary>
        /// The username credentials to run tests with.
        /// </summary>
        /// <return> The username credentials to run tests with. </return>
        public string getUsername()
        {
            return getProperty(USERNAME_PROPNAME);
        }

        /// <summary>
        /// The password credentials to run tests with.
        /// </summary>
        /// <return> The password credentials to run tests with. </return>
        public string getPassword()
        {
            return getProperty(PASSWORD_PROPNAME);
        }

        /// <summary>
        /// The timeout duration to fail tests on, should they receive no messages within it.
        /// </summary>
        /// <return> The timeout duration to fail tests on, should they receive no messages within it. </return>
        public long getTimeout()
        {
            return getPropertyAsLong(TIMEOUT_PROPNAME);
        }

        /// <summary>
        /// The number of messages to batch into each transaction in transational tests.
        /// </summary>
        /// <return> The number of messages to batch into each transaction in transational tests. </return>
        public int getTxBatchSize()
        {
            return getPropertyAsInteger(TX_BATCH_SIZE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that tests should use durable destinations.
        /// </summary>
        /// <return> Flag to indicate that tests should use durable destinations. </return>
        public bool getDurableDests()
        {
            return getPropertyAsBoolean(DURABLE_DESTS_PROPNAME);
        }

        /// <summary>
        /// The ack mode for message receivers to use.
        /// </summary>
        /// <return> The ack mode for message receivers to use. </return>
        public int getAckMode()
        {
            return getPropertyAsInteger(ACK_MODE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that tests should use durable subscriptions.
        /// </summary>
        /// <return> Flag to indicate that tests should use durable subscriptions. </return>
        public bool getDurableSubscription()
        {
            return getPropertyAsBoolean(DURABLE_SUBSCRIPTION_PROPNAME);
        }

        /// <summary>
        /// The maximum amount of in-flight data, in bytes, that tests should send at any time.
        /// </summary>
        /// <return> The maximum amount of in-flight data, in bytes, that tests should send at any time. </return>
        public int getMaxPending()
        {
            return getPropertyAsInteger(MAX_PENDING_PROPNAME);
        }

        /// <summary>
        /// The size of the prefetch queue to use.
        /// </summary>
        /// <return> The size of the prefetch queue to use. </return>
        public int getPrefetch()
        {
            return getPropertyAsInteger(PREFETCH_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that subscriptions should be no-local.
        /// </summary>
        /// <return> Flag to indicate that subscriptions should be no-local. </return>
        public bool getNoLocal()
        {
            return getPropertyAsBoolean(NO_LOCAL_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that subscriptions should be exclusive.
        /// </summary>
        /// <return> Flag to indicate that subscriptions should be exclusive. </return>
        public bool getExclusive()
        {
            return getPropertyAsBoolean(EXCLUSIVE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that messages must be delivered immediately.
        /// </summary>
        /// <return> Flag to indicate that messages must be delivered immediately. </return>
        public bool getImmediate()
        {
            return getPropertyAsBoolean(IMMEDIATE_PROPNAME);
        }

        /// <summary>
        /// Flag to indicate that messages must be routable.
        /// </summary>
        /// <return> Flag to indicate that messages must be routable. </return>
        public bool getMandatory()
        {
            return getPropertyAsBoolean(MANDATORY_PROPNAME);
        }

        /// <summary>
        /// Gets the value of a flag to indicate that the publisher should rollback all messages sent.
        /// </summary>
        /// <return> A flag to indicate that the publisher should rollback all messages sent. </return>
        public bool getRollbackPublisher()
        {
            return getPropertyAsBoolean(ROLLBACK_PUBLISHER_PROPNAME);
        }

        /// <summary>
        /// Gets the value of a flag to indicate that the receiver should rollback all messages received, then receive them
        /// again.
        /// </summary>
        /// <return> A flag to indicate that the publisher should rollback all messages received. </return>
        public bool getRollbackReceiver()
        {
            return getPropertyAsBoolean(ROLLBACK_RECEIVER_PROPNAME);
        }

        /// <summary>
        /// Gets the behavioural mode of not applicable assertions. Should be one of 'quiet', 'warn' or 'fail'.
        /// </summary>
        /// <return> The behavioural mode of not applicable assertions. </return>
        public string getNotApplicableAssertionMode()
        {
            return getProperty(NOT_APPLICABLE_ASSERTION_PROPNAME);
        }
    }
}