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
package org.apache.qpid.test.framework;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.Session;

import java.util.Properties;

/**
 * MessagingTestConfigProperties defines a set of property names and default values for specifying a messaging topology,
 * and test parameters for running a messaging test over that topology. A Properties object holding some of these
 * properties, superimposed onto the defaults, is used to establish test topologies and control test behaviour.
 *
 * <p/>A complete list of the parameters, default values and comments on their usage is provided here:
 *
 * <p/><table><caption>Parameters</caption>
 * <tr><th> Parameter        <th> Default  <th> Comments
 * <tr><td> messageSize      <td> 0        <td> Message size in bytes. Not including any headers.
 * <tr><td> destinationName  <td> ping     <td> The root name to use to generate destination names to ping.
 * <tr><td> persistent       <td> false    <td> Determines whether peristent delivery is used.
 * <tr><td> transacted       <td> false    <td> Determines whether messages are sent/received in transactions.
 * <tr><td> broker           <td> tcp://localhost:5672 <td> Determines the broker to connect to.
 * <tr><td> virtualHost      <td> test     <td> Determines the virtual host to send all ping over.
 * <tr><td> rate             <td> 0        <td> The maximum rate (in hertz) to send messages at. 0 means no limit.
 * <tr><td> verbose          <td> false    <td> The verbose flag for debugging. Prints to console on every message.
 * <tr><td> pubsub           <td> false    <td> Whether to ping topics or queues. Uses p2p by default.
 * <tr><td> username         <td> guest    <td> The username to access the broker with.
 * <tr><td> password         <td> guest    <td> The password to access the broker with.
 * <tr><td> selector         <td> null     <td> Not used. Defines a message selector to filter pings with.
 * <tr><td> destinationCount <td> 1        <td> The number of receivers listening to the pings.
 * <tr><td> timeout          <td> 30000    <td> In milliseconds. The timeout to stop waiting for replies.
 * <tr><td> commitBatchSize  <td> 1        <td> The number of messages per transaction in transactional mode.
 * <tr><td> uniqueDests      <td> true     <td> Whether each receivers only listens to one ping destination or all.
 * <tr><td> durableDests     <td> false    <td> Whether or not durable destinations are used.
 * <tr><td> ackMode          <td> AUTO_ACK <td> The message acknowledgement mode. Possible values are:
 *                                               0 - SESSION_TRANSACTED
 *                                               1 - AUTO_ACKNOWLEDGE
 *                                               2 - CLIENT_ACKNOWLEDGE
 *                                               3 - DUPS_OK_ACKNOWLEDGE
 *                                               257 - NO_ACKNOWLEDGE
 *                                               258 - PRE_ACKNOWLEDGE
 * <tr><td> maxPending       <td> 0        <td> The maximum size in bytes, of messages sent but not yet received.
 *                                              Limits the volume of messages currently buffered on the client
 *                                              or broker. Can help scale test clients by limiting amount of buffered
 *                                              data to avoid out of memory errors.
 * </table>
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide the names and defaults of all test parameters.
 * </table>
 *
 * @todo Put a type-safe wrapper around these properties, but continue to store the parameters as properties. This is
 *       simply to ensure that it is a simple matter to serialize/deserialize string/string pairs onto messages.
 */
public class MessagingTestConfigProperties extends ParsedProperties
{
    // ====================== Connection Properties ==================================

    /** Holds the name of the default connection configuration. */
    public static final String CONNECTION_NAME = "broker";

    /** Holds the name of the property to get the initial context factory name from. */
    public static final String INITIAL_CONTEXT_FACTORY_PROPNAME = "java.naming.factory.initial";

    /** Defines the class to use as the initial context factory by default. */
    public static final String INITIAL_CONTEXT_FACTORY_DEFAULT = "org.apache.qpid.jndi.PropertiesFileInitialContextFactory";

    /** Holds the name of the property to get the test broker url from. */
    public static final String BROKER_PROPNAME = "qpid.test.broker";

    /** Holds the default broker url for the test. */
    public static final String BROKER_DEFAULT = "vm://:1";

    /** Holds the name of the property to get the test broker virtual path. */
    public static final String VIRTUAL_HOST_PROPNAME = "virtualHost";

    /** Holds the default virtual path for the test. */
    public static final String VIRTUAL_HOST_DEFAULT = "";

    /** Holds the name of the property to get the broker access username from. */
    public static final String USERNAME_PROPNAME = "username";

    /** Holds the default broker log on username. */
    public static final String USERNAME_DEFAULT = "guest";

    /** Holds the name of the property to get the broker access password from. */
    public static final String PASSWORD_PROPNAME = "password";

    /** Holds the default broker log on password. */
    public static final String PASSWORD_DEFAULT = "guest";

    // ====================== Messaging Topology Properties ==========================

    /** Holds the name of the property to get the bind publisher procuder flag from. */
    public static final String PUBLISHER_PRODUCER_BIND_PROPNAME = "publisherProducerBind";

    /** Holds the default value of the publisher producer flag. */
    public static final boolean PUBLISHER_PRODUCER_BIND_DEFAULT = true;

    /** Holds the name of the property to get the bind publisher procuder flag from. */
    public static final String PUBLISHER_CONSUMER_BIND_PROPNAME = "publisherConsumerBind";

    /** Holds the default value of the publisher consumer flag. */
    public static final boolean PUBLISHER_CONSUMER_BIND_DEFAULT = false;

    /** Holds the name of the property to get the bind receivers procuder flag from. */
    public static final String RECEIVER_PRODUCER_BIND_PROPNAME = "receiverProducerBind";

    /** Holds the default value of the receivers producer flag. */
    public static final boolean RECEIVER_PRODUCER_BIND_DEFAULT = false;

    /** Holds the name of the property to get the bind receivers procuder flag from. */
    public static final String RECEIVER_CONSUMER_BIND_PROPNAME = "receiverConsumerBind";

    /** Holds the default value of the receivers consumer flag. */
    public static final boolean RECEIVER_CONSUMER_BIND_DEFAULT = true;

    /** Holds the name of the property to get the publishers consumer active flag from. */
    public static final String PUBLISHER_CONSUMER_ACTIVE_PROPNAME = "publisherConsumerActive";

    /** Holds the default value of the publishers consumer active flag. */
    public static final boolean PUBLISHER_CONSUMER_ACTIVE_DEFAULT = true;

    /** Holds the name of the property to get the receivers consumer active flag from. */
    public static final String RECEIVER_CONSUMER_ACTIVE_PROPNAME = "receiverConsumerActive";

    /** Holds the default value of the receivers consumer active flag. */
    public static final boolean RECEIVER_CONSUMER_ACTIVE_DEFAULT = true;

    /** Holds the name of the property to get the destination name root from. */
    public static final String SEND_DESTINATION_NAME_ROOT_PROPNAME = "sendDestinationRoot";

    /** Holds the root of the name of the default destination to send to. */
    public static final String SEND_DESTINATION_NAME_ROOT_DEFAULT = "sendTo";

    /** Holds the name of the property to get the destination name root from. */
    public static final String RECEIVE_DESTINATION_NAME_ROOT_PROPNAME = "receiveDestinationRoot";

    /** Holds the root of the name of the default destination to send to. */
    public static final String RECEIVE_DESTINATION_NAME_ROOT_DEFAULT = "receiveFrom";

    /** Holds the name of the proeprty to get the destination count from. */
    public static final String DESTINATION_COUNT_PROPNAME = "destinationCount";

    /** Defines the default number of destinations to ping. */
    public static final int DESTINATION_COUNT_DEFAULT = 1;

    /** Holds the name of the property to get the p2p or pub/sub messaging mode from. */
    public static final String PUBSUB_PROPNAME = "pubsub";

    /** Holds the pub/sub mode default, true means ping a topic, false means ping a queue. */
    public static final boolean PUBSUB_DEFAULT = false;

    // ======================  JMS Options and Flags =================================

    /** Holds the name of the property to get the test delivery mode from. */
    public static final String PERSISTENT_MODE_PROPNAME = "persistent";

    /** Holds the message delivery mode to use for the test. */
    public static final boolean PERSISTENT_MODE_DEFAULT = false;

    /** Holds the name of the property to get the test transactional mode from. */
    public static final String TRANSACTED_PUBLISHER_PROPNAME = "transactedPublisher";

    /** Holds the transactional mode to use for the test. */
    public static final boolean TRANSACTED_PUBLISHER_DEFAULT = false;

    /** Holds the name of the property to get the test transactional mode from. */
    public static final String TRANSACTED_RECEIVER_PROPNAME = "transactedReceiver";

    /** Holds the transactional mode to use for the test. */
    public static final boolean TRANSACTED_RECEIVER_DEFAULT = false;

    /** Holds the name of the property to set the no local flag from. */
    public static final String NO_LOCAL_PROPNAME = "noLocal";

    /** Defines the default value of the no local flag to use when consuming messages. */
    public static final boolean NO_LOCAL_DEFAULT = false;

    /** Holds the name of the property to get the message acknowledgement mode from. */
    public static final String ACK_MODE_PROPNAME = "ackMode";

    /** Defines the default message acknowledgement mode. */
    public static final int ACK_MODE_DEFAULT = Session.AUTO_ACKNOWLEDGE;

    /** Holds the name of the property to get the durable subscriptions flag from, when doing pub/sub messaging. */
    public static final String DURABLE_SUBSCRIPTION_PROPNAME = "durableSubscription";

    /** Defines the default value of the durable subscriptions flag. */
    public static final boolean DURABLE_SUBSCRIPTION_DEFAULT = false;

    // ======================  Qpid/AMQP Options and Flags ================================

    /** Holds the name of the property to set the exclusive flag from. */
    public static final String EXCLUSIVE_PROPNAME = "exclusive";

    /** Defines the default value of the exclusive flag to use when consuming messages. */
    public static final boolean EXCLUSIVE_DEFAULT = false;

    /** Holds the name of the property to set the immediate flag from. */
    public static final String IMMEDIATE_PROPNAME = "immediate";

    /** Defines the default value of the immediate flag to use when sending messages. */
    public static final boolean IMMEDIATE_DEFAULT = false;

    /** Holds the name of the property to set the mandatory flag from. */
    public static final String MANDATORY_PROPNAME = "mandatory";

    /** Defines the default value of the mandatory flag to use when sending messages. */
    public static final boolean MANDATORY_DEFAULT = false;

    /** Holds the name of the property to get the durable destinations flag from. */
    public static final String DURABLE_DESTS_PROPNAME = "durableDests";

    /** Default value for the durable destinations flag. */
    public static final boolean DURABLE_DESTS_DEFAULT = false;

    /** Holds the name of the property to set the prefetch size from. */
    public static final String PREFETCH_PROPNAME = "prefetch";

    /** Defines the default prefetch size to use when consuming messages. */
    public static final int PREFETCH_DEFAULT = 100;

    // ======================  Common Test Parameters ================================

    /** Holds the name of the property to get the test message size from. */
    public static final String MESSAGE_SIZE_PROPNAME = "messageSize";

    /** Used to set up a default message size. */
    public static final int MESSAGE_SIZE_DEAFULT = 0;

    /** Holds the name of the property to get the message rate from. */
    public static final String RATE_PROPNAME = "rate";

    /** Defines the default rate (in pings per second) to send pings at. 0 means as fast as possible, no restriction. */
    public static final int RATE_DEFAULT = 0;

    /** Holds the name of the proeprty to get the. */
    public static final String SELECTOR_PROPNAME = "selector";

    /** Holds the default message selector. */
    public static final String SELECTOR_DEFAULT = "";

    /** Holds the name of the property to get the waiting timeout for response messages. */
    public static final String TIMEOUT_PROPNAME = "timeout";

    /** Default time to wait before assuming that a ping has timed out. */
    public static final long TIMEOUT_DEFAULT = 30000;

    /** Holds the name of the property to get the commit batch size from. */
    public static final String TX_BATCH_SIZE_PROPNAME = "commitBatchSize";

    /** Defines the default number of pings to send in each transaction when running transactionally. */
    public static final int TX_BATCH_SIZE_DEFAULT = 1;

    /** Holds the name of the property to set the maximum amount of pending message data for a producer to hold. */
    public static final String MAX_PENDING_PROPNAME = "maxPending";

    /** Defines the default maximum quantity of pending message data to allow producers to hold. */
    public static final int MAX_PENDING_DEFAULT = 0;

    /** Holds the name of the property to get the publisher rollback flag from. */
    public static final String ROLLBACK_PUBLISHER_PROPNAME = "rollbackPublisher";

    /** Holds the default publisher roll back setting. */
    public static final boolean ROLLBACK_PUBLISHER_DEFAULT = false;

    /** Holds the name of the property to get the publisher rollback flag from. */
    public static final String ROLLBACK_RECEIVER_PROPNAME = "rollbackReceiver";

    /** Holds the default publisher roll back setting. */
    public static final boolean ROLLBACK_RECEIVER_DEFAULT = false;

    // ====================== Options that control the bahviour of the test framework. =========================

    /** Holds the name of the property to get the behavioural mode of not applicable assertions. */
    public static final String NOT_APPLICABLE_ASSERTION_PROPNAME = "notApplicableAssertion";

    /** Holds the default behavioral mode of not applicable assertions, which is logging them as a warning. */
    public static final String NOT_APPLICABLE_ASSERTION_DEFAULT = "warn";

    /** Holds the name of the property to get the verbose mode proeprty from. */
    public static final String VERBOSE_PROPNAME = "verbose";

    /** Holds the default verbose mode. */
    public static final boolean VERBOSE_DEFAULT = false;

    /** Holds the default configuration properties. */
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

    /**
     * Creates a test configuration based on the defaults.
     */
    public MessagingTestConfigProperties()
    {
        super(defaults);
    }

    /**
     * Creates a test configuration based on the supplied properties.
     *
     * @param properties The test configuration.
     */
    public MessagingTestConfigProperties(Properties properties)
    {
        super(properties);
    }

    /**
     * The size of test messages to send.
     *
     * @return The size of test messages to send.
     */
    public int getMessageSize()
    {
        return getPropertyAsInteger(MESSAGE_SIZE_PROPNAME);
    }

    /**
     * Flag to indicate that the publishing producer should be set up to publish to a destination.
     *
     * @return Flag to indicate that the publishing producer should be set up to publish to a destination.
     */
    public boolean getPublisherProducerBind()
    {
        return getPropertyAsBoolean(PUBLISHER_PRODUCER_BIND_PROPNAME);
    }

    /**
     * Flag to indicate that the publishing consumer should be set up to receive from a destination.
     *
     * @return Flag to indicate that the publishing consumer should be set up to receive from a destination.
     */
    public boolean getPublisherConsumerBind()
    {
        return getPropertyAsBoolean(PUBLISHER_CONSUMER_BIND_PROPNAME);
    }

    /**
     * Flag to indicate that the receiving producer should be set up to publish to a destination.
     *
     * @return Flag to indicate that the receiving producer should be set up to publish to a destination.
     */
    public boolean getReceiverProducerBind()
    {
        return getPropertyAsBoolean(RECEIVER_PRODUCER_BIND_PROPNAME);
    }

    /**
     * Flag to indicate that the receiving consumer should be set up to receive from a destination.
     *
     * @return Flag to indicate that the receiving consumer should be set up to receive from a destination.
     */
    public boolean getReceiverConsumerBind()
    {
        return getPropertyAsBoolean(RECEIVER_CONSUMER_BIND_PROPNAME);
    }

    /**
     * Flag to indicate that the publishing consumer should be created and actively listening.
     *
     * @return Flag to indicate that the publishing consumer should be created.
     */
    public boolean getPublisherConsumerActive()
    {
        return getPropertyAsBoolean(PUBLISHER_CONSUMER_ACTIVE_PROPNAME);
    }

    /**
     * Flag to indicate that the receiving consumers should be created and actively listening.
     *
     * @return Flag to indicate that the receiving consumers should be created and actively listening.
     */
    public boolean getReceiverConsumerActive()
    {
        return getPropertyAsBoolean(RECEIVER_CONSUMER_ACTIVE_PROPNAME);
    }

    /**
     * A root to create all test destination names from.
     *
     * @return A root to create all test destination names from.
     */
    public String getSendDestinationNameRoot()
    {
        return getProperty(SEND_DESTINATION_NAME_ROOT_PROPNAME);
    }

    /**
     * A root to create all receiving destination names from.
     *
     * @return A root to create all receiving destination names from.
     */
    public String getReceiveDestinationNameRoot()
    {
        return getProperty(RECEIVE_DESTINATION_NAME_ROOT_PROPNAME);
    }

    /**
     * Flag to indicate that persistent messages should be used.
     *
     * @return Flag to indicate that persistent messages should be used.
     */
    public boolean getPersistentMode()
    {
        return getPropertyAsBoolean(PERSISTENT_MODE_PROPNAME);
    }

    /**
     * Flag to indicate that transactional messages should be sent by the publisher.
     *
     * @return Flag to indicate that transactional messages should be sent by the publisher.
     */
    public boolean getPublisherTransacted()
    {
        return getPropertyAsBoolean(TRANSACTED_PUBLISHER_PROPNAME);
    }

    /**
     * Flag to indicate that transactional receives should be used by the receiver.
     *
     * @return Flag to indicate that transactional receives should be used by the receiver.
     */
    public boolean getReceiverTransacted()
    {
        return getPropertyAsBoolean(TRANSACTED_PUBLISHER_PROPNAME);
    }

    /**
     * The name of the virtual host to run all tests over.
     *
     * @return The name of the virtual host to run all tests over.
     */
    public String getVirtualHost()
    {
        return getProperty(VIRTUAL_HOST_PROPNAME);
    }

    /**
     * Limiting rate for each sender in messages per second, or zero for unlimited.
     *
     * @return Limiting rate for each sender in messages per second, or zero for unlimited.
     */
    public String getRate()
    {
        return getProperty(RATE_PROPNAME);
    }

    /**
     * Flag to indicate that test messages should be received publish/subscribe style by all receivers.
     *
     * @return Flag to indicate that test messages should be received publish/subscribe style by all receivers.
     */
    public boolean getPubsub()
    {
        return getPropertyAsBoolean(PUBSUB_PROPNAME);
    }

    /**
     * The username credentials to run tests with.
     *
     * @return The username credentials to run tests with.
     */
    public String getUsername()
    {
        return getProperty(USERNAME_PROPNAME);
    }

    /**
     * The password credentials to run tests with.
     *
     * @return The password credentials to run tests with.
     */
    public String getPassword()
    {
        return getProperty(PASSWORD_PROPNAME);
    }

    /**
     * The timeout duration to fail tests on, should they receive no messages within it.
     *
     * @return The timeout duration to fail tests on, should they receive no messages within it.
     */
    public long getTimeout()
    {
        return getPropertyAsLong(TIMEOUT_PROPNAME);
    }

    /**
     * The number of messages to batch into each transaction in transational tests.
     *
     * @return The number of messages to batch into each transaction in transational tests.
     */
    public int getTxBatchSize()
    {
        return getPropertyAsInteger(TX_BATCH_SIZE_PROPNAME);
    }

    /**
     * Flag to indicate that tests should use durable destinations.
     *
     * @return Flag to indicate that tests should use durable destinations.
     */
    public boolean getDurableDests()
    {
        return getPropertyAsBoolean(DURABLE_DESTS_PROPNAME);
    }

    /**
     * The ack mode for message receivers to use.
     *
     * @return The ack mode for message receivers to use.
     */
    public int getAckMode()
    {
        return getPropertyAsInteger(ACK_MODE_PROPNAME);
    }

    /**
     * Flag to indicate that tests should use durable subscriptions.
     *
     * @return Flag to indicate that tests should use durable subscriptions.
     */
    public boolean getDurableSubscription()
    {
        return getPropertyAsBoolean(DURABLE_SUBSCRIPTION_PROPNAME);
    }

    /**
     * The maximum amount of in-flight data, in bytes, that tests should send at any time.
     *
     * @return The maximum amount of in-flight data, in bytes, that tests should send at any time.
     */
    public int getMaxPending()
    {
        return getPropertyAsInteger(MAX_PENDING_PROPNAME);
    }

    /**
     * The size of the prefetch queue to use.
     *
     * @return The size of the prefetch queue to use.
     */
    public int getPrefetch()
    {
        return getPropertyAsInteger(PREFETCH_PROPNAME);
    }

    /**
     * Flag to indicate that subscriptions should be no-local.
     *
     * @return Flag to indicate that subscriptions should be no-local.
     */
    public boolean getNoLocal()
    {
        return getPropertyAsBoolean(NO_LOCAL_PROPNAME);
    }

    /**
     * Flag to indicate that subscriptions should be exclusive.
     *
     * @return Flag to indicate that subscriptions should be exclusive.
     */
    public boolean getExclusive()
    {
        return getPropertyAsBoolean(EXCLUSIVE_PROPNAME);
    }

    /**
     * Flag to indicate that messages must be delivered immediately.
     *
     * @return Flag to indicate that messages must be delivered immediately.
     */
    public boolean getImmediate()
    {
        return getPropertyAsBoolean(IMMEDIATE_PROPNAME);
    }

    /**
     * Flag to indicate that messages must be routable.
     *
     * @return Flag to indicate that messages must be routable.
     */
    public boolean getMandatory()
    {
        return getPropertyAsBoolean(MANDATORY_PROPNAME);
    }

    /**
     * Gets the value of a flag to indicate that the publisher should rollback all messages sent.
     *
     * @return A flag to indicate that the publisher should rollback all messages sent.
     */
    public boolean getRollbackPublisher()
    {
        return getPropertyAsBoolean(ROLLBACK_PUBLISHER_PROPNAME);
    }

    /**
     * Gets the value of a flag to indicate that the receiver should rollback all messages received, then receive them
     * again.
     *
     * @return A flag to indicate that the publisher should rollback all messages received.
     */
    public boolean getRollbackReceiver()
    {
        return getPropertyAsBoolean(ROLLBACK_RECEIVER_PROPNAME);
    }

    /**
     * Gets the behavioural mode of not applicable assertions. Should be one of 'quiet', 'warn' or 'fail'.
     *
     * @return The behavioural mode of not applicable assertions.
     */
    public String getNotApplicableAssertionMode()
    {
        return getProperty(NOT_APPLICABLE_ASSERTION_PROPNAME);
    }
}
