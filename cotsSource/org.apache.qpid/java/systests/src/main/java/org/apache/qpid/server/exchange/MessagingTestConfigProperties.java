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
package org.apache.qpid.server.exchange;

import org.apache.qpid.jms.Session;
import org.apache.qpid.junit.extensions.util.ParsedProperties;

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
 * <tr><td> uniqueDests      <td> true     <td> Whether each receiver only listens to one ping destination or all.
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
 */
public class MessagingTestConfigProperties
{
    // ====================== Connection Properties ==================================

    /** Holds the name of the default connection configuration. */
    public static final String CONNECTION_NAME = "broker";

    /** Holds the name of the property to get the initial context factory name from. */
    public static final String INITIAL_CONTEXT_FACTORY_PROPNAME = "java.naming.factory.initial";

    /** Defines the class to use as the initial context factory by default. */
    public static final String INITIAL_CONTEXT_FACTORY_DEFAULT = "org.apache.qpid.jndi.PropertiesFileInitialContextFactory";

    /** Holds the name of the default connection factory configuration property. */
    public static final String CONNECTION_PROPNAME = "connectionfactory.broker";

    /** Defeins the default connection configuration. */
    public static final String CONNECTION_DEFAULT = "amqp://guest:guest@clientid/?brokerlist='vm://:1'";

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

    /** Holds the name of the property to get the bind receiver procuder flag from. */
    public static final String RECEIVER_PRODUCER_BIND_PROPNAME = "receiverProducerBind";

    /** Holds the default value of the receiver producer flag. */
    public static final boolean RECEIVER_PRODUCER_BIND_DEFAULT = false;

    /** Holds the name of the property to get the bind receiver procuder flag from. */
    public static final String RECEIVER_CONSUMER_BIND_PROPNAME = "receiverConsumerBind";

    /** Holds the default value of the receiver consumer flag. */
    public static final boolean RECEIVER_CONSUMER_BIND_DEFAULT = true;

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
    public static final String TRANSACTED_PROPNAME = "transacted";

    /** Holds the transactional mode to use for the test. */
    public static final boolean TRANSACTED_DEFAULT = false;

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

    // ======================  Qpid Options and Flags ================================

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

    /** Holds the name of the proeprty to set the prefetch size from. */
    public static final String PREFECTH_PROPNAME = "prefetch";

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

    /** Holds the name of the property to get the verbose mode proeprty from. */
    public static final String VERBOSE_PROPNAME = "verbose";

    /** Holds the default verbose mode. */
    public static final boolean VERBOSE_DEFAULT = false;

    /** Holds the default configuration properties. */
    public static ParsedProperties defaults = new ParsedProperties();

    static
    {
        defaults.setPropertyIfNull(INITIAL_CONTEXT_FACTORY_PROPNAME, INITIAL_CONTEXT_FACTORY_DEFAULT);
        defaults.setPropertyIfNull(CONNECTION_PROPNAME, CONNECTION_DEFAULT);
        defaults.setPropertyIfNull(MESSAGE_SIZE_PROPNAME, MESSAGE_SIZE_DEAFULT);
        defaults.setPropertyIfNull(PUBLISHER_PRODUCER_BIND_PROPNAME, PUBLISHER_PRODUCER_BIND_DEFAULT);
        defaults.setPropertyIfNull(PUBLISHER_CONSUMER_BIND_PROPNAME, PUBLISHER_CONSUMER_BIND_DEFAULT);
        defaults.setPropertyIfNull(RECEIVER_PRODUCER_BIND_PROPNAME, RECEIVER_PRODUCER_BIND_DEFAULT);
        defaults.setPropertyIfNull(RECEIVER_CONSUMER_BIND_PROPNAME, RECEIVER_CONSUMER_BIND_DEFAULT);
        defaults.setPropertyIfNull(SEND_DESTINATION_NAME_ROOT_PROPNAME, SEND_DESTINATION_NAME_ROOT_DEFAULT);
        defaults.setPropertyIfNull(RECEIVE_DESTINATION_NAME_ROOT_PROPNAME, RECEIVE_DESTINATION_NAME_ROOT_DEFAULT);
        defaults.setPropertyIfNull(PERSISTENT_MODE_PROPNAME, PERSISTENT_MODE_DEFAULT);
        defaults.setPropertyIfNull(TRANSACTED_PROPNAME, TRANSACTED_DEFAULT);
        defaults.setPropertyIfNull(BROKER_PROPNAME, BROKER_DEFAULT);
        defaults.setPropertyIfNull(VIRTUAL_HOST_PROPNAME, VIRTUAL_HOST_DEFAULT);
        defaults.setPropertyIfNull(RATE_PROPNAME, RATE_DEFAULT);
        defaults.setPropertyIfNull(VERBOSE_PROPNAME, VERBOSE_DEFAULT);
        defaults.setPropertyIfNull(PUBSUB_PROPNAME, PUBSUB_DEFAULT);
        defaults.setPropertyIfNull(USERNAME_PROPNAME, USERNAME_DEFAULT);
        defaults.setPropertyIfNull(PASSWORD_PROPNAME, PASSWORD_DEFAULT);
        defaults.setPropertyIfNull(SELECTOR_PROPNAME, SELECTOR_DEFAULT);
        defaults.setPropertyIfNull(DESTINATION_COUNT_PROPNAME, DESTINATION_COUNT_DEFAULT);
        defaults.setPropertyIfNull(TIMEOUT_PROPNAME, TIMEOUT_DEFAULT);
        defaults.setPropertyIfNull(TX_BATCH_SIZE_PROPNAME, TX_BATCH_SIZE_DEFAULT);
        defaults.setPropertyIfNull(DURABLE_DESTS_PROPNAME, DURABLE_DESTS_DEFAULT);
        defaults.setPropertyIfNull(ACK_MODE_PROPNAME, ACK_MODE_DEFAULT);
        defaults.setPropertyIfNull(DURABLE_SUBSCRIPTION_PROPNAME, DURABLE_SUBSCRIPTION_DEFAULT);
        defaults.setPropertyIfNull(MAX_PENDING_PROPNAME, MAX_PENDING_DEFAULT);
        defaults.setPropertyIfNull(PREFECTH_PROPNAME, PREFETCH_DEFAULT);
        defaults.setPropertyIfNull(NO_LOCAL_PROPNAME, NO_LOCAL_DEFAULT);
        defaults.setPropertyIfNull(EXCLUSIVE_PROPNAME, EXCLUSIVE_DEFAULT);
        defaults.setPropertyIfNull(IMMEDIATE_PROPNAME, IMMEDIATE_DEFAULT);
        defaults.setPropertyIfNull(MANDATORY_PROPNAME, MANDATORY_DEFAULT);
    }
}
