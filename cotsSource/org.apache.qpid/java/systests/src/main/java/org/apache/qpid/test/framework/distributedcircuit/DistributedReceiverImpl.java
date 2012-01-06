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
package org.apache.qpid.test.framework.distributedcircuit;

import org.apache.qpid.test.framework.Assertion;
import org.apache.qpid.test.framework.Receiver;

import org.apache.qpid.junit.extensions.util.ParsedProperties;

/**
 * DistributedReceiverImpl represents the status of the receiving side of a test circuit. Its main purpose is to
 * provide assertions that can be applied to verify the behaviour of a non-local receiver.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide assertion that the receivers received no exceptions.
 * <tr><td> Provide assertion that the receivers received all test messages sent to it.
 * </table>
 */
public class DistributedReceiverImpl implements Receiver
{
    /**
     * Provides an assertion that the receivers encountered no exceptions.
     *
     * @return An assertion that the receivers encountered no exceptions.
     * @param testProps
     */
    public Assertion noExceptionsAssertion(ParsedProperties testProps)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Provides an assertion that the receivers got all messages that were sent to it.
     *
     * @return An assertion that the receivers got all messages that were sent to it.
     * @param testProps
     */
    public Assertion allMessagesReceivedAssertion(ParsedProperties testProps)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Provides an assertion that the receivers got none of the messages that were sent to it.
     *
     * @return An assertion that the receivers got none of the messages that were sent to it.
     * @param testProps
     */
    public Assertion noMessagesReceivedAssertion(ParsedProperties testProps)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Provides an assertion that the AMQP channel was forcibly closed by an error condition.
     *
     * @param testProps The test configuration properties.
     * @return An assertion that the AMQP channel was forcibly closed by an error condition.
     */
    public Assertion channelClosedAssertion(ParsedProperties testProps)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
     * Provides an assertion that the receiver got a given exception during the test.
     *
     * @param testProps
     *@param exceptionClass The exception class to check for. @return An assertion that the receiver got a given exception during the test.
     */
    public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass)
    {
        throw new RuntimeException("Not implemented.");
    }
}

