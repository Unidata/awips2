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

/**
 * A Publisher represents the status of the publishing side of a test circuit. Its main purpose is to provide assertions
 * that can be applied to test the behaviour of the publishers.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Provide assertion that the publishers received no exceptions.
 * </table>
 *
 * @todo There are mixtures of AMQP and JMS assertions in this interface. Either keep them here, but quietly (or with a
 *       warning or error) drop them from test cases where they are not relevant, or push them down into sub-classes.
 *       I am tempted to go with the dropping/warning/error approach, that would imply that it makes sense to pull
 *       the assertions back from AMQPPublisher to here.
 */
public interface Publisher
{
    // Assertions that are meaningfull to AMQP and to JMS.

    /**
     * Provides an assertion that the publisher encountered no exceptions.
     *
     * @param testProps The test configuration properties.
     *
     * @return An assertion that the publisher encountered no exceptions.
     */
    public Assertion noExceptionsAssertion(ParsedProperties testProps);

    // Assertions that are meaningfull only to AMQP.

    /**
     * Provides an assertion that the AMQP channel was forcibly closed by an error condition.
     *
     * @param testProps The test configuration properties.
     *
     * @return An assertion that the AMQP channel was forcibly closed by an error condition.
     */
    public Assertion channelClosedAssertion(ParsedProperties testProps);

    // Assertions that are meaningfull only to Java/JMS.

    /**
     * Provides an assertion that the publisher got a given exception during the test.
     *
     * @param testProps      The test configuration properties.
     * @param exceptionClass The exception class to check for.
     *
     * @return An assertion that the publisher got a given exception during the test.
     */
    public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass);
}
