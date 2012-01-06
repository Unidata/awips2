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
//using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;
using System;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A Receiver is a <see cref="CircuitEnd"/> that represents the status of the receiving side of a test circuit. Its main
    /// purpose is to provide assertions that can be applied to check the behaviour of the receivers.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide assertion that the receivers received no exceptions.
    /// <tr><td> Provide assertion that the receivers received all test messages sent to it.
    /// </table>
    /// </summary>
    public interface Receiver
    {
        /// <summary>
        /// Provides an assertion that the receivers encountered no exceptions.
        /// </summary>
        ///
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers encountered no exceptions. </return>
        Assertion NoExceptionsAssertion(TestModel testProps);

        /// <summary>
        /// Provides an assertion that the receivers got all messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers got all messages that were sent to it. </return>
        Assertion AllMessagesReceivedAssertion(TestModel testProps);

        /// <summary>
        /// Provides an assertion that the receivers got none of the messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers got none of the messages that were sent to it. </return>
        Assertion NoMessagesReceivedAssertion(TestModel testProps);

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        Assertion ChannelClosedAssertion(TestModel testProps);

        /// <summary>
        /// Provides an assertion that the receiver got a given exception during the test.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. </param>
        ///
        /// <return> An assertion that the receiver got a given exception during the test. </return>
        Assertion ExceptionAssertion(TestModel testProps, Type exceptionClass);
    }
}