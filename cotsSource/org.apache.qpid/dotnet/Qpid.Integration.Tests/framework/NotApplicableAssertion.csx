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
using log4net;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// NotApplicableAssertion is a messaging assertion that can be used when an assertion requested by a test-case is not
    /// applicable to the testing scenario. For example an assertion may relate to AMQP functionality, but a test case may be
    /// being run over a non-AMQP JMS implementation, in which case the request to create the assertion may return this
    /// instead of the proper assertion. The test framework is configurable to quietly drop these assertions, log them
    /// as warnings to the console, or raise them as test failures.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Quitely pass.
    /// <tr><td> Log a warning.
    /// <tr><td> Raise a test failure.
    /// </table>
    /// </summary>
    public class NotApplicableAssertion : Assertion
    {
        /// <summary> Used for logging to the console. </summary>
        private static ILog console = LogManager.GetLogger("CONSOLE." + NotApplicableAssertion.class.getName());

        /// <summary> The possible behavioural modes of this assertion. </summary>
        private enum Mode
        {
            /// <summary> Quietly ignore the assertion by passing. </summary>
            Quiet,

            /// <summary> Ignore the assertion by passing but log a warning about it. </summary>
            Warn,

            /// <summary> Fail the assertion. </summary>
            Fail;
        }

        /// <summary> The behavioural mode of the assertion. </summary>
        private Mode mode;

        /// <summary>
        /// Creates an assertion that is driven by the value of the 'notApplicableAssertion' property of the test
        /// configuration. Its value should match one of 'quiet', 'warn' or 'fail' and if it does not it is automatically
        /// read as 'fail'.
        /// </summary>
        /// <param name="testProperties"> The test configuration properties. </param>
        public NotApplicableAssertion(ParsedProperties testProperties)
        {
            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProperties);

            string modeName = props.getNotApplicableAssertionMode();

            if ("quiet".equals(modeName))
            {
                mode = Mode.Quiet;
            }
            else if ("warn".equals(modeName))
            {
                mode = Mode.Warn;
            }
            else
            {
                mode = Mode.Fail;
            }
        }

        /// <summary>
        /// Applies the assertion.
        /// </summary>
        /// <return> <tt>true</tt> if the assertion passes, <tt>false</tt> if it fails. </return>
        public bool apply()
        {
            switch (mode)
            {
            case Quiet:
                return true;

            case Warn:
                console.warn("Warning: Not applicable assertion being ignored.");

                return true;

            case Fail:
            default:
                return false;
            }
        }
    }
}