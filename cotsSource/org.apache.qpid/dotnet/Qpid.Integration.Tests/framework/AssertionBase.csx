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
using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// AssertionBase is a base class for implenmenting assertions. It provides a mechanism to store error messages, and
    /// report all error messages when its <see cref="#ToString()"/> method is called.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Collect error messages.
    /// </table>
    /// </summary>
    public abstract class AssertionBase : Assertion
    {
        /// <summary> Holds the error messages. </summary>
        IList<String> errors = new LinkedList<String>();

        /// <summary>
        /// Adds an error message to the assertion.
        /// </summary>
        /// <param name="error"> An error message to add to the assertion. </param>
        public void addError(string error)
        {
            errors.add(error);
        }

        /// <summary>
        /// Prints all of the error messages in the assertion into a string.
        /// </summary>
        /// <return> All of the error messages in the assertion as a string. </return>
        public string ToString()
        {
            string result = "";

            for (string error : errors)
            {
                result += error;
            }

            return result;
        }
    }
}