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
using Apache.Qpid.Integration.Tests.framework.CauseFailure;

using java.io.IOException;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// Causes a message broker failure by interactively prompting the user to cause it.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Cause messaging broker failure.
    /// </table>
    /// </summary>
    public class CauseFailureUserPrompt : CauseFailure
    {
        /// <summary> Causes the active message broker to fail.</summary>
        public void causeFailure()
        {
            waitForUser("Cause a broker failure now, then press Return.");
        }

        /// <summary>
        /// Outputs a prompt to the console and waits for the user to press return.
        /// </summary>
        /// <param name="prompt"> The prompt to display on the console. </param>
        private void waitForUser(string prompt)
        {
            System.out.println(prompt);

            try
            {
                System.in.read();
            }
            catch (IOException e)
            {
                // Ignored.
            }

            System.out.println("Continuing.");
        }
    }
}