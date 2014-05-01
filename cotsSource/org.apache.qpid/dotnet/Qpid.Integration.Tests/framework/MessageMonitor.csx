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
using Apache.Qpid.Messaging;

//using javax.jms.Message;
//using javax.jms.MessageListener;

//using java.util.concurrent.atomic.AtomicInteger;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// MessageMonitor is used to record information about messages received. This will provide methods to check various
    /// properties, such as the type, number and content of messages received in order to verify the correct behaviour of
    /// tests.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Count incoming messages.
    /// <tr><td> Record time ellapsed since the arrival of the first message.
    /// <tr><td> Reset all counts and timings.
    /// </table>
    /// </summary>
    public class MessageMonitor : MessageListener
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(MessageMonitor));

        /// <summary> Holds the count of messages received since the last query. </summary>
        protected AtomicInteger numMessages = new AtomicInteger();

        /// <summary> Holds the time of arrival of the first message. </summary>
        protected Long firstMessageTime = null;

        /// <summary>
        /// Handles received messages. Does Nothing.
        /// </summary>
        /// <param name="message"> The message. Ignored. </param>
        public void onMessage(Message message)
        {
            // log.debug("public void onMessage(Message message): called");

            numMessages.getAndIncrement();
        }

        /// <summary>
        /// Gets the count of messages.
        /// </summary>
        /// <return> The count of messages. </return>
        public int getNumMessage()
        {
            if (firstMessageTime == null)
            {
                firstMessageTime = System.nanoTime();
            }

            return numMessages.get();
        }

        /// <summary>
        /// Gets the time elapsed since the first message arrived, in nanos, or zero if no messages have arrived yet.
        /// </summary>
        /// <return> The time elapsed since the first message arrived, in nanos, or zero if no messages have arrived yet. </return>
        public long getTime()
        {
            if (firstMessageTime != null)
            {
                return System.nanoTime() - firstMessageTime;
            }
            else
            {
                return 0L;
            }
        }

        /// <summary> Resets the message count and timer to zero. </summary>
        public void reset()
        {
            numMessages.set(0);
            firstMessageTime = null;
        }
    }
}