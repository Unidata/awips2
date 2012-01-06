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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// Assertion models an assertion on a test <see cref="Circuit"/>.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Indicate whether or not the assertion passes when applied.
    /// </table>
    /// </summary>
    public interface Assertion
    {
        /// <summary>
        /// Applies the assertion.
        /// </summary>
        /// <return> <tt>true</tt> if the assertion passes, <tt>false</tt> if it fails. </return>
        public bool apply();
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// BrokerLifecycleAware is an awareness interface implemented by test cases that can run control the life-cycle of
    /// the brokers on which they run. Its purpose is to expose additional instrumentation of brokers during testing, that
    /// enables tests to use an automated failure mechanism to simulate broker failures, and to re-start failed brokers.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Indicate whether or not a test case is using an in-vm broker.
    /// <tr><td> Track which in-vm broker is currently in use.
    /// <tr><td> Accept setting of a failure mechanism. <td> <see cref="CauseFailure"/>.
    /// </table>
    /// </summary>
    ///
    /// <remarks> Need to think about how to present the brokers through this interface. Thinking numbering the available
    ///       brokers from 1 will do. Then can kill 1 and assume failing onto 2. Restart 1 and kill 2 and fail back onto
    ///       1 again? </remarks>
    public interface BrokerLifecycleAware
    {
        public void setInVmBrokers();

        /// <summary>
        /// Indicates whether or not a test case is using in-vm brokers.
        /// </summary>
        /// <return> <tt>true</tt> if the test is using in-vm brokers, <tt>false</tt> otherwise. </return>
        public bool usingInVmBroker();

        /// <summary>
        /// Sets the currently live in-vm broker.
        /// </summary>
        /// <param name="i"> The currently live in-vm broker. </param>
        public void setLiveBroker(int i);

        /// <summary>
        /// Reports the currently live in-vm broker.
        /// </summary>
        /// <return> The currently live in-vm broker. </return>
        public int getLiveBroker();

        /// <summary>
        /// Accepts a failure mechanism.
        /// </summary>
        /// <param name="failureMechanism"> The failure mechanism. </param>
        public void setFailureMechanism(CauseFailure failureMechanism);
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// CauseFailure provides a method to cause a failure in a messaging broker, usually used in conjunction with fail-over
    /// or other failure mode testing. In some cases failures may be automated, for example by shutting down an in-vm broker,
    /// or by sending a special control signal to a broker over a network connection. In other cases, it may be preferable
    /// to ask a user interactively to cause a failure scenario, in which case an implementation may display a prompt or
    /// dialog box asking for notification once the failure has been caused. The purpose of this interface is to abstract
    /// the exact cause and nature of a failure out of failure test cases.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Cause messaging broker failure.
    /// </table>
    /// </summary>
    public interface CauseFailure
    {
        /// <summary> Causes the active message broker to fail. </summary>
        void causeFailure();
    }
}
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
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A Circuit is the basic test unit against which test cases are to be written. A circuit consists of two 'ends', an
    /// instigating 'publisher' end and a more passive 'receivers' end.
    ///
    /// <p/>Once created, the life-cycle of a circuit may be controlled by <see cref="#start()"/>ing it, or <see cref="#close()"/>ing it.
    /// Once started, the circuit is ready to send messages over. Once closed the circuit can no longer be used.
    ///
    /// <p/>The state of the circuit may be taken with the <see cref="#check()"/> method, and asserted against by the
    /// <see cref="#applyAssertions(System.Collections.Generic.IList)"/> method.
    ///
    /// <p/>There is a default test procedure which may be performed against the circuit. The outline of this procedure is:
    ///
    /// <p/><pre>
    /// Start the circuit.
    /// Send test messages.
    /// Request a status report.
    /// Assert conditions on the publishing end of the circuit.
    /// Assert conditions on the receiving end of the circuit.
    /// Close the circuit.
    /// Pass with no failed assertions or fail with a list of failed assertions.
    /// </pre>
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Supply the publishing and receiving ends of a test messaging circuit.
    /// <tr><td> Start the circuit running.
    /// <tr><td> Close the circuit down.
    /// <tr><td> Take a reading of the circuits state.
    /// <tr><td> Apply assertions against the circuits state.
    /// <tr><td> Send test messages over the circuit.
    /// <tr><td> Perform the default test procedue on the circuit.
    /// </table>
    /// </summary>
    public interface Circuit
    {
        /// <summary>
        /// Gets the interface on the publishing end of the circuit.
        /// </summary>
        /// <return> The publishing end of the circuit. </return>
        public Publisher getPublisher();

        /// <summary>
        /// Gets the interface on the receiving end of the circuit.
        /// </summary>
        /// <return> The receiving end of the circuit. </return>
        public Receiver getReceiver();

        /// <summary>
        /// Connects and starts the circuit. After this method is called the circuit is ready to send messages.
        public void start();

        /// <summary>
        /// Checks the test circuit. The effect of this is to gather the circuits state, for both ends of the circuit,
        /// into a report, against which assertions may be checked.
        public void check();

        /// <summary>
        /// Closes the circuit. All associated resources are closed.
        public void close();

        /// <summary>
        /// Applied a list of assertions against the test circuit. The <see cref="#check()"/> method should be called before doing
        /// this, to ensure that the circuit has gathered its state into a report to assert against.
        /// </summary>
        /// <param name="assertions"> The list of assertions to apply to the circuit. </param>
        ///
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> applyAssertions(List<Assertion> assertions);

        /// <summary>
        /// Runs the default test procedure against the circuit, and checks that all of the specified assertions hold.
        /// </summary>
        /// <param name="numMessages"> The number of messages to send using the default test procedure. </param>
        /// <param name="assertions">  The list of assertions to apply. </param>
        ///
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> test(int numMessages, List<Assertion> assertions);
    }
}
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
using javax.jms.*;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A CircuitEnd is a pair consisting of one message producer and one message consumer, that represents one end of a
    /// test circuit. It is a standard unit of connectivity allowing a full-duplex conversation to be held, provided both
    /// the consumer and producer are instantiated and configured.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide a message producer for sending messages.
    /// <tr><td> Provide a message consumer for receiving messages.
    /// </table>
    /// </summary>
    ///
    /// <remarks> Update the <see cref="org.apache.qpid.util.ConversationFactory"/> so that it accepts these as the basic conversation
    /// connection units.</remarks>
    public interface CircuitEnd
    {
        /// <summary>
        /// Gets the message producer at this circuit end point.
        /// </summary>
        /// <return> The message producer at with this circuit end point. </return>
        public MessageProducer getProducer();

        /// <summary>
        /// Gets the message consumer at this circuit end point.
        /// </summary>
        /// <return> The message consumer at this circuit end point. </return>
        public MessageConsumer getConsumer();

        /// <summary>
        /// Send the specified message over the producer at this end point.
        /// </summary>
        /// <param name="message"> The message to send. </param>
        ///
        /// <exception cref="JMSException"> Any JMS exception occuring during the send is allowed to fall through. </exception>
        public void send(Message message) throws JMSException;

        /// <summary>
        /// Gets the JMS Session associated with this circuit end point.
        /// </summary>
        /// <return> The JMS Session associated with this circuit end point. </return>
        public Session getSession();

        /// <summary>
        /// Closes the message producers and consumers and the sessions, associated with this circuit end point.
        /// </summary>
        /// <exception cref="JMSException"> Any JMSExceptions occurring during the close are allowed to fall through. </exception>
        public void close() throws JMSException;

        /// <summary>
        /// Returns the message monitor for reporting on received messages on this circuit end.
        /// </summary>
        /// <return> The message monitor for this circuit end. </return>
        public MessageMonitor getMessageMonitor();

        /// <summary>
        /// Returns the exception monitor for reporting on exceptions received on this circuit end.
        /// </summary>
        /// <return> The exception monitor for this circuit end. </return>
        public ExceptionMonitor getExceptionMonitor();
    }
}
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
using javax.jms.*;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A CircuitEndBase is a pair consisting of one message producer and one message consumer, that represents one end of a
    /// test circuit. It is a standard unit of connectivity allowing a full-duplex conversation to be held, provided both
    /// the consumer and producer are instantiated and configured.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide a message producer for sending messages.
    /// <tr><td> Provide a message consumer for receiving messages.
    /// </table>
    /// </summary>
    public class CircuitEndBase : CircuitEnd
    {
        /// <summary> Holds the single message producer. </summary>
        MessageProducer producer;

        /// <summary> Holds the single message consumer. </summary>
        MessageConsumer consumer;

        /// <summary> Holds the controlSession for the circuit end. </summary>
        Session session;

        /// <summary> Holds the message monitor for the circuit end. </summary>
        MessageMonitor messageMonitor;

        /// <summary> Holds the exception monitor for the circuit end. </summary>
        ExceptionMonitor exceptionMonitor;

        /// <summary>
        /// Creates a circuit end point on the specified producer, consumer and controlSession. Monitors are also configured
        /// for messages and exceptions received by the circuit end.
        /// </summary>
        /// <param name="producer">         The message producer for the circuit end point. </param>
        /// <param name="consumer">         The message consumer for the circuit end point. </param>
        /// <param name="session">          The controlSession for the circuit end point. </param>
        /// <param name="messageMonitor">   The monitor to notify of all messages received by the circuit end. </param>
        /// <param name="exceptionMonitor"> The monitor to notify of all exceptions received by the circuit end. </param>
        public CircuitEndBase(MessageProducer producer, MessageConsumer consumer, Session session, MessageMonitor messageMonitor,
                              ExceptionMonitor exceptionMonitor)
        {
            this.producer = producer;
            this.consumer = consumer;
            this.session = session;

            this.messageMonitor = messageMonitor;
            this.exceptionMonitor = exceptionMonitor;
        }

        /// <summary>
        /// Gets the message producer at this circuit end point.
        /// </summary>
        /// <return> The message producer at with this circuit end point. </return>
        public MessageProducer getProducer()
        {
            return producer;
        }

        /// <summary>
        /// Gets the message consumer at this circuit end point.
        /// </summary>
        /// <return> The message consumer at this circuit end point. </return>
        public MessageConsumer getConsumer()
        {
            return consumer;
        }

        /// <summary>
        /// Send the specified message over the producer at this end point.
        /// </summary>
        /// <param name="message"> The message to send. </param>
        /// <exception cref="javax.jms.JMSException"> Any JMS exception occuring during the send is allowed to fall through. </exception>
        public void send(Message message) throws JMSException
        {
            producer.send(message);
        }

        /// <summary>
        /// Gets the JMS Session associated with this circuit end point.
        /// </summary>
        /// <return> The JMS Session associated with this circuit end point. </return>
        public Session getSession()
        {
            return session;
        }

        /// <summary>
        /// Closes the message producers and consumers and the sessions, associated with this circuit end point.
        /// </summary>
        /// <exception cref="javax.jms.JMSException"> Any JMSExceptions occurring during the close are allowed to fall through. </exception>
        public void close() throws JMSException
        {
            if (producer != null)
            {
                producer.close();
            }

            if (consumer != null)
            {
                consumer.close();
            }
        }

        /// <summary>
        /// Returns the message monitor for reporting on received messages on this circuit end.
        /// </summary>
        /// <return> The message monitor for this circuit end. </return>
        public MessageMonitor getMessageMonitor()
        {
            return messageMonitor;
        }

        /// <summary>
        /// Returns the exception monitor for reporting on exceptions received on this circuit end.
        /// </summary>
        /// <return> The exception monitor for this circuit end. </return>
        public ExceptionMonitor getExceptionMonitor()
        {
            return exceptionMonitor;
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// ClockSynchFailureException represents failure of a <see cref="ClockSynchronizer"/> to achieve synchronization. For example,
    /// this could be because a reference signal is not available, or because a desired accurracy cannot be attained.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Represent failure to achieve synchronization.
    /// </table>
    /// </summary>
    public class ClockSynchFailureException extends Exception
    {
        /// <summary>
        /// Creates a clock synch failure exception.
        /// </summary>
        /// <param name="message"> The detail message (which is saved for later retrieval by the <see cref="#getMessage()"/> method). </param>
        /// <param name="cause">   The cause (which is saved for later retrieval by the <see cref="#getCause()"/> method).  (A <tt>null</tt>
        ///                        value is permitted, and indicates that the cause is nonexistent or unknown.)</param>
        public ClockSynchFailureException(string message, Throwable cause)
        {
            super(message, cause);
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// ClockSynchronizer provides an interface through which two nodes may synchronize their clocks. It is expected that one
    /// node will act as the reference clock, to which no delta need be applied, and the other node will act as the slave,
    /// and which must apply a delta to its local clock to get a clock synchronized with the reference.
    ///
    /// <p/>The slave side will initiate the computation of a clock delta by calling the <see cref="#synch"/> method. This method
    /// will not return until the delta has been computed, at which point there is a method to return its value, as well as
    /// an estimate of the likely error (usually one standard deviation), in the synchronization. For convenience there is a
    /// <see cref="#nanoTime"/> method to return the value of System.nanoTime() with the delta added in.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Trigger a clock synchronization.
    /// <tr><td> Compute a clock delta to apply to the local clock.
    /// <tr><td> Estimate the error in the synchronzation.
    /// </table>
    /// </summary>
    public interface ClockSynchronizer
    {
        /// <summary>
        /// The slave side should call this to copute a clock delta with the reference.
        /// </summary>
        /// <exception cref="ClockSynchFailureException"> If synchronization cannot be achieved. </exception>
        public void synch() throws ClockSynchFailureException;

        /// <summary>
        /// Gets the clock delta in nano seconds.
        /// </summary>
        /// <return> The clock delta in nano seconds. </return>
        public long getDelta();

        /// <summary>
        /// Gets an estimate of the clock error in nan seconds.
        /// </summary>
        /// <return> An estimate of the clock error in nan seconds. </return>
        public long getEpsilon();

        /// <summary>
        /// Gets the local clock time with any computed delta added in.
        /// </summary>
        /// <return> The local clock time with any computed delta added in. </return>
        public long nanoTime();
    }
}
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

using uk.co.thebadgerset.junit.extensions.ShutdownHookable;
using uk.co.thebadgerset.junit.extensions.Throttle;

namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// ClockSynchThread is a convenient utility for running a thread that periodically synchronizes the clock against
    /// a reference. Supply it with a <see cref="ClockSynchronizer"/> and a <see cref="Throttle"/> and it will continually keep the
    /// clock up-to-date at a rate determined by the throttle.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Continually sychronize the clock at a throttled rate.
    /// </table>
    /// </summary>
    public class ClockSynchThread extends Thread : ShutdownHookable
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(ClockSynchThread));

        /// <summary> Holds the clock syncher for the synch thread. </summary>
        private ClockSynchronizer clockSyncher;

        /// <summary> Holds the throttle to limit the synch rate. </summary>
        private Throttle throttle;

        /// <summary> Flag to indicate that the periodic clock syncher should keep running. </summary>
        bool doSynch = true;

        /// <summary>
        /// Creates a clock synchronizer thread from a clock synchronizer and a throttle.
        /// </summary>
        /// <param name="syncher">  The clock synchronizer. </param>
        /// <param name="throttle"> The throttle. </param>
        public ClockSynchThread(ClockSynchronizer syncher, Throttle throttle)
        {
            this.clockSyncher = syncher;
            this.throttle = throttle;
        }

        /// <summary> Terminates the synchronization thread. </summary>
        public void terminate()
        {
            doSynch = false;
        }

        /// <summary> Continually updates the clock, until <see cref="#terminate()"/> is called. </summary>
        public void run()
        {
            while (doSynch)
            {
                // Perform a clock clockSynch.
                try
                {
                    // Wait controlled by the throttle before doing the next synch.
                    throttle.throttle();

                    clockSyncher.synch();
                    log.debug("Clock synched, delta = " + clockSyncher.getDelta() + ", epsilon = " + clockSyncher.getEpsilon()
                              + ".");
                }
                // Terminate the synch thread if the synchronization cannot be achieved.
                catch (ClockSynchFailureException e)
                {
                    log.debug("Cannot synchronize the clock (reference service may be down). Terminating the synch thread.");
                    doSynch = false;
                }
            }
        }

        /// <summary>
        /// Gets the clock synchronizer that is kept continually up to date.
        /// </summary>
        /// <return> The clock synchronizer that is kept continually up to date. </return>
        public ClockSynchronizer getClockSyncher()
        {
            return clockSyncher;
        }

        /// <summary>
        /// Supplies a shutdown hook, that terminates the synching thread.
        /// </summary>
        /// <return> The shut down hook. </return>
        public Thread getShutdownHook()
        {
            return new Thread(new Runnable()
                {
                    public void run()
                    {
                        doSynch = false;
                    }
                });
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{

    /// <summary>
    /// LocalClockSynchronizer is a fake <see cref="ClockSynchronizer"/> that simply calls System.nanoTime(). It exists so that
    /// the same tests can be run distributed or locally, taking timings against the ClockSynchronizer interface without
    /// being aware of how they are being run.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the local clock with no delta.
    /// </table>
    /// </summary>
    public class LocalClockSynchronizer : ClockSynchronizer
    {
        /// <summary>
        /// The slave side should call this to copute a clock delta with the reference.
        /// </summary>
        /// <exception cref="Apache.Qpid.Integration.Tests.framework.clocksynch.ClockSynchFailureException"> If synchronization cannot be achieved. </exception>
        public void synch() throws ClockSynchFailureException
        { }

        /// <summary>
        /// Gets the clock delta in nano seconds.
        /// </summary>
        /// <return> The clock delta in nano seconds. </return>
        public long getDelta()
        {
            return 0L;
        }

        /// <summary>
        /// Gets an estimate of the clock error in nan seconds.
        /// </summary>
        /// <return> An estimate of the clock error in nan seconds. </return>
        public long getEpsilon()
        {
            return 0L;
        }

        /// <summary>
        /// Gets the local clock time with any computed delta added in.
        /// </summary>
        /// <return> The local clock time with any computed delta added in. </return>
        public long nanoTime()
        {
            return System.nanoTime();
        }
    }
}
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

using uk.co.thebadgerset.junit.extensions.ShutdownHookable;

using java.io.IOException;
using java.net.*;
using java.nio.ByteBuffer;

namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// UDPClockReference supplies a refernce clock signal (generated from System.nanoTime()).
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply a reference clock signal.
    /// </table>
    /// </summary>
    ///
    /// <remarks> Port hard coded. Make configurable.</remarks>
    ///
    /// <remarks> Errors rethrown as runtimes, or silently terminate the service. Could add better error handling if needed.</remarks>
    public class UDPClockReference : Runnable, ShutdownHookable
    {
        /// <summary> Used for debugging. </summary>
        // private static ILog log = LogManager.GetLogger(typeof(UDPClockReference));

        /// <summary> Defines the timeout to use when polling the socket for time requests. </summary>
        private static final int TIMEOUT = 200;

        /// <summary> Defines the port to run the clock reference on. </summary>
        public static final int REFERENCE_PORT = 4444;

        /// <summary> Holds the socket to receive clock reference requests on. </summary>
        protected DatagramSocket socket = null;

        /// <summary> Flag used to indicate that the time server should keep running. Set to false to terminate. </summary>
        protected bool publish = true;

        /// <summary> Creates a clock reference service on the standard port. </summary>
        public UDPClockReference()
        {
            try
            {
                socket = new DatagramSocket(REFERENCE_PORT);
                socket.setSoTimeout(TIMEOUT);
            }
            catch (SocketException e)
            {
                throw new RuntimeException(e);
            }
        }

        /// <summary>
        /// Implements the run loop for this reference time server. This waits for incoming time requests, and replies to
        /// any, with a message with the local time stamp in it. Periodically (controlled by <see cref="#TIMEOUT"/>), the run
        /// loop will check if the <see cref="#publish"/> flag has been cleared, and terminate the reference time service if so.
        /// </summary>
        public void run()
        {
            byte[] buf = new byte[256];
            ByteBuffer bbuf = ByteBuffer.wrap(buf);

            while (publish)
            {
                try
                {
                    // Wait for a reference time request.
                    DatagramPacket packet = new DatagramPacket(buf, buf.length);
                    bool timedOut = false;

                    try
                    {
                        socket.receive(packet);
                    }
                    catch (SocketTimeoutException e)
                    {
                        timedOut = true;
                    }

                    if (!timedOut)
                    {
                        // Work out from the received packet, where to reply to.
                        InetAddress address = packet.getAddress();
                        int port = packet.getPort();

                        // Respond to the time request by sending back the local clock as the reference time.
                        bbuf.putLong(System.nanoTime());
                        bbuf.flip();
                        packet = new DatagramPacket(bbuf.array(), bbuf.capacity(), address, port);

                        socket.send(packet);
                    }
                }
                catch (IOException e)
                {
                    publish = false;
                }
            }

            socket.close();
        }

        /// <summary>
        /// Supplies a shutdown hook.
        /// </summary>
        /// <return> The shut down hook. </return>
        public Thread getShutdownHook()
        {
            return new Thread(new Runnable()
                {
                    public void run()
                    {
                        publish = false;
                    }
                });
        }

        /// <summary>
        /// For testing purposes. Runs a reference clock on the default port.
        /// </summary>
        /// <param name="args"> None. </param>
        public static void main(String[] args)
        {
            try
            {
                // Create the clock reference service.
                UDPClockReference clock = new UDPClockReference();

                // Set up a shutdown hook for it.
                Runtime.getRuntime().addShutdownHook(clock.getShutdownHook());

                // Start the service.
                clock.run();
            }
            catch (Exception e)
            {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }
}
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

using uk.co.thebadgerset.junit.extensions.util.CommandLineParser;
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using java.io.IOException;
using java.net.*;
using java.nio.ByteBuffer;
using java.util.Arrays;

namespace Apache.Qpid.Integration.Tests.framework.clocksynch
{
    /// <summary>
    /// UDPClockSynchronizer is a <see cref="ClockSynchronizer"/> that sends pings as UDP datagrams, and uses the following simple
    /// algorithm to perform clock synchronization:
    ///
    /// <ol>
    /// <li>Slave initiates synchronization with a Reference clock.</li>
    /// <li>Slave stamps current local time on a "time request" message and sends to the Reference.</li>
    /// <li>Upon receipt by Reference, Reference stamps Reference-time and returns.</li>
    /// <li>Upon receipt by Slave, Slave subtracts current time from sent time and divides by two to compute latency. It
    ///     subtracts current time from Reference time to determine Slave-Reference time delta and adds in the
    ///     half-latency to get the correct clock delta.</li>
    /// <li>The first result is immediately used to update the clock since it will get the local clock into at least
    ///     the right ballpark.</li>
    /// <li>The Slave repeats steps 2 through 4, 15 more times.</li>
    /// <li>The results of the packet receipts are accumulated and sorted in lowest-latency to highest-latency order. The
    ///     median latency is determined by picking the mid-point sample from this ordered list.</li>
    /// <li>All samples outside 1 standard-deviation from the median are discarded and the remaining samples
    ///     are averaged using an arithmetic mean.</li>
    /// </ol>
    ///
    /// <p/>The use of UDP datagrams, instead of TCP based communication eliminates the hidden delays that TCP can introduce,
    /// as it can transparently re-order or re-send packets, or introduce delays as packets are naggled.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Trigger a clock synchronziation.
    /// <tr><td> Compute a clock delta to apply to the local clock.
    /// <tr><td> Estimate the error in the synchronzation.
    /// </table>
    /// </summary>
    public class UDPClockSynchronizer : ClockSynchronizer
    {
        /// <summary> Used for debugging. </summary>
        // private static ILog log = LogManager.GetLogger(typeof(UDPClockSynchronizer));

        /// <summary> Defines the timeout to use when waiting for responses to time requests. </summary>
        private static final int TIMEOUT = 50;

        /// <summary> The clock delta. </summary>
        private long delta = 0L;

        /// <summary> Holds an estimate of the clock error relative to the reference clock. </summary>
        private long epsilon = 0L;

        /// <summary> Holds the address of the reference clock. </summary>
        private InetAddress referenceAddress;

        /// <summary> Holds the socket to communicate with the reference service over. </summary>
        private DatagramSocket socket;

        /// <summary> Used to control the shutdown in the main test loop. </summary>
        private static bool doSynch = true;

        /// <summary>
        /// Creates a clock synchronizer against the specified address for the reference.
        /// </summary>
        /// <param name="address"> The address of the reference service. </param>
        public UDPClockSynchronizer(string address)
        {
            try
            {
                referenceAddress = InetAddress.getByName(address);
            }
            catch (UnknownHostException e)
            {
                throw new RuntimeException(e);
            }
        }

        /// <summary>
        /// The slave side should call this to compute a clock delta with the reference.
        /// </summary>
        /// <exception cref="ClockSynchFailureException"> If synchronization cannot be achieved, due to unavailability of the reference
        /// time service. </exception>
        public void synch() throws ClockSynchFailureException
        {
            try
            {
                socket = new DatagramSocket();
                socket.setSoTimeout(TIMEOUT);

                // Synchronize on a single ping, to get the clock into the right ball-park.
                synch(1);

                // Synchronize on 15 pings.
                synch(15);

                // And again, for greater accuracy, on 31.
                synch(31);

                socket.close();
            }
            catch (SocketException e)
            {
                throw new RuntimeException(e);
            }
        }

        /// <summary>
        /// Updates the synchronization delta by performing the specified number of reference clock requests.
        /// </summary>
        /// <param name="n"> The number of reference clock request cycles to perform. </param>
        ///
        /// <exception cref="ClockSynchFailureException"> If synchronization cannot be achieved, due to unavailability of the reference 
        ///                                               time service. </exception>
        protected void synch(int n) throws ClockSynchFailureException
        {
            // log.debug("protected void synch(int n = " + n + "): called");

            // Create an array of deltas by performing n reference pings.
            long[] delta = new long[n];

            for (int i = 0; i < n; i++)
            {
                delta[i] = ping();
            }

            // Reject any deltas that are larger than 1 s.d. above the median.
            long median = median(delta);
            long sd = standardDeviation(delta);

            // log.debug("median = " + median);
            // log.debug("sd = " + sd);

            long[] tempDeltas = new long[n];
            int count = 0;

            for (int i = 0; i < n; i++)
            {
                if ((delta[i] <= (median + sd)) && (delta[i] >= (median - sd)))
                {
                    tempDeltas[count] = delta[i];
                    count++;
                }
                else
                {
                    // log.debug("Rejected: " + delta[i]);
                }
            }

            System.arraycopy(tempDeltas, 0, delta, 0, count);

            // Estimate the delta as the mean of the remaining deltas.
            this.delta += mean(delta);

            // Estimate the error as the standard deviation of the remaining deltas.
            this.epsilon = standardDeviation(delta);

            // log.debug("this.delta = " + this.delta);
            // log.debug("this.epsilon = " + this.epsilon);
        }

        /// <summary>
        /// Performs a single reference clock request cycle and returns the estimated delta relative to the local clock.
        /// This is computed as the half-latency of the requst cycle, plus the reference clock, minus the local clock.
        /// </summary>
        /// <return> The estimated clock delta. </return>
        ///
        /// <exception cref="ClockSynchFailureException"> If the reference service is not responding. </exception>
        protected long ping() throws ClockSynchFailureException
        {
            // log.debug("protected long ping(): called");

            try
            {
                byte[] buf = new byte[256];

                bool timedOut = false;
                long start = 0L;
                long refTime = 0L;
                long localTime = 0L;
                long latency = 0L;
                int failCount = 0;

                // Keep trying the ping until it gets a response, or 10 tries in a row all time out.
                do
                {
                    // Start timing the request latency.
                    start = nanoTime();

                    // Get the reference time.
                    DatagramPacket packet =
                        new DatagramPacket(buf, buf.length, referenceAddress, UDPClockReference.REFERENCE_PORT);
                    socket.send(packet);
                    packet = new DatagramPacket(buf, buf.length);

                    timedOut = false;

                    try
                    {
                        socket.receive(packet);
                    }
                    catch (SocketTimeoutException e)
                    {
                        timedOut = true;
                        failCount++;

                        continue;
                    }

                    ByteBuffer bbuf = ByteBuffer.wrap(packet.getData());
                    refTime = bbuf.getLong();

                    // Stop timing the request latency.
                    localTime = nanoTime();
                    latency = localTime - start;

                    // log.debug("refTime = " + refTime);
                    // log.debug("localTime = " + localTime);
                    // log.debug("start = " + start);
                    // log.debug("latency = " + latency);
                    // log.debug("delta = " + ((latency / 2) + (refTime - localTime)));

                }
                while (timedOut && (failCount < 10));

                // Fail completely if the fail count is too high.
                if (failCount >= 10)
                {
                    throw new ClockSynchFailureException("Clock reference not responding.", null);
                }

                // Estimate delta as (ref clock + half-latency) - local clock.
                return (latency / 2) + (refTime - localTime);
            }
            catch (IOException e)
            {
                throw new RuntimeException(e);
            }
        }

        /// <summary>
        /// Gets the clock delta in nano seconds.
        /// </summary>
        /// <return> The clock delta in nano seconds. </return>
        public long getDelta()
        {
            return delta;
        }

        /// <summary>
        /// Gets an estimate of the clock error in nan seconds.
        /// </summary>
        /// <return> An estimate of the clock error in nan seconds. </return>
        public long getEpsilon()
        {
            return epsilon;
        }

        /// <summary>
        /// Gets the local clock time with any computed delta added in.
        /// </summary>
        /// <return> The local clock time with any computed delta added in. </return>
        public long nanoTime()
        {
            return System.nanoTime() + delta;
        }

        /// <summary>
        /// Computes the median of a series of values.
        /// </summary>
        /// <param name="values"> The values. </param>
        ///
        /// <return> The median. </return>
        public static long median(long[] values)
        {
            // log.debug("public static long median(long[] values = " + Arrays.ToString(values) + "): called");

            long median;

            // Order the list of values.
            long[] orderedValues = new long[values.length];
            System.arraycopy(values, 0, orderedValues, 0, values.length);
            Arrays.sort(orderedValues);

            // Check if the median is computed from a pair of middle value.
            if ((orderedValues.length % 2) == 0)
            {
                int middle = orderedValues.length / 2;

                median = (orderedValues[middle] + orderedValues[middle - 1]) / 2;
            }
            // The median is computed from a single middle value.
            else
            {
                median = orderedValues[orderedValues.length / 2];
            }

            // log.debug("median = " + median);

            return median;
        }

        /// <summary>
        /// Computes the mean of a series of values.
        /// </summary>
        /// <param name="values"> The values. </param>
        ///
        /// <return> The mean. </return>
        public static long mean(long[] values)
        {
            // log.debug("public static long mean(long[] values = " + Arrays.ToString(values) + "): called");

            long total = 0L;

            for (long value : values)
            {
                total += value;
            }

            long mean = total / values.length;

            // log.debug("mean = " + mean);

            return mean;
        }

        /// <summary>
        /// Computes the variance of series of values.
        /// </summary>
        /// <param name="values"> The values. </param>
        ///
        /// <return> The variance of the values. </return>
        public static long variance(long[] values)
        {
            // log.debug("public static long variance(long[] values = " + Arrays.ToString(values) + "): called");

            long mean = mean(values);

            long totalVariance = 0;

            for (long value : values)
            {
                long diff = (value - mean);
                totalVariance += diff/// diff;
                    }

            long variance = totalVariance / values.length;

            // log.debug("variance = " + variance);

            return variance;
        }

        /// <summary>
        /// Computes the standard deviation of a series of values.
        /// </summary>
        /// <param name="values"> The values. </param>
        ///
        /// <return> The standard deviation. </return>
        public static long standardDeviation(long[] values)
        {
            // log.debug("public static long standardDeviation(long[] values = " + Arrays.ToString(values) + "): called");

            long sd = Double.valueOf(Math.sqrt(variance(values))).longValue();

            // log.debug("sd = " + sd);

            return sd;
        }

        /// <summary>
        /// For testing purposes. Supply address of reference clock as arg 1.
        /// </summary>
        /// <param name="args"> Address of reference clock as arg 1. </param>
        public static void main(String[] args)
        {
            ParsedProperties options =
                new ParsedProperties(CommandLineParser.processCommandLine(args,
                                                                          new CommandLineParser(
                                                                                                new String[][]
                                                                                                {
                                                                                                    { "1", "Address of clock reference service.", "address", "true" }
                                                                                                }), System.getProperties()));

            string address = options.getProperty("1");

            // Create a clock synchronizer.
            UDPClockSynchronizer clockSyncher = new UDPClockSynchronizer(address);

            // Set up a shutdown hook for it.
            Runtime.getRuntime().addShutdownHook(new Thread(new Runnable()
                {
                    public void run()
                    {
                        doSynch = false;
                    }
                }));

            // Repeat the clock synching until the user kills the progam.
            while (doSynch)
            {
                // Perform a clock clockSynch.
                try
                {
                    clockSyncher.synch();

                    // Print out the clock delta and estimate of the error.
                    System.out.println("Delta = " + clockSyncher.getDelta());
                    System.out.println("Epsilon = " + clockSyncher.getEpsilon());

                    try
                    {
                        Thread.sleep(250);
                    }
                    catch (InterruptedException e)
                    {
                        // Restore the interrupted status and terminate the loop.
                        Thread.currentThread().interrupt();
                        doSynch = false;
                    }
                }
                // Terminate if the reference time service is unavailable.
                catch (ClockSynchFailureException e)
                {
                    doSynch = false;
                }
            }
        }
    }
}
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

using Apache.Qpid.Integration.Tests.framework.*;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.TimingController;
using uk.co.thebadgerset.junit.extensions.TimingControllerAware;
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.Destination;
using javax.jms.JMSException;
using javax.jms.Message;
using javax.jms.Session;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework.distributedcircuit
{
    /// <summary>
    /// DistributedCircuitImpl is a distributed implementation of the test <see cref="Circuit"/>. Many publishers and receivers
    /// accross multiple machines may be combined to form a single test circuit. The test circuit extracts reports from
    /// all of its publishers and receivers, and applies its assertions to these reports.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the publishing and receiving ends of a test messaging circuit.
    /// <tr><td> Start the circuit running.
    /// <tr><td> Close the circuit down.
    /// <tr><td> Take a reading of the circuits state.
    /// <tr><td> Apply assertions against the circuits state.
    /// <tr><td> Send test messages over the circuit.
    /// <tr><td> Perform the default test procedue on the circuit.
    /// </table>
    /// </summary>
    ///
    /// <remarks> There is a short pause after receiving sender reports before asking for receiver reports, because receivers may
    ///       not have finished receiving all their test messages before the report request arrives. This is going to be a
    ///       problem for taking test timings and needs to be eliminiated. Suggested solution: have receiver send back reports
    ///       asynchronously, on test batch size boundaries, and do so automatically rather than having to have the report
    ///       request sent to them. Number each test run, or otherwise uniquely identify it, when a receiver does not get
    ///       any more messages on a test run for more than a timeout, it can assume the test is complete and send a final
    ///       report. On the coordinator end a future will need to be created to wait for all final reports to come in, and
    ///       to register results and timings for the test. This must work in such a way that a new test cycle can be started
    ///       without waiting for the results of the old one to come in.</remarks>
    ///
    /// <remarks> Add in setting of timing controller, from timing aware test cases.</remarks>
    public class DistributedCircuitImpl : Circuit, TimingControllerAware
    {
        /// <summary> Used for debugging purposes. </summary>
        private static ILog log = LogManager.GetLogger(typeof(DistributedCircuitImpl));

        /// <summary> Holds the conversation factory over which to coordinate the test. </summary>
        protected ConversationFactory conversationFactory;

        /// <summary> Holds the controlSession over which to hold the control conversation. </summary>
        protected Session controlSession;

        /// <summary> Holds the sender nodes in the test circuit. </summary>
        protected IList<TestClientDetails> senders;

        /// <summary> Holds the receiver nodes in the test circuit. </summary>
        protected IList<TestClientDetails> receivers;

        /// <summary> Holds the sender control conversations. </summary>
        protected ConversationFactory.Conversation[] senderConversation;

        /// <summary> Holds the receiver control conversations. </summary>
        protected ConversationFactory.Conversation[] receiverConversation;

        /// <summary> Holds the control topics for the senders in the test circuit. </summary>
        protected Destination[] senderControlTopic;

        /// <summary> Holds the control topics for the receivers in the test circuit. </summary>
        protected Destination[] receiverControlTopic;

        /// <summary> Holds the number of messages to send per test run. </summary>
        protected int numMessages;

        /// <summary>
        /// Holds the timing controller for the circuit. This is used to log test times asynchronously, when reciever nodes
        /// return their reports after senders have completed a test case.
        TimingController timingController;

        /// <summary>
        /// Creates a distributed test circuit on the specified senders and receivers.
        /// </summary>
        /// <param name="session">              The controlSession for all control conversations. </param>
        /// <param name="senders">              The senders. </param>
        /// <param name="receivers">            The receivers. </param>
        /// <param name="senderConversation">   A control conversation with the senders. </param>
        /// <param name="receiverConversation"> A control conversation with the receivers. </param>
        /// <param name="senderControlTopic">   The senders control topic. </param>
        /// <param name="receiverControlTopic"> The receivers control topic. </param>
        protected DistributedCircuitImpl(Session session, IList<TestClientDetails> senders, List<TestClientDetails> receivers,
                                         ConversationFactory.Conversation[] senderConversation, ConversationFactory.Conversation[] receiverConversation,
                                         Destination[] senderControlTopic, Destination[] receiverControlTopic)
        {
            this.controlSession = session;
            this.senders = senders;
            this.receivers = receivers;
            this.senderConversation = senderConversation;
            this.receiverConversation = receiverConversation;
            this.senderControlTopic = senderControlTopic;
            this.receiverControlTopic = receiverControlTopic;
        }

        /// <summary>
        /// Creates a distributed test circuit from the specified test parameters, on the senders and receivers
        /// given.
        /// </summary>
        /// <param name="testProps">           The test parameters. </param>
        /// <param name="senders">             The sender ends in the test circuit. </param>
        /// <param name="receivers">           The receiver ends in the test circuit. </param>
        /// <param name="conversationFactory"> A conversation factory for creating the control conversations with senders and receivers. </param>
        ///
        /// <return> A connected and ready to start, test circuit. </return>
        public static Circuit createCircuit(ParsedProperties testProps, IList<TestClientDetails> senders,
                                            IList<TestClientDetails> receivers, ConversationFactory conversationFactory)
        {
            log.debug("public static Circuit createCircuit(ParsedProperties testProps, IList<TestClientDetails> senders, "
                      + " IList<TestClientDetails> receivers, ConversationFactory conversationFactory)");

            try
            {
                Session session = conversationFactory.getSession();

                // Create control conversations with each of the senders.
                ConversationFactory.Conversation[] senderConversation = new ConversationFactory.Conversation[senders.size()];
                Destination[] senderControlTopic = new Destination[senders.size()];

                for (int i = 0; i < senders.size(); i++)
                {
                    TestClientDetails sender = senders.get(i);

                    senderControlTopic[i] = session.createTopic(sender.privateControlKey);
                    senderConversation[i] = conversationFactory.startConversation();
                }

                log.debug("Sender conversations created.");

                // Create control conversations with each of the receivers.
                ConversationFactory.Conversation[] receiverConversation = new ConversationFactory.Conversation[receivers.size()];
                Destination[] receiverControlTopic = new Destination[receivers.size()];

                for (int i = 0; i < receivers.size(); i++)
                {
                    TestClientDetails receiver = receivers.get(i);

                    receiverControlTopic[i] = session.createTopic(receiver.privateControlKey);
                    receiverConversation[i] = conversationFactory.startConversation();
                }

                log.debug("Receiver conversations created.");

                // Assign the sender role to each of the sending test clients.
                for (int i = 0; i < senders.size(); i++)
                {
                    TestClientDetails sender = senders.get(i);

                    Message assignSender = conversationFactory.getSession().createMessage();
                    TestUtils.setPropertiesOnMessage(assignSender, testProps);
                    assignSender.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                    assignSender.setStringProperty("ROLE", "SENDER");

                    senderConversation[i].send(senderControlTopic[i], assignSender);
                }

                log.debug("Sender role assignments sent.");

                // Assign the receivers role to each of the receiving test clients.
                for (int i = 0; i < receivers.size(); i++)
                {
                    TestClientDetails receiver = receivers.get(i);

                    Message assignReceiver = session.createMessage();
                    TestUtils.setPropertiesOnMessage(assignReceiver, testProps);
                    assignReceiver.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                    assignReceiver.setStringProperty("ROLE", "RECEIVER");

                    receiverConversation[i].send(receiverControlTopic[i], assignReceiver);
                }

                log.debug("Receiver role assignments sent.");

                // Wait for the senders and receivers to confirm their roles.
                for (int i = 0; i < senders.size(); i++)
                {
                    senderConversation[i].receive();
                }

                log.debug("Got all sender role confirmations");

                for (int i = 0; i < receivers.size(); i++)
                {
                    receiverConversation[i].receive();
                }

                log.debug("Got all receiver role confirmations");

                // Package everything up as a circuit.
                return new DistributedCircuitImpl(session, senders, receivers, senderConversation, receiverConversation,
                                                  senderControlTopic, receiverControlTopic);
            }
            catch (JMSException e)
            {
                throw new RuntimeException("JMSException not handled.");
            }
        }

        /// <summary>
        /// Used by tests cases that can supply a <see cref="uk.co.thebadgerset.junit.extensions.TimingController"/> to set the
        /// controller on an aware test.
        /// </summary>
        /// <param name="controller"> The timing controller. </param>
        public void setTimingController(TimingController controller)
        {
            this.timingController = controller;
        }

        /// <summary>
        /// Gets the interface on the publishing end of the circuit.
        /// </summary>
        /// <return> The publishing end of the circuit. </return>
        public Publisher getPublisher()
        {
            throw new RuntimeException("Not Implemented.");
        }

        /// <summary>
        /// Gets the interface on the receiving end of the circuit.
        /// </summary>
        /// <return> The receiving end of the circuit. </return>
        public Receiver getReceiver()
        {
            throw new RuntimeException("Not Implemented.");
        }

        /// <summary>
        /// Connects and starts the circuit. After this method is called the circuit is ready to send messages.
        public void start()
        {
            log.debug("public void start(): called");

            try
            {
                // Start the test on each of the senders.
                Message start = controlSession.createMessage();
                start.setStringProperty("CONTROL_TYPE", "START");
                start.setIntProperty("MESSAGE_COUNT", numMessages);

                for (int i = 0; i < senders.size(); i++)
                {
                    senderConversation[i].send(senderControlTopic[i], start);
                }

                log.debug("All senders told to start their tests.");
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Unhandled JMSException.", e);
            }
        }

        /// <summary>
        /// Checks the test circuit. The effect of this is to gather the circuits state, for both ends of the circuit,
        /// into a report, against which assertions may be checked.
        /// </summary>
        /// <remarks> Replace the asynch receiver report thread with a choice of direct or asynch executor, so that asynch
        ///       or synch logging of test timings is optional. Also need to provide an onMessage method that is capable
        ///       of receiving timing reports that receivers will generate during an ongoing test, on the test sample
        ///       size boundaries. The message timing logging code should be factored out as a common method that can
        ///       be called in response to the final report responses, or the onMessage method. Another alternative is
        ///       to abandon the final report request altogether and just use the onMessage method? I think the two
        ///       differ though, as the final report is used to apply assertions, and the ongoing report is just for
        ///       periodic timing results... In which case, maybe there needs to be a way for the onMessage method
        ///       to process just some of the incoming messages, and forward the rest on to the conversion helper, as
        ///       a sort of pre-conversation helper filter? Make conversation expose its onMessage method (it should
        ///       already) and allow another delivery thread to filter the incoming messages to the conversation.</remarks>
        public void check()
        {
            log.debug("public void check(): called");

            try
            {
                // Wait for all the test senders to return their reports.
                for (int i = 0; i < senders.size(); i++)
                {
                    Message senderReport = senderConversation[i].receive();
                    log.debug("Sender " + senderReport.getStringProperty("CLIENT_NAME") + " reports message count: "
                              + senderReport.getIntProperty("MESSAGE_COUNT"));
                    log.debug("Sender " + senderReport.getStringProperty("CLIENT_NAME") + " reports message time: "
                              + senderReport.getLongProperty("TEST_TIME"));
                }

                log.debug("Got all sender test reports.");

                // Apply sender assertions to pass/fail the tests.

                // Inject a short pause to give the receivers time to finish receiving their test messages.
                TestUtils.pause(500);

                // Ask the receivers for their reports.
                Message statusRequest = controlSession.createMessage();
                statusRequest.setStringProperty("CONTROL_TYPE", "STATUS_REQUEST");

                for (int i = 0; i < receivers.size(); i++)
                {
                    receiverConversation[i].send(receiverControlTopic[i], statusRequest);
                }

                log.debug("All receiver test reports requested.");

                // Wait for all receiver reports to come in, but do so asynchronously.
                Runnable gatherAllReceiverReports =
                    new Runnable()
                    {
                        public void run()
                        {
                            try
                            {
                                // Wait for all the receivers to send their reports.
                                for (int i = 0; i < receivers.size(); i++)
                                {
                                    Message receiverReport = receiverConversation[i].receive();

                                    string clientName = receiverReport.getStringProperty("CLIENT_NAME");
                                    int messageCount = receiverReport.getIntProperty("MESSAGE_COUNT");
                                    long testTime = receiverReport.getLongProperty("TEST_TIME");

                                    log.debug("Receiver " + clientName + " reports message count: " + messageCount);
                                    log.debug("Receiver " + receiverReport.getStringProperty("CLIENT_NAME")
                                              + " reports message time: " + testTime);

                                    // Apply receiver assertions to pass/fail the tests.

                                    // Log the test timings on the asynchronous test timing controller.
                                    /*try
                                      {
                                      timingController.completeTest(true, messageCount, testTime);
                                      }
                                      // The timing controll can throw InterruptedException is the current test is to be
                                      // interrupted.
                                      catch (InterruptedException e)
                                      {
                                      e.printStackTrace();
                                      }*/
                                }

                                log.debug("All receiver test reports received.");
                            }
                            catch (JMSException e)
                            {
                                throw new RuntimeException(e);
                            }
                        }
                    };

                Thread receiverReportsThread = new Thread(gatherAllReceiverReports);
                receiverReportsThread.start();

                // return new Message[] { senderReport, receiverReport };

            }
            catch (JMSException e)
            {
                throw new RuntimeException("Unhandled JMSException.", e);
            }
        }

        /// <summary> Closes the circuit. All associated resources are closed. </summary>
        public void close()
        {
            log.debug("public void close(): called");

            // End the current test on all senders and receivers.
        }

        /// <summary>
        /// Applies a list of assertions against the test circuit. The <see cref="#check()"/> method should be called before doing
        /// this, to ensure that the circuit has gathered its state into a report to assert against.
        /// </summary>
        /// <param name="assertions"> The list of assertions to apply. </param>
        ///
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> applyAssertions(List<Assertion> assertions)
        {
            log.debug("public IList<Assertion> applyAssertions(List<Assertion> assertions = " + assertions + "): called");

            IList<Assertion> failures = new LinkedList<Assertion>();

            for (Assertion assertion : assertions)
            {
                if (!assertion.apply())
                {
                    failures.add(assertion);
                }
            }

            return failures;
        }

        /// <summary>
        /// Runs the default test procedure against the circuit, and checks that all of the specified assertions hold.
        /// </summary>
        /// <param name="numMessages"> The number of messages to send using the default test procedure. </param>
        /// <param name="assertions">  The list of assertions to apply. </param>
        ///
        /// <return> Any assertions that failed. </return>
        ///
        /// <remarks> From check onwards needs to be handled as a future. The future must call back onto the test case to
        ///       report results asynchronously.</remarks>
        public IList<Assertion> test(int numMessages, List<Assertion> assertions)
        {
            log.debug("public IList<Assertion> test(int numMessages = " + numMessages + ", List<Assertion> assertions = "
                      + assertions + "): called");

            // Keep the number of messages to send per test run, where the send method can reference it.
            this.numMessages = numMessages;

            // Start the test running on all sender circuit ends.
            start();

            // Request status reports to be handed in.
            check();

            // Assert conditions on the publishing end of the circuit.
            // Assert conditions on the receiving end of the circuit.
            IList<Assertion> failures = applyAssertions(assertions);

            // Close the circuit ending the current test case.
            close();

            // Pass with no failed assertions or fail with a list of failed assertions.
            return failures;
        }
    }
}
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
using Apache.Qpid.Integration.Tests.framework.Assertion;
using Apache.Qpid.Integration.Tests.framework.Publisher;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

namespace Apache.Qpid.Integration.Tests.framework.distributedcircuit
{
    /// <summary>
    /// DistributedPublisherImpl represents the status of the publishing side of a test circuit. Its main purpose is to
    /// provide assertions that can be applied to verify the behaviour of a non-local publisher.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide assertion that the publishers received no exceptions.
    /// <tr><td> Provide assertion that the publishers received a no consumers error code on every message.
    /// <tr><td> Provide assertion that the publishers received a no route error code on every message.
    /// </table>
    /// </summary>
    public class DistributedPublisherImpl : Publisher
    {
        /// <summary>
        /// Provides an assertion that the publisher encountered no exceptions.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <return> An assertion that the publisher encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the publisher got a no consumers exception on every message.
        /// </summary>
        /// <return> An assertion that the publisher got a no consumers exception on every message. </return>
        public Assertion noConsumersAssertion()
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the publisher got a no rout exception on every message.
        /// </summary>
        /// <return> An assertion that the publisher got a no rout exception on every message. </return>
        public Assertion noRouteAssertion()
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the publisher got a given exception during the test.
        /// </summary>
        /// <param name="testProps">      The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. </param>
        /// <return> An assertion that the publisher got a given exception during the test. </return>
        public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass)
        {
            throw new RuntimeException("Not implemented.");
        }
    }
}
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
using Apache.Qpid.Integration.Tests.framework.Assertion;
using Apache.Qpid.Integration.Tests.framework.Receiver;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

namespace Apache.Qpid.Integration.Tests.framework.distributedcircuit
{
    /// <summary>
    /// DistributedReceiverImpl represents the status of the receiving side of a test circuit. Its main purpose is to
    /// provide assertions that can be applied to verify the behaviour of a non-local receiver.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide assertion that the receivers received no exceptions.
    /// <tr><td> Provide assertion that the receivers received all test messages sent to it.
    /// </table>
    /// </summary>
    public class DistributedReceiverImpl : Receiver
    {
        /// <summary>
        /// Provides an assertion that the receivers encountered no exceptions.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <return> An assertion that the receivers encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the receivers got all messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <return> An assertion that the receivers got all messages that were sent to it. </return>
        public Assertion allMessagesReceivedAssertion(ParsedProperties testProps)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the receivers got none of the messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <return> An assertion that the receivers got none of the messages that were sent to it. </return>
        public Assertion noMessagesReceivedAssertion(ParsedProperties testProps)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Provides an assertion that the receiver got a given exception during the test.
        ///
        /// <param name="testProps"> The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. <return> An assertion that the receiver got a given exception during the test. </return> </param>
        public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass)
        {
            throw new RuntimeException("Not implemented.");
        }
    }
}
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

using Apache.Qpid.Integration.Tests.framework.*;
using Apache.Qpid.Integration.Tests.framework.distributedtesting.TestClientControlledTest;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;
using uk.co.thebadgerset.junit.extensions.util.TestContextProperties;

using javax.jms.*;

namespace Apache.Qpid.Integration.Tests.framework.distributedcircuit
{
    /// <summary>
    /// A TestClientCircuitEnd is a <see cref="CircuitEnd"/> that may be controlled from a
    /// <see cref="Apache.Qpid.Integration.Tests.framework.distributedtesting.TestClient"/>, and that forms a single publishing or
    /// receiving end point in a distributed test <see cref="Apache.Qpid.Integration.Tests.framework.Circuit"/>.
    ///
    /// <p/>When operating in the SENDER role, this circuit end is capable of acting as part of the default circuit test
    /// procedure (described in the class comment for <see cref="Apache.Qpid.Integration.Tests.framework.Circuit"/>). That is, it will
    /// send the number of test messages required, using the test configuration parameters given in the test invite, and
    /// return a report on its activities to the circuit controller.
    ///
    /// <p/>When operation in the RECEIVER role, this circuit end acts as part of the default circuit test procedure. It will
    /// receive test messages, on the setup specified in the test configuration parameters, and keep count of the messages
    /// received, and time taken to receive them. When requested by the circuit controller to provide a report, it will
    /// return this report of its activities.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide a message producer for sending messages.
    ///     <td> <see cref="CircuitEnd"/>, <see cref="LocalCircuitFactory"/>, <see cref="TestUtils"/>
    /// <tr><td> Provide a message consumer for receiving messages.
    ///     <td> <see cref="CircuitEnd"/>, <see cref="LocalCircuitFactory"/>, <see cref="TestUtils"/>
    /// <tr><td> Supply the name of the test case that this implements.
    /// <tr><td> Accept/Reject invites based on test parameters. <td> <see cref="MessagingTestConfigProperties"/>
    /// <tr><td> Adapt to assigned roles. <td> <see cref="TestClientControlledTest.Roles"/>
    /// <tr><td> Perform test case actions. <td> <see cref="MessageMonitor"/>
    /// <tr><td> Generate test reports. <td> <see cref="MessageMonitor"/>
    /// </table>
    /// </summary>
    public class TestClientCircuitEnd : CircuitEnd, TestClientControlledTest
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(TestClientCircuitEnd));

        /// <summary> Holds the test parameters. </summary>
        ParsedProperties testProps;

        /// <summary> The number of test messages to send. </summary>
        private int numMessages;

        /// <summary> The role to be played by the test. </summary>
        private Roles role;

        /// <summary> The connection to send the test messages on. </summary>
        private Connection connection;

        /// <summary> Holds the circuit end for this test. </summary>
        CircuitEnd circuitEnd;

        /// <summary>
        /// Holds a message monitor for this circuit end, either the monitor on the consumer when in RECEIVER more, or
        /// a monitor updated on every message sent, when acting as a SENDER.
        MessageMonitor messageMonitor;

        /// <summary>
        /// Should provide the name of the test case that this class implements. The exact names are defined in the
        /// interop testing spec.
        /// </summary>
        /// <return> The name of the test case that this implements. </return>
        public string getName()
        {
            return "DEFAULT_CIRCUIT_TEST";
        }

        /// <summary>
        /// Determines whether the test invite that matched this test case is acceptable.
        /// </summary>
        /// <param name="inviteMessage"> The invitation to accept or reject. </param>
        /// <return> <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it. </return>
        /// </summary>
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public bool acceptInvite(Message inviteMessage) throws JMSException
        {
            log.debug("public bool acceptInvite(Message inviteMessage): called");

            // Populate the test parameters from the invitation.
            testProps = TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

            for (Object key : testProps.keySet())
            {
                string propName = (String) key;

                // If the test parameters is overridden by the invitation, use it instead.
                string inviteValue = inviteMessage.getStringProperty(propName);

                if (inviteValue != null)
                {
                    testProps.setProperty(propName, inviteValue);
                    log.debug("Test invite supplied override to " + propName + " of " + inviteValue);
                }

            }

            // Accept the invitation.
            return true;
        }

        /// <summary>
        /// Assigns the role to be played by this test case. The test parameters are fully specified in the
        /// assignment message. When this method return the test case will be ready to execute.
        /// </summary>
        /// <param name="role">              The role to be played; sender or receivers. </param>
        /// <param name="assignRoleMessage"> The role assingment message, contains the full test parameters. </param>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public void assignRole(Roles role, Message assignRoleMessage) throws JMSException
        {
            log.debug("public void assignRole(Roles role, Message assignRoleMessage): called");

            // Take note of the role to be played.
            this.role = role;

            // Extract and retain the test parameters.
            numMessages = 1; // assignRoleMessage.getIntProperty("NUM_MESSAGES");

            // Connect using the test parameters.
            connection = TestUtils.createConnection(testProps);

            // Create a circuit end that matches the assigned role and test parameters.
            LocalCircuitFactory circuitFactory = new LocalCircuitFactory();

            switch (role)
            {
                // Check if the sender role is being assigned, and set up a message producer if so.
            case SENDER:

                // Set up the publisher.
                circuitEnd = circuitFactory.createPublisherCircuitEnd(connection, testProps, 0L);

                // Create a custom message monitor that will be updated on every message sent.
                messageMonitor = new MessageMonitor();

                break;

                // Otherwise the receivers role is being assigned, so set this up to listen for messages.
            case RECEIVER:

                // Set up the receiver.
                circuitEnd = circuitFactory.createReceiverCircuitEnd(connection, testProps, 0L);

                // Use the message monitor from the consumer for stats.
                messageMonitor = getMessageMonitor();

                break;
            }

            // Reset all messaging stats for the report.
            messageMonitor.reset();

            connection.start();
        }

        /// <summary>
        /// Performs the test case actions. Returning from here, indicates that the sending role has completed its test.
        /// </summary>
        /// <param name="numMessages"> The number of test messages to send. </param>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        ///
        /// <remarks> Add round robin on destinations where multiple destinations being used.</remarks>
        ///
        /// <remarks> Add rate limiting when rate limit specified on publishers.</remarks>
        ///
        /// <remarks> Add Max pending message size protection. The receiver will have to send back some acks once in a while,
        ///       to notify the publisher that its messages are being consumed. This makes the safety valve harder to
        ///       implement than in the single VM case. For example, if the limit is 1000 messages, might want to get back
        ///       an ack every 500, to notify the publisher that it can keep sending. What about pub/sub tests? Will it be
        ///       necessary to wait for an ack from every receiver? This will have the effect of rate limiting to slow
        ///       consumers too.</remarks>
        ///
        /// <remarks> Add commits on every commit batch size boundary.</remarks>
        public void start(int numMessages) throws JMSException
        {
            log.debug("public void start(): called");

            // If in the SENDER role, send the specified number of test messages to the circuit destinations.
            if (role.equals(Roles.SENDER))
            {
                Message testMessage = getSession().createMessage();

                for (int i = 0; i < numMessages; i++)
                {
                    getProducer().send(testMessage);

                    // Increment the message count and timings.
                    messageMonitor.onMessage(testMessage);
                }
            }
        }

        /// <summary>
        /// Gets a report on the actions performed by the test case in its assigned role.
        /// </summary>
        /// <param name="session"> The controlSession to create the report message in. </param>
        /// <return> The report message. </return>
        ///
        /// <exception cref="JMSException"> Any JMSExceptions resulting from creating the report are allowed to fall through. </exception>
        public Message getReport(Session session) throws JMSException
        {
            Message report = session.createMessage();
            report.setStringProperty("CONTROL_TYPE", "REPORT");

            // Add the count of messages sent/received to the report.
            report.setIntProperty("MESSAGE_COUNT", messageMonitor.getNumMessage());

            // Add the time to send/receive messages to the report.
            report.setLongProperty("TEST_TIME", messageMonitor.getTime());

            // Add any exceptions detected to the report.

            return report;
        }

        /// <summary>
        /// Gets the message producer at this circuit end point.
        /// </summary>
        /// <return> The message producer at with this circuit end point. </return>
        public MessageProducer getProducer()
        {
            return circuitEnd.getProducer();
        }

        /// <summary>
        /// Gets the message consumer at this circuit end point.
        /// </summary>
        /// <return> The message consumer at this circuit end point. </return>
        public MessageConsumer getConsumer()
        {
            return circuitEnd.getConsumer();
        }

        /// <summary>
        /// Send the specified message over the producer at this end point.
        /// </summary>
        /// <param name="message"> The message to send. </param>
        ///
        /// <exception cref="JMSException"> Any JMS exception occuring during the send is allowed to fall through. </exception>
        public void send(Message message) throws JMSException
        {
            // Send the message on the circuit ends producer.
            circuitEnd.send(message);
        }

        /// <summary>
        /// Gets the JMS Session associated with this circuit end point.
        /// </summary>
        /// <return> The JMS Session associated with this circuit end point. </return>
        public Session getSession()
        {
            return circuitEnd.getSession();
        }

        /// <summary>
        /// Closes the message producers and consumers and the sessions, associated with this circuit end point.
        ///
        /// <exception cref="JMSException"> Any JMSExceptions occurring during the close are allowed to fall through. </exception>
        public void close() throws JMSException
        {
            // Close the producer and consumer.
            circuitEnd.close();
        }

        /// <summary>
        /// Returns the message monitor for reporting on received messages on this circuit end.
        /// </summary>
        /// <return> The message monitor for this circuit end. </return>
        public MessageMonitor getMessageMonitor()
        {
            return circuitEnd.getMessageMonitor();
        }

        /// <summary>
        /// Returns the exception monitor for reporting on exceptions received on this circuit end.
        /// </summary>
        /// <return> The exception monitor for this circuit end. </return>
        public ExceptionMonitor getExceptionMonitor()
        {
            return circuitEnd.getExceptionMonitor();
        }
    }
}
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
using junit.framework.Test;
using junit.framework.TestResult;
using junit.framework.TestSuite;

using log4net;
using org.apache.log4j.NDC;

using Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase;
using Apache.Qpid.Integration.Tests.framework.MessagingTestConfigProperties;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using Apache.Qpid.Integration.Tests.framework.TestUtils;
using Apache.Qpid.Integration.Tests.framework.clocksynch.UDPClockReference;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.TKTestRunner;
using uk.co.thebadgerset.junit.extensions.WrappedSuiteTestDecorator;
using uk.co.thebadgerset.junit.extensions.util.CommandLineParser;
using uk.co.thebadgerset.junit.extensions.util.MathUtils;
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;
using uk.co.thebadgerset.junit.extensions.util.TestContextProperties;

using javax.jms.*;

using java.net.InetAddress;
using java.util.*;
using java.util.concurrent.LinkedBlockingQueue;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// <p/>Implements the coordinator client described in the interop testing specification
    /// (http://cwiki.apache.org/confluence/display/qpid/Interop+Testing+Specification). This coordinator is built on
    /// top of the JUnit testing framework.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Find out what test clients are available. <td> <see cref="ConversationFactory"/>
    /// <tr><td> Decorate available tests to run on all available clients. <td> <see cref="DistributedTestDecorator"/>
    /// <tr><td> Attach XML test result logger.
    /// <tr><td> Terminate the interop testing framework.
    /// </table>
    /// </summary>
    ///
    /// <remarks> Should accumulate failures over all tests, and return with success or fail code based on all results. May need
    ///       to write a special TestResult to do this properly. At the moment only the last one used will be tested for
    ///       errors, as the start method creates a fresh one for each test case run.</remarks>
    public class Coordinator extends TKTestRunner
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(Coordinator));

        /// <summary> Used for reporting to the console. </summary>
        private static ILog console = LogManager.GetLogger("CONSOLE");

        /// <summary> Defines the possible distributed test engines available to run coordinated test cases with. </summary>
        public enum TestEngine
        {
            /// <summary> Specifies the interop test engine. This tests all available clients in pairs. </summary>
            INTEROP,

            /// <summary> Specifies the fanout test engine. This sets up one publisher role, and many reciever roles. </summary>
            FANOUT
        }

        /// <summary>
        /// Holds the test context properties that provides the default test parameters, plus command line overrides.
        /// This is initialized with the default test parameters, to which command line overrides may be applied.
        protected static ParsedProperties testContextProperties =
            TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

        /// <summary> Holds the URL of the broker to coordinate the tests on. </summary>
        protected string brokerUrl;

        /// <summary> Holds the virtual host to coordinate the tests on. If <tt>null</tt>, then the default virtual host is used. </summary>
        protected string virtualHost;

        /// <summary> Holds the list of all clients that enlisted, when the compulsory invite was issued. </summary>
        protected Set<TestClientDetails> enlistedClients = new HashSet<TestClientDetails>();

        /// <summary> Holds the conversation helper for the control conversation. </summary>
        protected ConversationFactory conversationFactory;

        /// <summary> Holds the connection that the coordinating messages are sent over. </summary>
        protected Connection connection;

        /// <summary> Holds the path of the directory to output test results too, if one is defined. </summary>
        protected string reportDir;

        /// <summary> Holds the coordinating test engine type to run the tests through. </summary>
        protected TestEngine engine;

        /// <summary> Flag that indicates that all test clients should be terminated upon completion of the test cases. </summary>
        protected bool terminate;

        /// <summary>
        /// Creates an interop test coordinator on the specified broker and virtual host.
        /// </summary>
        /// <param name="repetitions">        The number of times to repeat the test, or test batch size. </param>
        /// <param name="duration">           The length of time to run the tests for. -1 means no duration has been set. </param>
        /// <param name="threads">            The concurrency levels to ramp up to. </param>
        /// <param name="delay">              A delay in milliseconds between test runs. </param>
        /// <param name="params">             The sets of 'size' parameters to pass to test. </param>
        /// <param name="testCaseName">       The name of the test case to run. </param>
        /// <param name="reportDir">          The directory to output the test results to. </param>
        /// <param name="runName">            The name of the test run; used to name the output file. </param>
        /// <param name="verbose">            Whether to print comments during test run. </param>
        /// <param name="brokerUrl">          The URL of the broker to connect to. </param>
        /// <param name="virtualHost">        The virtual host to run all tests on. Optional, may be <tt>null</tt>. </param>
        /// <param name="engine">             The distributed test engine type to run the tests with. </param>
        /// <param name="terminate">          <tt>true</tt> if test client nodes should be terminated at the end of the tests. </param>
        /// <param name="csv">                <tt>true</tt> if the CSV results listener should be attached. </param>
        /// <param name="xml">                <tt>true</tt> if the XML results listener should be attached. </param>
        /// <param name="decoratorFactories"> List of factories for user specified decorators. </param>
        public Coordinator(Integer repetitions, Long duration, int[] threads, int delay, int[] params, string testCaseName,
                           string reportDir, string runName, bool verbose, string brokerUrl, string virtualHost, TestEngine engine,
                           bool terminate, bool csv, bool xml, IList<TestDecoratorFactory> decoratorFactories)
        {
            super(repetitions, duration, threads, delay, params, testCaseName, reportDir, runName, csv, xml, verbose,
                  decoratorFactories);

            log.debug("public Coordinator(Integer repetitions = " + repetitions + " , Long duration = " + duration
                      + ", int[] threads = " + Arrays.ToString(threads) + ", int delay = " + delay + ", int[] params = "
                      + Arrays.ToString(params) + ", string testCaseName = " + testCaseName + ", string reportDir = " + reportDir
                      + ", string runName = " + runName + ", bool verbose = " + verbose + ", string brokerUrl = " + brokerUrl
                      + ", string virtualHost =" + virtualHost + ", TestEngine engine = " + engine + ", bool terminate = "
                      + terminate + ", bool csv = " + csv + ", bool xml = " + xml + "): called");

            // Retain the connection parameters.
            this.brokerUrl = brokerUrl;
            this.virtualHost = virtualHost;
            this.reportDir = reportDir;
            this.engine = engine;
            this.terminate = terminate;
        }

        /// <summary>
        /// The entry point for the interop test coordinator. This client accepts the following command line arguments:
        ///
        /// <p/><table>
        /// <tr><td> -b         <td> The broker URL.   <td> Mandatory.
        /// <tr><td> -h         <td> The virtual host. <td> Optional.
        /// <tr><td> -o         <td> The directory to output test results to. <td> Optional.
        /// <tr><td> -e         <td> The type of test distribution engine to use. <td> Optional. One of: interop, fanout.
        /// <tr><td> ...        <td> Free arguments. The distributed test cases to run.
        ///                     <td> Mandatory. At least one must be defined.
        /// <tr><td> name=value <td> Trailing argument define name/value pairs. Added to the test contenxt properties.
        ///                     <td> Optional.
        /// </table>
        /// </summary>
        /// <param name="args"> The command line arguments. </param>
        public static void main(String[] args)
        {
            NDC.push("coordinator");
            log.debug("public static void main(String[] args = " + Arrays.ToString(args) + "): called");
            console.info("Qpid Distributed Test Coordinator.");

            // Override the default broker url to be localhost:5672.
            testContextProperties.setProperty(MessagingTestConfigProperties.BROKER_PROPNAME, "tcp://localhost:5672");

            try
            {
                // Use the command line parser to evaluate the command line with standard handling behaviour (print errors
                // and usage then exist if there are errors).
                // Any options and trailing name=value pairs are also injected into the test context properties object,
                // to override any defaults that may have been set up.
                ParsedProperties options =
                    new ParsedProperties(CommandLineParser.processCommandLine(args,
                                                                              new CommandLineParser(
                                                                                                    new String[][]
                                                                                                    {
                                                                                                        { "b", "The broker URL.", "broker", "false" },
                                                                                                        { "h", "The virtual host to use.", "virtual host", "false" },
                                                                                                        { "o", "The name of the directory to output test timings to.", "dir", "false" },
                                                                                                        {
                                                                                                            "e", "The test execution engine to use. Default is interop.", "engine", "interop",
                                                                                                            "^interop$|^fanout$", "true"
                                                                                                        },
                                                                                                        { "t", "Terminate test clients on completion of tests.", null, "false" },
                                                                                                        { "-csv", "Output test results in CSV format.", null, "false" },
                                                                                                        { "-xml", "Output test results in XML format.", null, "false" },
                                                                                                        {
                                                                                                            "-trefaddr", "To specify an alternative to hostname for time singal reference.",
                                                                                                            "address", "false"
                                                                                                        },
                                                                                                        {
                                                                                                            "c", "The number of tests to run concurrently.", "num", "false",
                                                                                                            MathUtils.SEQUENCE_REGEXP
                                                                                                        },
                                                                                                        { "r", "The number of times to repeat each test.", "num", "false" },
                                                                                                        {
                                                                                                            "d", "The length of time to run the tests for.", "duration", "false",
                                                                                                            MathUtils.DURATION_REGEXP
                                                                                                        },
                                                                                                        {
                                                                                                            "f", "The maximum rate to call the tests at.", "frequency", "false",
                                                                                                            "^([1-9][0-9]*)/([1-9][0-9]*)$"
                                                                                                        },
                                                                                                        { "s", "The size parameter to run tests with.", "size", "false", MathUtils.SEQUENCE_REGEXP },
                                                                                                        { "v", "Verbose mode.", null, "false" },
                                                                                                        { "n", "A name for this test run, used to name the output file.", "name", "true" },
                                                                                                        {
                                                                                                            "X:decorators", "A list of additional test decorators to wrap the tests in.",
                                                                                                            "\"class.name[:class.name]*\"", "false"
                                                                                                        }
                                                                                                    }), testContextProperties));

                // Extract the command line options.
                string brokerUrl = options.getProperty("b");
                string virtualHost = options.getProperty("h");
                string reportDir = options.getProperty("o");
                reportDir = (reportDir == null) ? "." : reportDir;
                string testEngine = options.getProperty("e");
                TestEngine engine = "fanout".equals(testEngine) ? TestEngine.FANOUT : TestEngine.INTEROP;
                bool terminate = options.getPropertyAsBoolean("t");
                bool csvResults = options.getPropertyAsBoolean("-csv");
                bool xmlResults = options.getPropertyAsBoolean("-xml");
                string threadsstring = options.getProperty("c");
                Integer repetitions = options.getPropertyAsInteger("r");
                string durationstring = options.getProperty("d");
                string paramsstring = options.getProperty("s");
                bool verbose = options.getPropertyAsBoolean("v");
                string testRunName = options.getProperty("n");
                string decorators = options.getProperty("X:decorators");

                int[] threads = (threadsstring == null) ? null : MathUtils.parseSequence(threadsString);
                int[] params = (paramsstring == null) ? null : MathUtils.parseSequence(paramsString);
                Long duration = (durationstring == null) ? null : MathUtils.parseDuration(durationString);

                // If broker or virtual host settings were specified as command line options, override the defaults in the
                // test context properties with them.

                // Collection all of the test cases to be run.
                Collection<Class<? extends FrameworkBaseCase>> testCaseClasses =
                    new ArrayList<Class<? extends FrameworkBaseCase>>();

                // Create a list of test decorator factories for use specified decorators to be applied.
                IList<TestDecoratorFactory> decoratorFactories = parseDecorators(decorators);

                // Scan for available test cases using a classpath scanner.
                // ClasspathScanner.getMatches(DistributedTestCase.class, "^Test.*", true);

                // Hard code the test classes till the classpath scanner is fixed.
                // Collections.addAll(testCaseClasses, InteropTestCase1DummyRun.class, InteropTestCase2BasicP2P.class,
                // InteropTestCase3BasicPubSub.class);

                // Parse all of the free arguments as test cases to run.
                for (int i = 1; true; i++)
                {
                    string nextFreeArg = options.getProperty(Integer.ToString(i));

                    // Terminate the loop once all free arguments have been consumed.
                    if (nextFreeArg == null)
                    {
                        break;
                    }

                    try
                    {
                        Class nextClass = Class.forName(nextFreeArg);

                        if (FrameworkBaseCase.class.isAssignableFrom(nextClass))
                        {
                            testCaseClasses.add(nextClass);
                            console.info("Found distributed test case: " + nextFreeArg);
                        }
                    }
                    catch (ClassNotFoundException e)
                    {
                        console.info("Unable to instantiate the test case: " + nextFreeArg + ".");
                    }
                }

                // Check that some test classes were actually found.
                if (testCaseClasses.isEmpty())
                {
                    throw new RuntimeException(
                                               "No test cases implementing FrameworkBaseCase were specified on the command line.");
                }

                // Extract the names of all the test classes, to pass to the start method.
                int i = 0;
                String[] testClassNames = new String[testCaseClasses.size()];

                for (Class testClass : testCaseClasses)
                {
                    testClassNames[i++] = testClass.getName();
                }

                // Create a coordinator and begin its test procedure.
                Coordinator coordinator =
                    new Coordinator(repetitions, duration, threads, 0, params, null, reportDir, testRunName, verbose, brokerUrl,
                                    virtualHost, engine, terminate, csvResults, xmlResults, decoratorFactories);

                TestResult testResult = coordinator.start(testClassNames);

                // Return different error codes, depending on whether or not there were test failures.
                if (testResult.failureCount() > 0)
                {
                    System.exit(FAILURE_EXIT);
                }
                else
                {
                    System.exit(SUCCESS_EXIT);
                }
            }
            catch (Exception e)
            {
                log.debug("Top level handler caught execption.", e);
                console.info(e.getMessage());
                e.printStackTrace();
                System.exit(EXCEPTION_EXIT);
            }
        }

        /// <summary>
        /// Starts all of the test classes to be run by this coordinator.
        /// </summary>
        /// <param name="testClassNames"> An array of all the coordinating test case implementations. </param>
        ///
        /// <return> A JUnit TestResult to run the tests with. </return>
        ///
        /// <exception cref="Exception"> Any underlying exceptions are allowed to fall through, and fail the test process. </exception>
        public TestResult start(String[] testClassNames) throws Exception
        {
            log.debug("public TestResult start(String[] testClassNames = " + Arrays.ToString(testClassNames) + ": called");

            // Connect to the broker.
            connection = TestUtils.createConnection(TestContextProperties.getInstance());
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            Destination controlTopic = session.createTopic("iop.control");
            Destination responseQueue = session.createQueue("coordinator");

            conversationFactory = new ConversationFactory(connection, responseQueue, LinkedBlockingQueue.class);
            ConversationFactory.Conversation conversation = conversationFactory.startConversation();

            connection.start();

            // Broadcast the compulsory invitation to find out what clients are available to test.
            Message invite = session.createMessage();
            invite.setStringProperty("CONTROL_TYPE", "INVITE");
            invite.setJMSReplyTo(responseQueue);

            conversation.send(controlTopic, invite);

            // Wait for a short time, to give test clients an opportunity to reply to the invitation.
            Collection<Message> enlists = conversation.receiveAll(0, 500);
            enlistedClients = extractEnlists(enlists);

            for (TestClientDetails client : enlistedClients)
            {
                log.debug("Got enlisted test client: " + client);
                console.info("Test node " + client.clientName + " available.");
            }

            // Start the clock reference service running.
            UDPClockReference clockReference = new UDPClockReference();
            Thread clockRefThread = new Thread(clockReference);
            registerShutdownHook(clockReference);
            clockRefThread.start();

            // Broadcast to all clients to synchronize their clocks against the coordinators clock reference.
            Message clockSynchRequest = session.createMessage();
            clockSynchRequest.setStringProperty("CONTROL_TYPE", "CLOCK_SYNCH");

            string localAddress = InetAddress.getByName(InetAddress.getLocalHost().getHostName()).getHostAddress();
            clockSynchRequest.setStringProperty("ADDRESS", localAddress);

            conversation.send(controlTopic, clockSynchRequest);

            // Run the test in the suite using JUnit.
            TestResult result = null;

            for (string testClassName : testClassNames)
            {
                // Record the current test class, so that the test results can be output to a file incorporating this name.
                this.currentTestClassName = testClassName;

                result = super.start(new String[] { testClassName });
            }

            // At this point in time, all tests have completed. Broadcast the shutdown message, if the termination option
            // was set on the command line.
            if (terminate)
            {
                Message terminate = session.createMessage();
                terminate.setStringProperty("CONTROL_TYPE", "TERMINATE");

                conversation.send(controlTopic, terminate);
            }

            return result;
        }

        /// <summary>
        /// For a collection of enlist messages, this method pulls out of the client details for the enlisting clients.
        /// </summary>
        /// <param name="enlists"> The enlist messages. </param>
        ///
        /// <return> A set of enlisting clients, extracted from the enlist messages. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSException is allowed to fall through. </exception>
        public static Set<TestClientDetails> extractEnlists(Collection<Message> enlists) throws JMSException
        {
            log.debug("public static Set<TestClientDetails> extractEnlists(Collection<Message> enlists = " + enlists
                      + "): called");

            Set<TestClientDetails> enlistedClients = new HashSet<TestClientDetails>();

            // Retain the list of all available clients.
            for (Message enlist : enlists)
            {
                TestClientDetails clientDetails = new TestClientDetails();
                clientDetails.clientName = enlist.getStringProperty("CLIENT_NAME");
                clientDetails.privateControlKey = enlist.getStringProperty("CLIENT_PRIVATE_CONTROL_KEY");

                string replyType = enlist.getStringProperty("CONTROL_TYPE");

                if ("ENLIST".equals(replyType))
                {
                    enlistedClients.add(clientDetails);
                }
                else if ("DECLINE".equals(replyType))
                {
                    log.debug("Test client " + clientDetails.clientName + " declined the invite.");
                }
                else
                {
                    log.warn("Got an unknown reply type, " + replyType + ", to the invite.");
                }
            }

            return enlistedClients;
        }

        /// <summary>
        /// Runs a test or suite of tests, using the super class implemenation. This method wraps the test to be run
        /// in any test decorators needed to add in the coordinators ability to invite test clients to participate in
        /// tests.
        /// </summary>
        /// <param name="test"> The test to run. </param>
        /// <param name="wait"> Undocumented. Nothing in the JUnit javadocs to say what this is for. </param>
        ///
        /// <return> The results of the test run. </return>
        public TestResult doRun(Test test, bool wait)
        {
            log.debug("public TestResult doRun(Test \"" + test + "\", bool " + wait + "): called");

            // Wrap all tests in the test suite with WrappedSuiteTestDecorators. This is quite ugly and a bit baffling,
            // but the reason it is done is because the JUnit implementation of TestDecorator has some bugs in it.
            WrappedSuiteTestDecorator targetTest = null;

            if (test instanceof TestSuite)
            {
                log.debug("targetTest is a TestSuite");

                TestSuite suite = (TestSuite) test;

                int numTests = suite.countTestCases();
                log.debug("There are " + numTests + " in the suite.");

                for (int i = 0; i < numTests; i++)
                {
                    Test nextTest = suite.testAt(i);
                    log.debug("suite.testAt(" + i + ") = " + nextTest);

                    if (nextTest instanceof FrameworkBaseCase)
                    {
                        log.debug("nextTest is a FrameworkBaseCase");
                    }
                }

                targetTest = new WrappedSuiteTestDecorator(suite);
                log.debug("Wrapped with a WrappedSuiteTestDecorator.");
            }

            // Apply any optional user specified decorators.
            targetTest = applyOptionalUserDecorators(targetTest);

            // Wrap the tests in a suitable distributed test decorator, to perform the invite/test cycle.
            targetTest = newTestDecorator(targetTest, enlistedClients, conversationFactory, connection);

            // TestSuite suite = new TestSuite();
            // suite.addTest(targetTest);

            // Wrap the tests in a scaled test decorator to them them as a 'batch' in one thread.
            // targetTest = new ScaledTestDecorator(targetTest, new int[] { 1 });

            return super.doRun(targetTest, wait);
        }

        /// <summary>
        /// Creates a wrapped test decorator, that is capable of inviting enlisted clients to participate in a specified
        /// test. This is the test engine that sets up the roles and sequences a distributed test case.
        /// </summary>
        /// <param name="targetTest">          The test decorator to wrap. </param>
        /// <param name="enlistedClients">     The enlisted clients available to run the test. </param>
        /// <param name="conversationFactory"> The conversation factory used to build conversation helper over the specified connection. </param>
        /// <param name="connection">          The connection to talk to the enlisted clients over. </param>
        ///
        /// <return> An invititing test decorator, that invites all the enlisted clients to participate in tests, in pairs. </return>
        protected DistributedTestDecorator newTestDecorator(WrappedSuiteTestDecorator targetTest,
                                                            Set<TestClientDetails> enlistedClients, ConversationFactory conversationFactory, Connection connection)
        {
            switch (engine)
            {
            case FANOUT:
                return new FanOutTestDecorator(targetTest, enlistedClients, conversationFactory, connection);
            case INTEROP:
            default:
                return new InteropTestDecorator(targetTest, enlistedClients, conversationFactory, connection);
            }
        }
    }
}
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
using junit.framework.TestResult;

using log4net;

using Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.WrappedSuiteTestDecorator;

using javax.jms.Connection;
using javax.jms.Destination;
using javax.jms.JMSException;
using javax.jms.Message;

using java.util.*;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// DistributedTestDecorator is a base class for writing test decorators that invite test clients to participate in
    /// distributed test cases. It provides a helper method, <see cref="#signupClients"/>, that broadcasts an invitation and
    /// returns the set of test clients that are available to particiapte in the test.
    ///
    /// <p/>When used to wrap a <see cref="FrameworkBaseCase"/> test, it replaces the default <see cref="CircuitFactory"/> implementations
    /// with a suitable circuit factory for distributed tests. Concrete implementations can use this to configure the sending
    /// and receiving roles on the test.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Broadcast test invitations and collect enlists. <td> <see cref="ConversationFactory"/>.
    /// </table>
    /// </summary>
    public abstract class DistributedTestDecorator extends WrappedSuiteTestDecorator
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(DistributedTestDecorator));

        /// <summary> Holds the contact information for all test clients that are available and that may take part in the test. </summary>
        Set<TestClientDetails> allClients;

        /// <summary> Holds the conversation helper for the control level conversation for coordinating the test through. </summary>
        ConversationFactory conversationFactory;

        /// <summary> Holds the connection that the control conversation is held over. </summary>
        Connection connection;

        /// <summary> Holds the underlying test suite that this decorator wraps. </summary>
        WrappedSuiteTestDecorator testSuite;

        /// <summary> Holds the control topic, on which test invitations are broadcast. </summary>
        protected Destination controlTopic;

        /// <summary>
        /// Creates a wrapped suite test decorator from another one.
        /// </summary>
        /// <param name="suite">               The test suite. </param>
        /// <param name="availableClients">    The list of all clients that responded to the compulsory invite. </param>
        /// <param name="controlConversation"> The conversation helper for the control level, test coordination conversation. </param>
        /// <param name="controlConnection">   The connection that the coordination messages are sent over. </param>
        public DistributedTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> availableClients,
                                        ConversationFactory controlConversation, Connection controlConnection)
        {
            super(suite);

            log.debug("public DistributedTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> allClients = "
                      + availableClients + ", ConversationHelper controlConversation = " + controlConversation + "): called");

            testSuite = suite;
            allClients = availableClients;
            conversationFactory = controlConversation;
            connection = controlConnection;

            // Set up the test control topic.
            try
            {
                controlTopic = conversationFactory.getSession().createTopic("iop.control");
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Unable to create the coordinating control topic to broadcast test invites on.", e);
            }
        }

        /// <summary>
        /// Should run all of the tests in the wrapped test suite.
        /// </summary>
        /// <param name="testResult"> The the results object to monitor the test results with. </param>
        public abstract void run(TestResult testResult);

        /// <summary>
        /// Should provide the distributed test sequencer to pass to <see cref="Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase"/>
        /// tests.
        /// </summary>
        /// <return> A distributed test sequencer. </return>
        public abstract CircuitFactory getTestSequencer();

        /// <summary>
        /// Broadcasts an invitation to participate in a coordinating test case to find out what clients are available to
        /// run the test case.
        /// </summary>
        /// <param name="coordTest"> The coordinating test case to broadcast an inviate for. </param>
        ///
        /// <return> A set of test clients that accepted the invitation. </return>
        protected Set<TestClientDetails> signupClients(FrameworkBaseCase coordTest)
        {
            // Broadcast the invitation to find out what clients are available to test.
            Set<TestClientDetails> enlists;
            try
            {
                Message invite = conversationFactory.getSession().createMessage();

                ConversationFactory.Conversation conversation = conversationFactory.startConversation();

                invite.setStringProperty("CONTROL_TYPE", "INVITE");
                invite.setStringProperty("TEST_NAME", coordTest.getTestCaseNameForTestMethod(coordTest.getName()));

                conversation.send(controlTopic, invite);

                // Wait for a short time, to give test clients an opportunity to reply to the invitation.
                Collection<Message> replies = conversation.receiveAll(allClients.size(), 500);
                enlists = Coordinator.extractEnlists(replies);
            }
            catch (JMSException e)
            {
                throw new RuntimeException("There was a JMSException during the invite/enlist conversation.", e);
            }

            return enlists;
        }

        /// <summary>
        /// Prints a string summarizing this test decorator, mainly for debugging purposes.
        /// </summary>
        /// <return> string representation for debugging purposes. </return>
        public string ToString()
        {
            return "DistributedTestDecorator: [ testSuite = " + testSuite + " ]";
        }
    }
}
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
using junit.framework.Test;
using junit.framework.TestResult;

using log4net;

using Apache.Qpid.Integration.Tests.framework.DropInTest;
using Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;
using Apache.Qpid.Integration.Tests.framework.sequencers.FanOutCircuitFactory;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.WrappedSuiteTestDecorator;

using javax.jms.Connection;
using javax.jms.JMSException;
using javax.jms.Message;
using javax.jms.MessageListener;

using System.Collections.Generic.Collection;
using java.util.Iterator;
using java.util.Set;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// FanOutTestDecorator is an <see cref="DistributedTestDecorator"/> that runs one test client in the sender role, and the remainder
    /// in the receivers role. It also has the capability to listen for new test cases joining the test beyond the initial start
    /// point. This feature can be usefull when experimenting with adding more load, in the form of more test clients, to assess
    /// its impact on a running test.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Execute coordinated test cases. <td> <see cref="FrameworkBaseCase"/>
    /// <tr><td> Accept test clients joining a running test.
    /// </table>
    /// </summary>
    public class FanOutTestDecorator extends DistributedTestDecorator : MessageListener
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(FanOutTestDecorator));

        /// <summary> Holds the currently running test case. </summary>
        FrameworkBaseCase currentTest = null;

        /// <summary>
        /// Creates a wrapped suite test decorator from another one.
        /// </summary>
        /// <param name="suite">               The test suite. </param>
        /// <param name="availableClients">    The list of all clients that responded to the compulsory invite. </param>
        /// <param name="controlConversation"> The conversation helper for the control level, test coordination conversation. </param>
        /// <param name="controlConnection">   The connection that the coordination messages are sent over. </param>
        public FanOutTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> availableClients,
                                   ConversationFactory controlConversation, Connection controlConnection)
        {
            super(suite, availableClients, controlConversation, controlConnection);

            log.debug("public DistributedTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> allClients = "
                      + availableClients + ", ConversationHelper controlConversation = " + controlConversation + "): called");

            testSuite = suite;
            allClients = availableClients;
            conversationFactory = controlConversation;
            connection = controlConnection;

            // Sign available clients up to the test.
            for (Test test : getAllUnderlyingTests())
            {
                FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

                // Get all of the clients able to participate in the test.
                Set<TestClientDetails> enlists = signupClients(coordTest);

                // Check that there were some clients available.
                if (enlists.size() == 0)
                {
                    throw new RuntimeException("No clients to test with");
                }

                // Create a distributed test circuit factory for the test.
                CircuitFactory circuitFactory = getTestSequencer();

                // Set up the first client in the sender role, and the remainder in the receivers role.
                Iterator<TestClientDetails> clients = enlists.iterator();
                circuitFactory.setSender(clients.next());

                while (clients.hasNext())
                {
                    // Set the sending and receiving client details on the test case.
                    circuitFactory.setReceiver(clients.next());
                }

                // Pass down the connection to hold the coordinating conversation over.
                circuitFactory.setConversationFactory(conversationFactory);

                // If the current test case is a drop-in test, set it up as the currently running test for late joiners to
                // add in to. Otherwise the current test field is set to null, to indicate that late joiners are not allowed.
                currentTest = (coordTest instanceof DropInTest) ? coordTest : null;

                // Execute the test case.
                coordTest.setCircuitFactory(circuitFactory);
            }
        }

        /// <summary>
        /// Broadcasts a test invitation and accepts enlists from participating clients. The wrapped test cases are run
        /// with one test client in the sender role, and the remaining test clients in the receiving role.
        ///
        /// <p/>Any JMSExceptions during the invite/enlist conversation will be allowed to fall through as runtime
        /// exceptions, resulting in the non-completion of the test run.
        /// </summary>
        /// <param name="testResult"> The the results object to monitor the test results with. </param>
        ///
        /// <remarks> Better error recovery for failure of the invite/enlist conversation could be added.</remarks>
        public void run(TestResult testResult)
        {
            log.debug("public void run(TestResult testResult): called");

            // Listen for late joiners on the control topic.
            try
            {
                conversationFactory.getSession().createConsumer(controlTopic).setMessageListener(this);
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Unable to set up the message listener on the control topic.", e);
            }

            // Run all of the test cases in the test suite.
            /*for (Test test : getAllUnderlyingTests())
              {
              FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

              // Get all of the clients able to participate in the test.
              Set<TestClientDetails> enlists = signupClients(coordTest);

              // Check that there were some clients available.
              if (enlists.size() == 0)
              {
              throw new RuntimeException("No clients to test with");
              }

              // Create a distributed test circuit factory for the test.
              CircuitFactory circuitFactory = getTestSequencer();

              // Set up the first client in the sender role, and the remainder in the receivers role.
              Iterator<TestClientDetails> clients = enlists.iterator();
              circuitFactory.setSender(clients.next());

              while (clients.hasNext())
              {
              // Set the sending and receiving client details on the test case.
              circuitFactory.setReceiver(clients.next());
              }

              // Pass down the connection to hold the coordinating conversation over.
              circuitFactory.setConversationFactory(conversationFactory);

              // If the current test case is a drop-in test, set it up as the currently running test for late joiners to
              // add in to. Otherwise the current test field is set to null, to indicate that late joiners are not allowed.
              currentTest = (coordTest instanceof DropInTest) ? coordTest : null;

              // Execute the test case.
              coordTest.setCircuitFactory(circuitFactory);
              }*/

            // Run all of the test cases in the test suite.
            for (Test test : getAllUnderlyingTests())
            {
                FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

                coordTest.run(testResult);

                currentTest = null;
            }
        }

        /// <summary>
        /// Should provide the distributed test sequencer to pass to <see cref="Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase"/>
        /// tests.
        /// </summary>
        /// <return> A distributed test sequencer. </return>
        public CircuitFactory getTestSequencer()
        {
            return new FanOutCircuitFactory();
        }

        /// <summary>
        /// Listens to incoming messages on the control topic. If the messages are 'join' messages, signalling a new
        /// test client wishing to join the current test, then the new client will be added to the current test in the
        /// receivers role.
        /// </summary>
        /// <param name="message"> The incoming control message. </param>
        public void onMessage(Message message)
        {
            try
            {
                // Check if the message is from a test client attempting to join a running test, and join it to the current
                // test case if so.
                if (message.getStringProperty("CONTROL_TYPE").equals("JOIN") && (currentTest != null))
                {
                    ((DropInTest) currentTest).lateJoin(message);
                }
            }
            // There is not a lot can be done with this error, so it is deliberately ignored.
            catch (JMSException e)
            {
                log.debug("Unable to process message:" + message);
            }
        }

        /// <summary>
        /// Prints a string summarizing this test decorator, mainly for debugging purposes.
        /// </summary>
        /// <return> string representation for debugging purposes. </return>
        public string ToString()
        {
            return "FanOutTestDecorator: [ testSuite = " + testSuite + " ]";
        }
    }
}
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
using junit.framework.Test;
using junit.framework.TestResult;

using log4net;

using Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;
using Apache.Qpid.Integration.Tests.framework.sequencers.InteropCircuitFactory;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.WrappedSuiteTestDecorator;

using javax.jms.Connection;

using java.util.*;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// DistributedTestDecorator is a test decorator, written to implement the interop test specification. Given a list
    /// of enlisted test clients, that are available to run interop tests, this decorator invites them to participate
    /// in each test in the wrapped test suite. Amongst all the clients that respond to the invite, all pairs are formed,
    /// and each pairing (in both directions, but excluding the reflexive pairings) is split into a sender and receivers
    /// role and a test case run between them. Any enlisted combinations that do not accept a test invite are automatically
    /// failed.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Broadcast test invitations and collect enlists. <td> <see cref="org.apache.qpid.util.ConversationFactory"/>.
    /// <tr><td> Output test failures for clients unwilling to run the test case. <td> <see cref="Coordinator"/>
    /// <tr><td> Execute distributed test cases. <td> <see cref="FrameworkBaseCase"/>
    /// <tr><td> Fail non-participating pairings. <td> <see cref="OptOutTestCase"/>
    /// </table>
    /// </summary>
    public class InteropTestDecorator extends DistributedTestDecorator
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(InteropTestDecorator));

        /// <summary>
        /// Creates a wrapped suite test decorator from another one.
        /// </summary>
        /// <param name="suite">               The test suite. </param>
        /// <param name="availableClients">    The list of all clients that responded to the compulsory invite. </param>
        /// <param name="controlConversation"> The conversation helper for the control level, test coordination conversation. </param>
        /// <param name="controlConnection">   The connection that the coordination messages are sent over. </param>
        public InteropTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> availableClients,
                                    ConversationFactory controlConversation, Connection controlConnection)
        {
            super(suite, availableClients, controlConversation, controlConnection);
        }

        /// <summary>
        /// Broadcasts a test invitation and accetps enlisting from participating clients. The wrapped test case is
        /// then repeated for every combination of test clients (provided the wrapped test case extends
        /// <see cref="FrameworkBaseCase"/>.
        ///
        /// <p/>Any JMSExceptions during the invite/enlist conversation will be allowed to fall through as runtime exceptions,
        /// resulting in the non-completion of the test run.
        /// </summary>
        /// <remarks> Better error recovery for failure of the invite/enlist conversation could be added.</remarks>
        /// <param name="testResult"> The the results object to monitor the test results with. </param>
        public void run(TestResult testResult)
        {
            log.debug("public void run(TestResult testResult): called");

            Collection<Test> tests = testSuite.getAllUnderlyingTests();

            for (Test test : getAllUnderlyingTests())
            {
                FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

                // Broadcast the invitation to find out what clients are available to test.
                Set<TestClientDetails> enlists = signupClients(coordTest);

                // Compare the list of willing clients to the list of all available.
                Set<TestClientDetails> optOuts = new HashSet<TestClientDetails>(allClients);
                optOuts.removeAll(enlists);

                // Output test failures for clients that will not particpate in the test.
                Set<List<TestClientDetails>> failPairs = allPairs(optOuts, allClients);

                for (List<TestClientDetails> failPair : failPairs)
                {
                    // Create a distributed test circuit factory for the test.
                    CircuitFactory circuitFactory = getTestSequencer();

                    // Create an automatic failure test for the opted out test pair.
                    FrameworkBaseCase failTest = new OptOutTestCase("testOptOut");
                    circuitFactory.setSender(failPair.get(0));
                    circuitFactory.setReceiver(failPair.get(1));
                    failTest.setCircuitFactory(circuitFactory);

                    failTest.run(testResult);
                }

                // Loop over all combinations of clients, willing to run the test.
                Set<List<TestClientDetails>> enlistedPairs = allPairs(enlists, enlists);

                for (List<TestClientDetails> enlistedPair : enlistedPairs)
                {
                    // Create a distributed test circuit factory for the test.
                    CircuitFactory circuitFactory = getTestSequencer();

                    // Set the sending and receiving client details on the test circuitFactory.
                    circuitFactory.setSender(enlistedPair.get(0));
                    circuitFactory.setReceiver(enlistedPair.get(1));

                    // Pass down the connection to hold the coordination conversation over.
                    circuitFactory.setConversationFactory(conversationFactory);

                    // Execute the test case.
                    coordTest.setCircuitFactory(circuitFactory);
                    coordTest.run(testResult);
                }
            }
        }

        /// <summary>
        /// Should provide the distributed test sequencer to pass to <see cref="Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase"/>
        /// tests.
        /// </summary>
        /// <return> A distributed test sequencer. </return>
        public CircuitFactory getTestSequencer()
        {
            return new InteropCircuitFactory();
        }

        /// <summary>
        /// Produces all pairs of combinations of elements from two sets. The ordering of the elements in the pair is
        /// important, that is the pair <l, r> is distinct from <r, l>; both pairs are generated. For any element, i, in
        /// both the left and right sets, the reflexive pair <i, i> is not generated.
        /// </summary>
        /// <param name="left">  The left set. </param>
        /// <param name="right"> The right set. </param>
        /// @param <E>   The type of the content of the pairs.
        /// </summary>
        /// <return> All pairs formed from the permutations of all elements of the left and right sets. </return>
        private <E> Set<List<E>> allPairs(Set<E> left, Set<E> right)
        {
            log.debug("private <E> Set<List<E>> allPairs(Set<E> left = " + left + ", Set<E> right = " + right + "): called");

            Set<List<E>> results = new HashSet<List<E>>();

            // Form all pairs from left to right.
            // Form all pairs from right to left.
            for (E le : left)
            {
                for (E re : right)
                {
                    if (!le.equals(re))
                    {
                        results.add(new Pair<E>(le, re));
                        results.add(new Pair<E>(re, le));
                    }
                }
            }

            log.debug("results = " + results);

            return results;
        }

        /// <summary> A simple implementation of a pair, using a list. </summary>
        private class Pair<T> extends ArrayList<T>
        {
            /// <summary>
            /// Creates a new pair of elements.
            /// </summary>
            /// <param name="first">  The first element. </param>
            /// <param name="second"> The second element. </param>
            public Pair(T first, T second)
            {
                super();
                super.add(first);
                super.add(second);
            }
        }
    }
}
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
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;
using Apache.Qpid.Integration.Tests.framework.FrameworkBaseCase;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// An OptOutTestCase is a test case that automatically fails. It is used when a list of test clients has been generated
    /// from a compulsory invite, but only some of those clients have responded to a specific test case invite. The clients
    /// that did not respond, may automatically be given a fail for some tests.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Fail the test with a suitable reason.
    /// </table>
    /// </summary>
    public class OptOutTestCase extends FrameworkBaseCase
    {
        /// <summary>
        /// Creates a new coordinating test case with the specified name.
        /// </summary>
        /// <param name="name"> The test case name. </param>
        public OptOutTestCase(string name)
        {
            super(name);
        }

        /// <summary> Generates an appropriate test failure assertion. </summary>
        public void testOptOut()
        {
            CircuitFactory circuitFactory = getCircuitFactory();

            fail("One of " + circuitFactory.getSender() + " and " + getCircuitFactory().getReceivers()
                 + " opted out of the test.");
        }

        /// <summary>
        /// Should provide a translation from the junit method name of a test to its test case name as defined in the
        /// interop testing specification. For example the method "testP2P" might map onto the interop test case name
        /// "TC2_BasicP2P".
        /// </summary>
        /// <param name="methodName"> The name of the JUnit test method. </param>
        /// <return> The name of the corresponding interop test case. </return>
        public string getTestCaseNameForTestMethod(string methodName)
        {
            return "OptOutTest";
        }
    }
}
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
using org.apache.log4j.NDC;

using Apache.Qpid.Integration.Tests.framework.MessagingTestConfigProperties;
using Apache.Qpid.Integration.Tests.framework.TestUtils;
using Apache.Qpid.Integration.Tests.framework.clocksynch.ClockSynchThread;
using Apache.Qpid.Integration.Tests.framework.clocksynch.UDPClockSynchronizer;
using org.apache.qpid.util.ReflectionUtils;
using org.apache.qpid.util.ReflectionUtilsException;

using uk.co.thebadgerset.junit.extensions.SleepThrottle;
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;
using uk.co.thebadgerset.junit.extensions.util.TestContextProperties;

using javax.jms.*;

using java.util.*;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// Implements a test client as described in the interop testing spec
    /// (http://cwiki.apache.org/confluence/display/qpid/Interop+Testing+Specification). A test client is an agent that
    /// reacts to control message sequences send by the test <see cref="Coordinator"/>.
    ///
    /// <p/><table><caption>Messages Handled by TestClient</caption>
    /// <tr><th> Message               <th> Action
    /// <tr><td> Invite(compulsory)    <td> Reply with Enlist.
    /// <tr><td> Invite(test case)     <td> Reply with Enlist if test case available.
    /// <tr><td> AssignRole(test case) <td> Reply with Accept Role if matches an enlisted test. Keep test parameters.
    /// <tr><td> Start                 <td> Send test messages defined by test parameters. Send report on messages sent.
    /// <tr><td> Status Request        <td> Send report on messages received.
    /// <tr><td> Terminate             <td> Terminate the test client.
    /// <tr><td> ClockSynch            <td> Synch clock against the supplied UDP address.
    /// </table>
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Handle all incoming control messages. <td> <see cref="TestClientControlledTest"/>
    /// <tr><td> Configure and look up test cases by name. <td> <see cref="TestClientControlledTest"/>
    /// </table>
    /// </summary>
    public class TestClient : MessageListener
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(TestClient));

        /// <summary> Used for reporting to the console. </summary>
        private static ILog console = LogManager.GetLogger("CONSOLE");

        /// <summary> Holds the default identifying name of the test client. </summary>
        public static final string CLIENT_NAME = "java";

        /// <summary> Holds the URL of the broker to run the tests on. </summary>
        public static string brokerUrl;

        /// <summary> Holds the virtual host to run the tests on. If <tt>null</tt>, then the default virtual host is used. </summary>
        public static string virtualHost;

        /// <summary>
        /// Holds the test context properties that provides the default test parameters, plus command line overrides.
        /// This is initialized with the default test parameters, to which command line overrides may be applied.
        /// </summary>
        public static ParsedProperties testContextProperties =
            TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

        /// <summary> Holds all the test cases loaded from the classpath. </summary>
        Map<String, TestClientControlledTest> testCases = new HashMap<String, TestClientControlledTest>();

        /// <summary> Holds the test case currently being run by this client. </summary>
        protected TestClientControlledTest currentTestCase;

        /// <summary> Holds the connection to the broker that the test is being coordinated on. </summary>
        protected Connection connection;

        /// <summary> Holds the message producer to hold the test coordination over. </summary>
        protected MessageProducer producer;

        /// <summary> Holds the JMS controlSession for the test coordination. </summary>
        protected Session session;

        /// <summary> Holds the name of this client, with a default value. </summary>
        protected string clientName = CLIENT_NAME;

        /// <summary> This flag indicates that the test client should attempt to join the currently running test case on start up. </summary>
        protected bool join;

        /// <summary> Holds the clock synchronizer for the test node. </summary>
        ClockSynchThread clockSynchThread;

        /// <summary>
        /// Creates a new interop test client, listenting to the specified broker and virtual host, with the specified client
        /// identifying name.
        /// </summary>
        /// <param name="pBrokerUrl">   The url of the broker to connect to. </param>
        /// <param name="pVirtualHost"> The virtual host to conect to. </param>
        /// <param name="clientName">  The client name to use. </param>
        /// <param name="join">        Flag to indicate that this client should attempt to join running tests. </param>
        public TestClient(string pBrokerUrl, string pVirtualHost, string clientName, bool join)
        {
            log.debug("public TestClient(string pBrokerUrl = " + pBrokerUrl + ", string pVirtualHost = " + pVirtualHost
                      + ", string clientName = " + clientName + ", bool join = " + join + "): called");

            // Retain the connection parameters.
            brokerUrl = pBrokerUrl;
            virtualHost = pVirtualHost;
            this.clientName = clientName;
            this.join = join;
        }

        /// <summary>
        /// The entry point for the interop test coordinator. This client accepts the following command line arguments:
        ///
        /// <p/><table>
        /// <tr><td> -b         <td> The broker URL.       <td> Optional.
        /// <tr><td> -h         <td> The virtual host.     <td> Optional.
        /// <tr><td> -n         <td> The test client name. <td> Optional.
        /// <tr><td> name=value <td> Trailing argument define name/value pairs. Added to system properties. <td> Optional.
        /// </table>
        /// </summary>
        /// <param name="args"> The command line arguments. </param>
        public static void main(String[] args)
        {
            log.debug("public static void main(String[] args = " + Arrays.ToString(args) + "): called");
            console.info("Qpid Distributed Test Client.");

            // Override the default broker url to be localhost:5672.
            testContextProperties.setProperty(MessagingTestConfigProperties.BROKER_PROPNAME, "tcp://localhost:5672");

            // Use the command line parser to evaluate the command line with standard handling behaviour (print errors
            // and usage then exist if there are errors).
            // Any options and trailing name=value pairs are also injected into the test context properties object,
            // to override any defaults that may have been set up.
            ParsedProperties options =
                new ParsedProperties(uk.co.thebadgerset.junit.extensions.util.CommandLineParser.processCommandLine(args,
                                                                                                                   new uk.co.thebadgerset.junit.extensions.util.CommandLineParser(
                                                                                                                                                                                  new String[][]
                                                                                                                                                                                  {
                                                                                                                                                                                      { "b", "The broker URL.", "broker", "false" },
                                                                                                                                                                                      { "h", "The virtual host to use.", "virtual host", "false" },
                                                                                                                                                                                      { "o", "The name of the directory to output test timings to.", "dir", "false" },
                                                                                                                                                                                      { "n", "The name of the test client.", "name", "false" },
                                                                                                                                                                                      { "j", "Join this test client to running test.", "false" }
                                                                                                                                                                                  }), testContextProperties));

            // Extract the command line options.
            string brokerUrl = options.getProperty("b");
            string virtualHost = options.getProperty("h");
            string clientName = options.getProperty("n");
            clientName = (clientName == null) ? CLIENT_NAME : clientName;
            bool join = options.getPropertyAsBoolean("j");

            // To distinguish logging output set up an NDC on the client name.
            NDC.push(clientName);

            // Create a test client and start it running.
            TestClient client = new TestClient(brokerUrl, virtualHost, clientName, join);

            // Use a class path scanner to find all the interop test case implementations.
            // Hard code the test classes till the classpath scanner is fixed.
            Collection<Class<? extends TestClientControlledTest>> testCaseClasses =
                new ArrayList<Class<? extends TestClientControlledTest>>();
            // ClasspathScanner.getMatches(TestClientControlledTest.class, "^TestCase.*", true);
            testCaseClasses.addAll(loadTestCases("org.apache.qpid.interop.clienttestcases.TestCase1DummyRun",
                                                 "org.apache.qpid.interop.clienttestcases.TestCase2BasicP2P",
                                                 "org.apache.qpid.interop.clienttestcases.TestCase3BasicPubSub",
                                                 "org.apache.qpid.interop.clienttestcases.TestCase4P2PMessageSize",
                                                 "org.apache.qpid.interop.clienttestcases.TestCase5PubSubMessageSize",
                                                 "Apache.Qpid.Integration.Tests.framework.distributedcircuit.TestClientCircuitEnd"));

            try
            {
                client.start(testCaseClasses);
            }
            catch (Exception e)
            {
                log.error("The test client was unable to start.", e);
                console.info(e.getMessage());
                System.exit(1);
            }
        }

        /// <summary>
        /// Parses a list of class names, and loads them if they are available on the class path.
        /// </summary>
        /// <param name="classNames"> The names of the classes to load. </param>
        ///
        /// <return> A list of the loaded test case classes. </return>
        public static IList<Class<? extends TestClientControlledTest>> loadTestCases(String... classNames)
        {
            IList<Class<? extends TestClientControlledTest>> testCases =
                new LinkedList<Class<? extends TestClientControlledTest>>();

            for (string className : classNames)
            {
                try
                {
                    Class<?> cls = ReflectionUtils.forName(className);
                    testCases.add((Class<? extends TestClientControlledTest>) cls);
                }
                catch (ReflectionUtilsException e)
                {
                    // Ignore, class could not be found, so test not available.
                    console.warn("Requested class " + className + " cannot be found, ignoring it.");
                }
                catch (ClassCastException e)
                {
                    // Ignore, class was not of correct type to be a test case.
                    console.warn("Requested class " + className + " is not an instance of TestClientControlledTest.");
                }
            }

            return testCases;
        }

        /// <summary>
        /// Starts the interop test client running. This causes it to start listening for incoming test invites.
        /// </summary>
        /// <param name="testCaseClasses"> The classes of the available test cases. The test case names from these are used to </param>
        ///                        matchin incoming test invites against.
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through. </exception>
        protected void start(Collection<Class<? extends TestClientControlledTest>> testCaseClasses) throws JMSException
        {
            log.debug("protected void start(Collection<Class<? extends TestClientControlledTest>> testCaseClasses = "
                      + testCaseClasses + "): called");

            // Create all the test case implementations and index them by the test names.
            for (Class<? extends TestClientControlledTest> nextClass : testCaseClasses)
            {
                try
                {
                    TestClientControlledTest testCase = nextClass.newInstance();
                    testCases.put(testCase.getName(), testCase);
                }
                catch (InstantiationException e)
                {
                    log.warn("Could not instantiate test case class: " + nextClass.getName(), e);
                    // Ignored.
                }
                catch (IllegalAccessException e)
                {
                    log.warn("Could not instantiate test case class due to illegal access: " + nextClass.getName(), e);
                    // Ignored.
                }
            }

            // Open a connection to communicate with the coordinator on.
            connection = TestUtils.createConnection(testContextProperties);
            session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            // Set this up to listen for control messages.
            Topic privateControlTopic = session.createTopic("iop.control." + clientName);
            MessageConsumer consumer = session.createConsumer(privateControlTopic);
            consumer.setMessageListener(this);

            Topic controlTopic = session.createTopic("iop.control");
            MessageConsumer consumer2 = session.createConsumer(controlTopic);
            consumer2.setMessageListener(this);

            // Create a producer to send replies with.
            producer = session.createProducer(null);

            // If the join flag was set, then broadcast a join message to notify the coordinator that a new test client
            // is available to join the current test case, if it supports it. This message may be ignored, or it may result
            // in this test client receiving a test invite.
            if (join)
            {
                Message joinMessage = session.createMessage();

                joinMessage.setStringProperty("CONTROL_TYPE", "JOIN");
                joinMessage.setStringProperty("CLIENT_NAME", clientName);
                joinMessage.setStringProperty("CLIENT_PRIVATE_CONTROL_KEY", "iop.control." + clientName);
                producer.send(controlTopic, joinMessage);
            }

            // Start listening for incoming control messages.
            connection.start();
        }

        /// <summary>
        /// Handles all incoming control messages.
        /// </summary>
        /// <param name="message"> The incoming message. </param>
        public void onMessage(Message message)
        {
            NDC.push(clientName);
            log.debug("public void onMessage(Message message = " + message + "): called");

            try
            {
                string controlType = message.getStringProperty("CONTROL_TYPE");
                string testName = message.getStringProperty("TEST_NAME");

                log.debug("Received control of type '" + controlType + "' for the test '" + testName + "'");

                // Check if the message is a test invite.
                if ("INVITE".equals(controlType))
                {
                    // Flag used to indicate that an enlist should be sent. Only enlist to compulsory invites or invites
                    // for which test cases exist.
                    bool enlist = false;

                    if (testName != null)
                    {
                        log.debug("Got an invite to test: " + testName);

                        // Check if the requested test case is available.
                        TestClientControlledTest testCase = testCases.get(testName);

                        if (testCase != null)
                        {
                            log.debug("Found implementing class for test '" + testName + "', enlisting for it.");

                            // Check if the test case will accept the invitation.
                            enlist = testCase.acceptInvite(message);

                            log.debug("The test case "
                                      + (enlist ? " accepted the invite, enlisting for it."
                                         : " did not accept the invite, not enlisting."));

                            // Make the requested test case the current test case.
                            currentTestCase = testCase;
                        }
                        else
                        {
                            log.debug("Received an invite to the test '" + testName + "' but this test is not known.");
                        }
                    }
                    else
                    {
                        log.debug("Got a compulsory invite, enlisting for it.");

                        enlist = true;
                    }

                    if (enlist)
                    {
                        // Reply with the client name in an Enlist message.
                        Message enlistMessage = session.createMessage();
                        enlistMessage.setStringProperty("CONTROL_TYPE", "ENLIST");
                        enlistMessage.setStringProperty("CLIENT_NAME", clientName);
                        enlistMessage.setStringProperty("CLIENT_PRIVATE_CONTROL_KEY", "iop.control." + clientName);
                        enlistMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                        log.debug("Sending enlist message '" + enlistMessage + "' to " + message.getJMSReplyTo());

                        producer.send(message.getJMSReplyTo(), enlistMessage);
                    }
                    else
                    {
                        // Reply with the client name in an Decline message.
                        Message enlistMessage = session.createMessage();
                        enlistMessage.setStringProperty("CONTROL_TYPE", "DECLINE");
                        enlistMessage.setStringProperty("CLIENT_NAME", clientName);
                        enlistMessage.setStringProperty("CLIENT_PRIVATE_CONTROL_KEY", "iop.control." + clientName);
                        enlistMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                        log.debug("Sending decline message '" + enlistMessage + "' to " + message.getJMSReplyTo());

                        producer.send(message.getJMSReplyTo(), enlistMessage);
                    }
                }
                else if ("ASSIGN_ROLE".equals(controlType))
                {
                    // Assign the role to the current test case.
                    string roleName = message.getStringProperty("ROLE");

                    log.debug("Got a role assignment to role: " + roleName);

                    TestClientControlledTest.Roles role = Enum.valueOf(TestClientControlledTest.Roles.class, roleName);

                    currentTestCase.assignRole(role, message);

                    // Reply by accepting the role in an Accept Role message.
                    Message acceptRoleMessage = session.createMessage();
                    acceptRoleMessage.setStringProperty("CLIENT_NAME", clientName);
                    acceptRoleMessage.setStringProperty("CONTROL_TYPE", "ACCEPT_ROLE");
                    acceptRoleMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                    log.debug("Sending accept role message '" + acceptRoleMessage + "' to " + message.getJMSReplyTo());

                    producer.send(message.getJMSReplyTo(), acceptRoleMessage);
                }
                else if ("START".equals(controlType) || "STATUS_REQUEST".equals(controlType))
                {
                    if ("START".equals(controlType))
                    {
                        log.debug("Got a start notification.");

                        // Extract the number of test messages to send from the start notification.
                        int numMessages;

                        try
                        {
                            numMessages = message.getIntProperty("MESSAGE_COUNT");
                        }
                        catch (NumberFormatException e)
                        {
                            // If the number of messages is not specified, use the default of one.
                            numMessages = 1;
                        }

                        // Start the current test case.
                        currentTestCase.start(numMessages);
                    }
                    else
                    {
                        log.debug("Got a status request.");
                    }

                    // Generate the report from the test case and reply with it as a Report message.
                    Message reportMessage = currentTestCase.getReport(session);
                    reportMessage.setStringProperty("CLIENT_NAME", clientName);
                    reportMessage.setStringProperty("CONTROL_TYPE", "REPORT");
                    reportMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                    log.debug("Sending report message '" + reportMessage + "' to " + message.getJMSReplyTo());

                    producer.send(message.getJMSReplyTo(), reportMessage);
                }
                else if ("TERMINATE".equals(controlType))
                {
                    console.info("Received termination instruction from coordinator.");

                    // Is a cleaner shutdown needed?
                    connection.close();
                    System.exit(0);
                }
                else if ("CLOCK_SYNCH".equals(controlType))
                {
                    log.debug("Received clock synch command.");
                    string address = message.getStringProperty("ADDRESS");

                    log.debug("address = " + address);

                    // Re-create (if necessary) and start the clock synch thread to synch the clock every ten seconds.
                    if (clockSynchThread != null)
                    {
                        clockSynchThread.terminate();
                    }

                    SleepThrottle throttle = new SleepThrottle();
                    throttle.setRate(0.1f);

                    clockSynchThread = new ClockSynchThread(new UDPClockSynchronizer(address), throttle);
                    clockSynchThread.start();
                }
                else
                {
                    // Log a warning about this but otherwise ignore it.
                    log.warn("Got an unknown control message, controlType = " + controlType + ", message = " + message);
                }
            }
            catch (JMSException e)
            {
                // Log a warning about this, but otherwise ignore it.
                log.warn("Got JMSException whilst handling message: " + message, e);
            }
            // Log any runtimes that fall through this message handler. These are fatal errors for the test client.
            catch (RuntimeException e)
            {
                log.error("The test client message handler got an unhandled exception: ", e);
                console.info("The message handler got an unhandled exception, terminating the test client.");
                System.exit(1);
            }
            finally
            {
                NDC.pop();
            }
        }
    }
}
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
using javax.jms.JMSException;
using javax.jms.Message;
using javax.jms.MessageListener;
using javax.jms.Session;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// TestClientControlledTest provides an interface that classes implementing test cases to run on a <see cref="TestClient"/>
    /// node can use. Implementations must be Java beans, that is, to provide a default constructor and to implement the
    /// <see cref="#getName"/> method.
    ///
    /// <p/>The methods specified in this interface are called when the <see cref="TestClient"/> receives control instructions to
    /// apply to the test. There are control instructions to present the test case with the test invite, so that it may
    /// choose whether or not to participate in the test, assign the test to play the sender or receiver role, start the
    /// test and obtain the test status report.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Supply the name of the test case that this implements.
    /// <tr><td> Accept/Reject invites based on test parameters.
    /// <tr><td> Adapt to assigned roles.
    /// <tr><td> Perform test case actions.
    /// <tr><td> Generate test reports.
    /// </table>
    /// </summary>
    public interface TestClientControlledTest
    {
        /// <summary> Defines the possible test case roles that an interop test case can take on. </summary>
        public enum Roles
        {
            /// <summary> Specifies the sender role. </summary>
            SENDER,

            /// <summary> Specifies the receivers role. </summary>
            RECEIVER
        }

        /// <summary>
        /// Should provide the name of the test case that this class implements. The exact names are defined in the
        /// interop testing spec.
        /// </summary>
        /// <return> The name of the test case that this implements. </return>
        public string getName();

        /// <summary>
        /// Determines whether the test invite that matched this test case is acceptable.
        /// </summary>
        /// <param name="inviteMessage"> The invitation to accept or reject. </param>
        ///
        /// <return> <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it. </return>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public bool acceptInvite(Message inviteMessage) throws JMSException;

        /// <summary>
        /// Assigns the role to be played by this test case. The test parameters are fully specified in the
        /// assignment message. When this method return the test case will be ready to execute.
        /// </summary>
        /// <param name="role">              The role to be played; sender or receivers. </param>
        /// <param name="assignRoleMessage"> The role assingment message, contains the full test parameters. </param>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public void assignRole(Roles role, Message assignRoleMessage) throws JMSException;

        /// <summary>
        /// Performs the test case actions. Returning from here, indicates that the sending role has completed its test.
        /// </summary>
        /// <param name="numMessages"> The number of test messages to send. </param>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public void start(int numMessages) throws JMSException;

        /// <summary>
        /// Gets a report on the actions performed by the test case in its assigned role.
        /// </summary>
        /// <param name="session"> The controlSession to create the report message in. </param>
        ///
        /// <return> The report message. </return>
        ///
        /// <exception cref="JMSException"> Any JMSExceptions resulting from creating the report are allowed to fall through. </exception>
        public Message getReport(Session session) throws JMSException;
    }
}
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
using javax.jms.JMSException;
using javax.jms.Message;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A DropIn test is a test case that can accept late joining test clients into a running test. This can be usefull,
    /// for interactive experimentation.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Accept late joining test clients.
    /// </table>
    /// </summary>
    public interface DropInTest
    {
        /// <summary>
        /// Should accept a late joining client into a running test case. The client will be enlisted with a control message
        /// with the 'CONTROL_TYPE' field set to the value 'LATEJOIN'. It should also provide values for the fields:
        ///
        /// <p/><table>
        /// <tr><td> CLIENT_NAME <td> A unique name for the new client.
        /// <tr><td> CLIENT_PRIVATE_CONTROL_KEY <td> The key for the route on which the client receives its control messages.
        /// </table>
        /// </summary>
        /// <param name="message"> The late joiners join message. </param>
        ///
        /// <exception cref="JMSException"> Any JMS Exception are allowed to fall through, indicating that the join failed. </exception>
        public void lateJoin(Message message) throws JMSException;
    }
}
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

using javax.jms.ExceptionListener;
using javax.jms.JMSException;

using java.io.PrintWriter;
using java.io.StringWriter;
using java.util.ArrayList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// An exception monitor, listens for JMS exception on a connection or consumer. It record all exceptions that it receives
    /// and provides methods to test the number and type of exceptions received.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Record all exceptions received.
    /// </table>
    /// </summary>
    public class ExceptionMonitor : ExceptionListener
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(ExceptionMonitor));

        /// <summary> Holds the received exceptions. </summary>
        IList<Exception> exceptions = new ArrayList<Exception>();

        /// <summary>
        /// Receives incoming exceptions.
        /// </summary>
        /// <param name="e"> The exception to record. </param>
        public synchronized void onException(JMSException e)
        {
            log.debug("public void onException(JMSException e): called", e);

            exceptions.add(e);
        }

        /// <summary>
        /// Checks that no exceptions have been received.
        /// </summary>
        /// <return> <tt>true</tt> if no exceptions have been received, <tt>false</tt> otherwise. </return>
        public synchronized bool assertNoExceptions()
        {
            return exceptions.isEmpty();
        }

        /// <summary>
        /// Checks that exactly one exception has been received.
        /// </summary>
        /// <return> <tt>true</tt> if exactly one exception been received, <tt>false</tt> otherwise. </return>
        public synchronized bool assertOneJMSException()
        {
            return exceptions.size() == 1;
        }

        /// <summary>
        /// Checks that exactly one exception, with a linked cause of the specified type, has been received.
        /// </summary>
        /// <param name="aClass"> The type of the linked cause. </param>
        ///
        /// <return> <tt>true</tt> if exactly one exception, with a linked cause of the specified type, been received, </return>
        ///         <tt>false</tt> otherwise.
        public synchronized bool assertOneJMSExceptionWithLinkedCause(Class aClass)
        {
            if (exceptions.size() == 1)
            {
                Exception e = exceptions.get(0);

                if (e instanceof JMSException)
                {
                    JMSException jmse = (JMSException) e;

                    Exception linkedCause = jmse.getLinkedException();

                    if ((linkedCause != null) && aClass.isInstance(linkedCause))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        /// <summary>
        /// Checks that at least one exception of the the specified type, has been received.
        /// </summary>
        /// <param name="exceptionClass"> The type of the exception. </param>
        ///
        /// <return> <tt>true</tt> if at least one exception of the specified type has been received, <tt>false</tt> otherwise. </return>
        public synchronized bool assertExceptionOfType(Class exceptionClass)
        {
            // Start by assuming that the exception has no been received.
            bool passed = false;

            // Scan all the exceptions for a match.
            for (Exception e : exceptions)
            {
                if (exceptionClass.isInstance(e))
                {
                    passed = true;

                    break;
                }
            }

            return passed;
        }

        /// <summary>
        /// Reports the number of exceptions held by this monitor.
        /// </summary>
        /// <return> The number of exceptions held by this monitor. </return>
        public synchronized int size()
        {
            return exceptions.size();
        }

        /// <summary>
        /// Clears the record of received exceptions.
        /// </summary>
        public synchronized void reset()
        {
            exceptions = new ArrayList<Exception>();
        }

        /// <summary>
        /// Provides a dump of the stack traces of all exceptions that this exception monitor was notified of. Mainly
        /// use for debugging/test failure reporting purposes.
        /// </summary>
        /// <return> A string containing a dump of the stack traces of all exceptions. </return>
        public synchronized string ToString()
        {
            string result = "ExceptionMonitor: holds " + exceptions.size() + " exceptions.\n\n";

            for (Exception ex : exceptions)
            {
                result += getStackTrace(ex) + "\n";
            }

            return result;
        }

        /// <summary>
        /// Prints an exception stack trace into a string.
        /// </summary>
        /// <param name="t"> The throwable to get the stack trace from. </param>
        ///
        /// <return> A string containing the throwables stack trace. </return>
        public static string getStackTrace(Throwable t)
        {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw, true);
            t.printStackTrace(pw);
            pw.flush();
            sw.flush();

            return sw.ToString();
        }
    }
}
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
using org.apache.log4j.NDC;

using Apache.Qpid.Integration.Tests.framework.BrokerLifecycleAware;
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;

using uk.co.thebadgerset.junit.extensions.AsymptoticTestCase;
using uk.co.thebadgerset.junit.extensions.SetupTaskAware;
using uk.co.thebadgerset.junit.extensions.SetupTaskHandler;
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;
using uk.co.thebadgerset.junit.extensions.util.TestContextProperties;

using java.util.ArrayList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// FrameworkBaseCase provides a starting point for writing test cases against the test framework. Its main purpose is
    /// to provide some convenience methods for testing.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create and clean up in-vm brokers on every test case.
    /// <tr><td> Produce lists of assertions from assertion creation calls.
    /// <tr><td> Produce JUnit failures from assertion failures.
    /// <tr><td> Convert failed assertions to error messages.
    /// </table>
    /// </summary>
    public class FrameworkBaseCase extends AsymptoticTestCase : FrameworkTestContext, SetupTaskAware,
        BrokerLifecycleAware
    {
        /// <summary> Used for debugging purposes. </summary>
        private static ILog log = LogManager.GetLogger(typeof(FrameworkBaseCase));

        /// <summary> Holds the test sequencer to create and run test circuits with. </summary>
        protected CircuitFactory circuitFactory = new LocalCircuitFactory();

        /// <summary> Used to read the tests configurable properties through. </summary>
        protected ParsedProperties testProps;

        /// <summary> A default setup task processor to delegate setup tasks to. </summary>
        protected SetupTaskHandler taskHandler = new SetupTaskHandler();

        /// <summary> Flag used to track whether the test is in-vm or not. </summary>
        protected bool isUsingInVM;

        /// <summary> Holds the failure mechanism. </summary>
        protected CauseFailure failureMechanism = new CauseFailureUserPrompt();

        /// <summary>
        /// Creates a new test case with the specified name.
        /// </summary>
        /// <param name="name"> The test case name. </param>
        public FrameworkBaseCase(string name)
        {
            super(name);
        }

        /// <summary>
        /// Returns the test case sequencer that provides test circuit, and test sequence implementations. The sequencer
        /// that this base case returns by default is suitable for running a test circuit with both circuit ends colocated
        /// on the same JVM.
        /// </summary>
        /// <return> The test case sequencer. </return>
        protected CircuitFactory getCircuitFactory()
        {
            return circuitFactory;
        }

        /// <summary>
        /// Overrides the default test circuit factory. Test decorators can use this to supply distributed test sequencers or
        /// other test circuit factory specializations.
        /// </summary>
        /// <param name="circuitFactory"> The new test circuit factory. </param>
        public void setCircuitFactory(CircuitFactory circuitFactory)
        {
            this.circuitFactory = circuitFactory;
        }

        /// <summary>
        /// Reports the current test case name.
        /// </summary>
        /// <return> The current test case name. </return>
        public TestCaseVector getTestCaseVector()
        {
            return new TestCaseVector(this.getName(), 0);
        }

        /// <summary>
        /// Reports the current test case parameters.
        /// </summary>
        /// <return> The current test case parameters. </return>
        public MessagingTestConfigProperties getTestParameters()
        {
            return new MessagingTestConfigProperties(testProps);
        }

        /// <summary>
        /// Creates a list of assertions.
        /// </summary>
        /// <param name="asserts"> The assertions to compile in a list. </param>
        ///
        /// <return> A list of assertions. </return>
        protected IList<Assertion> assertionList(Assertion... asserts)
        {
            IList<Assertion> result = new ArrayList<Assertion>();

            for (Assertion assertion : asserts)
            {
                result.add(assertion);
            }

            return result;
        }

        /// <summary>
        /// Generates a JUnit assertion exception (failure) if any assertions are passed into this method, also concatenating
        /// all of the error messages in the assertions together to form an error message to diagnose the test failure with.
        /// </summary>
        /// <param name="asserts"> The list of failed assertions. </param>
        protected static void assertNoFailures(List<Assertion> asserts)
        {
            log.debug("protected void assertNoFailures(List<Assertion> asserts = " + asserts + "): called");

            // Check if there are no assertion failures, and return without doing anything if so.
            if ((asserts == null) || asserts.isEmpty())
            {
                return;
            }

            // Compile all of the assertion failure messages together.
            string errorMessage = assertionsToString(asserts);

            // Fail with the error message from all of the assertions.
            fail(errorMessage);
        }

        /// <summary>
        /// Converts a list of failed assertions into an error message.
        /// </summary>
        /// <param name="asserts"> The failed assertions. </param>
        ///
        /// <return> The error message. </return>
        protected static string assertionsToString(List<Assertion> asserts)
        {
            string errorMessage = "";

            for (Assertion assertion : asserts)
            {
                errorMessage += assertion.ToString() + "\n";
            }

            return errorMessage;
        }

        /// <summary>
        /// Ensures that the in-vm broker is created and initialized.
        /// </summary>
        ///
        /// <exception cref="Exception"> Any exceptions allowed to fall through and fail the test. </exception>
        protected void setUp() throws Exception
        {
            NDC.push(getName());

            testProps = TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

            // Process all optional setup tasks. This may include in-vm broker creation, if a decorator has added it.
            taskHandler.runSetupTasks();
        }

        /// <summary> Ensures that the in-vm broker is cleaned up after each test run. </summary>
        protected void tearDown()
        {
            NDC.pop();

            // Process all optional tear down tasks. This may include in-vm broker clean up, if a decorator has added it.
            taskHandler.runTearDownTasks();
        }

        /// <summary>
        /// Adds the specified task to the tests setup.
        /// </summary>
        /// <param name="task"> The task to add to the tests setup. </param>
        public void chainSetupTask(Runnable task)
        {
            taskHandler.chainSetupTask(task);
        }

        /// <summary>
        /// Adds the specified task to the tests tear down.
        /// </summary>
        /// <param name="task"> The task to add to the tests tear down. </param>
        public void chainTearDownTask(Runnable task)
        {
            taskHandler.chainTearDownTask(task);
        }

        /// <summary>
        /// Should provide a translation from the junit method name of a test to its test case name as known to the test
        /// clients that will run the test. The purpose of this is to convert the JUnit method name into the correct test
        /// case name to place into the test invite. For example the method "testP2P" might map onto the interop test case
        /// name "TC2_BasicP2P".
        /// </summary>
        /// <param name="methodName"> The name of the JUnit test method. </param>
        ///
        /// <return> The name of the corresponding interop test case. </return>
        public string getTestCaseNameForTestMethod(string methodName)
        {
            return methodName;
        }

        public void setInVmBrokers()
        {
            isUsingInVM = true;
        }

        /// <summary>
        /// Indicates whether or not a test case is using in-vm brokers.
        /// </summary>
        /// <return> <tt>true</tt> if the test is using in-vm brokers, <tt>false</tt> otherwise. </return>
        public bool usingInVmBroker()
        {
            return isUsingInVM;
        }

        /// <summary>
        /// Sets the currently live in-vm broker.
        /// </summary>
        /// <param name="i"> The currently live in-vm broker. </param>
        public void setLiveBroker(int i)
        { }

        /// <summary>
        /// Reports the currently live in-vm broker.
        /// </summary>
        /// <return> The currently live in-vm broker. </return>
        public int getLiveBroker()
        {
            return 0;
        }

        /// <summary>
        /// Accepts a failure mechanism.
        /// </summary>
        /// <param name="failureMechanism"> The failure mechanism. </param>
        public void setFailureMechanism(CauseFailure failureMechanism)
        {
            this.failureMechanism = failureMechanism;
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework
{

    /// <summary>
    /// A FrameworkTestContext provides context information to test code about the current test case being run; its name, its
    /// parameters.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide the name of the current test case.
    /// <tr><td> Provide the test parameters.
    /// </table>
    /// </summary>
    public interface FrameworkTestContext
    {
        /// <summary>
        /// Reports the current test case name.
        /// </summary>
        /// <return> The current test case name. </return>
        TestCaseVector getTestCaseVector();

        /// <summary>
        /// Reports the current test case parameters.
        /// </summary>
        /// <return> The current test case parameters. </return>
        MessagingTestConfigProperties getTestParameters();
    }
}
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

using Apache.Qpid.Integration.Tests.framework.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.*;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework.localcircuit
{
    /// <summary>
    /// LocalCircuitImpl provides an implementation of the test circuit. This is a local only circuit implementation that
    /// supports a single producer/consumer on each end of the circuit, with both ends of the circuit on the same JVM.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Supply the publishing and receiving ends of a test messaging circuit.
    ///     <td> <see cref="LocalPublisherImpl"/>, <see cref="LocalReceiverImpl"/>
    /// <tr><td> Start the circuit running.
    /// <tr><td> Close the circuit down.
    /// <tr><td> Take a reading of the circuits state.
    /// <tr><td> Apply assertions against the circuits state. <td> <see cref="Assertion"/>
    /// <tr><td> Send test messages over the circuit.
    /// <tr><td> Perform the default test procedure on the circuit.
    /// <tr><td> Provide access to connection and controlSession exception monitors. <td> <see cref="ExceptionMonitor"/>
    /// </table>
    /// </summary>
    public class LocalCircuitImpl : Circuit
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(LocalCircuitImpl));

        /// <summary> Holds the test configuration for the circuit. </summary>
        private ParsedProperties testProps;

        /// <summary> Holds the publishing end of the circuit. </summary>
        private LocalPublisherImpl publisher;

        /// <summary> Holds the receiving end of the circuit. </summary>
        private LocalReceiverImpl receiver;

        /// <summary> Holds the connection for the publishing end of the circuit. </summary>
        private Connection connection;

        /// <summary> Holds the exception listener for the connection on the publishing end of the circuit. </summary>
        private ExceptionMonitor connectionExceptionMonitor;

        /// <summary> Holds the exception listener for the controlSession on the publishing end of the circuit. </summary>
        private ExceptionMonitor exceptionMonitor;

        /// <summary>
        /// Creates a test circuit using the specified test parameters. The publisher, receivers, connection and
        /// connection monitor must already have been created, to assemble the circuit.
        /// </summary>
        /// <param name="testProps">                  The test parameters. </param>
        /// <param name="publisher">                  The test publisher. </param>
        /// <param name="receiver">                   The test receivers. </param>
        /// <param name="connection">                 The connection. </param>
        /// <param name="connectionExceptionMonitor"> The connection exception monitor. </param>
        public LocalCircuitImpl(ParsedProperties testProps, LocalPublisherImpl publisher, LocalReceiverImpl receiver,
                                Connection connection, ExceptionMonitor connectionExceptionMonitor)
        {
            this.testProps = testProps;
            this.publisher = publisher;
            this.receiver = receiver;
            this.connection = connection;
            this.connectionExceptionMonitor = connectionExceptionMonitor;
            this.exceptionMonitor = new ExceptionMonitor();

            // Set this as the parent circuit on the publisher and receivers.
            publisher.setCircuit(this);
            receiver.setCircuit(this);
        }

        /// <summary>
        /// Gets the interface on the publishing end of the circuit.
        /// </summary>
        /// <return> The publishing end of the circuit. </return>
        public Publisher getPublisher()
        {
            return publisher;
        }

        /// <summary>
        /// Gets the local publishing circuit end, for direct manipulation.
        /// </summary>
        /// <return> The local publishing circuit end. </return>
        public CircuitEnd getLocalPublisherCircuitEnd()
        {
            return publisher;
        }

        /// <summary>
        /// Gets the interface on the receiving end of the circuit.
        /// </summary>
        /// <return> The receiving end of the circuit. </return>
        public Receiver getReceiver()
        {
            return receiver;
        }

        /// <summary>
        /// Gets the local receiving circuit end, for direct manipulation.
        /// </summary>
        /// <return> The local receiving circuit end. </return>
        public CircuitEnd getLocalReceiverCircuitEnd()
        {
            return receiver;
        }

        /// <summary>
        /// Checks the test circuit. The effect of this is to gather the circuits state, for both ends of the circuit,
        /// into a report, against which assertions may be checked.
        /// </summary>
        public void check()
        { }

        /// <summary>
        /// Applied a list of assertions against the test circuit. The <see cref="#check()"/> method should be called before doing
        /// this, to ensure that the circuit has gathered its state into a report to assert against.
        /// </summary>
        /// <param name="assertions"> The list of assertions to apply. </param>
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> applyAssertions(List<Assertion> assertions)
        {
            IList<Assertion> failures = new LinkedList<Assertion>();

            for (Assertion assertion : assertions)
            {
                if (!assertion.apply())
                {
                    failures.add(assertion);
                }
            }

            return failures;
        }

        /// <summary> Connects and starts the circuit. After this method is called the circuit is ready to send messages. </summary>
        public void start()
        { }

        /// <summary> Closes the circuit. All associated resources are closed. </summary>
        public void close()
        {
            try
            {
                publisher.close();
                receiver.close();
                connection.close();
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Got JMSException during close:" + e.getMessage(), e);
            }
        }

        /// <summary> Sends a message on the test circuit. The exact nature of the message sent is controlled by the test parameters. </summary>
        protected void send()
        {
            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

            bool transactional = props.getPublisherTransacted();
            bool rollback = props.getRollbackPublisher();

            // Send a message through the publisher and log any exceptions raised.
            try
            {
                CircuitEnd end = getLocalPublisherCircuitEnd();

                end.send(createTestMessage(end));

                if (rollback)
                {
                    end.getSession().rollback();
                }
                else if (transactional)
                {
                    end.getSession().commit();
                }
            }
            catch (JMSException e)
            {
                exceptionMonitor.onException(e);
            }
        }

        /// <summary>
        /// Runs the default test procedure against the circuit, and checks that all of the specified assertions hold. The
        /// outline of the default test procedure is:
        ///
        /// <p/><pre>
        /// Start the circuit.
        /// Send test messages.
        /// Request a status report.
        /// Assert conditions on the publishing end of the circuit.
        /// Assert conditions on the receiving end of the circuit.
        /// Close the circuit.
        /// Pass with no failed assertions or fail with a list of failed assertions.
        /// </pre>
        /// </summary>
        /// <param name="numMessages"> The number of messages to send using the default test procedure. </param>
        /// <param name="assertions">  The list of assertions to apply. </param>
        /// <return> Any assertions that failed. </return>
        public IList<Assertion> test(int numMessages, List<Assertion> assertions)
        {
            // Start the test circuit.
            start();

            // Send the requested number of test messages.
            for (int i = 0; i < numMessages; i++)
            {
                send();
            }

            // Inject a short pause to allow time for exceptions to come back asynchronously.
            TestUtils.pause(500L);

            // Request a status report.
            check();

            // Clean up the publisher/receivers/controlSession/connections.
            close();

            // Apply all of the requested assertions, keeping record of any that fail.
            IList<Assertion> failures = applyAssertions(assertions);

            // Return any failed assertions to the caller.
            return failures;
        }

        /// <summary>
        /// Creates a message with the properties defined as per the test parameters.
        /// </summary>
        /// <param name="client"> The circuit end to create the message on. </param>
        ///
        /// <return> The test message. </return>
        ///
        /// <exception cref="JMSException"> Any JMSException occurring during creation of the message is allowed to fall through. </exception>
        private Message createTestMessage(CircuitEnd client) throws JMSException
        {
            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

            return TestUtils.createTestMessageOfSize(client.getSession(), props.getMessageSize());
        }

        /// <summary>
        /// Gets the exception monitor for the publishing ends connection.
        /// </summary>
        /// <return> The exception monitor for the publishing ends connection. </return>
        public ExceptionMonitor getConnectionExceptionMonitor()
        {
            return connectionExceptionMonitor;
        }

        /// <summary>
        /// Gets the exception monitor for the publishing ends controlSession.
        /// </summary>
        /// <return> The exception monitor for the publishing ends controlSession. </return>
        public ExceptionMonitor getExceptionMonitor()
        {
            return exceptionMonitor;
        }
    }
}
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
using Apache.Qpid.Integration.Tests.framework.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.MessageConsumer;
using javax.jms.MessageProducer;
using javax.jms.Session;

namespace Apache.Qpid.Integration.Tests.framework.localcircuit
{
    /// <summary>
    /// Provides an implementation of the <see cref="Publisher"/> interface and wraps a single message producer and consumer on
    /// a single controlSession, as a <see cref="CircuitEnd"/>. A local publisher also acts as a circuit end, because for a locally
    /// located circuit the assertions may be applied directly, there does not need to be any inter-process messaging
    /// between the publisher and its single circuit end, in order to ascertain its status.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide a message producer for sending messages.
    /// <tr><td> Provide a message consumer for receiving messages.
    /// <tr><td> Provide assertion that the publisher received no exceptions.
    /// <tr><td> Provide assertion that the publisher received a no consumers error code.
    /// <tr><td> Provide assertion that the publisher received a no route error code.
    /// </table>
    /// </summary>
    public class LocalPublisherImpl extends CircuitEndBase : Publisher
    {
        /// <summary> Holds a reference to the containing circuit. </summary>
        protected LocalCircuitImpl circuit;

        /// <summary>
        /// Creates a circuit end point on the specified producer, consumer and controlSession. Monitors are also configured
        /// for messages and exceptions received by the circuit end.
        /// </summary>
        /// <param name="producer"> The message producer for the circuit end point. </param>
        /// <param name="consumer"> The message consumer for the circuit end point. </param>
        /// <param name="session">  The controlSession for the circuit end point. </param>
        /// <param name="messageMonitor">   The monitor to notify of all messages received by the circuit end. </param>
        /// <param name="exceptionMonitor"> The monitor to notify of all exceptions received by the circuit end. </param>
        public LocalPublisherImpl(MessageProducer producer, MessageConsumer consumer, Session session,
                                  MessageMonitor messageMonitor, ExceptionMonitor exceptionMonitor)
        {
            super(producer, consumer, session, messageMonitor, exceptionMonitor);
        }

        /// <summary>
        /// Creates a circuit end point from the producer, consumer and controlSession in a circuit end base implementation.
        /// </summary>
        /// <param name="end"> The circuit end base implementation to take producers and consumers from. </param>
        public LocalPublisherImpl(CircuitEndBase end)
        {
            super(end.getProducer(), end.getConsumer(), end.getSession(), end.getMessageMonitor(), end.getExceptionMonitor());
        }

        /// <summary> Provides an assertion that the publisher encountered no exceptions. </summary>
        ///
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the publisher encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps)
        {
            return new AssertionBase()
                {
                    public bool apply()
                    {
                        bool passed = true;
                        ExceptionMonitor sessionExceptionMonitor = circuit.getExceptionMonitor();
                        ExceptionMonitor connectionExceptionMonitor = circuit.getConnectionExceptionMonitor();

                        if (!connectionExceptionMonitor.assertNoExceptions())
                        {
                            passed = false;

                            addError("Was expecting no exceptions.\n");
                            addError("Got the following exceptions on the connection, "
                                     + circuit.getConnectionExceptionMonitor());
                        }

                        if (!sessionExceptionMonitor.assertNoExceptions())
                        {
                            passed = false;

                            addError("Was expecting no exceptions.\n");
                            addError("Got the following exceptions on the producer, " + circuit.getExceptionMonitor());
                        }

                        return passed;
                    }
            };
        }

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Provides an assertion that the publisher got a given exception during the test.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. </param>
        ///
        /// <return> An assertion that the publisher got a given exception during the test. </return>
        public Assertion exceptionAssertion(ParsedProperties testProps, final Class<? extends Exception> exceptionClass)
        {
            return new AssertionBase()
                {
                    public bool apply()
                    {
                        bool passed = true;
                        ExceptionMonitor connectionExceptionMonitor = circuit.getConnectionExceptionMonitor();

                        if (!connectionExceptionMonitor.assertExceptionOfType(exceptionClass))
                        {
                            passed = false;

                            addError("Was expecting linked exception type " + exceptionClass.getName()
                                     + " on the connection.\n");
                            addError((connectionExceptionMonitor.size() > 0)
                                     ? ("Actually got the following exceptions on the connection, " + connectionExceptionMonitor)
                                     : "Got no exceptions on the connection.");
                        }

                        return passed;
                    }
            };
        }

        /// <summary>
        /// Sets the contianing circuit.
        /// </summary>
        /// <param name="circuit"> The containing circuit. </param>
        public void setCircuit(LocalCircuitImpl circuit)
        {
            this.circuit = circuit;
        }
    }
}
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
using Apache.Qpid.Integration.Tests.framework.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.MessageConsumer;
using javax.jms.MessageProducer;
using javax.jms.Session;

namespace Apache.Qpid.Integration.Tests.framework.localcircuit
{
    /// <summary>
    /// Provides an implementation of the <see cref="Receiver"/> interface that wraps a single message producer and consumer on
    /// a single controlSession, as a <see cref="CircuitEnd"/>. A local receiver also acts as a circuit end, because for a locally
    /// located circuit the assertions may be applied directly, there does not need to be any inter process messaging
    /// between the publisher and its single circuit end, in order to ascertain its status.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide a message producer for sending messages.
    /// <tr><td> Provide a message consumer for receiving messages.
    /// <tr><td> Provide assertion that the receivers received no exceptions.
    /// <tr><td> Provide assertion that the receivers received all test messages sent to it.
    /// </table>
    /// </summary>
    public class LocalReceiverImpl extends CircuitEndBase : Receiver
    {
        /// <summary> Holds a reference to the containing circuit. </summary>
        private LocalCircuitImpl circuit;

        /// <summary>
        /// Creates a circuit end point on the specified producer, consumer and controlSession. Monitors are also configured
        /// for messages and exceptions received by the circuit end.
        /// </summary>
        /// <param name="producer"> The message producer for the circuit end point. </param>
        /// <param name="consumer"> The message consumer for the circuit end point. </param>
        /// <param name="session">  The controlSession for the circuit end point. </param>
        /// <param name="messageMonitor">   The monitor to notify of all messages received by the circuit end. </param>
        /// <param name="exceptionMonitor"> The monitor to notify of all exceptions received by the circuit end. </param>
        public LocalReceiverImpl(MessageProducer producer, MessageConsumer consumer, Session session,
                                 MessageMonitor messageMonitor, ExceptionMonitor exceptionMonitor)
        {
            super(producer, consumer, session, messageMonitor, exceptionMonitor);
        }

        /// <summary>
        /// Creates a circuit end point from the producer, consumer and controlSession in a circuit end base implementation.
        /// </summary>
        /// <param name="end"> The circuit end base implementation to take producers and consumers from. </param>
        public LocalReceiverImpl(CircuitEndBase end)
        {
            super(end.getProducer(), end.getConsumer(), end.getSession(), end.getMessageMonitor(), end.getExceptionMonitor());
        }

        /// <summary>
        /// Provides an assertion that the receivers encountered no exceptions.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Provides an assertion that the receivers got all messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers got all messages that were sent to it. </return>
        public Assertion allMessagesReceivedAssertion(ParsedProperties testProps)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Provides an assertion that the receivers got none of the messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers got none of the messages that were sent to it. </return>
        public Assertion noMessagesReceivedAssertion(ParsedProperties testProps)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Provides an assertion that the receiver got a given exception during the test.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. <return> An assertion that the receiver got a given exception during the test. </return> </param>
        public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass)
        {
            return new NotApplicableAssertion(testProps);
        }

        /// <summary>
        /// Sets the contianing circuit.
        /// </summary>
        /// <param name="circuit"> The containing circuit. </param>
        public void setCircuit(LocalCircuitImpl circuit)
        {
            this.circuit = circuit;
        }
    }
}
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

using Apache.Qpid.Integration.Tests.framework.localcircuit.LocalCircuitImpl;
using Apache.Qpid.Integration.Tests.framework.localcircuit.LocalPublisherImpl;
using Apache.Qpid.Integration.Tests.framework.localcircuit.LocalReceiverImpl;
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.*;

using System.Collections.Generic.IList;
using java.util.Properties;
using java.util.concurrent.atomic.AtomicLong;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// LocalCircuitFactory is a circuit factory that creates test circuits with publishing and receiving ends rooted
    /// on the same JVM. The ends of the circuit are presented as <see cref="Publisher"/> and <see cref="Receiver"/> interfaces, which
    /// in turn provide methods to apply assertions to the circuit. The creation of the circuit ends, and the presentation
    /// of the ends as publisher/receiver interfaces, are designed to be overriden, so that circuits and assertions that
    /// use messaging features not available in JMS can be written. This provides an extension point for writing tests
    /// against proprietary features of JMS implementations.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Provide a standard test procedure over a test circuit.
    /// <tr><td> Construct test circuits appropriate to a tests context.
    /// </table>
    /// </summary>
    public class LocalCircuitFactory : CircuitFactory
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(LocalCircuitFactory));

        /// <summary> Used to create unique destination names for each test. </summary>
        protected static AtomicLong uniqueDestsId = new AtomicLong();

        /// <summary>
        /// Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
        /// begining the test and gathering the test reports from the participants.
        /// </summary>
        /// <param name="testCircuit">    The test circuit. </param>
        /// <param name="assertions">     The list of assertions to apply to the test circuit. </param>
        /// <param name="testProperties"> The test case definition. </param>
        public void sequenceTest(Circuit testCircuit, IList<Assertion> assertions, Properties testProperties)
        {
            FrameworkBaseCase.assertNoFailures(testCircuit.test(1, assertions));
        }

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        ///
        /// <return> A test circuit. </return>
        public Circuit createCircuit(ParsedProperties testProperties)
        {
            Circuit result;

            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProperties);

            // Create a standard publisher/receivers test client pair on a shared connection, individual sessions.
            try
            {
                // Get a unique offset to append to destination names to make them unique to the connection.
                long uniqueId = uniqueDestsId.incrementAndGet();

                // Set up the connection.
                Connection connection = TestUtils.createConnection(testProperties);

                // Add the connection exception listener to assert on exception conditions with.
                // ExceptionMonitor exceptionMonitor = new ExceptionMonitor();
                // connection.setExceptionListener(exceptionMonitor);

                // Set up the publisher.
                CircuitEndBase publisherEnd = createPublisherCircuitEnd(connection, props, uniqueId);

                // Set up the receiver.
                CircuitEndBase receiverEnd = createReceiverCircuitEnd(connection, props, uniqueId);

                // Start listening for incoming messages.
                connection.start();

                // Namespace everything up.
                LocalPublisherImpl publisher = createPublisherFromCircuitEnd(publisherEnd);
                LocalReceiverImpl receiver = createReceiverFromCircuitEnd(receiverEnd);

                result = new LocalCircuitImpl(testProperties, publisher, receiver, connection, publisher.getExceptionMonitor());
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Could not create publisher/receivers pair due to a JMSException.", e);
            }

            return result;
        }

        /// <summary>
        /// Creates a local <see cref="Receiver"/> from a <see cref="CircuitEnd"/>. Sub-classes may override this to provide more
        /// specialized receivers if necessary.
        /// </summary>
        /// <param name="receiverEnd"> The receiving circuit end. </param>
        ///
        /// <return> A <see cref="Receiver"/>. </return>
        protected LocalReceiverImpl createReceiverFromCircuitEnd(CircuitEndBase receiverEnd)
        {
            return new LocalReceiverImpl(receiverEnd);
        }

        /// <summary>
        /// Creates a local <see cref="Publisher"/> from a <see cref="CircuitEnd"/>. Sub-classes may override this to provide more
        /// specialized receivers if necessary.
        /// </summary>
        /// <param name="publisherEnd"> The publishing circuit end. </param>
        ///
        /// <return> A <see cref="Receiver"/>. </return>
        protected LocalPublisherImpl createPublisherFromCircuitEnd(CircuitEndBase publisherEnd)
        {
            return new LocalPublisherImpl(publisherEnd);
        }

        /// <summary>
        /// Builds a circuit end suitable for the publishing side of a test circuit, from standard test parameters.
        /// </summary>
        /// <param name="connection"> The connection to build the circuit end on. </param>
        /// <param name="testProps">  The test parameters to configure the circuit end construction. </param>
        /// <param name="uniqueId">   A unique number to being numbering destinations from, to make this circuit unique. </param>
        ///
        /// <return> A circuit end suitable for the publishing side of a test circuit. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through and fail the creation. </exception>
        public CircuitEndBase createPublisherCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId)
            throws JMSException
        {
            log.debug(
                      "public CircuitEndBase createPublisherCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId = "
                      + uniqueId + "): called");

            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

            // Check that the test properties do not contain AMQP/Qpid specific settings, and fail if they do.
            if (props.getImmediate() || props.getMandatory())
            {
                throw new RuntimeException(
                                           "Cannot create a pure JMS circuit as the test properties require AMQP specific options.");
            }

            Session session = connection.createSession(props.getPublisherTransacted(), props.getAckMode());

            Destination destination =
                props.getPubsub() ? session.createTopic(props.getSendDestinationNameRoot() + "_" + uniqueId)
                : session.createQueue(props.getSendDestinationNameRoot() + "_" + uniqueId);

            MessageProducer producer = props.getPublisherProducerBind() ? session.createProducer(destination) : null;

            MessageConsumer consumer =
                props.getPublisherConsumerBind()
                ? session.createConsumer(session.createQueue(props.getReceiveDestinationNameRoot() + "_" + uniqueId)) : null;

            MessageMonitor messageMonitor = new MessageMonitor();

            if (consumer != null)
            {
                consumer.setMessageListener(messageMonitor);
            }

            ExceptionMonitor exceptionMonitor = new ExceptionMonitor();
            connection.setExceptionListener(exceptionMonitor);

            if (!props.getPublisherConsumerActive() && (consumer != null))
            {
                consumer.close();
            }

            return new CircuitEndBase(producer, consumer, session, messageMonitor, exceptionMonitor);
        }

        /// <summary>
        /// Builds a circuit end suitable for the receiving side of a test circuit, from standard test parameters.
        /// </summary>
        /// <param name="connection"> The connection to build the circuit end on. </param>
        /// <param name="testProps">  The test parameters to configure the circuit end construction. </param>
        /// <param name="uniqueId">   A unique number to being numbering destinations from, to make this circuit unique. </param>
        ///
        /// <return> A circuit end suitable for the receiving side of a test circuit. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through and fail the creation. </exception>
        public CircuitEndBase createReceiverCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId)
            throws JMSException
        {
            log.debug(
                      "public CircuitEndBase createReceiverCircuitEnd(Connection connection, ParsedProperties testProps, long uniqueId = "
                      + uniqueId + "): called");

            // Cast the test properties into a typed interface for convenience.
            MessagingTestConfigProperties props = new MessagingTestConfigProperties(testProps);

            // Check that the test properties do not contain AMQP/Qpid specific settings, and fail if they do.
            if (props.getImmediate() || props.getMandatory())
            {
                throw new RuntimeException(
                                           "Cannot create a pure JMS circuit as the test properties require AMQP specific options.");
            }

            Session session = connection.createSession(props.getPublisherTransacted(), props.getAckMode());

            MessageProducer producer =
                props.getReceiverProducerBind()
                ? session.createProducer(session.createQueue(props.getReceiveDestinationNameRoot() + "_" + uniqueId)) : null;

            Destination destination =
                props.getPubsub() ? session.createTopic(props.getSendDestinationNameRoot() + "_" + uniqueId)
                : session.createQueue(props.getSendDestinationNameRoot() + "_" + uniqueId);

            MessageConsumer consumer =
                props.getReceiverConsumerBind()
                ? ((props.getDurableSubscription() && props.getPubsub())
                   ? session.createDurableSubscriber((Topic) destination, "testsub") : session.createConsumer(destination))
                : null;

            MessageMonitor messageMonitor = new MessageMonitor();

            if (consumer != null)
            {
                consumer.setMessageListener(messageMonitor);
            }

            if (!props.getReceiverConsumerActive() && (consumer != null))
            {
                consumer.close();
            }

            return new CircuitEndBase(producer, consumer, session, messageMonitor, null);
        }

        /// <summary>
        /// Sets the sender test client to coordinate the test with.
        /// </summary>
        /// <param name="sender"> The contact details of the sending client in the test. </param>
        public void setSender(TestClientDetails sender)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Sets the receiving test client to coordinate the test with.
        /// </summary>
        /// <param name="receiver"> The contact details of the sending client in the test. </param>
        public void setReceiver(TestClientDetails receiver)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Supplies the sending test client.
        /// </summary>
        /// <return> The sending test client. </return>
        public TestClientDetails getSender()
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Supplies the receiving test client.
        /// </summary>
        /// <return> The receiving test client. </return>
        public IList<TestClientDetails> getReceivers()
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Accepts the conversation factory over which to hold the test coordinating conversation.
        /// </summary>
        /// <param name="conversationFactory"> The conversation factory to coordinate the test over. </param>
        public void setConversationFactory(ConversationFactory conversationFactory)
        {
            throw new RuntimeException("Not implemented.");
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// MessageIdentityVector provides a message identification scheme, that matches individual messages with test cases.
    /// Test messages are being sent by a number of test clients, sending messages over a set of routes, and being received
    /// by another set of test clients. Each test is itself, being run within a test cycle, of which there could be many. It
    /// is the job of the test coordinator to request and receive reports from the available test clients, on what has been
    /// sent, what has been received, and what errors may have occurred, and to reconcile this information against the
    /// assertions being applied by the test case. In order to be able to figure out which messages belong to which test,
    /// there needs to be an identification scheme, that the coordinator can use to correlate messages in senders and
    /// receiver reports. Every message sent in a test can be associated with this information.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Identify a test case, a handling client id, a circuit end within the client, and a test cycle number.
    /// </table>
    /// </summary>
    public class MessageIdentityVector
    {
        /// <summary> Holds the test case vector component of the message identity vector. </summary>
        private TestCaseVector testCaseVector;

        /// <summary> The unique client id. </summary>
        private string clientId;

        /// <summary> The unique circuit end number within the client id. </summary>
        private int circuitEndId;

        /// <summary>
        /// Creates a new identity vector for test messages.
        /// </summary>
        /// <param name="testCase">        The name of the test case generating the messages. </param>
        /// <param name="clientId">        The unique id of the client implementing a circuit end that is handling the messages. </param>
        /// <param name="circuitEndId">    The unique id number of the circuit end within the client. </param>
        /// <param name="testCycleNumber"> The cycle iteration number of the test case. </param>
        public MessageIdentityVector(string testCase, string clientId, int circuitEndId, int testCycleNumber)
        {
            this.testCaseVector = new TestCaseVector(testCase, testCycleNumber);
            this.clientId = clientId;
            this.circuitEndId = circuitEndId;
        }

        /// <summary>
        /// Reports the test case vector component of the message identity vector.
        /// </summary>
        /// <return> The test case vector component of the message identity vector. </return>
        public TestCaseVector getTestCaseVector()
        {
            return testCaseVector;
        }

        /// <summary>
        /// Reports the name of the test case.
        /// </summary>
        /// <return> The name of the test case. </return>
        public string getTestCase()
        {
            return testCaseVector.getTestCase();
        }

        /// <summary>
        /// Reports the test iteration cycle number within the test case.
        /// </summary>
        /// <return> The test iteration cycle number within the test case. </return>
        public int getTestCycleNumber()
        {
            return testCaseVector.getTestCycleNumber();
        }

        /// <summary>
        /// Resports the client id.
        /// </summary>
        /// <return> The client id. </return>
        public string getClientId()
        {
            return clientId;
        }

        /// <summary>
        /// Reports the circuit end number within the test client.
        /// </summary>
        /// <return> The circuit end number within the test client. </return>
        public int getCircuitEndId()
        {
            return circuitEndId;
        }

        /// <summary>
        /// Compares this identity vector with another for equality. All fields must match.
        /// </summary>
        /// <param name="o"> The identity vector to compare with. </param>
        ///
        /// <return> <tt>true</tt> if the identity vector is identical to this one by all fields, <tt>false</tt> otherwise. </return>
        public bool equals(Object o)
        {
            if (this == o)
            {
                return true;
            }

            if ((o == null) || (getClass() != o.getClass()))
            {
                return false;
            }

            MessageIdentityVector that = (MessageIdentityVector) o;

            if (circuitEndId != that.circuitEndId)
            {
                return false;
            }

            if ((clientId != null) ? (!clientId.equals(that.clientId)) : (that.clientId != null))
            {
                return false;
            }

            if ((testCaseVector != null) ? (!testCaseVector.equals(that.testCaseVector)) : (that.testCaseVector != null))
            {
                return false;
            }

            return true;
        }

        /// <summary>
        /// Computes a hash code for this identity vector based on all fields.
        /// </summary>
        /// <return> A hash code for this identity vector based on all fields. </return>
        public int hashCode()
        {
            int result;
            result = ((testCaseVector != null) ? testCaseVector.hashCode() : 0);
            result = (31 * result) + ((clientId != null) ? clientId.hashCode() : 0);
            result = (31 * result) + circuitEndId;

            return result;
        }
    }
}
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

using javax.jms.Message;
using javax.jms.MessageListener;

using java.util.concurrent.atomic.AtomicInteger;

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

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// A Publisher represents the status of the publishing side of a test circuit. Its main purpose is to provide assertions
    /// that can be applied to test the behaviour of the publishers.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide assertion that the publishers received no exceptions.
    /// </table>
    /// </summary>
    ///
    /// <remarks> There are mixtures of AMQP and JMS assertions in this interface. Either keep them here, but quietly (or with a
    ///       warning or error) drop them from test cases where they are not relevant, or push them down into sub-classes.
    ///       I am tempted to go with the dropping/warning/error approach, that would imply that it makes sense to pull
    ///       the assertions back from AMQPPublisher to here.</remarks>
    public interface Publisher
    {
        // Assertions that are meaningfull to AMQP and to JMS.

        /// <summary>
        /// Provides an assertion that the publisher encountered no exceptions.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the publisher encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps);

        // Assertions that are meaningfull only to AMQP.

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps);

        // Assertions that are meaningfull only to Java/JMS.

        /// <summary>
        /// Provides an assertion that the publisher got a given exception during the test.
        /// </summary>
        /// <param name="testProps">      The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. </param>
        ///
        /// <return> An assertion that the publisher got a given exception during the test. </return>
        public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass);
    }
}
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
    ///
    /// <remarks> There are mixtures of AMQP and JMS assertions in this interface. Either keep them here, but quietly (or with a
    ///       warning or error) drop them from test cases where they are not relevant, or push them down into sub-classes.
    ///       I am tempted to go with the dropping/warning/error approach.</remarks>
    public interface Receiver
    {
        // Assertions that are meaningfull to AMQP and to JMS.

        /// <summary>
        /// Provides an assertion that the receivers encountered no exceptions.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers encountered no exceptions. </return>
        public Assertion noExceptionsAssertion(ParsedProperties testProps);

        /// <summary>
        /// Provides an assertion that the receivers got all messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers got all messages that were sent to it. </return>
        public Assertion allMessagesReceivedAssertion(ParsedProperties testProps);

        /// <summary>
        /// Provides an assertion that the receivers got none of the messages that were sent to it.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the receivers got none of the messages that were sent to it. </return>
        public Assertion noMessagesReceivedAssertion(ParsedProperties testProps);

        // Assertions that are meaningfull only to AMQP.

        /// <summary>
        /// Provides an assertion that the AMQP channel was forcibly closed by an error condition.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        ///
        /// <return> An assertion that the AMQP channel was forcibly closed by an error condition. </return>
        public Assertion channelClosedAssertion(ParsedProperties testProps);

        // Assertions that are meaningfull only to Java/JMS.

        /// <summary>
        /// Provides an assertion that the receiver got a given exception during the test.
        /// </summary>
        /// <param name="testProps"> The test configuration properties. </param>
        /// <param name="exceptionClass"> The exception class to check for. </param>
        ///
        /// <return> An assertion that the receiver got a given exception during the test. </return>
        public Assertion exceptionAssertion(ParsedProperties testProps, Class<? extends Exception> exceptionClass);
    }
}
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

using Apache.Qpid.Integration.Tests.framework.Circuit;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using org.apache.qpid.util.ConversationFactory;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;
using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework.sequencers
{
    /// <summary>
    /// BaseCircuitFactory provides some functionality common to all <see cref="CircuitFactory"/>s, such as the details of
    /// all <see cref="Apache.Qpid.Integration.Tests.framework.distributedtesting.TestClient"/>s that make up the end-points of
    /// the circuits that the factory creates, and an active <see cref="ConversationFactory"/> that can be used to generate
    /// control conversations with those circuit end-points.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Hold the details of the sending and receiving end-points to create circuits from.
    /// <tr><td> Provide a conversation factory to create control conversations with the end-points.
    /// </table>
    /// </summary>
    public abstract class BaseCircuitFactory : CircuitFactory
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(BaseCircuitFactory));

        /// <summary> Holds the contact details for the sending test client. </summary>
        protected TestClientDetails sender;

        /// <summary> Holds the contact details for the receving test client. </summary>
        protected IList<TestClientDetails> receivers = new LinkedList<TestClientDetails>();

        /// <summary> Holds the conversation factory over which to coordinate the test. </summary>
        protected ConversationFactory conversationFactory;

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        /// <return> A test circuit. </return>
        public Circuit createCircuit(Properties testProperties)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Sets the sender test client to coordinate the test with.
        /// </summary>
        /// <param name="sender"> The contact details of the sending client in the test. </param>
        public void setSender(TestClientDetails sender)
        {
            log.debug("public void setSender(TestClientDetails sender = " + sender + "): called");

            this.sender = sender;
        }

        /// <summary>
        /// Sets the receiving test client to coordinate the test with.
        /// </summary>
        /// <param name="receiver"> The contact details of the sending client in the test. </param>
        public void setReceiver(TestClientDetails receiver)
        {
            log.debug("public void setReceiver(TestClientDetails receivers = " + receiver + "): called");

            this.receivers.add(receiver);
        }

        /// <summary>
        /// Supplies the sending test client.
        /// </summary>
        /// <return> The sending test client. </return>
        public TestClientDetails getSender()
        {
            return sender;
        }

        /// <summary>
        /// Supplies the receiving test client.
        /// </summary>
        /// <return> The receiving test client. </return>
        public IList<TestClientDetails> getReceivers()
        {
            return receivers;
        }

        /// <summary>
        /// Accepts the conversation factory over which to hold the test coordinating conversation.
        /// </summary>
        /// <param name="conversationFactory"> The conversation factory to coordinate the test over. </param>
        public void setConversationFactory(ConversationFactory conversationFactory)
        {
            this.conversationFactory = conversationFactory;
        }

        /// <summary>
        /// Provides the conversation factory for providing the distributed test sequencing conversations over the test
        /// connection.
        /// </summary>
        /// <return> The conversation factory to create test sequencing conversations with. </return>
        public ConversationFactory getConversationFactory()
        {
            return conversationFactory;
        }
    }
}
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
using Apache.Qpid.Integration.Tests.framework.Assertion;
using Apache.Qpid.Integration.Tests.framework.Circuit;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.JMSException;
using javax.jms.Message;

using System.Collections.Generic.IList;
using System.Collections.Generic.IDictionary;
using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework.sequencers
{
    /// <summary>
    /// A CircuitFactory is responsibile for creating test circuits appropriate to the context that a test case is
    /// running in, and providing an implementation of a standard test procedure over a test circuit.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide a standard test procedure over a test circuit.
    /// <tr><td> Construct test circuits appropriate to a tests context.
    /// </table>
    /// </summary>
    public interface CircuitFactory
    {
        /// <summary>
        /// Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
        /// begining the test, gathering the test reports from the participants, and checking for assertion failures against
        /// the test reports.
        /// </summary>
        /// <param name="testCircuit">    The test circuit. </param>
        /// <param name="assertions">     The list of assertions to apply to the test circuit. </param>
        /// <param name="testProperties"> The test case definition. </param>
        ///
        /// @deprecated Use test circuits and Circuit.test instead.
        public void sequenceTest(Circuit testCircuit, IList<Assertion> assertions, Properties testProperties);

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        ///
        /// <return> A test circuit. </return>
        public Circuit createCircuit(ParsedProperties testProperties);

        /// <summary>
        /// Sets the sender test client to coordinate the test with.
        /// </summary>
        /// <param name="sender"> The contact details of the sending client in the test. </param>
        public void setSender(TestClientDetails sender);

        /// <summary>
        /// Sets the receiving test client to coordinate the test with.
        /// </summary>
        /// <param name="receiver"> The contact details of the sending client in the test. </param>
        public void setReceiver(TestClientDetails receiver);

        /// <summary>
        /// Supplies the sending test client.
        /// </summary>
        /// <return> The sending test client. </return>
        public TestClientDetails getSender();

        /// <summary>
        /// Supplies the receiving test client.
        /// </summary>
        /// <return> The receiving test client. </return>
        public IList<TestClientDetails> getReceivers();

        /// <summary>
        /// Accepts the conversation factory over which to hold the test coordinating conversation.
        /// </summary>
        /// <param name="conversationFactory"> The conversation factory to coordinate the test over. </param>
        public void setConversationFactory(ConversationFactory conversationFactory);
    }
}
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

using Apache.Qpid.Integration.Tests.framework.Assertion;
using Apache.Qpid.Integration.Tests.framework.Circuit;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using Apache.Qpid.Integration.Tests.framework.TestUtils;
using Apache.Qpid.Integration.Tests.framework.distributedcircuit.DistributedCircuitImpl;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.Destination;
using javax.jms.JMSException;
using javax.jms.Message;
using javax.jms.Session;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;
using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework.sequencers
{
    /// <summary>
    /// FanOutCircuitFactory is a circuit factory that creates distributed test circuits. Given a set of participating
    /// test client nodes, it assigns one node to the SENDER role and the remainder to the RECEIVER role.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create distributed circuits from one to many test nodes, for fanout style testing.
    /// </table>
    /// </summary>
    ///
    /// <remarks> Adapt this to be an n*m topology circuit factory. Need to add circuit topology definitions to the test
    ///       parameters. Place n senders onto the available test clients, and m receivers. Where n or m is larger than
    ///       the available nodes, start stacking multiple test clients on each node. There will also be an option that
    ///       indicates whether nodes can play both roles, and how many nodes out of all available may be assigned to
    ///       each role.</remarks>
    ///
    /// <remarks> The createCircuit methods on this and InteropCircuitFactory are going to be identical. This is because the
    ///       partitioning into senders and receivers is already done by the test decorators. Either eliminate these factories
    ///       as unnesesary, or move the partitioning functionality into the factories, in which case the test decorators
    ///       can probably be merged or eliminated. There is confusion over the placement of responsibilities between the
    ///       factories and the test decorators... although the test decorators may well do more than just circuit creation
    ///       in the future. For example, there may have to be a special decorator for test repetition that does one circuit
    ///       creation, but the runs many tests over it, in which case the handling of responsibilities becomes clearer.</remarks>
    public class FanOutCircuitFactory extends BaseCircuitFactory
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(FanOutCircuitFactory));

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        /// <return> A test circuit. </return>
        public Circuit createCircuit(ParsedProperties testProperties)
        {
            log.debug("public Circuit createCircuit(ParsedProperties testProperties): called");

            IList<TestClientDetails> senders = new LinkedList<TestClientDetails>();
            senders.add(getSender());
            IList<TestClientDetails> receivers = getReceivers();
            ConversationFactory conversationFactory = getConversationFactory();

            return DistributedCircuitImpl.createCircuit(testProperties, senders, receivers, conversationFactory);
        }

        /// <summary>
        /// Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
        /// begining the test, gathering the test reports from the participants, and checking for assertion failures against
        /// the test reports.
        /// </summary>
        /// <param name="testCircuit">    The test circuit. </param>
        /// <param name="assertions">     The list of assertions to apply to the test circuit. </param>
        /// <param name="testProperties"> The test case definition. </param>
        ///
        /// @deprecated Scheduled for removal once existing tests converted over to use test circuits.
        public void sequenceTest(Circuit testCircuit, IList<Assertion> assertions, Properties testProperties)
        {
            log.debug("protected Message[] sequenceTest(Object... testProperties = " + testProperties + "): called");

            TestClientDetails sender = getSender();
            IList<TestClientDetails> receivers = getReceivers();
            ConversationFactory conversationFactory = getConversationFactory();

            try
            {
                // Create a conversation on the sender clients private control route.
                Session session = conversationFactory.getSession();
                Destination senderControlTopic = session.createTopic(sender.privateControlKey);
                ConversationFactory.Conversation senderConversation = conversationFactory.startConversation();

                // Assign the sender role to the sending test client.
                Message assignSender = conversationFactory.getSession().createMessage();
                TestUtils.setPropertiesOnMessage(assignSender, testProperties);
                assignSender.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                assignSender.setStringProperty("ROLE", "SENDER");
                assignSender.setStringProperty("CLIENT_NAME", "Sustained_SENDER");

                senderConversation.send(senderControlTopic, assignSender);

                // Wait for the sender to confirm its role.
                senderConversation.receive();

                // Assign the receivers roles.
                for (TestClientDetails receiver : receivers)
                {
                    assignReceiverRole(receiver, testProperties, true);
                }

                // Start the test on the sender.
                Message start = session.createMessage();
                start.setStringProperty("CONTROL_TYPE", "START");

                senderConversation.send(senderControlTopic, start);

                // Wait for the test sender to return its report.
                Message senderReport = senderConversation.receive();
                TestUtils.pause(500);

                // Ask the receivers for their reports.
                Message statusRequest = session.createMessage();
                statusRequest.setStringProperty("CONTROL_TYPE", "STATUS_REQUEST");

                // Gather the reports from all of the receiving clients.

                // Return all of the test reports, the senders report first.
                // return new Message[] { senderReport };
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Unhandled JMSException.");
            }
        }

        /// <summary>
        /// Assigns the receivers role to the specified test client that is to act as a receivers during the test. This method
        /// does not always wait for the receiving clients to confirm their role assignments. This is because this method
        /// may be called from an 'onMessage' method, when a client is joining the test at a later point in time, and it
        /// is not possible to do a synchronous receive during an 'onMessage' method. There is a flag to indicate whether
        /// or not to wait for role confirmations.
        /// </summary>
        /// <param name="receiver">       The test client to assign the receivers role to. </param>
        /// <param name="testProperties"> The test parameters. </param>
        /// <param name="confirm">        Indicates whether role confirmation should be waited for. </param>
        ///
        /// <exception cref="JMSException"> Any JMSExceptions occurring during the conversation are allowed to fall through. </exception>
        ///
        /// @deprecated Scheduled for removal once existing tests converted over to use test circuits.
        protected void assignReceiverRole(TestClientDetails receiver, Properties testProperties, bool confirm)
            throws JMSException
        {
            log.info("assignReceiverRole(TestClientDetails receivers = " + receiver + ", Map<String, Object> testProperties = "
                     + testProperties + "): called");

            ConversationFactory conversationFactory = getConversationFactory();

            // Create a conversation with the receiving test client.
            Session session = conversationFactory.getSession();
            Destination receiverControlTopic = session.createTopic(receiver.privateControlKey);
            ConversationFactory.Conversation receiverConversation = conversationFactory.startConversation();

            // Assign the receivers role to the receiving client.
            Message assignReceiver = session.createMessage();
            TestUtils.setPropertiesOnMessage(assignReceiver, testProperties);
            assignReceiver.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
            assignReceiver.setStringProperty("ROLE", "RECEIVER");
            assignReceiver.setStringProperty("CLIENT_NAME", receiver.clientName);

            receiverConversation.send(receiverControlTopic, assignReceiver);

            // Wait for the role confirmation to come back.
            if (confirm)
            {
                receiverConversation.receive();
            }
        }
    }
}
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

using Apache.Qpid.Integration.Tests.framework.Assertion;
using Apache.Qpid.Integration.Tests.framework.Circuit;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using Apache.Qpid.Integration.Tests.framework.TestUtils;
using Apache.Qpid.Integration.Tests.framework.distributedcircuit.DistributedCircuitImpl;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.Destination;
using javax.jms.JMSException;
using javax.jms.Message;
using javax.jms.Session;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;
using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework.sequencers
{
    /// <summary>
    /// InteropCircuitFactory is a circuit factory that creates distributed test circuits. Given a set of participating
    /// test client nodes, it assigns one node to the SENDER role and one the RECEIVER role.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create distributed circuits from pairs of test nodes, for interop style testing.
    /// </table>
    /// </summary>
    ///
    /// <remarks> The partitioning of a set of nodes into sender and receiver roles is actually done by the interop test
    ///       decorator. See the todo comment in FanOutCircuitFactory about merging the factories with the decorators, or
    ///       more carefully dividing up responsibilities between them.</remarks>
    ///
    /// <remarks> The squenceTest code is deprecated, but currently still used by the interop tests. It will be removed once it
    ///       have been fully replaced by the default test procedure.</remarks>
    public class InteropCircuitFactory extends BaseCircuitFactory
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(InteropCircuitFactory));

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        /// <return> A test circuit. </return>
        public Circuit createCircuit(ParsedProperties testProperties)
        {
            log.debug("public Circuit createCircuit(ParsedProperties testProperties): called");

            IList<TestClientDetails> senders = new LinkedList<TestClientDetails>();
            senders.add(getSender());
            IList<TestClientDetails> receivers = getReceivers();
            ConversationFactory conversationFactory = getConversationFactory();

            return DistributedCircuitImpl.createCircuit(testProperties, senders, receivers, conversationFactory);
        }

        /// <summary>
        /// Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
        /// begining the test, gathering the test reports from the participants, and checking for assertion failures against
        /// the test reports.
        /// </summary>
        /// <param name="testCircuit">    The test circuit. </param>
        /// <param name="assertions">     The list of assertions to apply to the test circuit. </param>
        /// <param name="testProperties"> The test case definition. </param>
        public void sequenceTest(Circuit testCircuit, IList<Assertion> assertions, Properties testProperties)
        {
            log.debug("protected Message[] sequenceTest(Object... testProperties = " + testProperties + "): called");

            TestClientDetails sender = getSender();
            IList<TestClientDetails> receivers = getReceivers();
            ConversationFactory conversationFactory = getConversationFactory();

            try
            {
                Session session = conversationFactory.getSession();
                Destination senderControlTopic = session.createTopic(sender.privateControlKey);
                Destination receiverControlTopic = session.createTopic(receivers.get(0).privateControlKey);

                ConversationFactory.Conversation senderConversation = conversationFactory.startConversation();
                ConversationFactory.Conversation receiverConversation = conversationFactory.startConversation();

                Message assignSender = conversationFactory.getSession().createMessage();
                TestUtils.setPropertiesOnMessage(assignSender, testProperties);
                assignSender.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                assignSender.setStringProperty("ROLE", "SENDER");

                senderConversation.send(senderControlTopic, assignSender);

                // Assign the receivers role the receiving client.
                Message assignReceiver = session.createMessage();
                TestUtils.setPropertiesOnMessage(assignReceiver, testProperties);
                assignReceiver.setStringProperty("CONTROL_TYPE", "ASSIGN_ROLE");
                assignReceiver.setStringProperty("ROLE", "RECEIVER");

                receiverConversation.send(receiverControlTopic, assignReceiver);

                // Wait for the senders and receivers to confirm their roles.
                senderConversation.receive();
                receiverConversation.receive();

                // Start the test.
                Message start = session.createMessage();
                start.setStringProperty("CONTROL_TYPE", "START");

                senderConversation.send(senderControlTopic, start);

                // Wait for the test sender to return its report.
                Message senderReport = senderConversation.receive();
                TestUtils.pause(500);

                // Ask the receivers for its report.
                Message statusRequest = session.createMessage();
                statusRequest.setStringProperty("CONTROL_TYPE", "STATUS_REQUEST");

                receiverConversation.send(receiverControlTopic, statusRequest);

                // Wait for the receivers to send its report.
                Message receiverReport = receiverConversation.receive();

                // return new Message[] { senderReport, receiverReport };

                // Apply assertions.
            }
            catch (JMSException e)
            {
                throw new RuntimeException("JMSException not handled.");
            }
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td>
    /// </table>
    /// </summary>
    public class TestCaseVector
    {
        /// <summary> The test case name. </summary>
        private string testCase;

        /// <summary> The test cycle number within the test case. </summary>
        private int testCycleNumber;

        public TestCaseVector(string testCase, int testCycleNumber)
        {
            this.testCase = testCase;
            this.testCycleNumber = testCycleNumber;
        }

        public string getTestCase()
        {
            return testCase;
        }

        public int getTestCycleNumber()
        {
            return testCycleNumber;
        }

        public bool equals(Object o)
        {
            if (this == o)
            {
                return true;
            }

            if ((o == null) || (getClass() != o.getClass()))
            {
                return false;
            }

            TestCaseVector that = (TestCaseVector) o;

            if (testCycleNumber != that.testCycleNumber)
            {
                return false;
            }

            if ((testCase != null) ? (!testCase.equals(that.testCase)) : (that.testCase != null))
            {
                return false;
            }

            return true;
        }

        public int hashCode()
        {
            int result;
            result = ((testCase != null) ? testCase.hashCode() : 0);
            result = (31 * result) + testCycleNumber;

            return result;
        }
    }
}
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
namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// TestClientDetails is used to encapsulate information about an interop test client. It pairs together the unique
    /// name of the client, and the route on which it listens to its control messages.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Record test clients control addresses together with their names.
    /// </table>
    /// </summary>
    public class TestClientDetails
    {
        /// <summary> The test clients name. </summary>
        public string clientName;

        /// <summary> The routing key of the test clients control topic. </summary>
        public string privateControlKey;

        /// <summary>
        /// Two TestClientDetails are considered to be equal, iff they have the same client name.
        /// </summary>
        /// <param name="o"> The object to compare to. </param>
        ///
        /// <return> <tt>If the object to compare to is a TestClientDetails equal to this one, <tt>false</tt> otherwise. </return>
        public bool equals(Object o)
        {
            if (this == o)
            {
                return true;
            }

            if (!(o instanceof TestClientDetails))
            {
                return false;
            }

            final TestClientDetails testClientDetails = (TestClientDetails) o;

            return !((clientName != null) ? (!clientName.equals(testClientDetails.clientName))
                     : (testClientDetails.clientName != null));
        }

        /// <summary>
        /// Computes a hash code compatible with the equals method; based on the client name alone.
        /// </summary>
        /// <return> A hash code for this. </return>
        public int hashCode()
        {
            return ((clientName != null) ? clientName.hashCode() : 0);
        }

        /// <summary>
        /// Outputs the client name and address details. Mostly used for debugging purposes.
        /// </summary>
        /// <return> The client name and address. </return>
        public string ToString()
        {
            return "TestClientDetails: [ clientName = " + clientName + ", privateControlKey = " + privateControlKey + " ]";
        }
    }
}
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

using static Apache.Qpid.Integration.Tests.framework.MessagingTestConfigProperties.*;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.*;
using javax.naming.Context;
using javax.naming.InitialContext;
using javax.naming.NamingException;

using System.Collections.Generic.IDictionary;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// TestUtils provides static helper methods that are usefull for writing tests against QPid.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create connections from test properties. <td> <see cref="MessagingTestConfigProperties"/>
    /// <tr><td> Create test messages.
    /// <tr><td> Inject a short pause in a test.
    /// <tr><td> Serialize properties into a message.
    /// </table>
    /// </summary>
    public class TestUtils
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(TestUtils));

        /// <summary> Some dummy data to stuff all test messages with. </summary>
        private static final byte[] MESSAGE_DATA_BYTES =
            "Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- Test Message -- "
            .getBytes();

        /// <summary>
        /// Establishes a JMS connection using a set of properties and qpids built in JNDI implementation. This is a simple
        /// convenience method for code that does not anticipate handling connection failures. All exceptions that indicate
        /// that the connection has failed, are wrapped as rutime exceptions, presumably handled by a top level failure
        /// handler.
        ///
        /// <p/>This utility makes use of the following test parameters from <see cref="MessagingTestConfigProperties"/> to control
        /// the connection creation:
        ///
        /// <p/><table>
        /// <tr><td> <see cref="MessagingTestConfigProperties#USERNAME_PROPNAME"/> <td> The username.
        /// <tr><td> <see cref="MessagingTestConfigProperties#PASSWORD_PROPNAME"/> <td> The password.
        /// <tr><td> <see cref="MessagingTestConfigProperties#VIRTUAL_HOST_PROPNAME"/> <td> The virtual host name.
        /// <tr><td> <see cref="MessagingTestConfigProperties#BROKER_PROPNAME"/> <td> The broker URL.
        /// <tr><td> <see cref="MessagingTestConfigProperties#CONNECTION_NAME"/> <td> The broker name in the initial context.
        /// </summary>
        /// <param name="messagingProps"> Connection properties as defined in <see cref="MessagingTestConfigProperties"/>. </param>
        ///
        /// <return> A JMS conneciton. </return>
        public static Connection createConnection(ParsedProperties messagingProps)
        {
            log.debug("public static Connection createConnection(ParsedProperties messagingProps = " + messagingProps
                      + "): called");

            try
            {
                // Extract the configured connection properties from the test configuration.
                string conUsername = messagingProps.getProperty(USERNAME_PROPNAME);
                string conPassword = messagingProps.getProperty(PASSWORD_PROPNAME);
                string virtualHost = messagingProps.getProperty(VIRTUAL_HOST_PROPNAME);
                string brokerUrl = messagingProps.getProperty(BROKER_PROPNAME);

                // Create the broker connection url.
                string connectionstring =
                    "amqp://" + conUsername + ":" + conPassword + "@clientid/" + ((virtualHost != null) ? virtualHost : "")
                    + "?brokerlist='" + brokerUrl + "'";

                // Create properties to create the initial context from, and inject the connection factory configuration
                // for the defined connection name into it.
                messagingProps.setProperty("connectionfactory." + CONNECTION_NAME, connectionString);

                Context ctx = new InitialContext(messagingProps);

                ConnectionFactory cf = (ConnectionFactory) ctx.lookup(CONNECTION_NAME);

                return cf.createConnection();
            }
            catch (NamingException e)
            {
                throw new RuntimeException("Got JNDI NamingException whilst looking up the connection factory.", e);
            }
            catch (JMSException e)
            {
                throw new RuntimeException("Could not establish connection due to JMSException.", e);
            }
        }

        /// <summary>
        /// Creates a test message of the specified size, on the given JMS session.
        /// </summary>
        /// <param name="session"> The JMS session. </param>
        /// <param name="size">    The size of the message in bytes. </param>
        ///
        /// <return> A bytes message, of the specified size, filled with dummy data. </return>
        ///
        /// <exception cref="JMSException"> Any underlying JMSExceptions are allowed to fall through. </exception>
        public static Message createTestMessageOfSize(Session session, int size) throws JMSException
        {
            BytesMessage message = session.createBytesMessage();

            if (size > 0)
            {
                int div = MESSAGE_DATA_BYTES.length / size;
                int mod = MESSAGE_DATA_BYTES.length % size;

                for (int i = 0; i < div; i++)
                {
                    message.writeBytes(MESSAGE_DATA_BYTES);
                }

                if (mod != 0)
                {
                    message.writeBytes(MESSAGE_DATA_BYTES, 0, mod);
                }
            }

            return message;
        }

        /// <summary>
        /// Pauses for the specified length of time. In the event of failing to pause for at least that length of time
        /// due to interuption of the thread, a RutimeException is raised to indicate the failure. The interupted status
        /// of the thread is restores in that case. This method should only be used when it is expected that the pause
        /// will be succesfull, for example in test code that relies on inejecting a pause.
        /// </summary>
        /// <param name="t"> The minimum time to pause for in milliseconds. </param>
        public static void pause(long t)
        {
            try
            {
                Thread.sleep(t);
            }
            catch (InterruptedException e)
            {
                // Restore the interrupted status
                Thread.currentThread().interrupt();

                throw new RuntimeException("Failed to generate the requested pause length.", e);
            }
        }

        /// <summary>
        /// Sets properties of different types on a JMS Message.
        /// </summary>
        /// <param name="message">    The message to set properties on. </param>
        /// <param name="properties"> The property name/value pairs to set. </param>
        ///
        /// <exception cref="javax.jms.JMSException"> All underlying JMSExceptions are allowed to fall through. </exception>
        ///
        /// <remarks> Move this helper method somewhere else. For example, TestUtils.</remarks>
        public static void setPropertiesOnMessage(Message message, Map<Object, Object> properties) throws JMSException
        {
            for (Map.Entry<Object, Object> entry : properties.entrySet())
            {
                string name = entry.getKey().ToString();
                Object value = entry.getValue();

                message.setObjectProperty(name, value);
            }
        }
    }
}