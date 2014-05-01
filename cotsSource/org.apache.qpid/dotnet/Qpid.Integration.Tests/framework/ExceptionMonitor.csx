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