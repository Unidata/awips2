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

import org.apache.log4j.Logger;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * An exception monitor, listens for JMS exception on a connection or consumer. It record all exceptions that it receives
 * and provides methods to test the number and type of exceptions received.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Record all exceptions received.
 * </table>
 */
public class ExceptionMonitor implements ExceptionListener
{
    /** Used for debugging. */
    private final Logger log = Logger.getLogger(ExceptionMonitor.class);

    /** Holds the received exceptions. */
    List<Exception> exceptions = new ArrayList<Exception>();

    /**
     * Receives incoming exceptions.
     *
     * @param e The exception to record.
     */
    public synchronized void onException(JMSException e)
    {
        log.debug("public void onException(JMSException e): called", e);

        exceptions.add(e);
    }

    /**
     * Checks that no exceptions have been received.
     *
     * @return <tt>true</tt> if no exceptions have been received, <tt>false</tt> otherwise.
     */
    public synchronized boolean assertNoExceptions()
    {
        return exceptions.isEmpty();
    }

    /**
     * Checks that exactly one exception has been received.
     *
     * @return <tt>true</tt> if exactly one exception been received, <tt>false</tt> otherwise.
     */
    public synchronized boolean assertOneJMSException()
    {
        return exceptions.size() == 1;
    }

    /**
     * Checks that exactly one exception, with a linked cause of the specified type, has been received.
     *
     * @param aClass The type of the linked cause.
     *
     * @return <tt>true</tt> if exactly one exception, with a linked cause of the specified type, been received,
     *         <tt>false</tt> otherwise.
     */
    public synchronized boolean assertOneJMSExceptionWithLinkedCause(Class aClass)
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

    /**
     * Checks that at least one exception of the the specified type, has been received.
     *
     * @param exceptionClass The type of the exception.
     *
     * @return <tt>true</tt> if at least one exception of the specified type has been received, <tt>false</tt> otherwise.
     */
    public synchronized boolean assertExceptionOfType(Class exceptionClass)
    {
        // Start by assuming that the exception has no been received.
        boolean passed = false;

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

    /**
     * Reports the number of exceptions held by this monitor.
     *
     * @return The number of exceptions held by this monitor.
     */
    public synchronized int size()
    {
        return exceptions.size();
    }

    /**
     * Clears the record of received exceptions.
     */
    public synchronized void reset()
    {
        exceptions = new ArrayList<Exception>();
    }

    /**
     * Provides a dump of the stack traces of all exceptions that this exception monitor was notified of. Mainly
     * use for debugging/test failure reporting purposes.
     *
     * @return A string containing a dump of the stack traces of all exceptions.
     */
    public synchronized String toString()
    {
        String result = "ExceptionMonitor: holds " + exceptions.size() + " exceptions.\n\n";

        for (Exception ex : exceptions)
        {
            result += getStackTrace(ex) + "\n";
        }

        return result;
    }

    /**
     * Prints an exception stack trace into a string.
     *
     * @param t The throwable to get the stack trace from.
     *
     * @return A string containing the throwables stack trace.
     */
    public static String getStackTrace(Throwable t)
    {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        t.printStackTrace(pw);
        pw.flush();
        sw.flush();

        return sw.toString();
    }
}
