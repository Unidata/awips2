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
package org.apache.qpid.client;

import javax.jms.IllegalStateException;
import javax.jms.JMSException;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Captures the 'closed' state of an object, that is initially open, can be tested to see if it is closed, and provides
 * a 'close' method to close it.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Mark an object as closed.
 * <tr><td> Check if an object is closed.
 * <tr><td> Raise a JMS exception if an object is closed.
 * </table>
 *
 * @todo Might be better to make this an interface. This whole class doesn't really encapsulate a terribly neat
 *       piece of re-usable functionality. A simple interface defining a close method would suffice.
 *
 * @todo The convenience method {@link #checkNotClosed} is not that helpfull, what if the caller wants to do something
 *       other than throw an exception? It doesn't really represent a very usefull re-usable piece of code. Consider
 *       inlining it and dropping the method.
 */
public abstract class Closeable
{
    /**
     * We use an atomic boolean so that we do not have to synchronized access to this flag. Synchronizing access to this
     * flag would mean have a synchronized block in every method.
     */
    protected final AtomicBoolean _closed = new AtomicBoolean(false);

    /**
     * Are we in the process of closing. We have this distinction so we can
     * still signal we are in the process of closing so other objects can tell
     * the difference and tidy up.
     */
    protected final AtomicBoolean _closing = new AtomicBoolean(false);

    /**
     * Checks if this is closed, and raises a JMSException if it is.
     *
     * @throws JMSException If this is closed.
     */
    protected void checkNotClosed() throws JMSException
    {
        if (isClosed())
        {
            throw new IllegalStateException("Object " + toString() + " has been closed");
        }
    }

    /**
     * Checks if this is closed.
     *
     * @return <tt>true</tt> if this is closed, <tt>false</tt> otherwise.
     */
    public boolean isClosed()
    {
        return _closed.get();
    }

    /**
     * Checks if this is closis.
     *
     * @return <tt>true</tt> if we are closing, <tt>false</tt> otherwise.
     */
    public boolean isClosing()
    {
        return _closing.get();
    }


    /**
     * Closes this object.
     *
     * @throws JMSException If this cannot be closed for any reason.
     */
    public abstract void close() throws JMSException;
}
