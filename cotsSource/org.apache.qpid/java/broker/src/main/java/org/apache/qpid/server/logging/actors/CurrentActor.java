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
package org.apache.qpid.server.logging.actors;

import org.apache.qpid.server.logging.LogActor;
import org.apache.qpid.server.logging.LogSubject;
import org.apache.qpid.server.logging.LogMessage;
import org.apache.qpid.server.logging.RootMessageLogger;

import java.util.EmptyStackException;
import java.util.Stack;

/**
 * The CurrentActor is a ThreadLocal wrapper that allows threads in the broker
 * to retrieve an actor to perform logging. This approach is used so for two
 * reasons:
 * 1) We do not have to pass a logging actor around the system
 * 2) We can set new actors at the point we have enough information. i.e.
 * - Set a low level ConnectionActor when processing bytes from the wire.
 * - Set a ChannelActor when we are processing the frame
 * - Set a SubscriptionActor when we are handling the subscription.
 * <p/>
 * The code performing the logging need not worry about what type of actor is
 * currently set so can perform its logging. The resulting log entry though will
 * contain customised details from the the currently set Actor.
 * <p/>
 * The Actor model also allows the pre-creation of fixed messages so the
 * performance impact of the additional logging data is minimised.
 * <p/>
 * This class does not perform any checks to ensure that there is an Actor set
 * when calling remove or get. As a result the application developer must ensure
 * that they have called set before they attempt to use the actor via get or
 * remove the set actor.
 * <p/>
 * The checking of the return via get should not be done as the logging is
 * desired. It is preferable to cause the NullPointerException to highlight the
 * programming error rather than miss a log message.
 * <p/>
 * The same is true for the remove. A NPE will occur if no set has been called
 * highlighting the programming error.
 */
public class CurrentActor
{
    /** The ThreadLocal variable with initialiser */
    private static final ThreadLocal<Stack<LogActor>> _currentActor = new ThreadLocal<Stack<LogActor>>()
    {
        // Initialise the CurrentActor to be an empty List
        protected Stack<LogActor> initialValue()
        {
            return new Stack<LogActor>();
        }
    };

    private static LogActor _defaultActor;

    /**
     * Set a new LogActor to be the Current Actor
     * <p/>
     * This pushes the Actor in to the LIFO Queue
     *
     * @param actor The new LogActor
     */
    public static void set(LogActor actor)
    {
        Stack<LogActor> stack = _currentActor.get();
        stack.push(actor);
    }

    /**
     * Remove the current LogActor.
     * <p/>
     * Calling remove without calling set will result in an EmptyStackException.
     */
    public static void remove()
    {
        Stack<LogActor> stack = _currentActor.get();
        stack.pop();
    }

    /**
     * Return the current head of the list of LogActors.
     * <p/>
     * If there has been no set call then this will return Null.
     *
     * @return Current LogActor
     */
    public static LogActor get()
    {
        try
        {
            return _currentActor.get().peek();
        }
        catch (EmptyStackException ese)
        {
            return _defaultActor;
        }
    }

    public static void setDefault(LogActor defaultActor)
    {
        _defaultActor = defaultActor;
    }
}
