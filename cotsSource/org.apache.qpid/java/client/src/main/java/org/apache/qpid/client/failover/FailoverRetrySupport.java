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
package org.apache.qpid.client.failover;

import org.apache.qpid.client.AMQConnection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * FailoverRetrySupport is a continuation that wraps another continuation, delaying its execution until it is notified
 * that a blocking condition has been met, and executing the continuation within a mutex. If the continuation fails, due
 * to the original condition being broken, whilst the continuation is waiting for a reponse to a synchronous request,
 * FailoverRetrySupport automatcally rechecks the condition and re-acquires the mutex and re-runs the continution. This
 * automatic retrying is continued until the continuation succeeds, or throws an exception (different to
 * FailoverException, which is used to signal the failure of the original condition).
 *
 * <p/>The blocking condition used is that the connection is not currently failing over, and the mutex used is the
 * connection failover mutex, which guards against the fail-over process being run during fail-over vulnerable methods.
 * These are used like a lock and condition variable.
 *
 * <p/>The wrapped operation may throw a FailoverException, this is an exception that can be raised by a
 * {@link org.apache.qpid.client.protocol.BlockingMethodFrameListener}, in response to it being notified that a
 * fail-over wants to start whilst it was waiting. Methods that are vulnerable to fail-over are those that are
 * synchronous, where a failure will prevent them from getting the reply they are waiting for and asynchronous
 * methods that should not be attempted when a fail-over is in progress.
 *
 * <p/>Wrapping a synchronous method in a FailoverRetrySupport will have the effect that the operation will not be
 * started during fail-over, but be delayed until any current fail-over has completed. Should a fail-over process want
 * to start whilst waiting for the synchrnous reply, the FailoverRetrySupport will detect this and rety the operation
 * until it succeeds. Synchronous methods are usually coordinated with a
 * {@link org.apache.qpid.client.protocol.BlockingMethodFrameListener} which is notified when a fail-over process wants
 * to start and throws a FailoverException in response to this.
 *
 * <p/>Wrapping an asynchronous method in a FailoverRetrySupport will have the effect that the operation will not be
 * started during fail-over, but be delayed until any current fail-over has completed.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide a continuation synchronized on a fail-over lock and condition.
 * <tr><td> Automatically retry the continuation accross fail-overs until it succeeds, or raises an exception.
 * </table>
 *
 * @todo Another continuation. Could use an interface Continuation (as described in other todos, for example, see
 *      {@link org.apache.qpid.pool.Job}). Then have a wrapping continuation (this), which blocks on an arbitrary
 *      Condition or Latch (specified in constructor call), that this blocks on before calling the wrapped Continuation.
 *      Must work on Java 1.4, so check retrotranslator works on Lock/Condition or latch first. Argument and return type
 *      to match wrapped condition as type parameters. Rename to AsyncConditionalContinuation or something like that.
 *
 * @todo InterruptedException not handled well.
 */
public class FailoverRetrySupport<T, E extends Exception> implements FailoverSupport<T, E>
{
    /** Used for debugging. */
    private static final Logger _log = LoggerFactory.getLogger(FailoverRetrySupport.class);

    /** The protected operation that is to be retried in the event of fail-over. */
    FailoverProtectedOperation<T, E> operation;

    /** The connection on which the fail-over protected operation is to be performed. */
    AMQConnection connection;

    /**
     * Creates an automatic retrying fail-over handler for the specified operation.
     *
     * @param operation The fail-over protected operation to wrap in this handler.
     */
    public FailoverRetrySupport(FailoverProtectedOperation<T, E> operation, AMQConnection con)
    {
        this.operation = operation;
        this.connection = con;
    }

    /**
     * Delays a continuation until the "not failing over" condition is met on the specified connection. Repeats
     * until the operation throws AMQException or succeeds without being interrupted by fail-over.
     *
     * @return The result of executing the continuation.
     *
     * @throws E Any underlying exception is allowed to fall through.
     */
    public T execute() throws E
    {
        return connection.executeRetrySupport(operation);
    }
}
