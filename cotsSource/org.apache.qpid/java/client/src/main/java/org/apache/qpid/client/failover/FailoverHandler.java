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

import org.apache.qpid.AMQDisconnectedException;
import org.apache.qpid.client.protocol.AMQProtocolHandler;
import org.apache.qpid.client.state.AMQStateManager;
import org.apache.qpid.client.state.AMQState;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.CountDownLatch;

/**
 * FailoverHandler is a continuation that performs the failover procedure on a protocol session. As described in the
 * class level comment for {@link AMQProtocolHandler}, a protocol connection can span many physical transport
 * connections, failing over to a new connection if the transport connection fails. The procedure to establish a new
 * connection is expressed as a continuation, in order that it may be run in a seperate thread to the i/o thread that
 * detected the failure and is used to handle the communication to establish a new connection.
 *
 * </p>The reason this needs to be a separate thread is because this work cannot be done inside the i/o processor
 * thread. The significant task is the connection setup which involves a protocol exchange until a particular state
 * is achieved. This procedure waits until the state is achieved which would prevent the i/o thread doing the work
 * it needs to do to achieve the new state.
 *
 * <p/>The failover procedure does the following:
 *
 * <ol>
 * <li>Sets the failing over condition to true.</li>
 * <li>Creates a {@link FailoverException} and gets the protocol connection handler to propagate this event to all
 *     interested parties.</li>
 * <li>Takes the failover mutex on the protocol connection handler.</li>
 * <li>Abandons the fail over if any of the interested parties vetoes it. The mutex is released and the condition
 *     reset.</li>
 * <li>Creates a new {@link AMQStateManager} and re-established the connection through it.</li>
 * <li>Informs the AMQConnection if the connection cannot be re-established.</li>
 * <li>Recreates all sessions from the old connection to the new.</li>
 * <li>Resets the failing over condition and releases the mutex.</li>
 * </ol>
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Update fail-over state <td> {@link AMQProtocolHandler}
 * </table>
 *
 * @todo The failover latch and mutex are used like a lock and condition. If the retrotranlator supports lock/condition
 *       then could change over to using them. 1.4 support still needed.
 *
 * @todo If the condition is set to null on a vetoes fail-over and there are already other threads waiting on the
 *       condition, they will never be released. It might be an idea to reset the condition in a finally block.
 *
 * @todo Creates a {@link AMQDisconnectedException} and passes it to the AMQConnection. No need to use an
 *       exception-as-argument here, could just as easily call a specific method for this purpose on AMQConnection.
 *
 * @todo Creates a {@link FailoverException} and propagates it to the MethodHandlers. No need to use an
 *       exception-as-argument here, could just as easily call a specific method for this purpose on
 *       {@link org.apache.qpid.protocol.AMQMethodListener}.
 */
public class FailoverHandler implements Runnable
{
    /** Used for debugging. */
    private static final Logger _logger = LoggerFactory.getLogger(FailoverHandler.class);

    /** Holds the protocol handler for the failed connection, upon which the new connection is to be set up. */
    private AMQProtocolHandler _amqProtocolHandler;

    /** Used to hold the host to fail over to. This is optional and if not set a reconnect to the previous host is tried. */
    private String _host;

    /** Used to hold the port to fail over to. */
    private int _port;

    /**
     * Creates a failover handler on a protocol session, for a particular MINA session (network connection).
     *
     * @param amqProtocolHandler The protocol handler that spans the failover.
     */
    public FailoverHandler(AMQProtocolHandler amqProtocolHandler)
    {
        _amqProtocolHandler = amqProtocolHandler;
    }

    /**
     * Performs the failover procedure. See the class level comment, {@link FailoverHandler}, for a description of the
     * failover procedure.
     */
    public void run()
    {
        if (Thread.currentThread().isDaemon())
        {
            throw new IllegalStateException("FailoverHandler must run on a non-daemon thread.");
        }

        // Create a latch, upon which tasks that must not run in parallel with a failover can wait for completion of
        // the fail over.
        _amqProtocolHandler.setFailoverLatch(new CountDownLatch(1));

        // We wake up listeners. If they can handle failover, they will extend the
        // FailoverRetrySupport class and will in turn block on the latch until failover
        // has completed before retrying the operation.
        _amqProtocolHandler.notifyFailoverStarting();

        // Since failover impacts several structures we protect them all with a single mutex. These structures
        // are also in child objects of the connection. This allows us to manipulate them without affecting
        // client code which runs in a separate thread.
        synchronized (_amqProtocolHandler.getConnection().getFailoverMutex())
        {
            //Clear the exception now that we have the failover mutex there can be no one else waiting for a frame so
            // we can clear the exception.
            _amqProtocolHandler.failoverInProgress();

            // We switch in a new state manager temporarily so that the interaction to get to the "connection open"
            // state works, without us having to terminate any existing "state waiters". We could theoretically
            // have a state waiter waiting until the connection is closed for some reason. Or in future we may have
            // a slightly more complex state model therefore I felt it was worthwhile doing this.
            AMQStateManager existingStateManager = _amqProtocolHandler.getStateManager();


            // Use a fresh new StateManager for the reconnection attempts
            _amqProtocolHandler.setStateManager(new AMQStateManager());


            if (!_amqProtocolHandler.getConnection().firePreFailover(_host != null))
            {
                _logger.info("Failover process veto-ed by client");

                //Restore Existing State Manager
                _amqProtocolHandler.setStateManager(existingStateManager);

                //todo: ritchiem these exceptions are useless... Would be better to attempt to propogate exception that
                // prompted the failover event.
                if (_host != null)
                {
                    _amqProtocolHandler.getConnection().exceptionReceived(new AMQDisconnectedException("Redirect was vetoed by client", null));
                }
                else
                {
                    _amqProtocolHandler.getConnection().exceptionReceived(new AMQDisconnectedException("Failover was vetoed by client", null));
                }

                _amqProtocolHandler.getFailoverLatch().countDown();
                _amqProtocolHandler.setFailoverLatch(null);

                return;
            }

            _logger.info("Starting failover process");

            boolean failoverSucceeded;
            // when host is non null we have a specified failover host otherwise we all the client to cycle through
            // all specified hosts

            // if _host has value then we are performing a redirect.
            if (_host != null)
            {
                failoverSucceeded = _amqProtocolHandler.getConnection().attemptReconnection(_host, _port);
            }
            else
            {
                failoverSucceeded = _amqProtocolHandler.getConnection().attemptReconnection();
            }

            if (!failoverSucceeded)
            {
                //Restore Existing State Manager
                _amqProtocolHandler.setStateManager(existingStateManager);

                _amqProtocolHandler.getConnection().exceptionReceived(
                        new AMQDisconnectedException("Server closed connection and no failover " +
                                "was successful", null));
            }
            else
            {
                // Set the new Protocol Session in the StateManager.
                existingStateManager.setProtocolSession(_amqProtocolHandler.getProtocolSession());

                // Now that the ProtocolHandler has been reconnected clean up
                // the state of the old state manager. As if we simply reinstate
                // it any old exception that had occured prior to failover may
                // prohibit reconnection.
                // e.g. During testing when the broker is shutdown gracefully.
                // The broker
                // Clear any exceptions we gathered
                if (existingStateManager.getCurrentState() != AMQState.CONNECTION_OPEN)
                {
                    // Clear the state of the previous state manager as it may
                    // have received an exception
                    existingStateManager.clearLastException();
                    existingStateManager.changeState(AMQState.CONNECTION_OPEN);
                }


                //Restore Existing State Manager
                _amqProtocolHandler.setStateManager(existingStateManager);
                try
                {
                    if (_amqProtocolHandler.getConnection().firePreResubscribe())
                    {
                        _logger.info("Resubscribing on new connection");
                        _amqProtocolHandler.getConnection().resubscribeSessions();
                    }
                    else
                    {
                        _logger.info("Client vetoed automatic resubscription");
                    }

                    _amqProtocolHandler.getConnection().fireFailoverComplete();
                    _amqProtocolHandler.setFailoverState(FailoverState.NOT_STARTED);
                    _logger.info("Connection failover completed successfully");
                }
                catch (Exception e)
                {
                    _logger.info("Failover process failed - exception being propagated by protocol handler");
                    _amqProtocolHandler.setFailoverState(FailoverState.FAILED);
                    /*try
                    {*/
                    _amqProtocolHandler.exception(e);
                    /*}
                    catch (Exception ex)
                    {
                        _logger.error("Error notifying protocol session of error: " + ex, ex);
                    }*/
                }
            }
        }

        _amqProtocolHandler.getFailoverLatch().countDown();
    }

    /**
     * Sets the host name to fail over to. This is optional and if not set a reconnect to the previous host is tried.
     *
     * @param host The host name to fail over to.
     */
    public void setHost(String host)
    {
        _host = host;
    }

    /**
     * Sets the port to fail over to.
     *
     * @param port The port to fail over to.
     */
    public void setPort(int port)
    {
        _port = port;
    }
}
