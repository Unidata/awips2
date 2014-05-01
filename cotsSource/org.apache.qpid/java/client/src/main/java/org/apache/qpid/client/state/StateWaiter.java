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
package org.apache.qpid.client.state;

import org.apache.qpid.client.util.BlockingWaiter;
import org.apache.qpid.client.failover.FailoverException;
import org.apache.qpid.AMQException;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import java.util.Set;

/**
 * This is an implementation of the {@link BlockingWaiter} to provide error handing and a waiting mechanism for state
 * changes.
 *
 * On construction the current state and a set of States to await for is provided.
 *
 * When await() is called the state at constuction is compared against the awaitStates. If the state at construction is
 * a desired state then await() returns immediately.
 *
 * Otherwise it will block for the set timeout for a desired state to be achieved.
 *
 * The state changes are notified via the {@link #process} method.
 *
 * Any notified error is handled by the BlockingWaiter and thrown from the {@link #block} method.
 *
 */
public class StateWaiter extends BlockingWaiter<AMQState>
{
    private static final Logger _logger = LoggerFactory.getLogger(StateWaiter.class);

    Set<AMQState> _awaitStates;
    private AMQState _startState;
    private AMQStateManager _stateManager;

    /**
     *
     * @param stateManager The StateManager
     * @param currentState
     * @param awaitStates
     */
    public StateWaiter(AMQStateManager stateManager, AMQState currentState, Set<AMQState> awaitStates)
    {
        _logger.info("New StateWaiter :" + currentState + ":" + awaitStates);
        _stateManager = stateManager;
        _awaitStates = awaitStates;
        _startState = currentState;
    }

    /**
     * When the state is changed this StateWaiter is notified to process the change.
     *
     * @param state The new state that has been achieved.
     * @return
     */
    public boolean process(AMQState state)
    {
        return _awaitStates.contains(state);
    }

    /**
     * Await for the requried State to be achieved within the default timeout.
     * @return The achieved state that was requested.
     * @throws AMQException The exception that prevented the required state from being achived.
     */
    public AMQState await() throws AMQException
    {
        return await(_stateManager.getWaitTimeout());
    }

    /**
     * Await for the requried State to be achieved.
     *
     * <b>It is the responsibility of this class to remove the waiter from the StateManager
     *
     * @param timeout The time in milliseconds to wait for any of the states to be achived.
     * @return The achieved state that was requested.
     * @throws AMQException The exception that prevented the required state from being achived.
     */
    public AMQState await(long timeout) throws AMQException
    {
        try
        {
            if (process(_startState))
            {
                return _startState;
            }

            try
            {
                return (AMQState) block(timeout);
            }
            catch (FailoverException e)
            {
                _logger.error("Failover occured whilst waiting for states:" + _awaitStates);

                return null;
            }
        }
        finally
        {
            //Prevent any more errors being notified to this waiter.
            close();

            //Remove the waiter from the notifcation list in the statee manager
            _stateManager.removeWaiter(this);
        }
    }
}
