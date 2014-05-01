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

/**
 * Defines the possible states of the failover process; not started, in progress, failed.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represent a one of the states of the fail-over process.
 * </table>
 */
public final class FailoverState
{
    /** The string description on this state. */
    private final String _state;

    /** Failover has not yet been attempted on this connection. */
    public static final FailoverState NOT_STARTED = new FailoverState("NOT STARTED");

    /** Failover has been requested on this connection but has not completed. */
    public static final FailoverState IN_PROGRESS = new FailoverState("IN PROGRESS");

    /** Failover has been attempted and failed. */
    public static final FailoverState FAILED = new FailoverState("FAILED");

    /**
     * Creates a type safe enumeration of a fail-over state.
     *
     * @param state The fail-over state description string.
     */
    private FailoverState(String state)
    {
        _state = state;
    }

    /**
     * Prints this state, mainly for debugging purposes.
     *
     * @return The string description of this state.
     */
    public String toString()
    {
        return "FailoverState: " + _state;
    }
}
