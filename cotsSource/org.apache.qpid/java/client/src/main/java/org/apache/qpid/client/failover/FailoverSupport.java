/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */

package org.apache.qpid.client.failover;

/**
 * FailoverSupport defines an interface for different types of fail-over handlers, that provide different types of
 * behaviour for handling fail-overs during operations that can be interrupted by the fail-over process. For example,
 * the support could automatically retry once the fail-over process completes, could prevent an operation from being
 * started whilst fail-over is running, or could quietly abandon the operation or raise an exception, and so on.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Perform a fail-over protected operation with handling for fail-over conditions.
 * </table>
 *
 * @todo Continuation, extend some sort of re-usable Continuation interface, which might look very like this one.
 */
public interface FailoverSupport<T, E extends Exception>
{
    /**
     * Delegates to another continuation which is to be provided with fail-over handling.
     *
     * @return The return value from the delegated to continuation.
     *
     * @throws E Any exception that the delegated to continuation may raise.
     */
    public T execute() throws E;
}
