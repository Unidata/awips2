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
 * FailoverProtectedOperation is a continuation for an operation that may throw a {@link FailoverException} because
 * it has been interrupted by the fail-over process. The {@link FailoverRetrySupport} class defines support wrappers
 * for failover protected operations, in order to provide different handling schemes when failovers occurr.
 *
 * <p/>The type of checked exception that the operation may perform has been generified, in order that fail over
 * protected operations can be defined that raise arbitrary exceptions. The actuall exception types used should not
 * be sub-classes of FailoverException, or else catching FailoverException in the {@link FailoverRetrySupport} classes
 * will mask the exception.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Perform an operation that may be interrupted by fail-over.
 * </table>
 */
public interface FailoverProtectedOperation<T, E extends Exception>
{
    /**
     * Performs the continuations work.
     *
     * @return Provdes scope for the continuation to return an arbitrary value.
     *
     * @throws FailoverException If the operation is interrupted by a fail-over notification.
     */
    public abstract T execute() throws E, FailoverException;
}
