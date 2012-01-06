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
 * FailoverException is used to indicate that a synchronous request has failed to receive the reply that it is waiting
 * for because the fail-over process has been started whilst it was waiting for its reply. Synchronous methods generally
 * raise this exception to indicate that they must be re-tried once the fail-over process has completed.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Used to indicate failure of a synchronous request due to fail-over.
 * </table>
 *
 * @todo This exception is created and passed as an argument to a method, rather than thrown. The exception is being
 *       used to represent an event, passed out to other threads. Use of exceptions as arguments rather than as
 *       exceptions is extremly confusing. Ideally use a condition or set a flag and check it instead.
 *       This exceptions-as-events pattern seems to be in a similar style to Mina code, which is not pretty, but
 *       potentially acceptable for that reason. We have the option of extending the mina model to add more events
 *       to it, that is, anything that is interested in handling failover as an event occurs below the main
 *       amq event handler, which knows the specific interface of the qpid handlers, which can pass this down as
 *       an explicit event, without it being an exception. Add failover method to BlockingMethodFrameListener,
 *       have it set a flag or interrupt the waiting thread, which then creates and raises this exception.
 */
public class FailoverException extends Exception
{
    public FailoverException(String message)
    {
        super(message);
    }
}
