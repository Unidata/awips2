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
using System;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Client
{
    /// <summary>Closeable provides monitoring of the state of a closeable resource; whether it is open or closed. It also provides a lock on which
    /// attempts to close the resource from multiple threads can be coordinated.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Close (and clean-up) a resource.
    /// <tr><td> Monitor the state of a closeable resource.
    /// <tr><td> Synchronous attempts to close resource from concurrent threads.
    /// </table>
    /// </summary>
    ///
    /// <remarks>Poor encapsulation of the close lock. Better to completely hide the implementation, such that there is a method, e.g., DoSingleClose,
    /// that sub-classes implement. Guaranteed to only be called by one thread at once, and iff the object is not already closed. That is, multiple
    /// simultaneous closes will result in a single call to the real close method. Put the wait and condition checking loop in this base class.
    /// </remarks>
    public abstract class Closeable : ICloseable
    {
        /// <summary> Constant representing the closed state. </summary>
        protected const int CLOSED = 1;

        /// <summary> Constant representing the open state. </summary>
        protected const int NOT_CLOSED = 2;

        /// <summary> Used to ensure orderly closing of the object. </summary>
        protected readonly object _closingLock = new object();

        /// <summary> Indicates the state of this resource; open or closed. </summary>
        protected int _closed = NOT_CLOSED;

        /// <summary>
        /// Checks the not closed.
        /// </summary>
        ///
        /// <remarks>Don't like check methods that throw exceptions. a) it can come as a surprise without checked exceptions, b) it limits the 
        /// callers choice, if the caller would prefer a boolean, c) it is not side-effect free programming, where such could be used. Get rid
        /// of this and replace with boolean.</remarks>
        protected void CheckNotClosed()
        {
            if (_closed == CLOSED)
            {
                throw new InvalidOperationException("Object " + ToString() + " has been closed");
            }
        }

        /// <summary>Indicates whether this resource is closed.</summary>
        /// <value><c>true</c> if closed; otherwise, <c>false</c>.</value>
        public bool Closed
        {
            get
            {
                return _closed == CLOSED;
            }
        }

        /// <summary> Close this resource. </summary>
        public abstract void Close();
    }
}
