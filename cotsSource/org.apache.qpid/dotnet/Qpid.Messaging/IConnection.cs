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

namespace Apache.Qpid.Messaging
{
    public delegate void ExceptionListenerDelegate(Exception ex);

  public interface IConnection : IDisposable, ICloseable
    {
        /// <summary>
        /// The connection listener that has been registered with this connection.
        /// </summary>
        IConnectionListener ConnectionListener
        {
            get;
            set;
        }

        ExceptionListenerDelegate ExceptionListener { get; set; }

        string ClientID { get; set; }

        /// <return>the maximum number of sessions supported by this Connection</return>     
        int MaximumChannelCount
        {
            get;
        }

        IChannel CreateChannel(bool transacted, AcknowledgeMode acknowledgeMode);
        IChannel CreateChannel(bool transacted, AcknowledgeMode acknowledgeMode, int prefetch);
        IChannel CreateChannel(bool transacted, AcknowledgeMode acknowledgeMode, int prefetchHigh, int prefetchLow);

        void Start();
        void Stop();
    }
}
