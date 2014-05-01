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
using log4net;

namespace Apache.Qpid.Client.Failover
{
    public abstract class FailoverSupport
    {
        private static readonly ILog _log = LogManager.GetLogger(typeof(FailoverSupport));

        public object execute(AMQConnection con)
        {
            // We wait until we are not in the middle of failover before acquiring the mutex and then proceeding.
            // Any method that can potentially block for any reason should use this class so that deadlock will not
            // occur. The FailoverException is propagated by the AMQProtocolHandler to any listeners (e.g. frame listeners)
            // that might be causing a block. When that happens, the exception is caught here and the mutex is released
            // before waiting for the failover to complete (either successfully or unsuccessfully).
            while (true)
            {
                con.ProtocolListener.BlockUntilNotFailingOver();
                lock (con.FailoverMutex)
                {
                    try
                    {
                        return operation();
                    }
                    catch (FailoverException e)
                    {
                        _log.Info("Failover exception caught during operation", e);
                    }
                }
            }
        }

        protected abstract object operation();
    }
}
