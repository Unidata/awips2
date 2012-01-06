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
using System.Collections.Generic;

namespace org.apache.qpid.transport
{
    public interface ISession : IInvoker
    {
        bool IsClosed { get; set; }
        string Name { get; }
        int CommandsIn { get; set; }
        byte[] GetName();
        void SetAutoSync(bool value);
        Dictionary<int, Method> GetOutstandingCommands();
        int GetCommandsOut();
        int NextCommandId();
        void Identify(Method cmd);
        void Processed(Method command);
        void Processed(int command);
        void Processed(int lower, int upper);
        void Processed(Range range);
        void FlushProcessed(params Option[] options);
        void KnownComplete(RangeSet kc);
        void SyncPoint();
        void Attach(Channel channel);
        Method GetCommand(int id);
        bool Complete(int lower, int upper);
        void Sync();
        void Sync(long timeout);
        void Result(int command, Struct result);
        void AddException(ExecutionException exc);
        void CloseCode(ConnectionClose close);
        List<ExecutionException> GetExceptions();

        void MessageTransfer(String destination,
                             MessageAcceptMode acceptMode,
                             MessageAcquireMode acquireMode,
                             Header header,
                             byte[] body,
                             params Option[] options);

        void MessageTransfer(String destination,
                             MessageAcceptMode acceptMode,
                             MessageAcquireMode acquireMode,
                             Header header,
                             String body,
                             params Option[] options);

        void Close();
        void Exception(Exception t);
        void Closed();
    }
}
