/*
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
*/
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.client
{
    public class ClientSessionDelegate : SessionDelegate
    {
        private static readonly Logger _log = Logger.Get(typeof (ClientSessionDelegate));

        //  --------------------------------------------
        //   Message methods
        // --------------------------------------------
        public override void MessageTransfer(Session session, MessageTransfer xfr)
        {
            if (((ClientSession) session).MessageListeners.ContainsKey(xfr.GetDestination()))
            {
                IMessageListener listener = ((ClientSession)session).MessageListeners[xfr.GetDestination()];
                listener.MessageTransfer( new Message(xfr));
            }
            else
            {
                _log.Warn("No listener set for: {0}", xfr);
            }
        }

        public override void MessageReject(Session session, MessageReject mstruct)
        {
            foreach (Range range in mstruct.GetTransfers())
            {
                for (long l = range.Lower; l <= range.Upper; l++)
                {
                    _log.Warn("message rejected: " + session.GetCommand((int) l));
                }
            }
        }
    }
}
