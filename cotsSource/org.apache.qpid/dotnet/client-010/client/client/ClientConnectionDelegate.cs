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

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using org.apache.qpid.transport;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.client
{
    internal class ClientConnectionDelegate : ClientDelegate
    {
        private static readonly Logger log = Logger.Get(typeof (ClientConnectionDelegate));
        private readonly Client _client;
        private string _username;
        private string _password;
        private Exception _exception;

        public ClientConnectionDelegate(Client client, string username, string pasword)
        {
            _client = client;
            _username = username;
            _password = pasword;
        }

        public Exception Exception
        {
            get { return _exception; }
        }

        public override SessionDelegate GetSessionDelegate()
        {
            return new ClientSessionDelegate();
        }

        public override void RaiseException(Exception exception)
        {
            _exception = exception;

            if (_negotiationComplete != null)
                _negotiationComplete.Set();
            else
                _client.RaiseException(exception);
        }

        public override void ConnectionStart(Channel context, ConnectionStart mystruct)
        {
            const string mechanism = "PLAIN";          
            MemoryStream stResponse = new MemoryStream();
            byte[] part = Encoding.UTF8.GetBytes(_username);
            stResponse.WriteByte(0);
            stResponse.Write(part, 0, part.Length);
            stResponse.WriteByte(0);
            part = Encoding.UTF8.GetBytes(_password);
            stResponse.Write(part, 0, part.Length);            
            Dictionary<String, Object> props = new Dictionary<String, Object>();
            context.ConnectionStartOk(props, mechanism, stResponse.ToArray(), "utf8");
        }

        public override void Closed()
        {
            log.Debug("Delegate Closed");
            lock (_client.CloseOk)
            {
                try
                {
                    _client.IsClosed = true;
                    Monitor.PulseAll(_client.CloseOk);
                }
                catch (Exception e)
                {
                    throw new SystemException("Error when closing client", e);
                } 
            }
        }

        public override void ConnectionClose(Channel context, ConnectionClose connectionClose)
        {
            base.ConnectionClose(context, connectionClose);
            ErrorCode errorCode = ErrorCode.GetErrorCode((int) connectionClose.GetReplyCode());

            if(_client.ClosedListener != null)
                _client.ClosedListener.OnClosed(errorCode, connectionClose.GetReplyText(), null);

            if (errorCode.Code != (int)QpidErrorCode.NO_ERROR)
                throw new Exception ("Server Closed the connection: Reason " + connectionClose.GetReplyText());
        }
    }
}
