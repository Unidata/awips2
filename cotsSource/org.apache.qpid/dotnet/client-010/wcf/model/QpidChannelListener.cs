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
using System.ServiceModel;
using System.ServiceModel.Channels;
using org.apache.qpid.client;

namespace org.apache.qpid.wcf.model
{
    public sealed class QpidChannelListener : QpidChannelListenerBase<IInputChannel>
    {

        private IInputChannel _channel;
        private ClientSession _session;
      
        public QpidChannelListener(BindingContext context)
            : base(context)
        {
           _channel = null;
            _session = null;          
        }

        protected override IInputChannel OnAcceptChannel(TimeSpan timeout)
        {
            // Since only one connection to a broker is required (even for communication
            // with multiple exchanges 
            if (_channel != null)
                return null;

            _channel = new QpidInputChannel(Context, _session, new EndpointAddress(Uri.ToString()));
            _channel.Closed += ListenChannelClosed;
            return _channel;
        }
        
        protected override bool OnWaitForChannel(TimeSpan timeout)
        {
            return false;
        }

        protected override void OnOpen(TimeSpan timeout)
        {
            _session = _bindingElement.Open(timeout.Milliseconds);
        }

        protected override void OnClose(TimeSpan timeout)
        {
            if (_channel != null)
            {
                _channel.Close();
                _channel = null;
            }
            _bindingElement.Close();
        }

        private void ListenChannelClosed(object sender, EventArgs args)
        {
            Close();
        }
}
}
