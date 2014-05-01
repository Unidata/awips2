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
    public class QpidChannelFactory : ChannelFactoryBase<IOutputChannel>
    {
        private readonly BindingContext _context;
        private readonly CommunicationOperation _openMethod;
        private readonly QpidTransportBindingElement _bindingElement;
        private ClientSession _session;

        public QpidChannelFactory(BindingContext context)
        {
            _context = context;
            _openMethod = Open;
            _bindingElement = context.Binding.Elements.Find<QpidTransportBindingElement>();            
        }

        protected override IOutputChannel OnCreateChannel(EndpointAddress address, Uri via)
        {
            return new QpidOutputChannel(_context, _session, address);
        }
        
        protected override IAsyncResult OnBeginOpen(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _openMethod.BeginInvoke(timeout, callback, state);
        }

        protected override void OnEndOpen(IAsyncResult result)
        {
            _openMethod.EndInvoke(result);
        }

        protected override void OnOpen(TimeSpan timeout)
        {
            _session = _bindingElement.Open(timeout.Milliseconds);
       }

        protected override void OnClose(TimeSpan timeout)
        {           
            _bindingElement.Close();
        }

        protected override void OnAbort()
        {
            base.OnAbort();
            OnClose(_context.Binding.CloseTimeout);
        }
    }
}
