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
using System.ServiceModel.Channels;
using System.ServiceModel.Description;

namespace org.apache.qpid.wcf.model
{
    public abstract class QpidChannelListenerBase<TChannel> : ChannelListenerBase<TChannel> where TChannel: class, IChannel
    {
        private readonly Uri _listenUri;
        private readonly BindingContext _context;
        protected QpidTransportBindingElement _bindingElement;
        private readonly CommunicationOperation _closeMethod;
        private readonly CommunicationOperation _openMethod;
        private readonly CommunicationOperation<TChannel> _acceptChannelMethod;
        private readonly CommunicationOperation<bool> _waitForChannelMethod;
       
        protected QpidChannelListenerBase(BindingContext context)
        {
            _context = context;
            _bindingElement = context.Binding.Elements.Find<QpidTransportBindingElement>();
            _closeMethod = OnClose;
            _openMethod = OnOpen;
            _waitForChannelMethod = OnWaitForChannel;
            _acceptChannelMethod = OnAcceptChannel;
            if (context.ListenUriMode == ListenUriMode.Explicit && context.ListenUriBaseAddress != null)
            {
                _listenUri = new Uri(context.ListenUriBaseAddress, context.ListenUriRelativeAddress);
            }
            else
            {
                _listenUri = new Uri(new Uri("soap.amqp:///"), Guid.NewGuid().ToString());
            }
         }

        protected override void OnAbort()
        {
            OnClose(_context.Binding.CloseTimeout);
        }

        protected override IAsyncResult OnBeginAcceptChannel(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _acceptChannelMethod.BeginInvoke(timeout, callback, state);
        }

        protected override TChannel OnEndAcceptChannel(IAsyncResult result)
        {
            return _acceptChannelMethod.EndInvoke(result);
        }

        protected override IAsyncResult OnBeginWaitForChannel(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _waitForChannelMethod.BeginInvoke(timeout, callback, state);
        }

        protected override bool OnEndWaitForChannel(IAsyncResult result)
        {
            return _waitForChannelMethod.EndInvoke(result);
        }
        
        protected override IAsyncResult OnBeginClose(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _closeMethod.BeginInvoke(timeout, callback, state);
        }

        protected override IAsyncResult OnBeginOpen(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _openMethod.BeginInvoke(timeout, callback, state);
        }

        protected override void OnEndClose(IAsyncResult result)
        {
            _closeMethod.EndInvoke(result);
        }

        protected override void OnEndOpen(IAsyncResult result)
        {
            _openMethod.EndInvoke(result);
        }

        public override Uri Uri
        {
            get { return _listenUri; }
        }

        protected BindingContext Context
        {
            get { return _context; }
        }
    }
}
