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

namespace org.apache.qpid.wcf.model
{
    internal abstract class QpidOutputChannelBase : QpidChannelBase, IOutputChannel
    {        
        
        private readonly SendOperation _sendMethod;
        private readonly EndpointAddress _address;

        protected QpidOutputChannelBase(BindingContext context, EndpointAddress address)
            : base(context)
        {
            _address = address;
            _sendMethod = Send;
        }

        #region Async Methods

        public IAsyncResult BeginSend(Message message, TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _sendMethod.BeginInvoke(message, timeout, callback, state);
        }

        public IAsyncResult BeginSend(Message message, AsyncCallback callback, object state)
        {
            return _sendMethod.BeginInvoke(message, Context.Binding.SendTimeout, callback, state);
        }

        public void EndSend(IAsyncResult result)
        {
            _sendMethod.EndInvoke(result);
        }

        #endregion

        public abstract void Send(Message message, TimeSpan timeout);

        public virtual void Send(Message message)
        {
            Send(message, Context.Binding.SendTimeout);
        }

        public EndpointAddress RemoteAddress
        {
            get { return _address; }
        }

        public Uri Via
        {
            get { throw new NotImplementedException(); }
        }
    }
}
