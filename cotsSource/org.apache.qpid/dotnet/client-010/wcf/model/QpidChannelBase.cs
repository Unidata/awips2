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
    internal abstract class QpidChannelBase : IChannel
    {
        private readonly CommunicationOperation _closeMethod;
        private readonly BindingContext _context;
        private readonly CommunicationOperation _openMethod;
        private CommunicationState _state;

        private QpidChannelBase()
        {
            _state = CommunicationState.Created;
            _closeMethod = Close;
            _openMethod = Open;
        }

        protected QpidChannelBase(BindingContext context)
            : this()
        {
            _context = context;
        }

        public abstract void Close(TimeSpan timeout);

        public abstract void Open(TimeSpan timeout);

        public virtual void Abort()
        {
            Close();
        }

        public virtual void Close()
        {
            Close(_context.Binding.CloseTimeout);
        }

        public virtual T GetProperty<T>() where T : class
        {
            return default(T);
        }

        public virtual void Open()
        {
            Open(_context.Binding.OpenTimeout);
        }

        #region Async Methods

        public virtual IAsyncResult BeginClose(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _closeMethod.BeginInvoke(timeout, callback, state);
        }

        public virtual IAsyncResult BeginClose(AsyncCallback callback, object state)
        {
            return _closeMethod.BeginInvoke(_context.Binding.CloseTimeout, callback, state);
        }

        public virtual IAsyncResult BeginOpen(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return _openMethod.BeginInvoke(timeout, callback, state);
        }

        public virtual IAsyncResult BeginOpen(AsyncCallback callback, object state)
        {
            return _openMethod.BeginInvoke(_context.Binding.OpenTimeout, callback, state);
        }

        public virtual void EndClose(IAsyncResult result)
        {
            _closeMethod.EndInvoke(result);
        }

        public virtual void EndOpen(IAsyncResult result)
        {
            _openMethod.EndInvoke(result);
        }

        #endregion

        #region Event Raising Methods

        protected void OnOpening()
        {
            _state = CommunicationState.Opening;
            if (Opening != null)
                Opening(this, null);
        }

        protected void OnOpened()
        {
            _state = CommunicationState.Opened;
            if (Opened != null)
                Opened(this, null);
        }

        protected void OnClosing()
        {
            _state = CommunicationState.Closing;
            if (Closing != null)
                Closing(this, null);
        }

        protected void OnClosed()
        {
            _state = CommunicationState.Closed;
            if (Closed != null)
                Closed(this, null);
        }

        protected void OnFaulted()
        {
            _state = CommunicationState.Faulted;
            if (Faulted != null)
                Faulted(this, null);
        }

        #endregion


        public CommunicationState State
        {
            get { return _state; }
        }

        protected BindingContext Context
        {
            get { return _context; }
        }


        public event EventHandler Closed;

        public event EventHandler Closing;

        public event EventHandler Faulted;

        public event EventHandler Opened;

        public event EventHandler Opening;
    }
}
