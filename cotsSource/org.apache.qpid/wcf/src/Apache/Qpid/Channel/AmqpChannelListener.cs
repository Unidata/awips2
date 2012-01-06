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

namespace Apache.Qpid.Channel
{
    using System;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.Threading;
    using System.Collections.Generic;
    using System.Collections.ObjectModel;

    class AmqpChannelListener : ChannelListenerBase<IInputChannel>
    {
        MessageEncoderFactory messageEncoderFactory;
        AmqpTransportBindingElement bindingElement;
        AmqpChannelProperties channelProperties;
        bool shared;
        int prefetchLimit;
        long maxBufferPoolSize;
        Uri uri;
        AmqpTransportChannel amqpTransportChannel;
        delegate IInputChannel AsyncOnAcceptCaller (TimeSpan timeout);
        AsyncOnAcceptCaller asyncOnAcceptCaller;
        ManualResetEvent acceptWaitEvent;

        internal AmqpChannelListener(AmqpTransportBindingElement bindingElement, BindingContext context)
            : base(context.Binding)
        {
            this.bindingElement = bindingElement;
            this.channelProperties = bindingElement.ChannelProperties.Clone();
            this.shared = bindingElement.Shared;
            this.prefetchLimit = bindingElement.PrefetchLimit;

            this.maxBufferPoolSize = bindingElement.MaxBufferPoolSize;

            // TODO: review this.  Should be unique hostname based
            this.uri = context.ListenUriBaseAddress;
            this.asyncOnAcceptCaller = new AsyncOnAcceptCaller(this.OnAcceptChannel);
            this.acceptWaitEvent = new ManualResetEvent(false);

            Collection<MessageEncodingBindingElement> messageEncoderBindingElements
                = context.BindingParameters.FindAll<MessageEncodingBindingElement>();

            if(messageEncoderBindingElements.Count > 1)
            {
                throw new InvalidOperationException("More than one MessageEncodingBindingElement was found in the BindingParameters of the BindingContext");
            }
            else if (messageEncoderBindingElements.Count == 1)
            {
                this.messageEncoderFactory = messageEncoderBindingElements[0].CreateMessageEncoderFactory();
            }
            else
            {
                this.messageEncoderFactory = new TextMessageEncodingBindingElement().CreateMessageEncoderFactory();
            }
        }

        public override Uri Uri
        {
            get
            {
                return this.uri;
            }
        }



        public override T GetProperty<T>()
        {
            T mep = messageEncoderFactory.Encoder.GetProperty<T>();
            if (mep != null)
            {
                return mep;
            }

            if (typeof(T) == typeof(MessageVersion))
            {
                return (T)(object)messageEncoderFactory.Encoder.MessageVersion;
            }

            return base.GetProperty<T>();
        }

        protected override void OnOpen(TimeSpan timeout)
        {
        }

        protected override IAsyncResult OnBeginOpen(TimeSpan timeout, AsyncCallback callback, object state)
        {
            throw new NotImplementedException("AmqpChannelListener OnBeginOpen");
            //// return null;
        }

        protected override void OnEndOpen(IAsyncResult result)
        {
            throw new NotImplementedException("AmqpChannelListener OnEndOpen");
        }

        protected override bool OnWaitForChannel(TimeSpan timeout)
        {
            throw new NotImplementedException("AmqpChannelListener OnWaitForChannel");
        }

        protected override IAsyncResult OnBeginWaitForChannel(TimeSpan timeout, AsyncCallback callback, object state)
        {
            throw new NotImplementedException("AmqpChannelListener OnBeginWaitForChannel");
        }

        protected override bool OnEndWaitForChannel(IAsyncResult result)
        {
            throw new NotImplementedException("AmqpChannelListener OnEndWaitForChannel");
        }

        protected override IInputChannel OnAcceptChannel(TimeSpan timeout)
        {
            if (this.IsDisposed)
            {
                return null;
            }

            if (amqpTransportChannel == null)
            {
                // TODO: add timeout processing
                amqpTransportChannel = new AmqpTransportChannel(this, this.channelProperties,
                        new EndpointAddress(uri), messageEncoderFactory.Encoder,
                        maxBufferPoolSize, this.shared, this.prefetchLimit);
                return (IInputChannel)(object)amqpTransportChannel;
            }

            // Singleton channel.  Subsequent Accepts wait until the listener is closed
            acceptWaitEvent.WaitOne();
            return null;
        }

        protected override IAsyncResult OnBeginAcceptChannel(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return asyncOnAcceptCaller.BeginInvoke(timeout, callback, state);
        }

        protected override IInputChannel OnEndAcceptChannel(IAsyncResult result)
        {
            return asyncOnAcceptCaller.EndInvoke(result);
        }

        protected override void OnClose(TimeSpan timeout)
        {
            if (amqpTransportChannel != null)
            {
                amqpTransportChannel.Close();
            }
            acceptWaitEvent.Set();
        }

        protected override IAsyncResult OnBeginClose(TimeSpan timeout, AsyncCallback callback, object state)
        {
            throw new NotImplementedException("AmqpChannelListener OnBeginClose");
        }

        protected override void OnEndClose(IAsyncResult result)
        {
            throw new NotImplementedException("AmqpChannelListener OnEndClose");
        }

        protected override void OnAbort()
        {
            if (amqpTransportChannel != null)
                amqpTransportChannel.Abort();
            acceptWaitEvent.Set();
        }
    }
}
