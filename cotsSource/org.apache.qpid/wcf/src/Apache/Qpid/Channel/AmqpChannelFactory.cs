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
    using System.Collections.Generic;
    using System.Collections.ObjectModel;

    class AmqpChannelFactory<TChannel> : ChannelFactoryBase<TChannel>
    {
        MessageEncoderFactory messageEncoderFactory;
        AmqpTransportBindingElement bindingElement;
        AmqpChannelProperties channelProperties;
        long maxBufferPoolSize;
        bool shared;
	    int prefetchLimit;
        List<AmqpTransportChannel> openChannels;

        internal AmqpChannelFactory(AmqpTransportBindingElement bindingElement, BindingContext context)
            : base(context.Binding)
        {
            this.bindingElement = bindingElement;
            this.channelProperties = bindingElement.ChannelProperties.Clone();
            this.shared = bindingElement.Shared;
            this.prefetchLimit = bindingElement.PrefetchLimit;
            this.maxBufferPoolSize = bindingElement.MaxBufferPoolSize;
            Collection<MessageEncodingBindingElement> messageEncoderBindingElements
                = context.BindingParameters.FindAll<MessageEncodingBindingElement>();

            if (messageEncoderBindingElements.Count > 1)
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

            openChannels = new List<AmqpTransportChannel>();
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
            throw new NotImplementedException("AmqpChannelFactory OnBeginOpen");
            //// return null;
        }

        protected override void OnEndOpen(IAsyncResult result)
        {
            throw new NotImplementedException("AmqpChannelFactory OnEndOpen");
        }

        protected override TChannel OnCreateChannel(EndpointAddress remoteAddress, Uri via)
        {
            AmqpTransportChannel channel = new AmqpTransportChannel(this, this.channelProperties, remoteAddress, this.messageEncoderFactory.Encoder, this.maxBufferPoolSize, this.shared, this.prefetchLimit);
            lock (openChannels)
            {
                channel.Closed += new EventHandler(channel_Closed);
                openChannels.Add(channel);
            }
            return (TChannel)(object) channel;
        }

        void channel_Closed(object sender, EventArgs e)
        {
            if (this.State != CommunicationState.Opened)
            {
                return;
            }

            lock (openChannels)
            {
                AmqpTransportChannel channel = (AmqpTransportChannel)sender;
                if (openChannels.Contains(channel))
                {
                    openChannels.Remove(channel);
                }
            }
        }

        protected override void OnClose(TimeSpan timeout)
        {
            base.OnClose(timeout);
            lock (openChannels)
            {
                foreach (AmqpTransportChannel channel in openChannels)
                {
                    channel.Close(timeout);
                }
            }
        }
    }
}
