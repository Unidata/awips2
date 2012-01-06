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
    using System.ServiceModel.Description;
    using Apache.Qpid.AmqpTypes;

    public class AmqpTransportBindingElement : TransportBindingElement, ITransactedBindingElement
    {
        AmqpChannelProperties channelProperties;
        bool shared;
	int prefetchLimit;

        public AmqpTransportBindingElement()
        {
            // start with default properties
            channelProperties = new AmqpChannelProperties();
        }

        protected AmqpTransportBindingElement(AmqpTransportBindingElement other)
            : base(other)
        {
            this.channelProperties = other.channelProperties.Clone();
            this.shared = other.shared;
            this.prefetchLimit = other.prefetchLimit;
        }

        public override IChannelFactory<TChannel> BuildChannelFactory<TChannel>(BindingContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException("context");
            }

            return (IChannelFactory<TChannel>)(object)new AmqpChannelFactory<TChannel>(this, context);
        }

        public override IChannelListener<TChannel> BuildChannelListener<TChannel>(BindingContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException("context");
            }

            return (IChannelListener<TChannel>)(object)new AmqpChannelListener(this, context);
        }



        public override bool CanBuildChannelFactory<TChannel>(BindingContext context)
        {
            return ((typeof(TChannel) == typeof(IOutputChannel)) ||
                    (typeof(TChannel) == typeof(IInputChannel)));
        }

        public override bool CanBuildChannelListener<TChannel>(BindingContext context)
        {
            return ((typeof(TChannel) == typeof(IInputChannel)));
        }

        public override BindingElement Clone()
        {
            return new AmqpTransportBindingElement(this);
        }

        internal AmqpChannelProperties ChannelProperties
        {
            get { return channelProperties; }
        }

        public string BrokerHost
        {
            get { return this.channelProperties.BrokerHost; }
            set { this.channelProperties.BrokerHost = value; }
        }

        public int BrokerPort
        {
            get { return this.channelProperties.BrokerPort; }
            set { this.channelProperties.BrokerPort = value; }
        }

        public int PrefetchLimit
        {
            get { return this.prefetchLimit; }
            set { this.prefetchLimit = value; }
        }

        public bool Shared
        {
            get { return this.shared; }
            set { this.shared = value; }
        }

        public bool TransactedReceiveEnabled
        {
            get { return true; }
        }

        public TransferMode TransferMode
        {
            get { return this.channelProperties.TransferMode; }
            set { this.channelProperties.TransferMode = value; }
        }

        public AmqpProperties DefaultMessageProperties
        {
            get { return this.channelProperties.DefaultMessageProperties; }

            set { this.channelProperties.DefaultMessageProperties = value; }
        }

        public override T GetProperty<T>(BindingContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException("context");
            }

            if (typeof(T) == typeof(MessageVersion))
            {
                return (T)(object)MessageVersion.Default;
            }


            return context.GetInnerProperty<T>();
        }

        public override string Scheme
        {
            get
            {
                return AmqpConstants.Scheme;
            }
        }

    }
}
