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
    using System.Net;
    using System.Net.Sockets;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.Globalization;

    using Apache.Qpid.AmqpTypes;

    /// <summary>
    /// Collection of constants used by the Amqp Channel classes
    /// </summary>
    static class AmqpConstants
    {
        internal const string Scheme = "amqp";
        internal const string AmqpBindingSectionName = "system.serviceModel/bindings/amqpBinding";
        internal const string AmqpBinaryBindingSectionName = "system.serviceModel/bindings/amqpBinaryBinding";
        internal const string AmqpTransportSectionName = "amqpTransport";
    }
 
    static class AmqpConfigurationStrings
    {
        public const string BrokerHost = "host";
        public const string BrokerPort = "port";
        public const string TransferMode = "transferMode";
        public const string Brokers = "brokers";
        public const string Shared = "shared";
        public const string PrefetchLimit = "prefetchLimit";
        public const string MaxBufferPoolSize = "maxBufferPoolSize";
        public const string MaxReceivedMessageSize = "maxReceivedMessageSize";
    }

    static class AmqpDefaults
    {
        internal const string BrokerHost = "localhost";
        internal const int BrokerPort = 5672;
        internal const TransferMode TransferMode = System.ServiceModel.TransferMode.Buffered;
        internal const long MaxBufferPoolSize = 64 * 1024;
        internal const int MaxReceivedMessageSize = 5 * 1024 * 1024; //64 * 1024;
    }

    // parking spot for properties that may be shared by separate channels on a single AMQP connection
    internal class AmqpChannelProperties
    {
        string brokerHost;
        int brokerPort;
        TransferMode transferMode;
        AmqpProperties defaultMessageProperties;
        
        long maxBufferPoolSize;
        int maxReceivedMessageSize;

        internal AmqpChannelProperties()
        {
            this.brokerHost = AmqpDefaults.BrokerHost;
            this.brokerPort = AmqpDefaults.BrokerPort;
            this.transferMode = AmqpDefaults.TransferMode;
            this.defaultMessageProperties = null;
            this.maxBufferPoolSize = AmqpDefaults.MaxBufferPoolSize;
            this.maxReceivedMessageSize = AmqpDefaults.MaxReceivedMessageSize;
        }

        public AmqpChannelProperties Clone()
        {
            AmqpChannelProperties props = (AmqpChannelProperties) this.MemberwiseClone();
            if (this.defaultMessageProperties != null)
            {
                props.defaultMessageProperties = this.defaultMessageProperties.Clone();
            }

            return props;
        }

        internal string BrokerHost
        {
            get { return this.brokerHost; }
            set { this.brokerHost = value; }
        }

        internal int BrokerPort
        {
            get { return this.brokerPort; }
            set { this.brokerPort = value; }
        }

        internal TransferMode TransferMode
        {
            get { return this.transferMode; }
            set { this.transferMode = value; }
        }

        internal AmqpProperties DefaultMessageProperties
        {
            get { return this.defaultMessageProperties; }
            set { this.defaultMessageProperties = value; }
        }

        internal long MaxBufferPoolSize
        {
            get { return this.maxBufferPoolSize; }
            set { this.maxBufferPoolSize = value; }
        }

        internal int MaxReceivedMessageSize
        {
            get { return this.maxReceivedMessageSize; }
            set { this.maxReceivedMessageSize = value; }
        }
    }

    static class AmqpChannelHelpers
    {
        internal static void ValidateTimeout(TimeSpan timeout)
        {
            if (timeout < TimeSpan.Zero)
            {
                throw new ArgumentOutOfRangeException("timeout", timeout, "Timeout must be greater than or equal to TimeSpan.Zero. To disable timeout, specify TimeSpan.MaxValue.");
            }
        }
    }
}
