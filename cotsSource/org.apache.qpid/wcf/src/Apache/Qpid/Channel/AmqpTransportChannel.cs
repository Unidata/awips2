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

// TODO: flow control
//       timeout handling
//       transactions
//       check if should split into separate input and output classes (little overlap)

namespace Apache.Qpid.Channel
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.Text;
    using System.Threading;
    using System.Globalization;
    using System.Xml;

    // the thin interop layer that provides access to the Qpid AMQP client libraries
    using Apache.Qpid.Interop;
    using Apache.Qpid.AmqpTypes;

    /// <summary>
    /// WCF client transport channel for accessing AMQP brokers using the Qpid C++ library
    /// </summary>
    public class AmqpTransportChannel : ChannelBase, IOutputChannel, IInputChannel
    {
        private static readonly EndpointAddress AnonymousAddress =
            new EndpointAddress("http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous");

        private EndpointAddress remoteAddress;
        private MessageEncoder encoder;
        private AmqpChannelProperties factoryChannelProperties;
        private bool shared;
        private int prefetchLimit;
        private string encoderContentType;

        // input = 0-10 queue, output = 0-10 exchange
        private string queueName;

        private String routingKey;
        private BufferManager bufferManager;
        private AmqpProperties outputMessageProperties;

        private InputLink inputLink;
        private OutputLink outputLink;

        private bool isInputChannel;
        private bool streamed;

        private AsyncTimeSpanCaller asyncOpenCaller;
        private AsyncTimeSpanCaller asyncCloseCaller;

        internal AmqpTransportChannel(ChannelManagerBase factory, AmqpChannelProperties channelProperties, EndpointAddress remoteAddress, MessageEncoder msgEncoder, long maxBufferPoolSize, bool sharedConnection, int prefetchLimit)
            : base(factory)
        {
            this.isInputChannel = (factory is ChannelListenerBase) || (factory is AmqpChannelFactory<IInputChannel>);

            if (remoteAddress == null)
            {
                throw new ArgumentException("Null Endpoint Address");
            }

            this.factoryChannelProperties = channelProperties;
            this.shared = sharedConnection;
            this.prefetchLimit = prefetchLimit;
            this.remoteAddress = remoteAddress;

            // pull out host, port, queue, and connection arguments
            this.ParseAmqpUri(remoteAddress.Uri);

            this.encoder = msgEncoder;
            string ct = String.Empty;
            if (this.encoder != null)
            {
                ct = this.encoder.ContentType;
                if (ct != null)
                {
                    int pos = ct.IndexOf(';');
                    if (pos != -1)
                    {
                        ct = ct.Substring(0, pos).Trim();
                    }
                }
                else
                {
                    ct = "application/octet-stream";
                }
            }

            this.encoderContentType = ct;

            if (this.factoryChannelProperties.TransferMode == TransferMode.Streamed)
            {
                this.streamed = true;
            }
            else
            {
                if (!(this.factoryChannelProperties.TransferMode == TransferMode.Buffered))
                {
                    throw new ArgumentException("TransferMode mode must be \"Streamed\" or \"Buffered\"");
                }

                this.streamed = false;
            }

            this.bufferManager = BufferManager.CreateBufferManager(maxBufferPoolSize, int.MaxValue);

            this.asyncOpenCaller = new AsyncTimeSpanCaller(this.OnOpen);
            this.asyncCloseCaller = new AsyncTimeSpanCaller(this.OnClose);

            if (this.isInputChannel)
            {
                this.inputLink = ConnectionManager.GetInputLink(this.factoryChannelProperties, shared, false, this.queueName);
                this.inputLink.PrefetchLimit = this.prefetchLimit;
            }
            else
            {
                this.outputLink = ConnectionManager.GetOutputLink(this.factoryChannelProperties, shared, false, this.queueName);
            }
        }

        private delegate bool AsyncTryReceiveCaller(TimeSpan timeout, out Message message);

        private delegate void AsyncTimeSpanCaller(TimeSpan timeout);

        EndpointAddress IOutputChannel.RemoteAddress
        {
            get
            {
                return this.remoteAddress;
            }
        }

        // i.e what you would insert into a ReplyTo header to reach
        // here.  Presumably should be exchange/link and routing info,
        // rather than the actual input queue name.
        EndpointAddress IInputChannel.LocalAddress
        {
            get
            {
                // TODO: something better
                return AnonymousAddress;
            }
        }

        AmqpProperties OutputMessageProperties
        {
            get
            {
                if (this.outputMessageProperties == null)
                {
                    this.outputMessageProperties = this.factoryChannelProperties.DefaultMessageProperties;
                    if (this.outputMessageProperties == null)
                    {
                        this.outputMessageProperties = new AmqpProperties();
                    }
                }

                return this.outputMessageProperties;
            }
        }

        Uri IOutputChannel.Via
        {
            get
            {
                return this.remoteAddress.Uri;
            }
        }

        public override T GetProperty<T>()
        {
            if (typeof(T) == typeof(IInputChannel))
            {
                if (this.isInputChannel)
                {
                    return (T)(object)this;
                }
            }
            else if (typeof(T) == typeof(IOutputChannel))
            {
                if (!this.isInputChannel)
                {
                    return (T)(object)this;
                }
            }

            return base.GetProperty<T>();
        }

        public void Send(Message message, TimeSpan timeout)
        {
            this.ThrowIfDisposedOrNotOpen();
            AmqpChannelHelpers.ValidateTimeout(timeout);

            try
            {
                using (AmqpMessage amqpMessage = this.WcfToQpid(message))
                {
                    this.outputLink.Send(amqpMessage, timeout);
                }
            }
            finally
            {
                message.Close();
            }
        }

        public void Send(Message message)
        {
            this.Send(message, this.DefaultSendTimeout);
        }

        public IAsyncResult BeginSend(Message message, TimeSpan timeout, AsyncCallback callback, object state)
        {
            this.ThrowIfDisposedOrNotOpen();
            AmqpChannelHelpers.ValidateTimeout(timeout);

            try
            {
                using (AmqpMessage amqpMessage = this.WcfToQpid(message))
                {
                    return this.outputLink.BeginSend(amqpMessage, timeout, callback, state);
                }
            }
            finally
            {
                message.Close();
            }
        }

        public IAsyncResult BeginSend(Message message, AsyncCallback callback, object state)
        {
            return this.BeginSend(message, this.DefaultSendTimeout, callback, state);
        }

        public void EndSend(IAsyncResult result)
        {
            this.outputLink.EndSend(result);
        }

        public Message Receive(TimeSpan timeout)
        {
            Message message;
            if (this.TryReceive(timeout, out message))
            {
                return message;
            }
            else
            {
                throw new TimeoutException("Receive");
            }
        }

        public Message Receive()
        {
            return this.Receive(this.DefaultReceiveTimeout);
        }

        public bool TryReceive(TimeSpan timeout, out Message message)
        {
            AmqpMessage amqpMessage;
            message = null;

            if (this.inputLink.TryReceive(timeout, out amqpMessage))
            {
                message = this.QpidToWcf(amqpMessage);
                return true;
            }

            return false;
        }

        public IAsyncResult BeginTryReceive(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return this.inputLink.BeginTryReceive(timeout, callback, state);
        }

        public bool EndTryReceive(IAsyncResult result, out Message message)
        {
            AmqpMessage amqpMessage = null;
            if (!this.inputLink.EndTryReceive(result, out amqpMessage))
            {
                message = null;
                return false;
            }
            message = QpidToWcf(amqpMessage);
            return true;
        }

        public bool WaitForMessage(TimeSpan timeout)
        {
            return this.inputLink.WaitForMessage(timeout);
        }

        public IAsyncResult BeginReceive(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return this.inputLink.BeginTryReceive(timeout, callback, state);
        }

        public IAsyncResult BeginReceive(AsyncCallback callback, object state)
        {
            return this.BeginReceive(this.DefaultReceiveTimeout, callback, state);
        }

        public IAsyncResult BeginWaitForMessage(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return this.inputLink.BeginWaitForMessage(timeout, callback, state);
        }

        public Message EndReceive(IAsyncResult result)
        {
            Message message;
            if (this.EndTryReceive(result, out message))
            {
                return message;
            }
            else
            {
                throw new TimeoutException("EndReceive");
            }
        }

        public bool EndWaitForMessage(IAsyncResult result)
        {
            return this.inputLink.EndWaitForMessage(result);
        }

        public void CloseEndPoint()
        {
            if (this.inputLink != null)
            {
                this.inputLink.Close();
            }
            if (this.outputLink != null)
            {
                this.outputLink.Close();
            }
        }

        /// <summary>
        /// Open connection to Broker
        /// </summary>
        protected override void OnOpen(TimeSpan timeout)
        {
            // TODO: move open logic to here from constructor
        }

        protected override IAsyncResult OnBeginOpen(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return this.asyncOpenCaller.BeginInvoke(timeout, callback, state);
        }

        protected override void OnEndOpen(IAsyncResult result)
        {
            this.asyncOpenCaller.EndInvoke(result);
        }

        protected override void OnAbort()
        {
            //// TODO: check for network-less qpid teardown or launch special thread
            this.CloseEndPoint();
            this.Cleanup();
        }

        /// <summary>
        /// Shutdown gracefully
        /// </summary>
        protected override void OnClose(TimeSpan timeout)
        {
            this.CloseEndPoint();
            this.Cleanup();
        }

        protected override IAsyncResult OnBeginClose(TimeSpan timeout, AsyncCallback callback, object state)
        {
            return this.asyncCloseCaller.BeginInvoke(timeout, callback, state);
        }

        protected override void OnEndClose(IAsyncResult result)
        {
            this.asyncCloseCaller.EndInvoke(result);
        }

        private AmqpMessage WcfToQpid(Message wcfMessage)
        {
            object obj;
            AmqpProperties applicationProperties = null;
            bool success = false;
            AmqpMessage amqpMessage = null;

            if (wcfMessage.Properties.TryGetValue("AmqpProperties", out obj))
            {
                applicationProperties = obj as AmqpProperties;
            }

            try
            {
                AmqpProperties outgoingProperties = new AmqpProperties();

                // Start with AMQP properties from the binding and the URI
                if (this.factoryChannelProperties.DefaultMessageProperties != null)
                {
                    outgoingProperties.MergeFrom(this.factoryChannelProperties.DefaultMessageProperties);
                }

                if (this.routingKey != null)
                {
                    outgoingProperties.RoutingKey = this.routingKey;
                }

                // Add the Properties set by the application on this particular message.
                // Application properties trump channel properties
                if (applicationProperties != null)
                {
                    outgoingProperties.MergeFrom(applicationProperties);
                }

                amqpMessage = this.outputLink.CreateMessage();
                amqpMessage.Properties = outgoingProperties;

                // copy the WCF message body to the AMQP message body
                if (this.streamed)
                {
                    this.encoder.WriteMessage(wcfMessage, amqpMessage.BodyStream);
                }
                else
                {
                    ArraySegment<byte> encodedBody = this.encoder.WriteMessage(wcfMessage, int.MaxValue, this.bufferManager);
                    try
                    {
                        amqpMessage.BodyStream.Write(encodedBody.Array, encodedBody.Offset, encodedBody.Count);
                    }
                    finally
                    {
                        this.bufferManager.ReturnBuffer(encodedBody.Array);
                    }
                }

                success = true;
            }
            finally
            {
                if (!success && (amqpMessage != null))
                {
                    amqpMessage.Dispose();
                }
            }
            return amqpMessage;
        }


        private Message QpidToWcf(AmqpMessage amqpMessage)
        {
            if (amqpMessage == null)
            {
                return null;
            }

            Message wcfMessage = null;
            byte[] managedBuffer = null;

            try
            {
                if (this.streamed)
                {
                    wcfMessage = this.encoder.ReadMessage(amqpMessage.BodyStream, int.MaxValue);
                }
                else
                {
                    int count = (int)amqpMessage.BodyStream.Length;
                    managedBuffer = this.bufferManager.TakeBuffer(count);
                    int nr = amqpMessage.BodyStream.Read(managedBuffer, 0, count);
                    ArraySegment<byte> bufseg = new ArraySegment<byte>(managedBuffer, 0, count);

                    wcfMessage = this.encoder.ReadMessage(bufseg, this.bufferManager);

                    // set to null for finally{} block, since the encoder is now responsible for 
                    // returning the BufferManager memory
                    managedBuffer = null;
                }

                // This message will be discarded unless the "To" header matches
                // the WCF endpoint dispatcher's address filter (or the service is
                // AddressFilterMode=AddressFilterMode.Any).

                this.remoteAddress.ApplyTo(wcfMessage);

                if (amqpMessage.Properties != null)
                {
                    wcfMessage.Properties.Add("AmqpProperties", amqpMessage.Properties);
                }
            }
            catch (XmlException xmlException)
            {
                throw new ProtocolException(
                    "There is a problem with the XML that was received from the network. See inner exception for more details.",
                    xmlException);
            }
            catch (Exception e)
            {
                // TODO: logging
                Console.WriteLine("TX channel encoder exception " + e);
            }
            finally
            {
                // close the amqpMessage unless the body will be read at a later time.
                if (!this.streamed || wcfMessage == null)
                {
                    amqpMessage.Close();
                }

                // the handoff to the encoder failed
                if (managedBuffer != null)
                {
                    this.bufferManager.ReturnBuffer(managedBuffer);
                }
            }

            return wcfMessage;
        }

        private void Cleanup()
        {
            this.bufferManager.Clear();
        }

        // "amqp:queue1" | "amqp:stocks@broker1.com" | "amqp:queue3?routingkey=key"
        private void ParseAmqpUri(Uri uri)
        {
            if (uri.Scheme != AmqpConstants.Scheme)
            {
                throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
                    "The scheme {0} specified in address is not supported.", uri.Scheme), "uri");
            }

            this.queueName = uri.LocalPath;

            if ((this.queueName.IndexOf('@') != -1) && this.isInputChannel)
            {
                throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
                    "Invalid input queue name: \"{0}\" specified.", this.queueName), "uri");
            }

            // search out session parameters in the query portion of the URI

            string routingParseKey = "routingkey=";
            char[] charSeparators = new char[] { '?', ';' };
            string[] args = uri.Query.Split(charSeparators, StringSplitOptions.RemoveEmptyEntries);
            foreach (string s in args)
            {
                if (s.StartsWith(routingParseKey))
                {
                    this.routingKey = s.Substring(routingParseKey.Length);
                }
            }

            if (this.queueName == String.Empty)
            {
                if (this.isInputChannel)
                {
                    throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
                        "Empty queue target specifier not allowed."), "uri");
                }
                else
                {
                    if (this.routingKey == null)
                    {
                        throw new ArgumentException(string.Format(CultureInfo.CurrentCulture,
                        "No target queue or routing key specified."), "uri");
                    }
                }
            }
        }
    }
}
