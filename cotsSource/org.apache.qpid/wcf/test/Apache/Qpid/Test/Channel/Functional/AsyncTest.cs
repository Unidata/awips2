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

namespace Apache.Qpid.Test.Channel.Functional
{
    using System;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.Threading;
    using NUnit.Framework;

    [TestFixture]
    public class AsyncTest
    {
        private const int MessageCount = 20;
        private const string Queue = "amqp:amq.direct?routingkey=routing_key";
        private Uri endpoint = new Uri("amqp:message_queue");
        private TimeSpan standardTimeout = TimeSpan.FromSeconds(10.0); // seconds

        [Test]
        public void NonTryReceives()
        {
            this.SendMessages(this.standardTimeout, this.standardTimeout);
            this.ReceiveNonTryMessages(this.standardTimeout, this.standardTimeout);
        }

        [Test]
        public void TryReceives()
        {
            this.SendMessages(this.standardTimeout, this.standardTimeout);
            this.ReceiveTryMessages(this.standardTimeout, this.standardTimeout);
        }

        [Test]
        public void SmallTimeout()
        {
            // This code is commented out due to a bug in asynchronous channel open.
            ////IChannelListener parentListener;
            ////try
            ////{
            ////    this.RetrieveAsyncChannel(new Uri("amqp:fake_queue_do_not_create"), TimeSpan.FromMilliseconds(10.0), out parentListener);
            ////    parentListener.Close();
            ////    Assert.Fail("Accepting the channel did not time out.");
            ////}
            ////catch (TimeoutException)
            ////{
            ////    // Intended exception.
            ////}

            try
            {
                this.ReceiveNonTryMessages(this.standardTimeout, TimeSpan.FromMilliseconds(10.0));
                Assert.Fail("Receiving a message did not time out.");
            }
            catch (TimeoutException)
            {
                // Intended exception.
            }
        }

        private void SendMessages(TimeSpan channelTimeout, TimeSpan messageSendTimeout)
        {
            ChannelFactory<IOutputChannel> channelFactory = 
                    new ChannelFactory<IOutputChannel>(Util.GetBinding(), Queue);
            IOutputChannel proxy = channelFactory.CreateChannel();
            IAsyncResult[] resultArray = new IAsyncResult[MessageCount];

            for (int i = 0; i < MessageCount; i++)
            {
                Message toSend = Message.CreateMessage(MessageVersion.Default, string.Empty, i);
                resultArray[i] = proxy.BeginSend(toSend, messageSendTimeout, null, null);
            }

            for (int j = 0; j < MessageCount; j++)
            {
                proxy.EndSend(resultArray[j]);
            }

            IAsyncResult iocCloseResult = proxy.BeginClose(channelTimeout, null, null);
            Thread.Sleep(TimeSpan.FromMilliseconds(50.0)); // Dummy work
            proxy.EndClose(iocCloseResult);

            IAsyncResult chanFactCloseResult = channelFactory.BeginClose(channelTimeout, null, null);
            Thread.Sleep(TimeSpan.FromMilliseconds(50.0)); // Dummy work
            channelFactory.EndClose(chanFactCloseResult);
        }

        private void ReceiveNonTryMessages(TimeSpan channelTimeout, TimeSpan messageTimeout)
        {
            IChannelListener inputChannelParentListener;
            IInputChannel inputChannel = this.RetrieveAsyncChannel(this.endpoint, channelTimeout, out inputChannelParentListener);

            inputChannel.Open();

            IAsyncResult[] resultArray = new IAsyncResult[MessageCount];
            try
            {
                for (int i = 0; i < MessageCount; i++)
                {
                    resultArray[i] = inputChannel.BeginReceive(messageTimeout, null, null);
                }

                for (int j = 0; j < MessageCount; j++)
                {
                    inputChannel.EndReceive(resultArray[j]);
                }
            }
            finally
            {
                IAsyncResult channelCloseResult = inputChannel.BeginClose(channelTimeout, null, null);
                Thread.Sleep(TimeSpan.FromMilliseconds(50.0)); // Dummy work
                inputChannel.EndClose(channelCloseResult);

                // Asynchronous listener close has not been implemented.
                ////IAsyncResult listenerCloseResult = inputChannelParentListener.BeginClose(channelTimeout, null, null);
                ////Thread.Sleep(TimeSpan.FromMilliseconds(50.0)); // Dummy work
                ////inputChannelParentListener.EndClose(listenerCloseResult);

                inputChannelParentListener.Close();
            }
        }

        private void ReceiveTryMessages(TimeSpan channelAcceptTimeout, TimeSpan messageReceiveTimeout)
        {
            IChannelListener<IInputChannel> listener = Util.GetBinding().BuildChannelListener<IInputChannel>(this.endpoint, new BindingParameterCollection());
            listener.Open();
            IInputChannel inputChannel = listener.AcceptChannel(channelAcceptTimeout);
            IAsyncResult channelResult = inputChannel.BeginOpen(channelAcceptTimeout, null, null);
            Thread.Sleep(TimeSpan.FromMilliseconds(50.0));
            inputChannel.EndOpen(channelResult);

            IAsyncResult[] resultArray = new IAsyncResult[MessageCount];

            for (int i = 0; i < MessageCount; i++)
            {
                resultArray[i] = inputChannel.BeginTryReceive(messageReceiveTimeout, null, null);
            }

            for (int j = 0; j < MessageCount; j++)
            {
                Message tempMessage;
                Assert.True(inputChannel.EndTryReceive(resultArray[j], out tempMessage), "Did not successfully receive message #{0}", j);
            }

            inputChannel.Close();
            listener.Close();
        }

        private IInputChannel RetrieveAsyncChannel(Uri queue, TimeSpan timeout, out IChannelListener parentListener)
        {
            IChannelListener<IInputChannel> listener =
                    Util.GetBinding().BuildChannelListener<IInputChannel>(queue, new BindingParameterCollection());
            listener.Open();
            IInputChannel inputChannel;

            try
            {
                IAsyncResult acceptResult = listener.BeginAcceptChannel(timeout, null, null);
                Thread.Sleep(TimeSpan.FromMilliseconds(300.0)); // Dummy work
                inputChannel = listener.EndAcceptChannel(acceptResult);
            }
            catch (TimeoutException e)
            {
                listener.Close();
                throw e;
            }
            finally
            {
                parentListener = listener;
            }
            return inputChannel;
        }
    }
}
