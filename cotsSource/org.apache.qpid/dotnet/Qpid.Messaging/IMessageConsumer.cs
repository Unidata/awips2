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

namespace Apache.Qpid.Messaging
{
    /// <summary>
    /// Describes an object that can be used to receive (consume)
    /// messages from an AMQP queue.
    /// </summary>
    /// <remarks>
    /// Consumers are created using either 
    /// <see cref="IChannel.CreateConsumer"/> or using 
    /// the builder pattern (preferred) with 
    /// <see cref="IChannel.CreateConsumerBuilder"/>.
    /// 
    /// <para>
    /// Consumers offer two different ways of receiving messages:
    /// You can attach a delegate to the <see cref="OnMessage"/>
    /// event and be notified when a message arrives, or you can
    /// use the <see cref="Receive"/> and <see cref="ReceiveNoWait"/>
    /// methods to control when you receive messages. Be aware that you can use 
    /// one or the other, but not both at the same time.
    /// </para>
    /// <para>
    /// Regardless of which method you choose, the prefetch settings
    /// specified when creating the channel will still control when messages
    /// are actually received from the AMQP broker. Any messages that arrive
    /// between the prefetch window will be queued by the channel
    /// until they can be delivered to the consumer (either though the event
    /// or until the consumer actively calls <see cref="Receive"/>).
    /// </para>
    /// </remarks>
    public interface IMessageConsumer : IDisposable, ICloseable
    {
      /// <summary>
      /// Fired when a message is received from the broker by the consumer
      /// </summary>
      MessageReceivedDelegate OnMessage { get; set; }

      /// <summary>
      /// Wait infinitely for a message to be received from the broker
      /// </summary>
      /// <returns>The message received</returns>
      IMessage Receive();

      /// <summary>
      /// Wait the specified time until a message is receive from the broker
      /// </summary>
      /// <param name="delay">Maximum number of milliseconds to wait for a message</param>
      /// <returns>The message received, or null if the timeout expires</returns>
      IMessage Receive(long delay);

      /// <summary>
      /// Return a message if one is already available in the channel. 
      /// Does not wait for one to be received from the broker.
      /// </summary>
      /// <returns>The message, if it was available, otherwise null</returns>
      IMessage ReceiveNoWait();
   }
}
