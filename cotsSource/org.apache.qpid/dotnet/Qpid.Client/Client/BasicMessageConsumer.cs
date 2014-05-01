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
using System.Threading;
using System.Collections;
using System.Collections.Generic;
using log4net;
using Apache.Qpid.Client.Message;
using Apache.Qpid.Collections;
using Apache.Qpid.Framing;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Client
{
    public class BasicMessageConsumer : Closeable, IMessageConsumer
    {
        private static readonly ILog _logger = LogManager.GetLogger(typeof(BasicMessageConsumer));

        private bool _noLocal;

        /** Holds the exclusive status flag for the consumers access to its queue. */
        private bool _exclusive;

        public bool Exclusive
        {
            get { return _exclusive; }
        }

        public bool NoLocal
        {
            get { return _noLocal; }
            set { _noLocal = value; }
        }

        private AcknowledgeMode _acknowledgeMode;

        public AcknowledgeMode AcknowledgeMode
        {
            get { return _acknowledgeMode; }
        }

        private MessageReceivedDelegate _messageListener;

        private bool IsMessageListenerSet
        {
            get { return _messageListener != null; }
        }

        /// <summary>
        /// The consumer tag allows us to close the consumer by sending a jmsCancel method to the
        /// broker
        /// </summary>
        private string _consumerTag;

        /// <summary>
        /// We need to know the channel id when constructing frames
        /// </summary>
        private ushort _channelId;

        private readonly string _queueName;

        /// <summary>
        /// Protects the setting of a messageListener
        /// </summary>
        private readonly object _syncLock = new object();

        /// <summary>
        /// We store the high water prefetch field in order to be able to reuse it when resubscribing in the event of failover
        /// </summary>
        private int _prefetchHigh;

        /// <summary>
        /// We store the low water prefetch field in order to be able to reuse it when resubscribing in the event of failover
        /// </summary>
        private int _prefetchLow;

        /// <summary>
        /// When true indicates that either a message listener is set or that
        /// a blocking receive call is in progress
        /// </summary>
        private bool _receiving;

        /// <summary>
        /// Used in the blocking receive methods to receive a message from
        /// the Channel thread. 
        /// </summary>
        private readonly ConsumerProducerQueue _messageQueue = new ConsumerProducerQueue();

        private MessageFactoryRegistry _messageFactory;

        private AmqChannel _channel;

        // <summary>
        // Tag of last message delievered, whoch should be acknowledged on commit in transaction mode.
        // </summary>
        //private long _lastDeliveryTag;

        /// <summary>
        /// Explicit list of all received but un-acked messages in a transaction. Used to ensure acking is completed when transaction is committed.
        /// </summary>
        private LinkedList<long> _receivedDeliveryTags;

        /// <summary>
        /// Number of messages unacknowledged in DUPS_OK_ACKNOWLEDGE mode
        /// </summary>
        private int _outstanding;

        /// <summary>
        /// Switch to enable sending of acknowledgements when using DUPS_OK_ACKNOWLEDGE mode.
        /// Enabled when _outstannding number of msgs >= _prefetchHigh and disabled at < _prefetchLow
        /// </summary>
        private bool _dups_ok_acknowledge_send;

        internal BasicMessageConsumer(ushort channelId, string queueName, bool noLocal,
                                      MessageFactoryRegistry messageFactory, AmqChannel channel,
                                      int prefetchHigh, int prefetchLow, bool exclusive)
        {
            _channelId = channelId;
            _queueName = queueName;
            _noLocal = noLocal;
            _messageFactory = messageFactory;
            _channel = channel;
            _acknowledgeMode = _channel.AcknowledgeMode;
            _prefetchHigh = prefetchHigh;
            _prefetchLow = prefetchLow;
            _exclusive = exclusive;

            if (_acknowledgeMode == AcknowledgeMode.SessionTransacted)
            {
                _receivedDeliveryTags = new LinkedList<long>();
            }
        }

        #region IMessageConsumer Members

        public MessageReceivedDelegate OnMessage
        {
            get
            {
                return _messageListener;
            }
            set
            {
                CheckNotClosed();

                lock (_syncLock)
                {
                    // If someone is already receiving
                    if (_messageListener != null && _receiving)
                    {
                        throw new InvalidOperationException("Another thread is already receiving...");
                    }

                    _messageListener = value;

                    _receiving = (_messageListener != null);

                    if (_receiving)
                    {
                        _logger.Debug("Message listener set for queue with name " + _queueName);
                    }
                }
            }
        }

        public IMessage Receive(long delay)
        {
            CheckNotClosed();

            lock (_syncLock)
            {
                // If someone is already receiving
                if (_receiving)
                {
                    throw new InvalidOperationException("Another thread is already receiving (possibly asynchronously)...");
                }

                _receiving = true;
            }

            try
            {
                object o = _messageQueue.Dequeue(delay);
                
                return ReturnMessageOrThrowAndPostDeliver(o);
            }
            finally
            {
                lock (_syncLock)
                {
                    _receiving = false;
                }
            }
        }

        private IMessage ReturnMessageOrThrowAndPostDeliver(object o)
        {
            IMessage m = ReturnMessageOrThrow(o);
            if (m != null)
            {
                PostDeliver(m);
            }
            return m;
        }

        public IMessage Receive()
        {
            return Receive(Timeout.Infinite);
        }

        public IMessage ReceiveNoWait()
        {
           return Receive(0);
        }

        #endregion

        /// <summary>
        /// We can get back either a Message or an exception from the queue. This method examines the argument and deals
        /// with it by throwing it (if an exception) or returning it (in any other case).
        /// </summary>
        /// <param name="o">the object off the queue</param>
        /// <returns> a message only if o is a Message</returns>
        /// <exception>JMSException if the argument is a throwable. If it is a QpidMessagingException it is rethrown as is, but if not
        /// a QpidMessagingException is created with the linked exception set appropriately</exception>
        private IMessage ReturnMessageOrThrow(object o)
        {
            // errors are passed via the queue too since there is no way of interrupting the poll() via the API.
            if (o is Exception)
            {
                Exception e = (Exception) o;
                throw new QpidException("Message consumer forcibly closed due to error: " + e, e);
            }
            else
            {
                return (IMessage) o;
            }
        }

        #region IDisposable Members

        public void Dispose()
        {
            Close();
        }

        #endregion

        public override void Close()
        {
        	if (_closed == CLOSED) 
        	{
        		return;        		
        	}
        	// FIXME: Don't we need FailoverSupport here (as we have SyncWrite). i.e. rather than just locking FailOverMutex
            lock (_channel.Connection.FailoverMutex)
            {
                lock (_closingLock)
                {
                    Interlocked.Exchange(ref _closed, CLOSED);

                    AMQFrame cancelFrame = BasicCancelBody.CreateAMQFrame(_channelId, _consumerTag, false);

                    try
                    {
                        _channel.Connection.ConvenientProtocolWriter.SyncWrite(
                            cancelFrame, typeof(BasicCancelOkBody));
                    }
                    catch (AMQException e)
                    {
                        _logger.Error("Error closing consumer: " + e, e);
                        throw new QpidException("Error closing consumer: " + e);
                    }
                    finally
                    {
                        DeregisterConsumer();
                    }
                }
            }
        }

        /**
         * Called from the AMQSession when a message has arrived for this consumer. This methods handles both the case
         * of a message listener or a synchronous receive() caller.
         *
         * @param messageFrame the raw unprocessed mesage
         * @param channelId    channel on which this message was sent
         */
        internal void NotifyMessage(UnprocessedMessage messageFrame, int channelId)
        {
            if (_logger.IsDebugEnabled)
            {
                _logger.Debug("notifyMessage called with message number " + messageFrame.DeliverBody.DeliveryTag);
            }
            try
            {
                AbstractQmsMessage jmsMessage = _messageFactory.CreateMessage((long)messageFrame.DeliverBody.DeliveryTag,
                                                                              messageFrame.DeliverBody.Redelivered,
                                                                              messageFrame.ContentHeader,
                                                                              messageFrame.Bodies);

                _logger.Debug("Message is of type: " + jmsMessage.GetType().Name);

                PreDeliver(jmsMessage);

                if (IsMessageListenerSet)
                {
                    // We do not need a lock around the test above, and the dispatch below as it is invalid
                    // for an application to alter an installed listener while the session is started.
#if __MonoCS__
                        _messageListener(jmsMessage);
#else
                    _messageListener.Invoke(jmsMessage);
#endif
                    PostDeliver(jmsMessage);
                }
                else
                {
                    _messageQueue.Enqueue(jmsMessage);
                }
            }
            catch (Exception e)
            {
                _logger.Error("Caught exception (dump follows) - ignoring...", e); // FIXME
            }
        }


        internal void NotifyError(Exception cause)
        {
            lock (_syncLock)
            {
                SetClosed();

                // we have no way of propagating the exception to a message listener - a JMS limitation - so we
                // deal with the case where we have a synchronous receive() waiting for a message to arrive
                if (_messageListener == null)
                {
                    // offer only succeeds if there is a thread waiting for an item from the queue
                   _messageQueue.Enqueue(cause);
                    _logger.Debug("Passed exception to synchronous queue for propagation to receive()");
                }
                DeregisterConsumer();
            }
        }

        private void SetClosed()
        {
            Interlocked.Exchange(ref _closed, CLOSED);
        }

        /// <summary>
        /// Perform cleanup to deregister this consumer. This occurs when closing the consumer in both the clean
        /// case and in the case of an error occurring.
        /// </summary>
        internal void DeregisterConsumer()
        {
            _channel.DeregisterConsumer(_consumerTag);
        }

        public string ConsumerTag
        {
            get
            {
                return _consumerTag;
            }
            set
            {
                _consumerTag = value;
            }
        }

        /**
         * Called when you need to invalidate a consumer. Used for example when failover has occurred and the
         * client has vetoed automatic resubscription.
         * The caller must hold the failover mutex.
         */
        internal void MarkClosed()
        {
            SetClosed();
            DeregisterConsumer();
        }

        public string QueueName
        {
            get { return _queueName; }
        }

        /// <summary>
        /// Acknowledge up to last message delivered (if any). Used when commiting.
        /// </summary>
        internal void AcknowledgeDelivered()
        {
            foreach (long tag in _receivedDeliveryTags)
            {
                _channel.AcknowledgeMessage((ulong)tag, false);
            }

            _receivedDeliveryTags.Clear();
        }

        internal void RejectUnacked()
        {
            foreach (long tag in _receivedDeliveryTags)
            {
                _channel.RejectMessage((ulong)tag, true);
            }

            _receivedDeliveryTags.Clear();
        }

        private void PreDeliver(AbstractQmsMessage msg)
        {
            switch (AcknowledgeMode)
            {
                case AcknowledgeMode.PreAcknowledge:
                    _channel.AcknowledgeMessage((ulong)msg.DeliveryTag, false);
                    break;

                case AcknowledgeMode.ClientAcknowledge:
                    // We set the session so that when the user calls acknowledge() it can call the method on session
                    // to send out the appropriate frame.
                    //msg.setAMQSession(_session);
                    msg.Channel = _channel;
                    break;
            }
        }

        private void PostDeliver(IMessage m)
        {
            AbstractQmsMessage msg = (AbstractQmsMessage) m;
            switch (AcknowledgeMode)
            {
                case AcknowledgeMode.DupsOkAcknowledge:
                    if (++_outstanding >= _prefetchHigh)
                    {
                        _dups_ok_acknowledge_send = true;
                    }
                    if (_outstanding <= _prefetchLow)
                    {
                        _dups_ok_acknowledge_send = false;
                    }
                    if (_dups_ok_acknowledge_send)
                    {
                        _channel.AcknowledgeMessage((ulong)msg.DeliveryTag, true);
                    }
                    break;

                case AcknowledgeMode.AutoAcknowledge:
                    _channel.AcknowledgeMessage((ulong)msg.DeliveryTag, true);
                    break;
                
                case AcknowledgeMode.SessionTransacted:
                    _receivedDeliveryTags.AddLast(msg.DeliveryTag);
                    break;
            }
        }
    }
}
