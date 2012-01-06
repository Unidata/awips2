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
using System.Collections;
using log4net;
using Apache.Qpid.Client.Message;
using Apache.Qpid.Client.Transport;
using Apache.Qpid.Framing;
using Apache.Qpid.Sasl;

namespace Apache.Qpid.Client.Protocol
{
    public class AMQProtocolSession
    {
        private static readonly ILog _logger = LogManager.GetLogger(typeof(AMQProtocolSession));

        private readonly IProtocolWriter _protocolWriter;
        private readonly IConnectionCloser _connectionCloser;

        /// <summary>
        /// Maps from the channel id to the AmqChannel that it represents.
        /// </summary>
        //private ConcurrentMap _channelId2SessionMap = new ConcurrentHashMap();
        private Hashtable _channelId2SessionMap = Hashtable.Synchronized(new Hashtable());

        //private ConcurrentMap _closingChannels = new ConcurrentHashMap();
        private Hashtable _closingChannels = Hashtable.Synchronized(new Hashtable());

        /// <summary>
        /// Maps from a channel id to an unprocessed message. This is used to tie together the
        /// JmsDeliverBody (which arrives first) with the subsequent content header and content bodies.
        /// </summary>
        //private ConcurrentMap _channelId2UnprocessedMsgMap = new ConcurrentHashMap();
        private Hashtable _channelId2UnprocessedMsgMap = Hashtable.Synchronized(new Hashtable());

        private AMQConnection _connection;
        
        public string ClientID { get { return _connection.ClientID; } }

        public AMQProtocolSession(IProtocolWriter protocolWriter, IConnectionCloser connectionCloser, AMQConnection connection)
        {
            _protocolWriter = protocolWriter;
            _connectionCloser = connectionCloser;
            _connection = connection;
        }

        public void Init()
        {
            // start the process of setting up the connection. This is the first place that
            // data is written to the server.
            _protocolWriter.Write(new ProtocolInitiation());
        }

        public string Username
        {
            get
            {
                return AMQConnection.Username;
            }            
        }

        public string Password
        {
            get
            {
                return AMQConnection.Password;
            }            
        }

        ConnectionTuneParameters _connectionTuneParameters; // TODO: should be able to have this in the Java too.
        
        public ConnectionTuneParameters ConnectionTuneParameters
        {
            get
            {
                return _connectionTuneParameters;
            }            
            set
            {
                _connectionTuneParameters = value;
                AMQConnection con = AMQConnection;
                con.SetMaximumChannelCount(value.ChannelMax);
                con.MaximumFrameSize = value.FrameMax;
            }
        }

        private ISaslClient _saslClient;
        public ISaslClient SaslClient
        {
            get { return _saslClient; }
            set { _saslClient = value; }
        }
        
        /// <summary>
        /// Callback invoked from the BasicDeliverMethodHandler when a message has been received.
        /// This is invoked on the MINA dispatcher thread.
        /// </summary>
        /// <param name="message">the unprocessed message</param>
        /// <exception cname="AMQException">if this was not expected</exception>         
        public void UnprocessedMessageReceived(UnprocessedMessage message)
        {
            _channelId2UnprocessedMsgMap[message.ChannelId] = message;
        }

        public void MessageContentHeaderReceived(ushort channelId, ContentHeaderBody contentHeader)                
        {
            UnprocessedMessage msg = (UnprocessedMessage) _channelId2UnprocessedMsgMap[channelId];
            if (msg == null)
            {
                throw new AMQException("Error: received content header without having received a JMSDeliver frame first");
            }
            if (msg.ContentHeader != null)
            {
                throw new AMQException("Error: received duplicate content header or did not receive correct number of content body frames");
            }
            msg.ContentHeader = contentHeader;
            if (contentHeader.BodySize == 0)
            {
                DeliverMessageToAMQSession(channelId, msg);
            }
        }

        public void MessageContentBodyReceived(ushort channelId, ContentBody contentBody)
        {
            UnprocessedMessage msg = (UnprocessedMessage) _channelId2UnprocessedMsgMap[channelId];
            if (msg == null)
            {
                throw new AMQException("Error: received content body without having received a BasicDeliver frame first");
            }
            if (msg.ContentHeader == null)
            {
                _channelId2UnprocessedMsgMap.Remove(channelId);
                throw new AMQException("Error: received content body without having received a ContentHeader frame first");
            }
            try
            {
                msg.ReceiveBody(contentBody);
            }
            catch (UnexpectedBodyReceivedException e)
            {
                _channelId2UnprocessedMsgMap.Remove(channelId);
                throw e;
            }
            if (msg.IsAllBodyDataReceived())
            {
                DeliverMessageToAMQSession(channelId,  msg);
            }
        }

        /// <summary>
        /// Deliver a message to the appropriate session, removing the unprocessed message
        /// from our map
        /// <param name="channelId">the channel id the message should be delivered to</param>
        /// <param name="msg"> the message</param>         
        private void DeliverMessageToAMQSession(ushort channelId, UnprocessedMessage msg)
        {
            AmqChannel channel = (AmqChannel) _channelId2SessionMap[channelId];
            channel.MessageReceived(msg);
            _channelId2UnprocessedMsgMap.Remove(channelId);
        }

        /// <summary>
        /// Convenience method that writes a frame to the protocol session. Equivalent
        /// to calling getProtocolSession().write().
        /// </summary>
        /// <param name="frame">the frame to write</param>
        public void WriteFrame(IDataBlock frame)
        {
            _protocolWriter.Write(frame);
        }

        public void AddSessionByChannel(ushort channelId, AmqChannel channel)
        {            
            if (channel == null)
            {
                throw new ArgumentNullException("Attempt to register a null channel");
            }
            _logger.Debug("Add channel with channel id  " + channelId);
            _channelId2SessionMap[channelId] = channel;
        }

        public void RemoveSessionByChannel(ushort channelId)
        {            
            _logger.Debug("Removing session with channelId " + channelId);
            _channelId2SessionMap.Remove(channelId);
        }

        /// <summary>
        /// Starts the process of closing a channel
        /// </summary>
        /// <param name="channel" the AmqChannel being closed</param>         
        public void CloseSession(AmqChannel channel)
        {
            _logger.Debug("closeSession called on protocol channel for channel " + channel.ChannelId);
            ushort channelId = channel.ChannelId;
            
            // we need to know when a channel is closing so that we can respond
            // with a channel.close frame when we receive any other type of frame
            // on that channel
            _closingChannels[channelId] = channel;

        }

        /// <summary>
        /// Called from the ChannelClose handler when a channel close frame is received.
        /// This method decides whether this is a response or an initiation. The latter
        /// case causes the AmqChannel to be closed and an exception to be thrown if
        /// appropriate.
        /// </summary>
        /// <param name="channelId">the id of the channel (session)</param>
        /// <returns>true if the client must respond to the server, i.e. if the server
        /// initiated the channel close, false if the channel close is just the server
        /// responding to the client's earlier request to close the channel.</returns>         
        public bool ChannelClosed(ushort channelId, int code, string text)
        {            
            // if this is not a response to an earlier request to close the channel            
            if (!_closingChannels.ContainsKey(channelId))
            {
                _closingChannels.Remove(channelId);
                AmqChannel channel = (AmqChannel) _channelId2SessionMap[channelId];
                channel.ClosedWithException(new AMQException(_logger, code, text));
                return true;
            }
            else
            {
                return false;
            }
        }

        public AMQConnection AMQConnection
        {
            get
            {
                return _connection;
            }
        }

        public void CloseProtocolSession()
        {
            _logger.Debug("Closing protocol session");
            _connectionCloser.Close();
        }

        internal string GenerateQueueName()
        {
        	return "ntmp_" + System.Guid.NewGuid();
        }
    }
}
