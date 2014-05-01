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
using System.Collections.Generic;
using Logger = org.apache.qpid.transport.util.Logger;

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// Connection
    /// </summary>
    public class Connection
    {
        private static readonly Logger log = Logger.Get(typeof (Connection));

        private readonly ISender<IProtocolEvent> _sender;
        private readonly ConnectionDelegate _connDdelegate;
        private  int _channelMax = 1;
        private int _connectionId;
        private readonly IReceiver<ReceivedPayload<IProtocolEvent>> _receiver;

        private readonly Dictionary<int, Channel> _channels = new Dictionary<int, Channel>();

        public Connection(IReceiver<ReceivedPayload<IProtocolEvent>> receiver, ISender<IProtocolEvent> sender, ConnectionDelegate connDdelegate)
        {
            _receiver = receiver;
            _sender = sender;
            _connDdelegate = connDdelegate;
        }

        public int ConnectionId
        {
            get { return _connectionId; }
            set { _connectionId = value; }
        }

        public ConnectionDelegate ConnectionDelegate
        {
            get { return _connDdelegate; }
        }

        public int ChannelMax
        {
            get { return _channelMax; }
            set { _channelMax = value; }
        }

        public void Send(IProtocolEvent pevent)
        {
            log.Debug("SEND: [{0}] {1}", this, pevent);
            _sender.Send(pevent);
        }

        public void Flush()
        {
            log.Debug("FLUSH: [{0}]", this);
            _sender.Flush();
        }


        public Channel GetChannel()
        {
            lock (_channels)
            {
                for (int i = 0; i < ChannelMax; i++)
                {
                    if (!_channels.ContainsKey(i))
                    {
                        return GetChannel(i);
                    }
                }
                throw new Exception("no more _channels available");
            }
        }

        public Channel GetChannel(int number)
        {
            lock (_channels)
            {
                Channel channel = null;
                if (_channels.Count > 0)
                {
                    if( _channels.ContainsKey(number))
                        channel = _channels[number];
                }
                if (channel == null)
                {
                    channel = new Channel(this, number, _connDdelegate.GetSessionDelegate());                   
                    _receiver.Received += channel.On_ReceivedEvent;                   
                    _channels.Add(number, channel);
                }
                return channel;
            }
        }

        public void RemoveChannel(int number)
        {
            lock (_channels)
            {
                _receiver.Received -= _channels[number].On_ReceivedEvent;
                _channels.Remove(number);                
            }
        }

        public void On_ReceivedEvent(object sender, ReceivedPayload<IProtocolEvent> payload)
        {
           log.Debug("RECV: [{0}] {1}", this, payload.Payload);
            if (_channels.ContainsKey(payload.Payload.Channel)) return;
            Channel channel = GetChannel(payload.Payload.Channel);
            channel.On_ReceivedEvent(sender, payload);
        }

        public void On_ReceivedException(Object sender, ExceptionArgs arg)
        {
            _connDdelegate.RaiseException(arg.Exception);
        }

        public void On_ReceivedClosed(Object sender, EventArgs arg)
        {
            log.Debug("Connection Closed: {0}", this);
            lock (_channels)
            {
                foreach (Channel ch in _channels.Values)
                {
                    ch.ClosedFromConnection();
                }
            }
            _channels.Clear();
            _connDdelegate.Closed();
        }


        public void CloseCode(ConnectionClose close)
        {
            lock (_channels)
            {
                foreach (Channel ch in _channels.Values)
                {
                    ch.CloseCode(close);
                }
            }
        }

        public void Close()
        {
            _sender.Close();
        }
    }

}
