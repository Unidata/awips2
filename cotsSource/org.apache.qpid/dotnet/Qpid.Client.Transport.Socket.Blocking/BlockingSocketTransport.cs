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
using System.Threading;
using log4net;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Framing;

namespace Qpid.Client.Transport.Socket.Blocking
{
    public class BlockingSocketTransport : ITransport
    {
//        static readonly ILog _log = LogManager.GetLogger(typeof(BlockingSocketTransport));

        // Configuration variables.
        string _host;
        int _port;
        IProtocolListener _protocolListener;

        // Runtime variables.
        private BlockingSocketProcessor _socketProcessor;
        private AmqpChannel _amqpChannel;
        
        private ReaderRunner _readerRunner;
        private Thread _readerThread;       

        public BlockingSocketTransport(string host, int port, AMQConnection connection)
        {
            _host = host;
            _port = port;
            _protocolListener = connection.ProtocolListener;
        }
        
        public void Open()
        {
            _socketProcessor = new BlockingSocketProcessor(_host, _port, _protocolListener);
            _socketProcessor.Connect();
            _amqpChannel = new AmqpChannel(_socketProcessor.ByteChannel);
            _readerRunner = new ReaderRunner(this);
            _readerThread = new Thread(new ThreadStart(_readerRunner.Run));  
            _readerThread.Start();
        }

        public string getLocalEndPoint()
        {
            return _socketProcessor.getLocalEndPoint();
        }

        public void Close()
        {
            StopReaderThread();
            _socketProcessor.Disconnect();
        }

        public IProtocolChannel ProtocolChannel { get { return _amqpChannel;  } }
        public IProtocolWriter ProtocolWriter { get { return _amqpChannel; } }

        public void StopReaderThread()
        {
            _readerRunner.Stop();
        }

        class ReaderRunner
        {
            BlockingSocketTransport _transport;
            bool _running = true;

            public ReaderRunner(BlockingSocketTransport transport)
            {
                _transport = transport;
            }

            public void Run()
            {
                try
                {
                    while (_running)
                    {
                        Queue frames = _transport.ProtocolChannel.Read();

                        foreach (IDataBlock dataBlock in frames)
                        {
                            _transport._protocolListener.OnMessage(dataBlock);
                        }
                    }
                }
                catch (Exception e)
                {
                    _transport._protocolListener.OnException(e);
                }
            }

            public void Stop()
            {
                // TODO: Check if this is thread safe. running is not volitile....
                _running = false;
            }
        }
    }
}


