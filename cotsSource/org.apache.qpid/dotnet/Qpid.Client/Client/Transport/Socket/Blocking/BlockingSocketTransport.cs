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
using System.IO;
using System.Threading;
using Apache.Qpid.Client.Qms;
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Codec;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Transport.Socket.Blocking
{
   /// <summary>
   /// TCP Socket transport supporting both
   /// SSL and non-SSL connections.
   /// </summary>
   public class BlockingSocketTransport : ITransport
   {
      // Configuration variables.
      IProtocolListener _protocolListener;

      // Runtime variables.
      private ISocketConnector _connector;
      private IoHandler _ioHandler;
      private AmqpChannel _amqpChannel;
      private ManualResetEvent _stopEvent;

      public IProtocolWriter ProtocolWriter
      {
         get { return _amqpChannel; }
      }
      public string LocalEndpoint
      {
         get { return _connector.LocalEndpoint; }
      }

      
      /// <summary>
      /// Connect to the specified broker
      /// </summary>
      /// <param name="broker">The broker to connect to</param>
      /// <param name="connection">The AMQ connection</param>
      public void Connect(IBrokerInfo broker, AMQConnection connection)
      {
         _stopEvent = new ManualResetEvent(false);
         _protocolListener = connection.ProtocolListener;

         _ioHandler = MakeBrokerConnection(broker, connection);
         // todo: get default read size from config!

         IProtocolDecoderOutput decoderOutput =
            new ProtocolDecoderOutput(_protocolListener);
         _amqpChannel = 
            new AmqpChannel(new ByteChannel(_ioHandler), decoderOutput);

         // post an initial async read
         _amqpChannel.BeginRead(new AsyncCallback(OnAsyncReadDone), this);
      }

      /// <summary>
      /// Close the broker connection
      /// </summary>
      public void Close()
      {
         StopReading();
         CloseBrokerConnection();
      }

      private void StopReading()
      {
         _stopEvent.Set();
      }

      private void CloseBrokerConnection()
      {
         if ( _ioHandler != null )
         {
            _ioHandler.Dispose();
            _ioHandler = null;
         }
         if ( _connector != null )
         {
            _connector.Dispose();
            _connector = null;
         }
      }

      private IoHandler MakeBrokerConnection(IBrokerInfo broker, AMQConnection connection)
      {
         if ( broker.UseSSL )
         {
            _connector = new SslSocketConnector();
         } else
         {
            _connector = new SocketConnector();
         }

         Stream stream = _connector.Connect(broker);
         return new IoHandler(stream, connection.ProtocolListener);
      }

      private void OnAsyncReadDone(IAsyncResult result)
      {
         try
         {
            _amqpChannel.EndRead(result);

            bool stopping = _stopEvent.WaitOne(0, false);
            if ( !stopping )
               _amqpChannel.BeginRead(new AsyncCallback(OnAsyncReadDone), null);
         } catch ( Exception e )
         {
            // ignore any errors during closing
            bool stopping = _stopEvent.WaitOne(0, false);
            if ( !stopping )
               _protocolListener.OnException(e);
         }
      }

      #region IProtocolDecoderOutput Members

      public void Write(object message)
      {
         _protocolListener.OnMessage((IDataBlock)message);
      }

      #endregion
   }
}


