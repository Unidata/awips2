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
using Apache.Qpid.Buffer;
using Apache.Qpid.Codec;
using Apache.Qpid.Codec.Support;
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Transport
{
    public class AmqpChannel : IProtocolChannel
    {
        // Warning: don't use this log for regular logging.
        static readonly ILog _protocolTraceLog = LogManager.GetLogger("TRACE.Qpid.Client.ProtocolChannel");
        
        IByteChannel _byteChannel;
        IProtocolEncoder _encoder;
        IProtocolDecoder _decoder;
        IProtocolDecoderOutput _decoderOutput;
        private object _syncLock;

        public AmqpChannel(IByteChannel byteChannel, IProtocolDecoderOutput decoderOutput)
        {
            _byteChannel = byteChannel;
            _decoderOutput = decoderOutput;
            _syncLock = new object();
            
            AMQProtocolProvider protocolProvider = new AMQProtocolProvider();
            IProtocolCodecFactory factory = protocolProvider.CodecFactory;
            _encoder = factory.Encoder;
            _decoder = factory.Decoder;
        }

        public void Read()
        {
            ByteBuffer buffer = _byteChannel.Read();
            Decode(buffer);
        }
        
        public IAsyncResult BeginRead(AsyncCallback callback, object state)
        {
           return _byteChannel.BeginRead(callback, state);
        }

        public void EndRead(IAsyncResult result)
        {
           ByteBuffer buffer = _byteChannel.EndRead(result);
           Decode(buffer);
        }

        public void Write(IDataBlock o)
        {
            // TODO: Refactor to decorator.
            if (_protocolTraceLog.IsDebugEnabled)
            {
                _protocolTraceLog.Debug(String.Format("WRITE {0}", o));
            }
            // we should be doing an async write, but apparently
            // the mentalis library doesn't queue async read/writes
            // correctly and throws random IOException's. Stay sync for a while
            //_byteChannel.BeginWrite(Encode(o), OnAsyncWriteDone, null);
            _byteChannel.Write(Encode(o));
        }

        // not used for now
        //private void OnAsyncWriteDone(IAsyncResult result)
        //{
        //   _byteChannel.EndWrite(result);
        //}

        private void Decode(ByteBuffer buffer)
        {
           // make sure we don't try to decode more than
           // one buffer at the same time
           lock ( _syncLock )
           {
              _decoder.Decode(buffer, _decoderOutput);
           }
        }

        private ByteBuffer Encode(object o)
        {
            SingleProtocolEncoderOutput output = new SingleProtocolEncoderOutput();
            _encoder.Encode(o, output);
            return output.buffer;
        }

    }
}


