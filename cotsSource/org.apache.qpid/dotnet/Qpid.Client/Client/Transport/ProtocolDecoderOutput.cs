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
using Apache.Qpid.Client.Protocol;
using Apache.Qpid.Codec;
using Apache.Qpid.Framing;
using log4net;

namespace Apache.Qpid.Client.Transport
{
   /// <summary>
   /// <see cref="IProtocolDecoderOutput"/> implementation that forwards
   /// each <see cref="IDataBlock"/> as it is decoded to the
   /// protocol listener
   /// </summary>
   internal class ProtocolDecoderOutput : IProtocolDecoderOutput
   {
      private IProtocolListener _protocolListener;
      static readonly ILog _protocolTraceLog = LogManager.GetLogger("TRACE.Qpid.Client.ProtocolChannel");

      public ProtocolDecoderOutput(IProtocolListener protocolListener)
      {
         if ( protocolListener == null )
            throw new ArgumentNullException("protocolListener");

         _protocolListener = protocolListener;
      }

      public void Write(object message)
      {
         IDataBlock block = message as IDataBlock;
         if ( block != null )
         {
            _protocolTraceLog.Debug(String.Format("READ {0}", block));
            _protocolListener.OnMessage(block);
         }
      }
   }
} // namespace Apache.Qpid.Client.Transport


