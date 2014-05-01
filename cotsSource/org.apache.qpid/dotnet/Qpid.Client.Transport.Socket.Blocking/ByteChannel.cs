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
using log4net;
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Client.Transport.Socket.Blocking
{
    class ByteChannel : IByteChannel
    {
        // Warning: don't use this log for regular logging.
        private static readonly ILog _ioTraceLog = LogManager.GetLogger("TRACE.Qpid.Client.ByteChannel");

        BlockingSocketProcessor processor;
        
        public ByteChannel(BlockingSocketProcessor processor)
        {
            this.processor = processor;
        }

        public ByteBuffer Read()
        {
            ByteBuffer result = processor.Read();

            // TODO: Move into decorator.
            if (_ioTraceLog.IsDebugEnabled)
            {
                _ioTraceLog.Debug(String.Format("READ {0}", result));
            }
            
            return result;
        }

        public void Write(ByteBuffer buffer)
        {
            // TODO: Move into decorator.
            if (_ioTraceLog.IsDebugEnabled)
            {
                _ioTraceLog.Debug(String.Format("WRITE {0}", buffer));
            }
            
            processor.Write(buffer);
        }
    }
}
