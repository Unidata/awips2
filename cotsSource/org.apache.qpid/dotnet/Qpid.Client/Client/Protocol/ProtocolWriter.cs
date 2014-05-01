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
using Apache.Qpid.Client.Protocol.Listener;
using Apache.Qpid.Client.Transport;
using Apache.Qpid.Framing;

using log4net;

namespace Apache.Qpid.Client.Protocol
{
    /// <summary>
    /// A convenient interface to writing protocol frames.
    /// </summary>
    public class ProtocolWriter
    {

        private ILog _logger = LogManager.GetLogger(typeof(ProtocolWriter));

        IProtocolWriter _protocolWriter;
        IProtocolListener _protocolListener;
        
        public ProtocolWriter(IProtocolWriter protocolWriter, IProtocolListener protocolListener)
        {
            _protocolWriter = protocolWriter;
            _protocolListener = protocolListener;
        }

        public void WriteFrame(IDataBlock frame)
        {
            _protocolWriter.Write(frame);
        }

        /// <summary>
        /// Convenience method that writes a frame to the protocol session and waits for
        /// a particular response. Equivalent to calling getProtocolSession().write() then
        /// waiting for the response.
        /// </summary>
        /// <param name="frame">the frame</param>
        /// <param name="listener">the blocking listener. Note the calling thread will block.</param>         
        /// <param name="timeout">set the number of milliseconds to wait</param>
        private AMQMethodEvent SyncWrite(AMQFrame frame, BlockingMethodFrameListener listener, int timeout)
        {
            try
            {
                _protocolListener.AddFrameListener(listener);
                _protocolWriter.Write(frame);
                
                return listener.BlockForFrame(timeout);
            }
            finally
            {
                _protocolListener.RemoveFrameListener(listener);
            }
            // When control resumes before this line, a reply will have been received
            // that matches the criteria defined in the blocking listener
        }

        /// <summary>
        /// Convenience method that writes a frame to the protocol session and waits for
        /// a particular response. Equivalent to calling getProtocolSession().write() then
        /// waiting for the response.
        /// </summary>
        /// <param name="frame">the frame</param>
        /// <param name="responseType">the type of method response</param>
        public AMQMethodEvent SyncWrite(AMQFrame frame, Type responseType)
        {
            // TODO: If each frame knew it's response type, then the responseType argument would
            // TODO: not be neccesary.
            return SyncWrite(frame, responseType, DefaultTimeouts.MaxWaitForSyncWriter);
        }

        /// <summary>
        /// Convenience method that writes a frame to the protocol session and waits for
        /// a particular response. Equivalent to calling getProtocolSession().write() then
        /// waiting for the response.
        /// </summary>
        /// <param name="frame">the frame</param>
        /// <param name="responseType">the type of method response</param>
        /// <param name="timeout">set the number of milliseconds to wait</param>
        /// <returns>set the number of milliseconds to wait</returns>
        public AMQMethodEvent SyncWrite(AMQFrame frame, Type responseType, int timeout)
        {
            return SyncWrite(frame, new SpecificMethodFrameListener(frame.Channel, responseType), timeout);
        }
    }
}


