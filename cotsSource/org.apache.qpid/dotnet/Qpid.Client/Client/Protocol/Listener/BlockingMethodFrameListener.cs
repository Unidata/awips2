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
using Apache.Qpid.Framing;

namespace Apache.Qpid.Client.Protocol.Listener
{
    public abstract class BlockingMethodFrameListener : IAMQMethodListener
    {
        private ManualResetEvent _resetEvent;
        
        public abstract bool ProcessMethod(ushort channelId, AMQMethodBody frame);        

        /// <summary>
        /// This is set if there is an exception thrown from processCommandFrame and the
        /// exception is rethrown to the caller of blockForFrame()
        /// </summary>
        private volatile Exception _error;

        protected ushort _channelId;

        protected AMQMethodEvent _doneEvt = null;

        public BlockingMethodFrameListener(ushort channelId)
        {
            _channelId = channelId;
            _resetEvent = new ManualResetEvent(false);
        }

        /// <summary>
        /// This method is called by the MINA dispatching thread. Note that it could
        /// be called before BlockForFrame() has been called.
        /// </summary>
        /// <param name="evt">the frame event</param>
        /// <returns>true if the listener has dealt with this frame</returns>
        /// <exception cref="AMQException"></exception>
        public bool MethodReceived(AMQMethodEvent evt)
        {
            AMQMethodBody method = evt.Method;

            try
            {
                bool ready = (evt.ChannelId == _channelId) && ProcessMethod(evt.ChannelId, method);
                if (ready)
                {                    
                    _doneEvt = evt;                        
                    _resetEvent.Set();
                }

                return ready;
            }
            catch (AMQException e)
            {
                Error(e);
                // we rethrow the error here, and the code in the frame dispatcher will go round
                // each listener informing them that an exception has been thrown
                throw e;
            }
        }

        /// <summary>
        /// This method is called by the thread that wants to wait for a frame.
        /// </summary>
        /// <param name="timeout">Set the number of milliseconds to wait</param>
        public AMQMethodEvent BlockForFrame(int timeout)
        {
            _resetEvent.WaitOne(timeout, true);
            //at this point the event will have been signalled. The error field might or might not be set
            // depending on whether an error occurred
            if (_error != null)
            {
                throw _error;
            }

            return _doneEvt;
        }

        /// <summary>
        /// This is a callback, called by the MINA dispatcher thread only. It is also called from within this
        /// class to avoid code repetition but again is only called by the MINA dispatcher thread.
        /// </summary>
        /// <param name="e">the exception that caused the error</param>         
        public void Error(Exception e)
        {
            // set the error so that the thread that is blocking (in BlockForFrame())
            // can pick up the exception and rethrow to the caller
            _error = e;
            _resetEvent.Set();
        }
    }
}
