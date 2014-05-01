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
using System.IO;
using System.Threading;
using Logger = org.apache.qpid.transport.util.Logger;


namespace org.apache.qpid.transport.network.io
{
    /// <summary> 
    /// IoReceiver
    /// </summary>
    public sealed class IoReceiver : IReceiver<ReceivedPayload<MemoryStream>>
    {
        private static readonly Logger log = Logger.Get(typeof(IoReceiver));      
        private readonly int m_bufferSize;
        private readonly Stream m_bufStream;
        private readonly int m_timeout;
        private readonly Thread m_thread;
        private bool m_closed;
        private readonly Object m_objectLock = new object();
        
        // the event raised when a buffer is read from the wire        
        event EventHandler<ReceivedPayload<MemoryStream>> ReceivedBuffer;
        event EventHandler<ExceptionArgs> ExceptionReading;
        event EventHandler ReceiverClosed;

        event EventHandler<ReceivedPayload<MemoryStream>> IReceiver<ReceivedPayload<MemoryStream>>.Received
        {
            add
            {
                lock (m_objectLock)
                {
                    ReceivedBuffer += value;                  
                }
            }
            remove
            {
                lock (m_objectLock)
                {
                    ReceivedBuffer -= value;
                }
            }
        }

        event EventHandler<ExceptionArgs> IReceiver<ReceivedPayload<MemoryStream>>.Exception
        {
            add
            {
                lock (m_objectLock)
                {
                    ExceptionReading += value;
                }
            }
            remove
            {
                lock (m_objectLock)
                {
                    ExceptionReading -= value;
                }
            }
        }

        event EventHandler IReceiver<ReceivedPayload<MemoryStream>>.Closed
        {
            add
            {
                lock (m_objectLock)
                {
                    ReceiverClosed += value;
                }
            }
            remove
            {
                lock (m_objectLock)
                {
                    ReceiverClosed -= value;
                }
            }
        }

        public IoReceiver(Stream stream, int bufferSize, int timeout)
        {     
            m_bufferSize = bufferSize;
            m_bufStream = stream; 
            m_timeout = timeout;
            m_thread = new Thread(Go);
            m_thread.Name = String.Format("IoReceiver - {0}", stream);
            m_thread.IsBackground = true;
            m_thread.Start();
        }

        public void Close()
        {
            Mutex mut = new Mutex();
            mut.WaitOne();
            if (!m_closed)
            {
                m_closed = true;               
                try
                {
                    log.Debug("Receiver closing");
                    m_bufStream.Close();
                    m_thread.Join(m_timeout);
                    if (m_thread.IsAlive)
                    {
                        throw new TransportException("join timed out");
                    }
                }
                catch (ThreadInterruptedException e)
                {
                    throw new TransportException(e);
                }
                catch (IOException e)
                {
                    throw new TransportException(e);
                }                                                    
            }
            mut.ReleaseMutex();
        }

        void Go()
        {
            // create a BufferedStream on top of the NetworkStream.
            int threshold = m_bufferSize/2;
            byte[] buffer = new byte[m_bufferSize];
            try
            {
                int read;
                int offset = 0;
                ReceivedPayload<MemoryStream> payload = new ReceivedPayload<MemoryStream>();
                while ((read = m_bufStream.Read(buffer, offset, m_bufferSize - offset)) > 0)
                {
                    MemoryStream memStream = new MemoryStream(buffer, offset, read);
                    if (ReceivedBuffer != null)
                    {
                        // call the event 
                        payload.Payload = memStream;
                        ReceivedBuffer(this, payload);
                    }
                    offset += read;
                    if (offset > threshold)
                    {
                        offset = 0;
                        buffer = new byte[m_bufferSize];
                    }
                }
                log.Debug("Receiver thread terminating");
            }
            catch (Exception t)
            {
                if (ExceptionReading != null)
                {
                    ExceptionReading(this, new ExceptionArgs(t));
                }
            }
            finally
            {
                if (ReceiverClosed != null)
                {
                    ReceiverClosed(this, new EventArgs());
                }
            }
        }
    }
}