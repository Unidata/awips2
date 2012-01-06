/*
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
*/
using System;
using System.IO;
using System.Threading;
using common.org.apache.qpid.transport.util;
using org.apache.qpid.transport.util;

namespace org.apache.qpid.transport.network.io
{
    public sealed class IoSender : IIoSender<MemoryStream>
    {
        private static readonly Logger log = Logger.Get(typeof (IoReceiver));
        private readonly Stream bufStream;
        private bool closed;
        private readonly Mutex mutClosed = new Mutex();
        private readonly CircularBuffer<byte[]> queue;
        private readonly Thread thread;
        private readonly int timeout;
        private readonly MemoryStream _tobeSent = new MemoryStream();
        public IoSender(IIoTransport transport, int queueSize, int timeout)
        {
            this.timeout = timeout;
            bufStream = transport.Stream;
            queue = new CircularBuffer<byte[]>(queueSize);
            thread = new Thread(Go);
            log.Debug("Creating IoSender thread");
            thread.Name = String.Format("IoSender - {0}", transport.Socket) ;
            thread.IsBackground = true;
            thread.Start();
        }

        public void Send(MemoryStream str)
        {
            int pos = (int) str.Position;
            str.Seek(0, SeekOrigin.Begin);
            Send(str, pos);
        }

        public void Send(MemoryStream str, int size)
        {
            mutClosed.WaitOne();
            if (closed)
            {
                throw new TransportException("sender is Closed");
            }
            mutClosed.ReleaseMutex();                    
            byte[] buf = new byte[size];
            str.Read(buf, 0, size);
            _tobeSent.Write(buf, 0, size);          
        }

        public void Flush()
        {
            int length = (int)_tobeSent.Position;
            byte[] buf = new byte[length];
            _tobeSent.Seek(0, SeekOrigin.Begin);
            _tobeSent.Read(buf, 0, length);           
            queue.Enqueue(buf);
           // bufStream.Write(buf, 0, length);
          //  _tobeSent = new MemoryStream();
           // _writer.Write(buf, 0, length);
          //  _writer.Flush();
            _tobeSent.Seek(0, SeekOrigin.Begin);
        }

        public void Close()
        {
            log.Debug("Closing Sender");
            mutClosed.WaitOne();
            if (!closed)
            {
                try
                {
                    closed = true;
                    queue.Close();
                    thread.Join(timeout);
                    if (thread.IsAlive)
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
            mutClosed.ReleaseMutex();
        }

        private void Go()
        {
            while (! closed)
            {
                //MemoryStream st = queue.Dequeue();
                byte[] st = queue.Dequeue();
                if (st != null)
                {
                    try
                    {                                       
                     //   int length = (int) st.Length;
                      //  byte[] buf = new byte[length];
                      //  st.Read(buf, 0, length);
                        bufStream.Write(st, 0, st.Length);                          
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine(e);
                    }
                }
            }
        }
    }
}
