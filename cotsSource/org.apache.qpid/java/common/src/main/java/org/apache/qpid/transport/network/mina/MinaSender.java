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
package org.apache.qpid.transport.network.mina;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.CloseFuture;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.WriteFuture;
import org.apache.qpid.transport.Sender;
import org.apache.qpid.transport.TransportException;


/**
 * MinaSender
 */

public class MinaSender implements Sender<java.nio.ByteBuffer>
{
    private static final int TIMEOUT = 2 * 60 * 1000;

    private final IoSession session;
    private WriteFuture lastWrite = null;

    public MinaSender(IoSession session)
    {
        this.session = session;
    }

    public void send(java.nio.ByteBuffer buf)
    {
        if (session.isClosing())
        {
            throw new TransportException("attempted to write to a closed socket");
        }

        synchronized (this)
        {
            lastWrite = session.write(ByteBuffer.wrap(buf));
        }
    }

    public void flush()
    {
        // pass
    }

    public synchronized void close()
    {
        // MINA will sometimes throw away in-progress writes when you
        // ask it to close
        synchronized (this)
        {
            if (lastWrite != null)
            {
                lastWrite.join();
            }
        }
        CloseFuture closed = session.close();
        closed.join();
    }
    
    public void setIdleTimeout(long l)
    {
      //noop
    }
    
    public long getIdleTimeout()
    {
        return 0;
    }
    
}
