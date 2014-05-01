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
package org.apache.qpid.transport.network.ssl;

import java.nio.ByteBuffer;

import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

import org.apache.qpid.transport.Receiver;
import org.apache.qpid.transport.TransportException;
import org.apache.qpid.transport.util.Logger;

public class SSLReceiver implements Receiver<ByteBuffer>
{
    private Receiver<ByteBuffer> delegate;
    private SSLEngine engine;
    private SSLSender sender;
    private int sslBufSize;
    private ByteBuffer appData;
    private ByteBuffer localBuffer;
    private boolean dataCached = false;
    private final Object notificationToken;

    private static final Logger log = Logger.get(SSLReceiver.class);

    public SSLReceiver(SSLEngine engine, Receiver<ByteBuffer> delegate,SSLSender sender)
    {
        this.engine = engine;
        this.delegate = delegate;
        this.sender = sender;
        this.sslBufSize = engine.getSession().getApplicationBufferSize();
        appData = ByteBuffer.allocate(sslBufSize);
        localBuffer = ByteBuffer.allocate(sslBufSize);
        notificationToken = sender.getNotificationToken();
    }

    public void closed()
    {
       delegate.closed();
    }

    public void exception(Throwable t)
    {
        delegate.exception(t);
    }

    private ByteBuffer addPreviouslyUnreadData(ByteBuffer buf)
    {
        if (dataCached)
        {
            ByteBuffer b = ByteBuffer.allocate(localBuffer.remaining() + buf.remaining());
            b.put(localBuffer);
            b.put(buf);
            b.flip();
            dataCached = false;
            return b;
        }
        else
        {
            return buf;
        }
    }

    public void received(ByteBuffer buf)
    {
        ByteBuffer netData = addPreviouslyUnreadData(buf);

        HandshakeStatus handshakeStatus;
        Status status;

        while (netData.hasRemaining())
        {
            try
            {
                SSLEngineResult result = engine.unwrap(netData, appData);
                synchronized (notificationToken)
                {
                    notificationToken.notifyAll();
                }

                int read = result.bytesProduced();
                status = result.getStatus();
                handshakeStatus = result.getHandshakeStatus();

                if (read > 0)
                {
                    int limit = appData.limit();
                    appData.limit(appData.position());
                    appData.position(appData.position() - read);

                    ByteBuffer data = appData.slice();

                    appData.limit(limit);
                    appData.position(appData.position() + read);

                    delegate.received(data);
                }


                switch(status)
                {
                    case CLOSED:
                        synchronized(notificationToken)
                        {
                            notificationToken.notifyAll();
                        }
                        return;

                    case BUFFER_OVERFLOW:
                        appData = ByteBuffer.allocate(sslBufSize);
                        continue;

                    case BUFFER_UNDERFLOW:
                        localBuffer.clear();
                        localBuffer.put(netData);
                        localBuffer.flip();
                        dataCached = true;
                        break;

                    case OK:
                        break; // do nothing

                    default:
                        throw new IllegalStateException("SSLReceiver: Invalid State " + status);
                }

                switch (handshakeStatus)
                {
                    case NEED_UNWRAP:
                        if (netData.hasRemaining())
                        {
                            continue;
                        }
                        break;

                    case NEED_TASK:
                        sender.doTasks();
                        handshakeStatus = engine.getHandshakeStatus();

                    case NEED_WRAP:
                    case FINISHED:
                    case NOT_HANDSHAKING:
                        synchronized(notificationToken)
                        {
                            notificationToken.notifyAll();
                        }
                        break;

                    default:
                        throw new IllegalStateException("SSLReceiver: Invalid State " + status);
                }


            }
            catch(SSLException e)
            {
                throw new TransportException("Error in SSLReceiver",e);
            }

        }
    }
}
