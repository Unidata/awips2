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
 *
 */
package org.apache.qpid.transport.network.ssl;

import java.nio.ByteBuffer;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

import org.apache.qpid.transport.Sender;
import org.apache.qpid.transport.SenderException;
import org.apache.qpid.transport.util.Logger;

public class SSLSender implements Sender<ByteBuffer>
{
    private Sender<ByteBuffer> delegate;
    private SSLEngine engine;
    private int sslBufSize;
    private ByteBuffer netData;
    private long timeout = 30000;

    private final Object engineState = new Object();
    private final AtomicBoolean closed = new AtomicBoolean(false);

    private static final Logger log = Logger.get(SSLSender.class);

    public SSLSender(SSLEngine engine, Sender<ByteBuffer> delegate)
    {
        this.engine = engine;
        this.delegate = delegate;
        sslBufSize = engine.getSession().getPacketBufferSize();
        netData = ByteBuffer.allocate(sslBufSize);
        timeout = Long.getLong("qpid.ssl_timeout", 60000);
    }

    public void close()
    {
        if (!closed.getAndSet(true))
        {
            if (engine.isOutboundDone())
            {
                return;
            }
            log.debug("Closing SSL connection");
            engine.closeOutbound();
            try
            {
                tearDownSSLConnection();
            }
            catch(Exception e)
            {
                throw new SenderException("Error closing SSL connection",e);
            }

            while (!engine.isOutboundDone())
            {
                synchronized(engineState)
                {
                    try
                    {
                        engineState.wait();
                    }
                    catch(InterruptedException e)
                    {
                        // pass
                    }
                }
            }
            delegate.close();
        }
    }

    private void tearDownSSLConnection() throws Exception
    {
        SSLEngineResult result = engine.wrap(ByteBuffer.allocate(0), netData);
        Status status = result.getStatus();
        int read   = result.bytesProduced();
        while (status != Status.CLOSED)
        {
            if (status == Status.BUFFER_OVERFLOW)
            {
                netData.clear();
            }
            if(read > 0)
            {
                int limit = netData.limit();
                netData.limit(netData.position());
                netData.position(netData.position() - read);

                ByteBuffer data = netData.slice();

                netData.limit(limit);
                netData.position(netData.position() + read);

                delegate.send(data);
                flush();
            }
            result = engine.wrap(ByteBuffer.allocate(0), netData);
            status = result.getStatus();
            read   = result.bytesProduced();
        }
    }

    public void flush()
    {
        delegate.flush();
    }

    public void send(ByteBuffer appData)
    {
        if (closed.get())
        {
            throw new SenderException("SSL Sender is closed");
        }

        HandshakeStatus handshakeStatus;
        Status status;

        while(appData.hasRemaining())
        {

            int read = 0;
            try
            {
                SSLEngineResult result = engine.wrap(appData, netData);
                read   = result.bytesProduced();
                status = result.getStatus();
                handshakeStatus = result.getHandshakeStatus();

            }
            catch(SSLException e)
            {
                throw new SenderException("SSL, Error occurred while encrypting data",e);
            }

            if(read > 0)
            {
                int limit = netData.limit();
                netData.limit(netData.position());
                netData.position(netData.position() - read);

                ByteBuffer data = netData.slice();

                netData.limit(limit);
                netData.position(netData.position() + read);

                delegate.send(data);
            }

            switch(status)
            {
                case CLOSED:
                    throw new SenderException("SSLEngine is closed");

                case BUFFER_OVERFLOW:
                    netData.clear();
                    continue;

                case OK:
                    break; // do nothing

                default:
                    throw new IllegalStateException("SSLReceiver: Invalid State " + status);
            }

            switch (handshakeStatus)
            {
                case NEED_WRAP:
                    if (netData.hasRemaining())
                    {
                        continue;
                    }

                case NEED_TASK:
                    doTasks();
                    break;

                case NEED_UNWRAP:
                    flush();
                    synchronized(engineState)
                    {
                        switch (engine.getHandshakeStatus())
                        {
                        case NEED_UNWRAP:
                            long start = System.currentTimeMillis();
                            try
                            {
                                engineState.wait(timeout);
                            }
                            catch(InterruptedException e)
                            {
                                // pass
                            }

                            if (System.currentTimeMillis()- start >= timeout)
                            {
                                throw new SenderException(
                                                          "SSL Engine timed out waiting for a response." +
                                                          "To get more info,run with -Djavax.net.debug=ssl");
                            }
                            break;
                        }
                    }
                    break;

                case FINISHED:
                case NOT_HANDSHAKING:
                    break; //do  nothing

                default:
                    throw new IllegalStateException("SSLReceiver: Invalid State " + status);
            }

        }
    }

    public void doTasks()
    {
        Runnable runnable;
        while ((runnable = engine.getDelegatedTask()) != null) {
            runnable.run();
        }
    }

    public Object getNotificationToken()
    {
        return engineState;
    }

    public void setIdleTimeout(long l)
    {
        delegate.setIdleTimeout(l);
    }
}
