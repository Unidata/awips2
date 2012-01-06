/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 */
package org.apache.mina.transport.socket.nio;

import edu.emory.mathcs.backport.java.util.concurrent.Executor;
import org.apache.mina.common.ConnectFuture;
import org.apache.mina.common.ExceptionMonitor;
import org.apache.mina.common.IoConnector;
import org.apache.mina.common.IoConnectorConfig;
import org.apache.mina.common.IoHandler;
import org.apache.mina.common.IoServiceConfig;
import org.apache.mina.common.support.BaseIoConnector;
import org.apache.mina.common.support.DefaultConnectFuture;
import org.apache.mina.util.NamePreservingRunnable;
import org.apache.mina.util.NewThreadExecutor;
import org.apache.mina.util.Queue;

import java.io.IOException;
import java.net.ConnectException;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.Set;

/**
 * {@link IoConnector} for socket transport (TCP/IP).
 *
 * @author The Apache Directory Project (mina-dev@directory.apache.org)
 * @version $Rev: 627427 $, $Date: 2008-02-13 14:39:10 +0000 (Wed, 13 Feb 2008) $
 */
public class ExistingSocketConnector extends BaseIoConnector
{
    /** @noinspection StaticNonFinalField */
    private static volatile int nextId = 0;

    private final Object lock = new Object();
    private final int id = nextId++;
    private final String threadName = "SocketConnector-" + id;
    private SocketConnectorConfig defaultConfig = new SocketConnectorConfig();
    private final Queue connectQueue = new Queue();
    private final SocketIoProcessor[] ioProcessors;
    private final int processorCount;
    private final Executor executor;

    /** @noinspection FieldAccessedSynchronizedAndUnsynchronized */
    private Selector selector;
    private Worker worker;
    private int processorDistributor = 0;
    private int workerTimeout = 60;  // 1 min.
    private Socket _openSocket = null;

    /** Create a connector with a single processing thread using a NewThreadExecutor */
    public ExistingSocketConnector()
    {
        this(1, new NewThreadExecutor());
    }

    /**
     * Create a connector with the desired number of processing threads
     *
     * @param processorCount Number of processing threads
     * @param executor       Executor to use for launching threads
     */
    public ExistingSocketConnector(int processorCount, Executor executor)
    {
        if (processorCount < 1)
        {
            throw new IllegalArgumentException("Must have at least one processor");
        }

        this.executor = executor;
        this.processorCount = processorCount;
        ioProcessors = new SocketIoProcessor[processorCount];

        for (int i = 0; i < processorCount; i++)
        {
            ioProcessors[i] = new SocketIoProcessor("SocketConnectorIoProcessor-" + id + "." + i, executor);
        }
    }

    /**
     * How many seconds to keep the connection thread alive between connection requests
     *
     * @return Number of seconds to keep connection thread alive
     */
    public int getWorkerTimeout()
    {
        return workerTimeout;
    }

    /**
     * Set how many seconds the connection worker thread should remain alive once idle before terminating itself.
     *
     * @param workerTimeout Number of seconds to keep thread alive. Must be >=0
     */
    public void setWorkerTimeout(int workerTimeout)
    {
        if (workerTimeout < 0)
        {
            throw new IllegalArgumentException("Must be >= 0");
        }
        this.workerTimeout = workerTimeout;
    }

    public ConnectFuture connect(SocketAddress address, IoHandler handler, IoServiceConfig config)
    {
        return connect(address, null, handler, config);
    }

    public ConnectFuture connect(SocketAddress address, SocketAddress localAddress,
                                 IoHandler handler, IoServiceConfig config)
    {
        /** Changes here from the Mina OpenSocketConnector.
         * Ignoreing all address as they are not needed */

        if (handler == null)
        {
            throw new NullPointerException("handler");
        }


        if (config == null)
        {
            config = getDefaultConfig();
        }

        if (_openSocket == null)
        {
            throw new IllegalArgumentException("Specifed Socket not active");
        }

        boolean success = false;

        try
        {
            DefaultConnectFuture future = new DefaultConnectFuture();
            newSession(_openSocket, handler, config, future);
            success = true;
            return future;
        }
        catch (IOException e)
        {
            return DefaultConnectFuture.newFailedFuture(e);
        }
        finally
        {
            if (!success && _openSocket != null)
            {
                try
                {
                    _openSocket.close();
                }
                catch (IOException e)
                {
                    ExceptionMonitor.getInstance().exceptionCaught(e);
                }
            }
        }
    }

    public IoServiceConfig getDefaultConfig()
    {
        return defaultConfig;
    }

    /**
     * Sets the config this connector will use by default.
     *
     * @param defaultConfig the default config.
     *
     * @throws NullPointerException if the specified value is <code>null</code>.
     */
    public void setDefaultConfig(SocketConnectorConfig defaultConfig)
    {
        if (defaultConfig == null)
        {
            throw new NullPointerException("defaultConfig");
        }
        this.defaultConfig = defaultConfig;
    }

    private synchronized void startupWorker() throws IOException
    {
        if (worker == null)
        {
            selector = Selector.open();
            worker = new Worker();
            executor.execute(new NamePreservingRunnable(worker));
        }
    }

    private void registerNew()
    {
        if (connectQueue.isEmpty())
        {
            return;
        }

        for (; ;)
        {
            ConnectionRequest req;
            synchronized (connectQueue)
            {
                req = (ConnectionRequest) connectQueue.pop();
            }

            if (req == null)
            {
                break;
            }

            SocketChannel ch = req.channel;
            try
            {
                ch.register(selector, SelectionKey.OP_CONNECT, req);
            }
            catch (IOException e)
            {
                req.setException(e);
            }
        }
    }

    private void processSessions(Set keys)
    {
        Iterator it = keys.iterator();

        while (it.hasNext())
        {
            SelectionKey key = (SelectionKey) it.next();

            if (!key.isConnectable())
            {
                continue;
            }

            SocketChannel ch = (SocketChannel) key.channel();
            ConnectionRequest entry = (ConnectionRequest) key.attachment();

            boolean success = false;
            try
            {
                ch.finishConnect();
                newSession(ch, entry.handler, entry.config, entry);
                success = true;
            }
            catch (Throwable e)
            {
                entry.setException(e);
            }
            finally
            {
                key.cancel();
                if (!success)
                {
                    try
                    {
                        ch.close();
                    }
                    catch (IOException e)
                    {
                        ExceptionMonitor.getInstance().exceptionCaught(e);
                    }
                }
            }
        }

        keys.clear();
    }

    private void processTimedOutSessions(Set keys)
    {
        long currentTime = System.currentTimeMillis();
        Iterator it = keys.iterator();

        while (it.hasNext())
        {
            SelectionKey key = (SelectionKey) it.next();

            if (!key.isValid())
            {
                continue;
            }

            ConnectionRequest entry = (ConnectionRequest) key.attachment();

            if (currentTime >= entry.deadline)
            {
                entry.setException(new ConnectException());
                try
                {
                    key.channel().close();
                }
                catch (IOException e)
                {
                    ExceptionMonitor.getInstance().exceptionCaught(e);
                }
                finally
                {
                    key.cancel();
                }
            }
        }
    }

    private void newSession(Socket socket, IoHandler handler, IoServiceConfig config, ConnectFuture connectFuture)
            throws IOException
    {
        SocketSessionImpl session = new SocketSessionImpl(this,
                                                          nextProcessor(),
                                                          getListeners(),
                                                          config,
                                                          socket.getChannel(),
                                                          handler,
                                                          socket.getRemoteSocketAddress());

        newSession(session, config, connectFuture);
    }

    private void newSession(SocketChannel ch, IoHandler handler, IoServiceConfig config, ConnectFuture connectFuture)
            throws IOException

    {
        SocketSessionImpl session = new SocketSessionImpl(this,
                                                          nextProcessor(),
                                                          getListeners(),
                                                          config,
                                                          ch,
                                                          handler,
                                                          ch.socket().getRemoteSocketAddress());

        newSession(session, config, connectFuture);
    }

    private void newSession(SocketSessionImpl session, IoServiceConfig config, ConnectFuture connectFuture)
            throws IOException
    {
        try
        {
            getFilterChainBuilder().buildFilterChain(session.getFilterChain());
            config.getFilterChainBuilder().buildFilterChain(session.getFilterChain());
            config.getThreadModel().buildFilterChain(session.getFilterChain());
        }
        catch (Throwable e)
        {
            throw (IOException) new IOException("Failed to create a session.").initCause(e);
        }
        session.getIoProcessor().addNew(session);
        connectFuture.setSession(session);
    }

    private SocketIoProcessor nextProcessor()
    {
        return ioProcessors[processorDistributor++ % processorCount];
    }

    public void setOpenSocket(Socket openSocket)
    {
        _openSocket = openSocket;
    }

    private class Worker implements Runnable
    {
        private long lastActive = System.currentTimeMillis();

        public void run()
        {
            Thread.currentThread().setName(ExistingSocketConnector.this.threadName);

            for (; ;)
            {
                try
                {
                    int nKeys = selector.select(1000);

                    registerNew();

                    if (nKeys > 0)
                    {
                        processSessions(selector.selectedKeys());
                    }

                    processTimedOutSessions(selector.keys());

                    if (selector.keys().isEmpty())
                    {
                        if (System.currentTimeMillis() - lastActive > workerTimeout * 1000L)
                        {
                            synchronized (lock)
                            {
                                if (selector.keys().isEmpty() &&
                                    connectQueue.isEmpty())
                                {
                                    worker = null;
                                    try
                                    {
                                        selector.close();
                                    }
                                    catch (IOException e)
                                    {
                                        ExceptionMonitor.getInstance().exceptionCaught(e);
                                    }
                                    finally
                                    {
                                        selector = null;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    else
                    {
                        lastActive = System.currentTimeMillis();
                    }
                }
                catch (IOException e)
                {
                    ExceptionMonitor.getInstance().exceptionCaught(e);

                    try
                    {
                        Thread.sleep(1000);
                    }
                    catch (InterruptedException e1)
                    {
                        ExceptionMonitor.getInstance().exceptionCaught(e1);
                    }
                }
            }
        }
    }

    private class ConnectionRequest extends DefaultConnectFuture
    {
        private final SocketChannel channel;
        private final long deadline;
        private final IoHandler handler;
        private final IoServiceConfig config;

        private ConnectionRequest(SocketChannel channel, IoHandler handler, IoServiceConfig config)
        {
            this.channel = channel;
            long timeout;
            if (config instanceof IoConnectorConfig)
            {
                timeout = ((IoConnectorConfig) config).getConnectTimeoutMillis();
            }
            else
            {
                timeout = ((IoConnectorConfig) getDefaultConfig()).getConnectTimeoutMillis();
            }
            this.deadline = System.currentTimeMillis() + timeout;
            this.handler = handler;
            this.config = config;
        }
    }
}
