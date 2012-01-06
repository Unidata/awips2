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
import edu.emory.mathcs.backport.java.util.concurrent.locks.ReentrantLock;
import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.ExceptionMonitor;
import org.apache.mina.common.IdleStatus;
import org.apache.mina.common.IoFilter.WriteRequest;
import org.apache.mina.common.WriteTimeoutException;
import org.apache.mina.util.IdentityHashSet;
import org.apache.mina.util.NamePreservingRunnable;
import org.apache.mina.util.Queue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * Performs all I/O operations for sockets which is connected or bound. This class is used by MINA internally.
 *
 * @author The Apache Directory Project (mina-dev@directory.apache.org)
 * @version $Rev: 619823 $, $Date: 2008-02-08 10:09:37 +0000 (Fri, 08 Feb 2008) $,
 */
class MultiThreadSocketIoProcessor extends SocketIoProcessor
{
    Logger _logger = LoggerFactory.getLogger(MultiThreadSocketIoProcessor.class);
    Logger _loggerRead = LoggerFactory.getLogger(MultiThreadSocketIoProcessor.class + ".Reader");
    Logger _loggerWrite = LoggerFactory.getLogger(MultiThreadSocketIoProcessor.class + ".Writer");

    private static final long SELECTOR_TIMEOUT = 1000L;

    private int MAX_READ_BYTES_PER_SESSION = 524288; //512K
    private int MAX_FLUSH_BYTES_PER_SESSION = 524288; //512K

    private final Object readLock = new Object();
    private final Object writeLock = new Object();

    private final String threadName;
    private final Executor executor;

    private ReentrantLock trafficMaskUpdateLock = new ReentrantLock();

    /** @noinspection FieldAccessedSynchronizedAndUnsynchronized */
    private volatile Selector selector, writeSelector;

    private final Queue newSessions = new Queue();
    private final Queue removingSessions = new Queue();
    private final BlockingQueue flushingSessions = new LinkedBlockingQueue();
    private final IdentityHashSet flushingSessionsSet = new IdentityHashSet();

    private final Queue trafficControllingSessions = new Queue();

    private ReadWorker readWorker;
    private WriteWorker writeWorker;
    private long lastIdleReadCheckTime = System.currentTimeMillis();
    private long lastIdleWriteCheckTime = System.currentTimeMillis();

    MultiThreadSocketIoProcessor(String threadName, Executor executor)
    {
        super(threadName, executor);
        this.threadName = threadName;
        this.executor = executor;
    }

    void addNew(SocketSessionImpl session) throws IOException
    {
        synchronized (newSessions)
        {
            newSessions.push(session);
        }

        startupWorker();

        selector.wakeup();
        writeSelector.wakeup();
    }

    void remove(SocketSessionImpl session) throws IOException
    {
        scheduleRemove(session);
        startupWorker();
        selector.wakeup();
    }

    private void startupWorker() throws IOException
    {
        synchronized (readLock)
        {
            if (readWorker == null)
            {
                selector = Selector.open();
                readWorker = new ReadWorker();
                executor.execute(new NamePreservingRunnable(readWorker));
            }
        }

        synchronized (writeLock)
        {
            if (writeWorker == null)
            {
                writeSelector = Selector.open();
                writeWorker = new WriteWorker();
                executor.execute(new NamePreservingRunnable(writeWorker));
            }
        }

    }

    void flush(SocketSessionImpl session)
    {
        scheduleFlush(session);
        Selector selector = this.writeSelector;

        if (selector != null)
        {
            selector.wakeup();
        }
    }

    void updateTrafficMask(SocketSessionImpl session)
    {
        scheduleTrafficControl(session);
        Selector selector = this.selector;
        if (selector != null)
        {
            selector.wakeup();
        }
    }

    private void scheduleRemove(SocketSessionImpl session)
    {
        synchronized (removingSessions)
        {
            removingSessions.push(session);
        }
    }

    private void scheduleFlush(SocketSessionImpl session)
    {
        synchronized (flushingSessionsSet)
        {
            //if flushingSessions grows to contain Integer.MAX_VALUE sessions
            // then this will fail.
            if (flushingSessionsSet.add(session))
            {
                flushingSessions.offer(session);
            }
        }
    }

    private void scheduleTrafficControl(SocketSessionImpl session)
    {
        synchronized (trafficControllingSessions)
        {
            trafficControllingSessions.push(session);
        }
    }

    private void doAddNewReader() throws InterruptedException
    {
        if (newSessions.isEmpty())
        {
            return;
        }

        for (; ;)
        {
            MultiThreadSocketSessionImpl session;

            synchronized (newSessions)
            {
                session = (MultiThreadSocketSessionImpl) newSessions.peek();
            }

            if (session == null)
            {
                break;
            }

            SocketChannel ch = session.getChannel();


            try
            {

                ch.configureBlocking(false);
                session.setSelectionKey(ch.register(selector,
                                                    SelectionKey.OP_READ,
                                                    session));

                //System.out.println("ReadDebug:"+"Awaiting Registration");
                session.awaitRegistration();
                sessionCreated(session);
            }
            catch (IOException e)
            {
                // Clear the AbstractIoFilterChain.CONNECT_FUTURE attribute
                // and call ConnectFuture.setException().
                session.getFilterChain().fireExceptionCaught(session, e);
            }
        }
    }


    private void doAddNewWrite() throws InterruptedException
    {
        if (newSessions.isEmpty())
        {
            return;
        }

        for (; ;)
        {
            MultiThreadSocketSessionImpl session;

            synchronized (newSessions)
            {
                session = (MultiThreadSocketSessionImpl) newSessions.peek();
            }

            if (session == null)
            {
                break;
            }

            SocketChannel ch = session.getChannel();

            try
            {
                ch.configureBlocking(false);
                synchronized (flushingSessionsSet)
                {
                    flushingSessionsSet.add(session);
                }

                session.setWriteSelectionKey(ch.register(writeSelector,
                                                         SelectionKey.OP_WRITE,
                                                         session));

                //System.out.println("WriteDebug:"+"Awaiting Registration");
                session.awaitRegistration();
                sessionCreated(session);
            }
            catch (IOException e)
            {

                // Clear the AbstractIoFilterChain.CONNECT_FUTURE attribute
                // and call ConnectFuture.setException().
                session.getFilterChain().fireExceptionCaught(session, e);
            }
        }
    }


    private void sessionCreated(SocketSessionImpl sessionParam) throws InterruptedException
    {
        MultiThreadSocketSessionImpl session = (MultiThreadSocketSessionImpl) sessionParam;
        synchronized (newSessions)
        {
            if (!session.created())
            {
                _logger.debug("Popping new session");
                newSessions.pop();

                // AbstractIoFilterChain.CONNECT_FUTURE is cleared inside here
                // in AbstractIoFilterChain.fireSessionOpened().
                session.getServiceListeners().fireSessionCreated(session);

                session.doneCreation();
            }
        }
    }

    private void doRemove()
    {
        if (removingSessions.isEmpty())
        {
            return;
        }

        for (; ;)
        {
            MultiThreadSocketSessionImpl session;

            synchronized (removingSessions)
            {
                session = (MultiThreadSocketSessionImpl) removingSessions.pop();
            }

            if (session == null)
            {
                break;
            }

            SocketChannel ch = session.getChannel();
            SelectionKey key = session.getReadSelectionKey();
            SelectionKey writeKey = session.getWriteSelectionKey();

            // Retry later if session is not yet fully initialized.
            // (In case that Session.close() is called before addSession() is processed)
            if (key == null || writeKey == null)
            {
                scheduleRemove(session);
                break;
            }
            // skip if channel is already closed
            if (!key.isValid() || !writeKey.isValid())
            {
                continue;
            }

            try
            {
                //System.out.println("ReadDebug:"+"Removing Session: " + System.identityHashCode(session));
                synchronized (readLock)
                {
                    key.cancel();
                }
                synchronized (writeLock)
                {
                    writeKey.cancel();
                }
                ch.close();
            }
            catch (IOException e)
            {
                session.getFilterChain().fireExceptionCaught(session, e);
            }
            finally
            {
                releaseWriteBuffers(session);
                session.getServiceListeners().fireSessionDestroyed(session);
            }
        }
    }

    private void processRead(Set selectedKeys)
    {
        Iterator it = selectedKeys.iterator();

        while (it.hasNext())
        {
            SelectionKey key = (SelectionKey) it.next();
            MultiThreadSocketSessionImpl session = (MultiThreadSocketSessionImpl) key.attachment();

            synchronized (readLock)
            {
                if (key.isValid() && key.isReadable() && session.getTrafficMask().isReadable())
                {
                    read(session);
                }
            }

        }

        selectedKeys.clear();
    }

    private void processWrite(Set selectedKeys)
    {
        Iterator it = selectedKeys.iterator();

        while (it.hasNext())
        {
            SelectionKey key = (SelectionKey) it.next();
            SocketSessionImpl session = (SocketSessionImpl) key.attachment();

            synchronized (writeLock)
            {
                if (key.isValid() && key.isWritable() && session.getTrafficMask().isWritable())
                {

                    // Clear OP_WRITE
                    key.interestOps(key.interestOps() & (~SelectionKey.OP_WRITE));

                    synchronized (flushingSessionsSet)
                    {
                        flushingSessions.offer(session);
                    }
                }
            }
        }

        selectedKeys.clear();
    }

    private void read(SocketSessionImpl session)
    {

        //if (_loggerWrite.isDebugEnabled())
        {
            //System.out.println("WriteDebug:"+"Starting read for Session:" + System.identityHashCode(session));
        }

        int totalReadBytes = 0;

        while (totalReadBytes <= MAX_READ_BYTES_PER_SESSION)
        {
            ByteBuffer buf = ByteBuffer.allocate(session.getReadBufferSize());
            SocketChannel ch = session.getChannel();

            try
            {
                buf.clear();

                int readBytes = 0;
                int ret;

                try
                {
                    while ((ret = ch.read(buf.buf())) > 0)
                    {
                        readBytes += ret;
                        totalReadBytes += ret;
                    }
                }
                finally
                {
                    buf.flip();
                }


                if (readBytes > 0)
                {
                    session.increaseReadBytes(readBytes);

                    session.getFilterChain().fireMessageReceived(session, buf);
                    buf = null;
                }

                if (ret <= 0)
                {
                    if (ret == 0)
                    {
                        if (readBytes == session.getReadBufferSize())
                        {
                            continue;
                        }
                    }
                    else
                    {
                        scheduleRemove(session);
                    }

                    break;
                }
            }
            catch (Throwable e)
            {
                if (e instanceof IOException)
                {
                    scheduleRemove(session);
                }
                session.getFilterChain().fireExceptionCaught(session, e);

                //Stop Reading this session.
                return;
            }
            finally
            {
                if (buf != null)
                {
                    buf.release();
                }
            }
        }//for

        // if (_loggerWrite.isDebugEnabled())
        {
            //System.out.println("WriteDebug:"+"Read for Session:" + System.identityHashCode(session) + " got: " + totalReadBytes);
        }
    }


    private void notifyReadIdleness()
    {
        // process idle sessions
        long currentTime = System.currentTimeMillis();
        if ((currentTime - lastIdleReadCheckTime) >= 1000)
        {
            lastIdleReadCheckTime = currentTime;
            Set keys = selector.keys();
            if (keys != null)
            {
                for (Iterator it = keys.iterator(); it.hasNext();)
                {
                    SelectionKey key = (SelectionKey) it.next();
                    SocketSessionImpl session = (SocketSessionImpl) key.attachment();
                    notifyReadIdleness(session, currentTime);
                }
            }
        }
    }

    private void notifyWriteIdleness()
    {
        // process idle sessions
        long currentTime = System.currentTimeMillis();
        if ((currentTime - lastIdleWriteCheckTime) >= 1000)
        {
            lastIdleWriteCheckTime = currentTime;
            Set keys = writeSelector.keys();
            if (keys != null)
            {
                for (Iterator it = keys.iterator(); it.hasNext();)
                {
                    SelectionKey key = (SelectionKey) it.next();
                    SocketSessionImpl session = (SocketSessionImpl) key.attachment();
                    notifyWriteIdleness(session, currentTime);
                }
            }
        }
    }

    private void notifyReadIdleness(SocketSessionImpl session, long currentTime)
    {
        notifyIdleness0(
                session, currentTime,
                session.getIdleTimeInMillis(IdleStatus.BOTH_IDLE),
                IdleStatus.BOTH_IDLE,
                Math.max(session.getLastIoTime(), session.getLastIdleTime(IdleStatus.BOTH_IDLE)));
        notifyIdleness0(
                session, currentTime,
                session.getIdleTimeInMillis(IdleStatus.READER_IDLE),
                IdleStatus.READER_IDLE,
                Math.max(session.getLastReadTime(), session.getLastIdleTime(IdleStatus.READER_IDLE)));

        notifyWriteTimeout(session, currentTime, session
                .getWriteTimeoutInMillis(), session.getLastWriteTime());
    }

    private void notifyWriteIdleness(SocketSessionImpl session, long currentTime)
    {
        notifyIdleness0(
                session, currentTime,
                session.getIdleTimeInMillis(IdleStatus.BOTH_IDLE),
                IdleStatus.BOTH_IDLE,
                Math.max(session.getLastIoTime(), session.getLastIdleTime(IdleStatus.BOTH_IDLE)));
        notifyIdleness0(
                session, currentTime,
                session.getIdleTimeInMillis(IdleStatus.WRITER_IDLE),
                IdleStatus.WRITER_IDLE,
                Math.max(session.getLastWriteTime(), session.getLastIdleTime(IdleStatus.WRITER_IDLE)));

        notifyWriteTimeout(session, currentTime, session
                .getWriteTimeoutInMillis(), session.getLastWriteTime());
    }

    private void notifyIdleness0(SocketSessionImpl session, long currentTime,
                                 long idleTime, IdleStatus status,
                                 long lastIoTime)
    {
        if (idleTime > 0 && lastIoTime != 0
            && (currentTime - lastIoTime) >= idleTime)
        {
            session.increaseIdleCount(status);
            session.getFilterChain().fireSessionIdle(session, status);
        }
    }

    private void notifyWriteTimeout(SocketSessionImpl session,
                                    long currentTime,
                                    long writeTimeout, long lastIoTime)
    {

        MultiThreadSocketSessionImpl sesh = (MultiThreadSocketSessionImpl) session;
        SelectionKey key = sesh.getWriteSelectionKey();

        synchronized (writeLock)
        {
            if (writeTimeout > 0
                && (currentTime - lastIoTime) >= writeTimeout
                && key != null && key.isValid()
                && (key.interestOps() & SelectionKey.OP_WRITE) != 0)
            {
                session.getFilterChain().fireExceptionCaught(session, new WriteTimeoutException());
            }
        }
    }

    private SocketSessionImpl getNextFlushingSession()
    {
        return (SocketSessionImpl) flushingSessions.poll();
    }

    private void releaseSession(SocketSessionImpl session)
    {
        synchronized (session.getWriteRequestQueue())
        {
            synchronized (flushingSessionsSet)
            {
                if (session.getScheduledWriteRequests() > 0)
                {
                    if (_loggerWrite.isDebugEnabled())
                    {
                        //System.out.println("WriteDebug:"+"Reflush" + System.identityHashCode(session));
                    }
                    flushingSessions.offer(session);
                }
                else
                {
                    if (_loggerWrite.isDebugEnabled())
                    {
                        //System.out.println("WriteDebug:"+"Releasing session " + System.identityHashCode(session));
                    }
                    flushingSessionsSet.remove(session);
                }
            }
        }
    }

    private void releaseWriteBuffers(SocketSessionImpl session)
    {
        Queue writeRequestQueue = session.getWriteRequestQueue();
        WriteRequest req;

        //Should this be synchronized?
        synchronized (writeRequestQueue)
        {
            while ((req = (WriteRequest) writeRequestQueue.pop()) != null)
            {
                try
                {
                    ((ByteBuffer) req.getMessage()).release();
                }
                catch (IllegalStateException e)
                {
                    session.getFilterChain().fireExceptionCaught(session, e);
                }
                finally
                {
                    req.getFuture().setWritten(false);
                }
            }
        }
    }

    private void doFlush()
    {
        MultiThreadSocketSessionImpl session;

        while ((session = (MultiThreadSocketSessionImpl) getNextFlushingSession()) != null)
        {
            if (!session.isConnected())
            {
                releaseWriteBuffers(session);
                releaseSession(session);
                continue;
            }

            SelectionKey key = session.getWriteSelectionKey();
            // Retry later if session is not yet fully initialized.
            // (In case that Session.write() is called before addSession() is processed)
            if (key == null)
            {
                scheduleFlush(session);
                releaseSession(session);
                continue;
            }
            // skip if channel is already closed
            if (!key.isValid())
            {
                releaseSession(session);
                continue;
            }

            try
            {
                if (doFlush(session))
                {
                    releaseSession(session);
                }
            }
            catch (IOException e)
            {
                releaseSession(session);
                scheduleRemove(session);
                session.getFilterChain().fireExceptionCaught(session, e);
            }

        }

    }

    private boolean doFlush(SocketSessionImpl sessionParam) throws IOException
    {
        MultiThreadSocketSessionImpl session = (MultiThreadSocketSessionImpl) sessionParam;
        // Clear OP_WRITE
        SelectionKey key = session.getWriteSelectionKey();
        synchronized (writeLock)
        {
            key.interestOps(key.interestOps() & (~SelectionKey.OP_WRITE));
        }
        SocketChannel ch = session.getChannel();
        Queue writeRequestQueue = session.getWriteRequestQueue();

        long totalFlushedBytes = 0;
        while (true)
        {
            WriteRequest req;

            synchronized (writeRequestQueue)
            {
                req = (WriteRequest) writeRequestQueue.first();
            }

            if (req == null)
            {
                break;
            }

            ByteBuffer buf = (ByteBuffer) req.getMessage();
            if (buf.remaining() == 0)
            {
                synchronized (writeRequestQueue)
                {
                    writeRequestQueue.pop();
                }

                session.increaseWrittenMessages();

                buf.reset();
                session.getFilterChain().fireMessageSent(session, req);
                continue;
            }


            int writtenBytes = 0;

            // Reported as DIRMINA-362
            //note: todo: fixme: Not sure it is important but if we see NoyYetConnected exceptions or 100% CPU in the kernel then this is it.
            if (key.isWritable())
            {
                writtenBytes = ch.write(buf.buf());
                totalFlushedBytes += writtenBytes;
            }

            if (writtenBytes > 0)
            {
                session.increaseWrittenBytes(writtenBytes);
            }

            if (buf.hasRemaining() || (totalFlushedBytes <= MAX_FLUSH_BYTES_PER_SESSION))
            {
                // Kernel buffer is full
                synchronized (writeLock)
                {
                    key.interestOps(key.interestOps() | SelectionKey.OP_WRITE);
                }
                if (_loggerWrite.isDebugEnabled())
                {
                    //System.out.println("WriteDebug:"+"Written BF: " + (session.getWrittenBytes() - totalFlushedBytes) + " bytes");
                }
                return false;
            }
        }

        if (_loggerWrite.isDebugEnabled())
        {
            //System.out.println("WriteDebug:"+"Written : " + (session.getWrittenBytes() - totalFlushedBytes) + " bytes");
        }
        return true;
    }

    private void doUpdateTrafficMask()
    {
        if (trafficControllingSessions.isEmpty() || trafficMaskUpdateLock.isLocked())
        {
            return;
        }

        // Synchronize over entire operation as this method should be called
        // from both read and write thread and we don't want the order of the
        //  updates to get changed.
        trafficMaskUpdateLock.lock();
        try
        {
            for (; ;)
            {
                MultiThreadSocketSessionImpl session;

                session = (MultiThreadSocketSessionImpl) trafficControllingSessions.pop();

                if (session == null)
                {
                    break;
                }

                SelectionKey key = session.getReadSelectionKey();
                // Retry later if session is not yet fully initialized.
                // (In case that Session.suspend??() or session.resume??() is
                // called before addSession() is processed)
                if (key == null)
                {
                    scheduleTrafficControl(session);
                    break;
                }
                // skip if channel is already closed
                if (!key.isValid())
                {
                    continue;
                }

                // The normal is OP_READ and, if there are write requests in the
                // session's write queue, set OP_WRITE to trigger flushing.

                //Sset to Read and Write if there is nothing then the cost
                // is one loop through the flusher.
                int ops = SelectionKey.OP_READ;

                // Now mask the preferred ops with the mask of the current session
                int mask = session.getTrafficMask().getInterestOps();
                synchronized (readLock)
                {
                    key.interestOps(ops & mask);
                }
                //Change key to the WriteSelection Key
                key = session.getWriteSelectionKey();
                if (key != null && key.isValid())
                {
                    Queue writeRequestQueue = session.getWriteRequestQueue();
                    synchronized (writeRequestQueue)
                    {
                        if (!writeRequestQueue.isEmpty())
                        {
                            ops = SelectionKey.OP_WRITE;
                            synchronized (writeLock)
                            {
                                key.interestOps(ops & mask);
                            }
                        }
                    }
                }
            }
        }
        finally
        {
            trafficMaskUpdateLock.unlock();
        }

    }

    private class WriteWorker implements Runnable
    {

        public void run()
        {
            Thread.currentThread().setName(MultiThreadSocketIoProcessor.this.threadName + "Writer");

            //System.out.println("WriteDebug:"+"Startup");
            for (; ;)
            {
                try
                {
                    int nKeys = writeSelector.select(SELECTOR_TIMEOUT);

                    doAddNewWrite();
                    doUpdateTrafficMask();

                    if (nKeys > 0)
                    {
                        //System.out.println("WriteDebug:"+nKeys + " keys from writeselector");
                        processWrite(writeSelector.selectedKeys());
                    }
                    else
                    {
                        //System.out.println("WriteDebug:"+"No keys from writeselector");
                    }

                    doRemove();
                    notifyWriteIdleness();

                    if (flushingSessionsSet.size() > 0)
                    {
                        doFlush();
                    }

                    if (writeSelector.keys().isEmpty())
                    {
                        synchronized (writeLock)
                        {

                            if (writeSelector.keys().isEmpty() && newSessions.isEmpty())
                            {
                                writeWorker = null;
                                try
                                {
                                    writeSelector.close();
                                }
                                catch (IOException e)
                                {
                                    ExceptionMonitor.getInstance().exceptionCaught(e);
                                }
                                finally
                                {
                                    writeSelector = null;
                                }

                                break;
                            }
                        }
                    }

                }
                catch (Throwable t)
                {
                    ExceptionMonitor.getInstance().exceptionCaught(t);

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
            //System.out.println("WriteDebug:"+"Shutdown");
        }

    }

    private class ReadWorker implements Runnable
    {

        public void run()
        {
            Thread.currentThread().setName(MultiThreadSocketIoProcessor.this.threadName + "Reader");

            //System.out.println("ReadDebug:"+"Startup");
            for (; ;)
            {
                try
                {
                    int nKeys = selector.select(SELECTOR_TIMEOUT);

                    doAddNewReader();
                    doUpdateTrafficMask();

                    if (nKeys > 0)
                    {
                        //System.out.println("ReadDebug:"+nKeys + " keys from selector");

                        processRead(selector.selectedKeys());
                    }
                    else
                    {
                        //System.out.println("ReadDebug:"+"No keys from selector");
                    }


                    doRemove();
                    notifyReadIdleness();

                    if (selector.keys().isEmpty())
                    {

                        synchronized (readLock)
                        {
                            if (selector.keys().isEmpty() && newSessions.isEmpty())
                            {
                                readWorker = null;
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
                catch (Throwable t)
                {
                    ExceptionMonitor.getInstance().exceptionCaught(t);

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
            //System.out.println("ReadDebug:"+"Shutdown");
        }

    }
}
