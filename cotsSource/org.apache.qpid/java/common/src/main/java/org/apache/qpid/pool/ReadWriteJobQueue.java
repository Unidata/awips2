package org.apache.qpid.pool;

import java.util.AbstractQueue;
import java.util.Iterator;
import java.util.Collection;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.atomic.AtomicInteger;

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
public class ReadWriteJobQueue extends AbstractQueue<Runnable> implements BlockingQueue<Runnable>
{

    private final AtomicInteger _count = new AtomicInteger(0);

    private final ReentrantLock _takeLock = new ReentrantLock();

    private final Condition _notEmpty = _takeLock.newCondition();

    private final ReentrantLock _putLock = new ReentrantLock();

    private final ConcurrentLinkedQueue<ReadWriteRunnable> _readJobQueue = new ConcurrentLinkedQueue<ReadWriteRunnable>();

    private final ConcurrentLinkedQueue<ReadWriteRunnable> _writeJobQueue = new ConcurrentLinkedQueue<ReadWriteRunnable>();


    private class ReadWriteJobIterator implements Iterator<Runnable>
    {

        private boolean _onReads;
        private Iterator<ReadWriteRunnable> _iter = _writeJobQueue.iterator();

        public boolean hasNext()
        {
            if(!_iter.hasNext())
            {
                if(_onReads)
                {
                    _iter = _readJobQueue.iterator();
                    _onReads = true;
                    return _iter.hasNext();
                }
                else
                {
                    return false;
                }
            }
            else
            {
                return true;
            }
        }

        public Runnable next()
        {
            if(_iter.hasNext())
            {
                return _iter.next();
            }
            else
            {
                return null;
            }
        }

        public void remove()
        {
            _takeLock.lock();
            try
            {
                _iter.remove();
                _count.decrementAndGet();
            }
            finally
            {
                _takeLock.unlock();
            }
        }
    }

    public Iterator<Runnable> iterator()
    {
        return new ReadWriteJobIterator();
    }

    public int size()
    {
        return _count.get();
    }

    public boolean offer(final Runnable runnable)
    {
        final ReadWriteRunnable job = (ReadWriteRunnable) runnable;
        final ReentrantLock putLock = _putLock;
        putLock.lock();
        try
        {
            if(job.isRead())
            {
                _readJobQueue.offer(job);
            }
            else
            {
                _writeJobQueue.offer(job);
            }
            if(_count.getAndIncrement() == 0)
            {
                _takeLock.lock();
                try
                {
                    _notEmpty.signal();
                }
                finally
                {
                    _takeLock.unlock();
                }
            }
            return true;
        }
        finally
        {
            putLock.unlock();
        }
    }

    public void put(final Runnable runnable) throws InterruptedException
    {
        final ReadWriteRunnable job = (ReadWriteRunnable) runnable;
        final ReentrantLock putLock = _putLock;
        putLock.lock();

        try
        {
            if(job.isRead())
            {
                _readJobQueue.offer(job);
            }
            else
            {
                _writeJobQueue.offer(job);
            }
            if(_count.getAndIncrement() == 0)
            {
                                _takeLock.lock();
                try
                {
                    _notEmpty.signal();
                }
                finally
                {
                    _takeLock.unlock();
                }
            }

        }
        finally
        {
            putLock.unlock();
        }
    }



    public boolean offer(final Runnable runnable, final long timeout, final TimeUnit unit) throws InterruptedException
    {
        final ReadWriteRunnable job = (ReadWriteRunnable) runnable;
        final ReentrantLock putLock = _putLock;
        putLock.lock();

        try
        {
            if(job.isRead())
            {
                _readJobQueue.offer(job);
            }
            else
            {
                _writeJobQueue.offer(job);
            }
            if(_count.getAndIncrement() == 0)
            {
                _takeLock.lock();
                try
                {
                    _notEmpty.signal();
                }
                finally
                {
                    _takeLock.unlock();
                }
            }

            return true;
        }
        finally
        {
            putLock.unlock();
        }

    }

    public Runnable take() throws InterruptedException
    {
        final ReentrantLock takeLock = _takeLock;
        takeLock.lockInterruptibly();
        try
        {
            try
            {
                while (_count.get() == 0)
                {
                    _notEmpty.await();
                }
            }
            catch (InterruptedException ie)
            {
                _notEmpty.signal();
                throw ie;
            }

            ReadWriteRunnable job = _writeJobQueue.poll();
            if(job == null)
            {
                job = _readJobQueue.poll();
            }
            int c = _count.getAndDecrement();
            if (c > 1)
            {
                _notEmpty.signal();
            }
            return job;
        }
        finally
        {
            takeLock.unlock();
        }


    }

    public Runnable poll(final long timeout, final TimeUnit unit) throws InterruptedException
    {
        final ReentrantLock takeLock = _takeLock;
        final AtomicInteger count = _count;
        long nanos = unit.toNanos(timeout);
        takeLock.lockInterruptibly();
        ReadWriteRunnable job = null;
        try
        {

            for (;;)
            {
                if (count.get() > 0)
                {
                    job = _writeJobQueue.poll();
                    if(job == null)
                    {
                        job = _readJobQueue.poll();
                    }
                    int c = count.getAndDecrement();
                    if (c > 1)
                    {
                        _notEmpty.signal();
                    }
                    break;
                }
                if (nanos <= 0)
                {
                    return null;
                }
                try
                {
                    nanos = _notEmpty.awaitNanos(nanos);
                }
                catch (InterruptedException ie)
                {
                    _notEmpty.signal();
                    throw ie;
                }
            }
        }
        finally
        {
            takeLock.unlock();
        }

        return job;
    }

    public int remainingCapacity()
    {
        return Integer.MAX_VALUE;
    }

    public int drainTo(final Collection<? super Runnable> c)
    {
        int total = 0;

        _putLock.lock();
        _takeLock.lock();
        try
        {
            ReadWriteRunnable job;
            while((job = _writeJobQueue.peek())!= null)
            {
                c.add(job);
                _writeJobQueue.poll();
                _count.decrementAndGet();
                total++;
            }

            while((job = _readJobQueue.peek())!= null)
            {
                c.add(job);
                _readJobQueue.poll();
                _count.decrementAndGet();
                total++;
            }

        }
        finally
        {
            _takeLock.unlock();
            _putLock.unlock();
        }
        return total;
    }

    public int drainTo(final Collection<? super Runnable> c, final int maxElements)
    {
        int total = 0;

        _putLock.lock();
        _takeLock.lock();
        try
        {
            ReadWriteRunnable job;
            while(total<=maxElements && (job = _writeJobQueue.peek())!= null)
            {
                c.add(job);
                _writeJobQueue.poll();
                _count.decrementAndGet();
                total++;
            }

            while(total<=maxElements && (job = _readJobQueue.peek())!= null)
            {
                c.add(job);
                _readJobQueue.poll();
                _count.decrementAndGet();
                total++;
            }

        }
        finally
        {
            _takeLock.unlock();
            _putLock.unlock();
        }
        return total;

    }

    public Runnable poll()
    {
        final ReentrantLock takeLock = _takeLock;
        takeLock.lock();
        try
        {
            if(_count.get() > 0)
            {
                ReadWriteRunnable job = _writeJobQueue.poll();
                if(job == null)
                {
                    job = _readJobQueue.poll();
                }
                _count.decrementAndGet();
                return job;
            }
            else
            {
                return null;
            }
        }
        finally
        {
            takeLock.unlock();
        }

    }

    public Runnable peek()
    {
        final ReentrantLock takeLock = _takeLock;
        takeLock.lock();
        try
        {
            ReadWriteRunnable job = _writeJobQueue.peek();
            if(job == null)
            {
                job = _readJobQueue.peek();
            }
            return job;
        }
        finally
        {
            takeLock.unlock();
        }
    }
}
