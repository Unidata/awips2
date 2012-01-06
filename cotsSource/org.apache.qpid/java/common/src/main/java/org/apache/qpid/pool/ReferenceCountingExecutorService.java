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
package org.apache.qpid.pool;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * ReferenceCountingExecutorService wraps an ExecutorService in order to provide shared reference to it. It counts
 * the references taken, instantiating the service on the first reference, and shutting it down when the last
 * reference is released.
 *
 * <p/>It is important to ensure that an executor service is correctly shut down as failing to do so prevents the JVM
 * from terminating due to the existence of non-daemon threads.
 *
 * <p/><table id="crc><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Provide a shared exector service. <td> {@link Executors}
 * <tr><td> Shutdown the executor service when not needed. <td> {@link ExecutorService}
 * <tr><td> Track references to the executor service.
 * <tr><td> Provide configuration of the executor service.
 * </table>
 *
 * @todo Might be more elegant to make this actually implement ExecutorService, providing better hiding of the
 *       implementation details. Also this class introduces a pattern (albeit specific to this usage) that could be
 *       generalized to reference count anything. That is, on first instance call a create method, on release of last
 *       instance call a destroy method. This could definitely be abstracted out as a re-usable piece of code; a
 *       reference counting factory. It could then be re-used to do reference counting in other places (such as
 *       messages). Countable objects have a simple create/destroy life cycle, capturable by an interface that the
 *       ref counting factory can call to manage the lifecycle.
 *
 * @todo {@link #_poolSize} should be static?
 *
 * @todo The {@link #getPool()} method breaks the encapsulation of the reference counter. Generally when getPool is used
 *       further checks are applied to ensure that the exector service has not been shutdown. This passes responsibility
 *       for managing the lifecycle of the reference counted object onto the caller rather than neatly encapsulating it
 *       here. Could think about adding more state to the lifecycle, to mark ref counted objects as invalid, and have an
 *       isValid method, or could make calling code deal with RejectedExecutionException raised by shutdown executors.
 */
public class ReferenceCountingExecutorService
{
    /** Defines the smallest thread pool that will be allocated, irrespective of the number of processors. */
    private static final int MINIMUM_POOL_SIZE = 4;

    /** Holds the number of processors on the machine. */
    private static final int NUM_CPUS = Runtime.getRuntime().availableProcessors();

    /** Defines the thread pool size to use, which is the larger of the number of CPUs or the minimum size. */
    private static final int DEFAULT_POOL_SIZE = Math.max(NUM_CPUS, MINIMUM_POOL_SIZE);

    /**
     * Holds the singleton instance of this reference counter. This is only created once, statically, so the
     * {@link #getInstance()} method does not need to be synchronized.
     */
    private static final ReferenceCountingExecutorService _instance = new ReferenceCountingExecutorService();

    /** This lock is used to ensure that reference counts are updated atomically with create/destroy operations. */
    private final Object _lock = new Object();

    /** The shared executor service that is reference counted. */
    private ExecutorService _pool;

    /** Holds the number of references given out to the executor service. */
    private int _refCount = 0;

    /** Holds the number of executor threads to create. */
    private int _poolSize = Integer.getInteger("amqj.read_write_pool_size", DEFAULT_POOL_SIZE);

    private final boolean _useBiasedPool = Boolean.getBoolean("org.apache.qpid.use_write_biased_pool");

    /**
     * Retrieves the singleton instance of this reference counter.
     *
     * @return The singleton instance of this reference counter.
     */
    public static ReferenceCountingExecutorService getInstance()
    {
        return _instance;
    }

    /**
     * Private constructor to ensure that only a singleton instance can be created.
     */
    private ReferenceCountingExecutorService()
    { }

    /**
     * Provides a reference to a shared executor service, incrementing the reference count.
     *
     * @return An executor service.
     */
    public ExecutorService acquireExecutorService()
    {
        synchronized (_lock)
        {
            if (_refCount++ == 0)
            {
//                _pool = Executors.newFixedThreadPool(_poolSize);

                // Use a job queue that biases to writes
                if(_useBiasedPool)
                {
                    _pool =  new ThreadPoolExecutor(_poolSize, _poolSize,
                                          0L, TimeUnit.MILLISECONDS,
                                          new ReadWriteJobQueue());
                }
                else
                {
                    _pool = Executors.newFixedThreadPool(_poolSize);
                }
            }


            return _pool;
        }
    }

    /**
     * Releases a reference to a shared executor service, decrementing the reference count. If the refence count falls
     * to zero, the executor service is shut down.
     */
    public void releaseExecutorService()
    {
        synchronized (_lock)
        {
            if (--_refCount == 0)
            {
                _pool.shutdownNow();
            }
        }
    }

    /**
     * Provides access to the executor service, without touching the reference count.
     *
     * @return The shared executor service, or <tt>null</tt> if none has been instantiated yet.
     */
    public ExecutorService getPool()
    {
        return _pool;
    }

    /**
     * Return the ReferenceCount to this ExecutorService
     * @return reference count
     */
    public int getReferenceCount()
    {
        return _refCount;
    }
}
