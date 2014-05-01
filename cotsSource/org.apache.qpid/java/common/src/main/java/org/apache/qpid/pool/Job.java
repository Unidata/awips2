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

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A Job is a continuation that batches together other continuations, specifically {@link Event}s, into one continuation.
 * The {@link Event}s themselves provide methods to process themselves, so processing a job simply consists of sequentially
 * processing all of its aggregated events.
 *
 * The constructor accepts a maximum number of events for the job, and only runs up to that maximum number when
 * processing the job, but the add method does not enforce this maximum. In other words, not all the enqueued events
 * may be processed in each run of the job, several runs may be required to clear the queue.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Aggregate many coninuations together into a single continuation.
 * <tr><td> Sequentially process aggregated continuations. <td> {@link Event}
 * <tr><td> Provide running and completion status of the aggregate continuation.
 * <tr><td> Execute a terminal continuation upon job completion. <td> {@link JobCompletionHandler}
 * </table>
 *
 * @todo Could make Job implement Runnable, FutureTask, or a custom Continuation interface, to clarify its status as a
 *       continuation. Job is a continuation that aggregates other continuations and as such is a usefull re-usable
 *       piece of code. There may be other palces than the mina filter chain where continuation batching is used within
 *       qpid, so abstracting this out could provide a usefull building block. This also opens the way to different
 *       kinds of job with a common interface, e.g. parallel or sequential jobs etc.
 *
 * @todo For better re-usability could make the completion handler optional. Only run it when one is set.
 */
public class Job implements ReadWriteRunnable
{
    
    /** Defines the maximum number of events that will be batched into a single job. */
    public static final int MAX_JOB_EVENTS = Integer.getInteger("amqj.server.read_write_pool.max_events", 10);

    /** The maximum number of events to process per run of the job. More events than this may be queued in the job. */
    private final int _maxEvents;

    /** Holds the queue of events that make up the job. */
    private final java.util.Queue<Runnable> _eventQueue = new ConcurrentLinkedQueue<Runnable>();

    /** Holds a status flag, that indicates when the job is actively running. */
    private final AtomicBoolean _active = new AtomicBoolean();

    private final boolean _readJob;

    private ReferenceCountingExecutorService _poolReference;

    private final static Logger _logger = LoggerFactory.getLogger(Job.class);
    
    public Job(ReferenceCountingExecutorService poolReference, int maxEvents, boolean readJob)
    {
        _poolReference = poolReference;
        _maxEvents = maxEvents;
        _readJob = readJob;
    }

    /**
     * Enqueus a continuation for sequential processing by this job.
     *
     * @param evt The continuation to enqueue.
     */
    public void add(Runnable evt)
    {
        _eventQueue.add(evt);
    }

    /**
     * Sequentially processes, up to the maximum number per job, the aggregated continuations in enqueued in this job.
     */
    boolean processAll()
    {
        // limit the number of events processed in one run
        int i = _maxEvents;
        while( --i != 0 )
        {
            Runnable e = _eventQueue.poll();
            if (e == null)
            {
                return true;
            }
            else
            {
                e.run();
            }
        }
        return false;
    }

    /**
     * Tests if there are no more enqueued continuations to process.
     *
     * @return <tt>true</tt> if there are no enqueued continuations in this job, <tt>false</tt> otherwise.
     */
    public boolean isComplete()
    {
        return _eventQueue.peek() == null;
    }

    /**
     * Marks this job as active if it is inactive. This method is thread safe.
     *
     * @return <tt>true</tt> if this job was inactive and has now been marked as active, <tt>false</tt> otherwise.
     */
    public boolean activate()
    {
        return _active.compareAndSet(false, true);
    }

    /**
     * Marks this job as inactive. This method is thread safe.
     */
    public void deactivate()
    {
        _active.set(false);
    }

    /**
     * Processes a batch of aggregated continuations, marks this job as inactive and call the terminal continuation.
     */
    public void run()
    {
        if(processAll())
        {
            deactivate();
            completed();
        }
        else
        {
            notCompleted();
        }
    }

    public boolean isRead()
    {
        return _readJob;
    }
    
    /**
     * Adds an {@link Event} to a {@link Job}, triggering the execution of the job if it is not already running.
     *
     * @param job The job.
     * @param event   The event to hand off asynchronously.
     */
    public static void fireAsynchEvent(ExecutorService pool, Job job, Runnable event)
    {

        job.add(event);


        if(pool == null)
        {
            return;
        }

        // rather than perform additional checks on pool to check that it hasn't shutdown.
        // catch the RejectedExecutionException that will result from executing on a shutdown pool
        if (job.activate())
        {
            try
            {
                pool.execute(job);
            }
            catch(RejectedExecutionException e)
            {
                _logger.warn("Thread pool shutdown while tasks still outstanding");
            }
        }

    }
    
    /**
     * Implements a terminal continuation for the {@link Job} for this filter. Whenever the Job completes its processing
     * of a batch of events this is called. This method simply re-activates the job, if it has more events to process.
     *
     * @param session The Mina session to work in.
     * @param job     The job that completed.
     */
    public void completed()
    {
        if (!isComplete())
        {
            final ExecutorService pool = _poolReference.getPool();

            if(pool == null)
            {
                return;
            }


            // ritchiem : 2006-12-13 Do we need to perform the additional checks here?
            // Can the pool be shutdown at this point?
            if (activate())
            {
                try
                {
                    pool.execute(this);
                }
                catch(RejectedExecutionException e)
                {
                    _logger.warn("Thread pool shutdown while tasks still outstanding");
                }

            }
        }
    }

    public void notCompleted()
    {
        final ExecutorService pool = _poolReference.getPool();

        if(pool == null)
        {
            return;
        }

        try
        {
            pool.execute(this);
        }
        catch(RejectedExecutionException e)
        {
            _logger.warn("Thread pool shutdown while tasks still outstanding");
        }
    }
    
}
