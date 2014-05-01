package org.apache.qpid.util.concurrent;
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


import java.util.concurrent.locks.AbstractQueuedSynchronizer;

/**
 * A BooleanLatch is like a set of traffic lights, where threads can wait at a red light until another thread gives
 * the green light. When threads arrive at the latch it is initially red. They queue up until the green signal is
 * given, at which point they can all acquire the latch in shared mode and continue to run concurrently. Once the latch
 * is signalled it cannot be reset to red again.
 *
 * <p/> The latch uses a {@link java.util.concurrent.locks.AbstractQueuedSynchronizer} to implement its synchronization.
 * This has two internal states, 0 which means that the latch is blocked, and 1 which means that the latch is open.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Block threads until a go signal is given.
 * </table>
 *
 * @todo Might be better to use a countdown latch to count down from 1. Its await method can throw interrupted
 *       exception which makes the possibility of interruption more explicit, and provides a reminder to recheck the
 *       latch condition before continuing.
 */
public class BooleanLatch
{
    /** Holds the synchronizer that provides the thread queueing synchronization. */
    private final Sync sync = new Sync();

    /**
     * Tests whether or not the latch has been signalled, that is to say that, the light is green.
     *
     * <p/>This method is non-blocking.
     *
     * @return <tt>true</tt> if the latch may be acquired; the light is green.
     */
    public boolean isSignalled()
    {
        return sync.isSignalled();
    }

    /**
     * Waits on the latch until the signal is given and the light is green. If the light is already green then the
     * latch will be acquired and the thread will not have to wait.
     *
     * <p/>This method will block until the go signal is given or the thread is otherwise interrupted. Before carrying
     * out any processing threads that return from this method should confirm that the go signal has really been given
     * on this latch by calling the {@link #isSignalled()} method.
     */
    public void await()
    {
        sync.acquireShared(1);
    }

    /**
     * Releases any threads currently waiting on the latch. This flips the light to green allowing any threads that
     * were waiting for this condition to now run.
     *
     * <p/>This method is non-blocking.
     */
    public void signal()
    {
        sync.releaseShared(1);
    }

    /**
     * Implements a thread queued synchronizer. The internal state 0 means that the queue is blocked and the internl
     * state 1 means that the queue is released and that all waiting threads can acquire the synchronizer in shared
     * mode.
     */
    private static class Sync extends AbstractQueuedSynchronizer
    {
        /**
         * Attempts to acquire this synchronizer in shared mode. It may be acquired once it has been released.
         *
         * @param ignore This parameter is ignored.
         *
         * @return 1 if the shared acquisition succeeds and -1 if it fails.
         */
        protected int tryAcquireShared(int ignore)
        {
            return isSignalled() ? 1 : -1;
        }

        /**
         * Releases the synchronizer, setting its internal state to 1.
         *
         * @param ignore This parameter is ignored.
         *
         * @return <tt>true</tt> always.
         */
        protected boolean tryReleaseShared(int ignore)
        {
            setState(1);

            return true;
        }

        /**
         * Tests if the synchronizer is signalled. It is signalled when its internal state it 1.
         *
         * @return <tt>true</tt> if the internal state is 1, <tt>false</tt> otherwise.
         */
        boolean isSignalled()
        {
            return getState() != 0;
        }
    }
}
