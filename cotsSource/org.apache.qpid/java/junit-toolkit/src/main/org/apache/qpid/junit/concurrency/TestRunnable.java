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
package org.apache.qpid.junit.concurrency;

/**
 * TestRunnable is an extension of java.util.Runnable that adds some features to make it easier to coordinate the
 * activities of threads in such a way as to expose bugs in multi threaded code.
 *
 * <p/>Sometimes several threads will run in a particular order so that a bug is not revealed. Other times the ordering
 * of the threads will expose a bug. Such bugs can be hard to replicate as the exact execution ordering of threads is not
 * usually controlled. This class adds some methods that allow threads to synchronize other threads, either allowing them
 * to run, or waiting for them to allow this thread to run. It also provides convenience methods to gather error messages
 * and exceptions from threads, which will often be reported in unit testing code.
 *
 * <p/>Coordination between threads is handled by the {@link ThreadTestCoordinator}. It is called through the convenience
 * methods {@link #allow} and {@link #waitFor}. Threads to be coordinated must be set up with the coordinator and assigned
 * integer ids. It is then possible to call the coordinator with an array of thread ids requesting that those threads
 * be allowed to continue, or to wait until one of them allows this thread to continue. The otherwise non-deterministic
 * execution order of threads can be controlled into a carefully determined sequence using these methods in order
 * to reproduce race conditions, dead locks, live locks, dirty reads, phantom reads, non repeatable reads and so on.
 *
 * <p/>When waiting for another thread to give a signal to continue it is sometimes the case that the other thread has
 * become blocked by the code under test. For example in testing for a dirty read (for example in database code),
 * thread 1 lets thread 2 perform a write but not commit it, then thread 2 lets thread 1 run and attempt to perform a
 * dirty read on its uncommitted write. Transaction synchronization code being tested against the possibility of a dirty
 * write may make use of snapshots in which case both threads should be able to read and write without blocking. It may
 * make use of explicit keys in which case thread 2 may become blocked on its write attempt because thread 1 holds a
 * read lock and it must wait until thread 1 completes its transaction before it can acquire this lock. The
 * {@link #waitFor} method accepts a boolean parameter to indicate that threads being blocked (other than on the
 * coordinator) can be interpreted the same as if the thread explicitly allows the thread calling waitFor to continue.
 * Using this technique a dirty read test could be written that works against either the snapshot or the locking
 * implementation, allowing both approaches to pass the test yet arranging for multiple threads to run against the
 * implementation in such a way that a potential dirty read bug is exposed.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Wait for another thread to allow this one to continue.
 * <tr><td> Allow another thread to continue.
 * <tr><td> Accumulate error messages.
 * <tr><td> Record exceptions from thread run.
 * <tr><td> Maintain link to thread coordinator.
 * <tr><td> Explicitly mark a thread with an integer id.
 * <tr><td> Maintian a flag to indicate whether or not this thread is waiting on the coordinator.
 * </table>
 *
 * @todo The allow then waitFor operations are very often used as a pair. So create a method allowAndWait that combines
 *       them into a single method call.
 *
 * @author Rupert Smith
 */
public abstract class TestRunnable implements Runnable
{
    /** Holds a reference to the thread coordinator. */
    private ThreadTestCoordinator coordinator;

    /** Holds the explicit integer id of this thread. */
    private int id;

    /** Used to indicate that this thread is waiting on the coordinator and not elsewhere. */
    private boolean waitingOnCoordinator = false;

    /** Used to accumulate error messsages. */
    private String errorMessage = "";

    /** Holds the Java thread object that this is running under. */
    private Thread thisThread;

    /** Used to hold any exceptions resulting from the run method. */
    private Exception runException = null;

    /**
     * Implementations override this to perform coordinated thread sequencing.
     *
     * @throws Exception Any exception raised by the implementation will be caught by the default {@link #run()}
     *                   implementation for later querying by the {@link #getException()} method.
     */
    public abstract void runWithExceptions() throws Exception;

    /**
     * Provides a default implementation of the run method that allows exceptions to be thrown and keeps a record
     * of those exceptions. Defers to the {@link #runWithExceptions()} method to provide the thread body implementation
     * and catches any exceptions thrown by it.
     */
    public void run()
    {
        try
        {
            runWithExceptions();
        }
        catch (Exception e)
        {
            this.runException = e;
        }
    }

    /**
     * Attempt to consume an allow event from one of the specified threads and blocks until such an event occurrs.
     *
     * @param threads          The set of threads that can allow this one to continue.
     * @param otherWaitIsAllow If set to <tt>true</tt> if the threads being waited on are blocked other than on
     *                         the coordinator itself then this is to be interpreted as allowing this thread to
     *                         continue.
     *
     * @return If the <tt>otherWaitIsAllow</tt> flag is set, then <tt>true</tt> is returned when the thread being waited on is found
     *         to be blocked outside of the thread test coordinator. <tt>false</tt> under all other conditions.
     */
    protected boolean waitFor(int[] threads, boolean otherWaitIsAllow)
    {
        return coordinator.consumeAllowEvent(threads, otherWaitIsAllow, id, this);
    }

    /**
     * Produces allow events on each of the specified threads.
     *
     * @param threads The set of threads that are to be allowed to continue.
     */
    protected void allow(int[] threads)
    {
        coordinator.produceAllowEvents(threads, id, this);
    }

    /**
     * Keeps the error message for later reporting by the coordinator.
     *
     * @param message The error message to keep.
     */
    protected void addErrorMessage(String message)
    {
        errorMessage += message;
    }

    /**
     * Sets the coordinator for this thread.
     *
     * @param coordinator The coordinator for this thread.
     */
    void setCoordinator(ThreadTestCoordinator coordinator)
    {
        this.coordinator = coordinator;
    }

    /**
     * Reports whether or not this thread is waiting on the coordinator.
     *
     * @return <tt>If this thread is waiting on the coordinator.
     */
    boolean isWaitingOnCoordinator()
    {
        return waitingOnCoordinator;
    }

    /**
     * Sets the value of the waiting on coordinator flag.
     *
     * @param waiting The value of the waiting on coordinator flag.
     */
    void setWaitingOnCoordinator(boolean waiting)
    {
        waitingOnCoordinator = waiting;
    }

    /**
     * Sets up the explicit int id for this thread.
     *
     * @param id The integer id.
     */
    void setId(int id)
    {
        this.id = id;
    }

    /**
     * Reports any accumulated error messages.
     *
     * @return Any accumulated error messages.
     */
    String getErrorMessage()
    {
        return errorMessage;
    }

    /**
     * Reports any exception thrown by the {@link #runWithExceptions} method.
     *
     * @return Any exception thrown by the {@link #runWithExceptions} method.
     */
    Exception getException()
    {
        return runException;
    }

    /**
     * Sets the Java thread under which this runs.
     *
     * @param thread The Java thread under which this runs.
     */
    void setThread(Thread thread)
    {
        thisThread = thread;
    }

    /**
     * Gets the Java thread under which this runs.
     *
     * @return The Java thread under which this runs.
     */
    Thread getThread()
    {
        return thisThread;
    }

    /**
     * Provides a string summary of this test threads status.
     *
     * @return Summarizes this threads status.
     */
    public String toString()
    {
        return "id = " + id + ", waitingOnCoordinator = " + waitingOnCoordinator;
    }
}
