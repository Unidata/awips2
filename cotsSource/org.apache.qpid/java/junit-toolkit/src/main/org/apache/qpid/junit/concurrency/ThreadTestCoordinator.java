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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ThreadFactory;

/**
 * ThreadTestCoordinator provides an array of binary latches that allows threads to wait for other threads or to send
 * them a signal that allows them to continue running or to wait for another thread to signal them. The binary latch
 * array is always a square array, allowing one latch from and to every thread. Upon accepting an allow signal from one
 * sender the latches for all senders for a are cleared. This class is always used in conjunction with
 * {@link TestRunnable} for writing concurrent test code that coordinates multi-threaded activity in order to reproduce
 * concurrency bugs.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Accept test threads to coordinate.
 * <tr><td> Allow test threads to send 'allow to continue' signals.
 * <tr><td> Allow test threads to wait on this coordinator for 'allow to continue' signals.
 * <tr><td> Report error messages from test threads.
 * <tr><td> Report exceptions from test threads.
 * <tr><td> Provide method to wait until all test threads have completed.
 * </table>
 *
 * @todo This code was hacked together as a bit of an experiment, because I wasn't sure if this idea would work. It has
 *       proved extremely usefull. Some documentation for this needs to be written to explain it better.
 *
 * @todo Consider how deadlock detection will be handled. If all threads are blocking on the coordinator, waiting for
 *       each other, they are deadlocked and there is something wrong with the test code that put them in that
 *       situation. If they are all blocked elsewhere, they may be deadlocked, or could just be waiting on some
 *       external event. A timeout should be used. Timeout is already implemented, just need to sanity check how
 *       this is working and document it.
 *
 * @todo Consider how livelock detection could be implemented? LockFree data structures might cause live locks. I
 *       guess a longish timeout is the only thing that can be done for that.
 *
 * @todo Only course grained synchronous at the method class level can be obtained. This is because test code can
 *       only insert synchronization points between method calls it makes. So this code will not be usefull for
 *       checking sequences of events within methods, unless the code under test is explicitly instrumented for it.
 *       It might be possible to instrument code by using labels, and then use the debugger/profiler interface to
 *       put breakpoints on the labels and use them as synchronization points. Not perfect, but at the unused labels
 *       can be left in the code, without altering its behaviour.
 *
 * @author Rupert Smith
 */
public class ThreadTestCoordinator
{
    /** Used for logging. */
    private static final Logger log = LoggerFactory.getLogger(ThreadTestCoordinator.class);

    /** Keeps track of the test threads by their ids. */
    private TestRunnable[] testThreads; // = new TestRunnable[2];

    /** An explicit thread monitor for the coordinator. Threads wait on the coordinator whilst waiting for events. */
    private final Object coordinatorLock = new Object();

    /** A set of monitors for each test thread. */
    private Object[] locks;

    /** The binary latch array, this is always a square array allowing one event from and to every thread. */
    private boolean[][] allowEvents;

    /** Keeps track of the number of threads being coordinated. */
    private int threadCount = 0;

    /** Accumulates any exceptions resulting from the threads run methods. */
    private Collection<Exception> exceptions = new ArrayList<Exception>();

    /**
     * Holds the deadlock timeout after which threads are given a runtime exception to signal that a potential
     * deadlock may be happening.
     */
    private long deadlockTimeout = 1000 * 1000000;

    /** Holds the factory to create test thread with. */
    private ThreadFactory threadFactory;

    /**
     * Creates a new test thread coordinator. The number of threads to run must be specified here.
     *
     * @param numThreads The number of threads to run.
     */
    public ThreadTestCoordinator(int numThreads)
    {
        this.threadCount = numThreads;

        // Create an array big enough to hold all the test threads.
        testThreads = new TestRunnable[threadCount];

        // Use the default thread factory, as none specified.
        threadFactory = new DefaultThreadFactory();
    }

    /**
     * Creates a new test thread coordinator with a specific thread factory. The number of threads to run must be
     * specified here.
     *
     * @param numThreads    The number of threads to run.
     * @param threadFactory The factory to use to create the test threads.
     */
    public ThreadTestCoordinator(int numThreads, ThreadFactory threadFactory)
    {
        this.threadCount = numThreads;

        // Create an array big enough to hold all the test threads.
        testThreads = new TestRunnable[threadCount];

        // Use the specified thread factory.
        this.threadFactory = threadFactory;
    }

    /**
     * Adds a thread to this coordinator and assigns an id to it. The ids must be numbered sequentially from 0 and
     * it is up to the caller to do this.
     *
     * @param runnable The test thread.
     * @param id       The explicit id to assign to the test thread.
     */
    public void addTestThread(TestRunnable runnable, int id)
    {
        testThreads[id] = runnable;
        runnable.setCoordinator(this);
        runnable.setId(id);
    }

    /**
     * Starts all the coordinated threads running.
     */
    public void run()
    {
        // Create the monitors for each thread.
        locks = new Object[threadCount];

        // Create an appropriately sized event queue to allow one event from and to each thread.
        allowEvents = new boolean[threadCount][threadCount];

        // Initialize the monitors and clear the event queues.
        for (int i = 0; i < locks.length; i++)
        {
            locks[i] = new Object();

            for (int j = 0; j < locks.length; j++)
            {
                allowEvents[i][j] = false;
            }
        }

        // Start all the threads running.
        for (TestRunnable nextRunnable : testThreads)
        {
            // Create a Java thread for the test thread.
            Thread newThread = threadFactory.newThread(nextRunnable);
            nextRunnable.setThread(newThread);

            // Start it running.
            newThread.start();
        }
    }

    /**
     * Waits until all the test threads have completed and returns any accumulated error messages from them. Any
     * exceptions thrown by their run methods are also kept at this point.
     *
     * @return The accumulated error messages from all the threads concatenated together.
     */
    public String joinAndRetrieveMessages()
    {
        // Create an empty error message.
        String errorMessage = "";

        // Join all the test threads.
        for (TestRunnable r : testThreads)
        {
            Thread t = r.getThread();

            try
            {
                t.join();
            }
            catch (InterruptedException e)
            { }

            // Add any accumulated error messages to the return value.
            errorMessage += r.getErrorMessage();

            // Keep any exceptions resulting from the threads run method.
            Exception e = r.getException();

            if (e != null)
            {
                exceptions.add(e);
            }
        }

        return errorMessage;
    }

    /**
     * Reports any accumulated exceptions from the test threads run methods. This method must be called after
     * {@link #joinAndRetrieveMessages}.
     *
     * @return Any accumulated exceptions from the test threads run methods. This method must be called after
     */
    public Collection<Exception> getExceptions()
    {
        return exceptions;
    }

    /**
     * Sets a timeout to break out of potential deadlocks. If all threads are waiting for other threads to send
     * them continue events for longer than this timeout then the threads are all terminated.
     *
     * @param millis The minimum time to allow to pass before breaking out of any potential deadlocks.
     *
     * @todo This has not been implemented yet. If a potential deadlock happens then the joinAndRetrieveMessages
     *       method should throw a PotentialDeadlockException.
     */
    public void setDeadlockTimeout(long millis)
    {
        deadlockTimeout = millis * 1000000;
    }

    /**
     * Creates a set of 'allow to continue' events on the event queues of the specified threads.
     *
     * @param threads  The set of threads to allow to continue.
     * @param callerId The explicit id of the calling test thread.
     * @param caller   The calling test thread.
     */
    void produceAllowEvents(int[] threads, int callerId, TestRunnable caller)
    {
        // Generate some debugging messages. Very usefull to know how thread synchronization is progressing.
        String message = "Thread " + callerId + " is allowing threads [ ";

        for (int j = 0; j < threads.length; j++)
        {
            message += threads[j] + ((j < (threads.length - 1)) ? ", " : "");
        }

        message += " ] to continue.";
        log.debug(message);

        // For each allow event, synchronize on the threads lock then set the event flag to true.
        for (int id : threads)
        {
            // Set the waiting on coordinator flag to true in case the coordinator tries to test this thread for
            // being blocked at this time.
            caller.setWaitingOnCoordinator(true);

            synchronized (locks[id])
            {
                // Release the wating on coordinator flag now that this thread is running again.
                caller.setWaitingOnCoordinator(false);

                // Send the allow to continue event to the receiving thread.
                allowEvents[id][callerId] = true;
            }
        }

        // Wake up any threads waiting on the coordinator lock to recheck their event queues.
        // Set the waiting on coordinator flag to true in case the coordinator tries to test this thread for
        // being blocked at this time.
        caller.setWaitingOnCoordinator(true);

        synchronized (coordinatorLock)
        {
            // Release the wating on coordinator flag now that this thread is running again.
            caller.setWaitingOnCoordinator(false);
            coordinatorLock.notifyAll();
        }
    }

    /**
     * Consumes an 'allow to continue' from one of the specified threads or waits until one is available or in some
     * cases if one of the specified threads is blocked elsewhere to accept that as an 'allow to continue' event.
     *
     * @param threads          The set of threads to accept an allow to continue event from.
     * @param otherWaitIsAllow Whether or not to accept threads being blocked elsewhere as permission to continue.
     * @param callerId         The explicit id of the calling test thread.
     * @param caller           The calling test thread.
     *
     * @return If the <tt>otherWaitIsAllow</tt> flag is set, then <tt>true</tt> is returned when the thread being waited on is found
     *         to be blocked outside of the thread test coordinator. <tt>false</tt> under all other conditions.
     */
    boolean consumeAllowEvent(int[] threads, boolean otherWaitIsAllow, int callerId, TestRunnable caller)
    {
        // Generate some debugging messages. Very usefull to know how thread synchronization is progressing.
        String message = "Thread " + callerId + " is requesting threads [ ";

        // Record the time at which this method was called. Will be used for breaking out of potential deadlocks.
        long startTime = System.nanoTime();

        for (int j = 0; j < threads.length; j++)
        {
            message += threads[j] + ((j < (threads.length - 1)) ? ", " : "");
        }

        message += " ] to allow it to continue.";
        log.debug(message);

        // Loop until an allow to continue event is received.
        while (true)
        {
            // Look at all the allowing thread to see if one has created an event for consumption.
            for (int allowerId : threads)
            {
                // Get the threads lock for the event to consume.
                // Set the waiting on coordinator flag to true in case the coordinator tries to test this thread for
                // being blocked at this time.
                caller.setWaitingOnCoordinator(true);

                synchronized (locks[callerId])
                {
                    // Release the wating on coordinator flag now that this thread is running again.
                    caller.setWaitingOnCoordinator(false);

                    // Check if there is an event on the queue from the allowing thread to this one.
                    if (allowEvents[callerId][allowerId])
                    {
                        log.debug("Found an allow event, thread " + allowerId + ", is allowing thread " + callerId
                            + ", to continue.");

                        // Consume all the allow events for this thread.
                        /*for (int i = 0; i < allowEvents[callerId].length; i++)
                        {
                            allowEvents[callerId][i] = false;
                        }*/

                        // Consume just the event from the allower to the consumer, leaving other pending allow events alone.
                        allowEvents[callerId][allowerId] = false;

                        return false;
                    }
                }
            }

            // If waiting elsewhere is to be interpreted as an 'allow to continue' event, then look at the thread status
            // for the threads being waited on to see if any are blocked on other resources.
            if (otherWaitIsAllow)
            {
                log.debug("Other wait is to be interpreted as an allow event.");

                // Look at all the potential allower threads.
                for (int allowerId : threads)
                {
                    // Get the Java thread state for the allowing thread.
                    Thread threadToTest = testThreads[allowerId].getThread();
                    Thread.State state = threadToTest.getState();

                    // Check if the thread is blocked and so a potential candidate for releasing this one.
                    if ((state == Thread.State.BLOCKED) || (state == Thread.State.WAITING)
                            || (state == Thread.State.TIMED_WAITING))
                    {
                        log.debug("Found an allower thread, id = " + allowerId + ", that is blocked or wating.");

                        // Check that the allower thread is not waiting on the coordinator lock or any of the
                        // individual thread locks. It must be waiting or blocked on another monitor.
                        TestRunnable allowingRunnable = testThreads[allowerId];
                        boolean isWaitingOnCoordinator = allowingRunnable.isWaitingOnCoordinator();

                        if (!isWaitingOnCoordinator)
                        {
                            log.debug("The allower thread, id = " + allowerId
                                + ", is blocked or waiting other than on the coordinator.");

                            // Get the threads lock for the event to consume.
                            caller.setWaitingOnCoordinator(true);

                            synchronized (locks[callerId])
                            {
                                caller.setWaitingOnCoordinator(false);

                                // Consume all the allow events for this thread.
                                for (int i = 0; i < allowEvents[callerId].length; i++)
                                {
                                    allowEvents[callerId][i] = false;
                                }

                                return true;
                            }
                        }
                        else
                        {
                            log.debug("The waiting allower thread, " + allowerId
                                + ", is waiting on the coordinator so does not allow thread " + callerId + " to continue.");
                        }
                    }
                }
            }

            // Keep waiting until an 'allow to continue' event can be consumed.
            try
            {
                // Set the waiting on coordinator flag to true in case the coordinator tries to test this thread for
                // being blocked at this time.
                caller.setWaitingOnCoordinator(true);

                synchronized (coordinatorLock)
                {
                    // Release the wating on coordinator flag now that this thread is running again.
                    caller.setWaitingOnCoordinator(false);

                    log.debug("Thread " + callerId + " is waiting on coordinator lock for more allow events.");

                    // Set the waiting on coordinator flag to true in case the coordinator tries to test this thread for
                    // being blocked at this time.
                    caller.setWaitingOnCoordinator(true);
                    coordinatorLock.wait(10);
                }
            }
            catch (InterruptedException e)
            { }

            // Release the waiting on coordinator flag now that this thread is running again.
            caller.setWaitingOnCoordinator(false);

            // Check if this thread has been waiting for longer than the deadlock timeout and raise a possible
            // deadlock exception if so.
            long waitTime = System.nanoTime() - startTime;
            log.debug("Thread " + callerId + " has been waiting for " + (waitTime / 1000000) + " milliseconds.");

            if (waitTime > deadlockTimeout)
            {
                // Throw a possible deadlock exception.
                throw new PossibleDeadlockException("Possible deadlock due to timeout with state:\n" + this);
            }

            log.debug("Thread " + callerId + " has woken up, was waiting for more allow events to become available.");
        }
    }

    /**
     * Pretty prints the state of the thread test coordinator, for debugging purposes.
     *
     * @return Pretty printed state of the thread test coordinator.
     */
    public String toString()
    {
        String result = "[";

        for (int i = 0; i < allowEvents.length; i++)
        {
            for (int j = 0; j < allowEvents[i].length; j++)
            {
                result += allowEvents[i][j];

                result += (j < (allowEvents[i].length - 1)) ? ", " : "";
            }

            result += (i < (allowEvents.length - 1)) ? ",\n " : "";
        }

        result += "]";

        for (int i = 0; i < testThreads.length; i++)
        {
            result += "thread[" + i + "] = " + testThreads[i].toString();
        }

        return result;
    }

}
