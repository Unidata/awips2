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

import org.apache.log4j.Logger;

/**
 * An example to illustrate the use of the {@link ThreadTestCoordinator} and {@link TestRunnable}s.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Demo multi-threaded testing.
 * </table>
 *
 * @author Rupert Smith
 */
public class ThreadTestExample
{
    /** Used for logging. */
    private static final Logger log = Logger.getLogger(ThreadTestExample.class);

    /** Test thread 1. */
    TestRunnable testThread1 =
        new TestRunnable()
        {
            public void runWithExceptions() throws Exception
            {
                log.debug("public void run(): called");
                log.info("in testThread0, block 1");

                // Wait for t2 to allow t1 to continue.
                allow(new int[] { 1 });
                waitFor(new int[] { 1 }, false);

                log.info("in testThread0, block 2");

                // Wait for t2 to allow t1 to continue. T2 is allowed to be blocked elsewhere than giving explicit
                // permission to allow t1 to continue.
                allow(new int[] { 1 });
                waitFor(new int[] { 1 }, true);

                log.info("in testThread0, block 3");

                // Release thread 2 from waiting on the shared lock.
                synchronized (sharedLock)
                {
                    sharedLock.notifyAll();
                }

                allow(new int[] { 1 });
            }
        };

    /** A shared lock between the test threads. */
    final Object sharedLock = new Object();

    /** Test thread 2. */
    TestRunnable testThread2 =
        new TestRunnable()
        {
            public void runWithExceptions() throws Exception
            {
                log.debug("public void run(): called");
                log.info("in testThread1, block 1");

                // Wait for t1 to allow t2 to continue.
                allow(new int[] { 0 });
                waitFor(new int[] { 0 }, false);

                log.info("in testThread1, block 2");

                // Wait on another resource. T1 should accept this as permission to continue.
                try
                {
                    synchronized (sharedLock)
                    {
                        log.debug("in testThread1, waiting on shared lock.");
                        sharedLock.wait();
                    }
                }
                catch (InterruptedException e)
                {
                    // Bail-out with a runtime if this happens.
                    throw new RuntimeException("Interrupted whilst waiting for shared lock.", e);
                }

                log.info("in testThread1, finished waiting on shared lock.");

                // allow(new int[] { 0 });

                // Wait for t1 to allow t2 to continue.
                waitFor(new int[] { 0 }, false);

                log.info("in testThread1, block 3");

                allow(new int[] { 0 });
            }
        };

    /**
     * Executes the test threads with coordination.
     *
     * @param args Ignored.
     */
    public void main(String[] args)
    {
        ThreadTestCoordinator tt = new ThreadTestCoordinator(2);

        tt.addTestThread(testThread1, 0);
        tt.addTestThread(testThread2, 1);
        tt.setDeadlockTimeout(500);
        tt.run();

        String errorMessage = tt.joinAndRetrieveMessages();

        // Print any error messages or exceptions.
        log.info(errorMessage);

        if (!tt.getExceptions().isEmpty())
        {
            for (Exception e : tt.getExceptions())
            {
                log.warn("Exception thrown during test thread: ", e);
            }
        }
    }
}
