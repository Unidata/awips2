/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.core;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Base class for Timer based threading. Allows previous thread based paradigms
 * to hook in to a camel context with minimal work.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2014 2826       rjpeter     Initial creation.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public abstract class EdexTimerBasedThread implements IContextStateProcessor {
    /**
     * Number of threads that have been started.
     */
    protected final AtomicInteger threadCount = new AtomicInteger(0);

    /**
     * Current active threads.
     */
    protected final List<Thread> threads = new LinkedList<Thread>();

    /**
     * Whether the container is running or not.
     */
    protected volatile boolean running = true;

    /**
     * Interval thread should sleep between calls.
     */
    protected int threadSleepInterval = 30000;

    /**
     * The name to use for the threads.
     * 
     * @return
     */
    public abstract String getThreadGroupName();

    /**
     * Method to do the work. Should return when done. Run method handles start
     * up/shutdown mechanism.
     * 
     * @throws Exception
     */
    public abstract void process() throws Exception;

    /**
     * Can be overridden to do any work to cleanup the thread on shutdown.
     */
    public void dispose() {

    }

    /**
     * Called by camel to do the processing. Will run until the context is
     * shutdown.
     */
    public void run() {
        synchronized (threads) {
            threads.add(Thread.currentThread());
        }

        try {
            Thread.currentThread().setName(
                    getThreadGroupName() + "-" + threadCount.incrementAndGet());

            while (running) {
                try {
                    process();
                } catch (Exception e) {
                    UFStatus.getHandler().error(
                            "Error occurred during processing", e);
                }

                if (running) {
                    try {
                        /*
                         * use waiter to allow shutdown to wake thread for
                         * immediate shutdown
                         */
                        synchronized (threads) {
                            threads.wait(threadSleepInterval);
                        }
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }
            }
        } finally {
            synchronized (threads) {
                threads.remove(Thread.currentThread());
                threads.notify();
            }
            dispose();
        }
    }

    @Override
    public void preStart() {
    }

    @Override
    public void postStart() {
    }

    @Override
    public void preStop() {
        running = false;
        synchronized (threads) {
            threads.notifyAll();
        }
    }

    @Override
    public void postStop() {
        IUFStatusHandler statusHandler = UFStatus.getHandler();
        String msg = "Waiting for " + getThreadGroupName()
                + " threads to finish";

        synchronized (threads) {
            while (!threads.isEmpty()) {
                statusHandler.info(msg);
                try {
                    threads.wait(10000);
                } catch (Exception e) {
                    // ignore
                }
            }
        }
    }

    public int getThreadSleepInterval() {
        return threadSleepInterval;
    }

    public void setThreadSleepInterval(int threadSleepInterval) {
        this.threadSleepInterval = threadSleepInterval;
    }
}
