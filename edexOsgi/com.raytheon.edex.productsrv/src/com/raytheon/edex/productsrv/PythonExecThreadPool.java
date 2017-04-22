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
package com.raytheon.edex.productsrv;

import java.util.concurrent.Callable;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class PythonExecThreadPool {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PythonExecThreadPool.class);

    private int maxPoolSize = 0;

    private int currentPoolSize = 0;

    private int readyThreads = 0;

    /**
     * use when checking or changing counters for ready threads and pool size
     */
    private static final Object threadCountLock = new Object();

    private static ThreadPoolExecutor execService = null;

    private static PythonExecThreadPool instance = new PythonExecThreadPool(1);

    private PythonExecThreadPool(int maxPoolSize) {
        this.maxPoolSize = maxPoolSize;
        this.readyThreads = maxPoolSize;
        this.currentPoolSize = 0;
        execService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable>());
    }

    public static PythonExecThreadPool getInstance() {
        return instance;
    }

    public void setMaxSize(int newMaxSize) {
        synchronized (threadCountLock) {
            this.maxPoolSize = newMaxSize;
        }
    }

    public int getMaxSize() {
        int rval = 0;
        synchronized (threadCountLock) {
            rval = this.maxPoolSize;
        }
        return rval;
    }

    public Object queueScriptBlocking(Callable<?> callable) throws Exception {

        // grow internal pool if needed
        synchronized (threadCountLock) {
            if (readyThreads < 1) {
                if (currentPoolSize < maxPoolSize) {

                    statusHandler.handle(Priority.INFO,
                            "Python Pool -- Growing internal pool to "
                                    + (currentPoolSize + 1) + " max size is "
                                    + maxPoolSize);

                    currentPoolSize += 1;
                    growPool(currentPoolSize);

                    // add a ready thread
                    readyThreads += 1;
                }
            }

            // shrink ready threads by one
            readyThreads -= 1;
        }

        // submit callable and update ready thread count when it returns
        Object rval = execService.submit(callable).get();
        synchronized (threadCountLock) {
            readyThreads += 1;
        }

        // return result!
        return rval;
    }

    private static void growPool(int newSize) {
        execService.setMaximumPoolSize(newSize);
        execService.setCorePoolSize(newSize);
    }
}
