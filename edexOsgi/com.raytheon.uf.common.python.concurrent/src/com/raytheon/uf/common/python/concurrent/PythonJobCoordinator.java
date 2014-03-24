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
package com.raytheon.uf.common.python.concurrent;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import jep.JepException;

import com.raytheon.uf.common.python.PythonInterpreter;

/**
 * Interface to get to the {@link ExecutorService}. Allows multiple thread pools
 * to be created in a single JVM, by passing in a different application name.
 * 
 * This class will be used in this way:
 * 
 * 
 * <pre>
 * 
 *       AbstractPythonScriptFactory<PythonInterpreter, Object> factory = new CAVEPythonFactory();
 *       PythonJobCoordinator coordinator = PythonJobCoordinator
 *               .newInstance(factory);
 *       IPythonExecutor<PythonInterpreter, Object> executor = new CAVEExecutor(
 *               args);
 *       try {
 *           coordinator.submitAsyncJob(executor, listener);
 *       } catch (Exception e) {
 *           e.printStackTrace();
 *       }
 * 
 * }
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013            mnash       Initial creation
 * Jun 04, 2013 2041       bsteffen    Improve exception handling for concurrent
 *                                     python.
 * Mar 21, 2014 2868       njensen     Changed getInstance() from throwing
 *                                     RuntimeException to IllegalArgumentException
 *                                     Added refCount
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class PythonJobCoordinator<P extends PythonInterpreter> {

    private ExecutorService execService = null;

    private ThreadLocal<P> threadLocal = null;

    /**
     * Tracks the number of times newInstance() vs shutdown() is called for an
     * instance of a PythonJobCoordinator
     */
    private AtomicInteger refCount;

    private static Map<String, PythonJobCoordinator<? extends PythonInterpreter>> pools = new ConcurrentHashMap<String, PythonJobCoordinator<? extends PythonInterpreter>>();

    private PythonJobCoordinator(final AbstractPythonScriptFactory<P> factory) {
        threadLocal = new ThreadLocal<P>() {
            @Override
            protected P initialValue() {
                try {
                    return factory.createPythonScript();
                } catch (JepException e) {
                    throw new ScriptCreationException(e);
                }
            };
        };
        execService = Executors.newFixedThreadPool(factory.getMaxThreads(),
                new PythonThreadFactory(threadLocal, factory.getName()));
        refCount = new AtomicInteger();
    }

    /**
     * Gets the instance by name, or throw a {@link IllegalArgumentException}.
     * 
     * @param name
     * @return
     */
    public static <S extends PythonInterpreter> PythonJobCoordinator<S> getInstance(
            String name) {
        synchronized (pools) {
            if (pools.containsKey(name)) {
                return (PythonJobCoordinator<S>) pools.get(name);
            } else {
                throw new IllegalArgumentException(
                        "Unable to find instance of PythonJobCoordinator named "
                                + name
                                + ", please call newInstance(AbstractPythonScriptFactory)");
            }
        }
    }

    /**
     * Creates a new instance of this class for a new application. If the same
     * name already exists, it assumes that it is the same application and
     * returns the existing instance. Also increments the reference count of
     * applications using this PythonJobCoordinator. For each time that
     * newInstance() is called, a corresponding call to shutdown() will be
     * needed if you truly want to shut the job coordinator down.
     * 
     * @param name
     * @param numThreads
     * @return
     */
    public static <S extends PythonInterpreter> PythonJobCoordinator<S> newInstance(
            AbstractPythonScriptFactory<S> factory) {
        synchronized (pools) {
            PythonJobCoordinator<S> pool = null;
            if (pools.containsKey(factory.getName())) {
                pool = (PythonJobCoordinator<S>) pools.get(factory.getName());
            } else {
                pool = new PythonJobCoordinator<S>(factory);
                pools.put(factory.getName(), pool);
            }
            pool.refCount.getAndIncrement();
            return pool;
        }
    }

    /**
     * Submits a job to the {@link ExecutorService}. Fires a listener back after
     * it is done. This should be used for asynchronous operations.
     * 
     * @param callable
     * @return
     * @throws Exception
     */
    public <R> void submitAsyncJob(IPythonExecutor<P, R> executor,
            IPythonJobListener<R> listener) throws Exception {
        // fail if the listener is null, bad things happen then
        if (listener == null) {
            throw new IllegalArgumentException("Listener cannot be null");
        }
        // submit job
        PythonJob<P, R> job = new PythonJob<P, R>(executor, listener,
                threadLocal);
        execService.submit(job);
    }

    /**
     * Submits a job to the {@link ExecutorService}. Waits on the result before
     * returning back. This should be used for synchronous operations.
     * 
     * @param executor
     * @return
     * @throws InterruptedException
     * @throws ExecutionException
     */
    public <R> R submitSyncJob(IPythonExecutor<P, R> executor)
            throws InterruptedException, ExecutionException {
        // submit job
        PythonJob<P, R> job = new PythonJob<P, R>(executor, threadLocal);
        Future<R> future = execService.submit(job);
        // wait for return object
        return future.get();
    }

    /**
     * This function should take the {@link PythonInterpreter} on each thread in
     * the thread pool and dispose of it and then shutdown the
     * {@link ExecutorService}. This will reduce the reference count by 1, and
     * will only shut down the underlying executor service and python
     * interpreters if the refCount is less than 1.
     * 
     * @param name
     */
    public void shutdown() {
        synchronized (pools) {
            int count = refCount.decrementAndGet();
            if (count < 1) {
                pools.values().remove(this);
                execService.shutdown();
            }
        }
    }

    /**
     * This function should cancel any listeners for a certain task and then
     * remove those corresponding tasks off of the queue to be ran. It should
     * NOT try to cancel any running python interpreters.
     * 
     * @param name
     */
    public void shutdownTask(String name) {
        /*
         * TODO need to add for future functionality, arg should probably be an
         * IPythonExecutor, not a String
         */
    }

}
