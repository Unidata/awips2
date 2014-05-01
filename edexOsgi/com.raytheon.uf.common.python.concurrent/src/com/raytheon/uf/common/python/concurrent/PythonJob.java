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

import java.util.concurrent.Callable;

import jep.JepException;

import com.raytheon.uf.common.python.PythonInterpreter;

/**
 * Task that will invoke methods on a {@link IPythonExecutor} subclass
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013            mnash       Initial creation
 * Jun 04, 2013 2041       bsteffen    Improve exception handling for concurrent
 *                                     python.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class PythonJob<P extends PythonInterpreter, R extends Object>
        implements Callable<R> {

    /*
     * When code creates a new PythonJob, this gets set to the instance of the
     * ThreadLocal that corresponds with whichever thread the application is
     * running on
     */
    private ThreadLocal<P> threadPython = null;

    private IPythonJobListener<R> listener;

    private IPythonExecutor<P, R> executor;

    public PythonJob(IPythonExecutor<P, R> executor,
            IPythonJobListener<R> listener, ThreadLocal<P> threadLocal) {
        this.listener = listener;
        this.threadPython = threadLocal;
        this.executor = executor;
    }

    public PythonJob(IPythonExecutor<P, R> executor, ThreadLocal<P> threadLocal) {
        this.threadPython = threadLocal;
        this.executor = executor;
    }

    @Override
    public R call() {
        P script = threadPython.get();
        R result = null;
        if (listener == null) {
            try {
                result = executor.execute(script);
            } catch (JepException e) {
                throw new PythonJobFailedException(e);
            }
        } else {
            try {
                result = executor.execute(script);
            } catch (Throwable t) {
                listener.jobFailed(t);
                return null;
            }

            // fire listener to alert the original caller that we are done
            listener.jobFinished(result);
        }
        return result;
    }
}
