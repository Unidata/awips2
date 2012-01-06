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
package com.raytheon.viz.gfe.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 * This is a placeholder job whose only purpose is to cause a progress bar to
 * appear while the parent job is doing work in another thread. It should only
 * be created within the parent job's run() method. Its run() method simply
 * returns ASYNCH_FINISH, unless the job has been canceled while it was being
 * moved from the sleeping state. It is the parent thread's responsibility to
 * call done() on this job when it finishes the work.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2011            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class AsyncProgressJob extends Job {

    Job parent;

    /**
     * Constructor. Sets the name of the job for the progress monitor and calls
     * setThread() (as required for ASYNCH_FINISH), using the parent's thread as
     * the argument. Since the parent's getThread() will usually return null
     * unless it is running, this constructor should only be called from within
     * the parent job's run() method.
     * 
     * @param name
     *            The job name to show in the progress monitor
     * @param parent
     *            The (running) job that will do the work.
     */
    public AsyncProgressJob(String name, Job parent) {
        super(name);
        this.parent = parent;
        Thread parentThread = parent.getThread();
        setThread(parentThread);
    }

    /**
     * A dummy method that just returns ASYNCH_FINISH. Once this job has been
     * scheduled, it is the parent job's responsibility to call done().
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     *      IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (monitor.isCanceled()) {
            return Status.CANCEL_STATUS;
        }
        return Job.ASYNC_FINISH;
    }

}
