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
package com.raytheon.viz.gfe.core.parm;

import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Job to queue up and work of parm save requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2013     #2302  randerso    Initial creation
 * Jun 30, 2014     #3332  randerso    Added exception handling
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ParmSaveJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmSaveJob.class);

    public static class ParmSaveStatus {
        boolean successful = false;

        Semaphore complete = new Semaphore(0);

        /**
         * @return the successful
         */
        public boolean isSuccessful() {
            try {
                complete.acquire();
                return successful;
            } catch (InterruptedException e) {
                return false;
            }
        }

        /**
         * @param successful
         *            the successful to set
         */
        private void setSuccessful(boolean successful) {
            this.successful = successful;
            complete.release();
        }

    }

    private class ParmSaveRequest {
        private List<TimeRange> times;

        private ParmSaveStatus status;

        public ParmSaveRequest(List<TimeRange> times) {
            this.times = times;
            this.status = new ParmSaveStatus();
        }
    }

    private Parm parm;

    private ConcurrentLinkedQueue<ParmSaveRequest> saveQueue;

    /**
     * @param parm
     */
    public ParmSaveJob(Parm parm) {
        super("ParmSaveJob");
        this.setSystem(true);
        this.parm = parm;
        this.saveQueue = new ConcurrentLinkedQueue<ParmSaveJob.ParmSaveRequest>();
    }

    /**
     * Request save.
     * 
     * Caller should call isSuccessful on the returned ParmSaveStatus object to
     * await completion and to determine success
     * 
     * @param times
     * @return ParmSaveStatus object
     */
    public ParmSaveStatus requestSave(List<TimeRange> times) {
        ParmSaveRequest request = new ParmSaveRequest(times);
        this.saveQueue.add(request);
        this.schedule();
        return request.status;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        ParmSaveRequest req = null;
        while ((req = this.saveQueue.poll()) != null) {
            try {
                boolean successful = parm.saveParameterSubClass(req.times);
                req.status.setSuccessful(successful);
            } catch (Exception e) {
                statusHandler.error("Error saving grids for " + this.parm, e);
                req.status.setSuccessful(false);
            }
        }
        return Status.OK_STATUS;
    }
}
