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
package com.raytheon.uf.viz.core.time;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;

/**
 * A job to asynchronously schedule time matching.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class TimeMatchingJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeMatchingJob.class);

    private static Map<IDescriptor, TimeMatchingJob> map = new ConcurrentHashMap<IDescriptor, TimeMatchingJob>();

    private IDescriptor request;

    private boolean keepAround = false;

    private TimeMatchingJob(IDescriptor desc) {
        super("Time Matching...");
        this.request = desc;
    }

    public static synchronized void scheduleTimeMatch(IDescriptor desc) {
        if (desc != null && desc.getTimeMatcher() != null) {
            TimeMatchingJob job = map.get(desc);
            if (job == null) {
                job = new TimeMatchingJob(desc);
                job.setSystem(true);
            } else {
                job.keepAround = true;
            }
            map.put(desc, job);
            job.schedule();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            long t0 = System.currentTimeMillis();
            request.getTimeMatcher().redoTimeMatching(request);
            System.out.println("time matching took: "
                    + (System.currentTimeMillis() - t0));
            if (!this.keepAround) {
                map.remove(request);
            } else {
                this.keepAround = false;
            }
            request.getRenderableDisplay().refresh();
            if (request.getRenderableDisplay().getContainer() instanceof IEditorPart) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        VizGlobalsManager.getCurrentInstance().updateUI(
                                request.getRenderableDisplay().getContainer(),
                                request.getRenderableDisplay());
                    }
                });
            }

        } catch (Throwable e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error redoing time matching", e);
            return new Status(Status.ERROR, Activator.PLUGIN_ID,
                    "Error redoing time matching", e);
        }

        return Status.OK_STATUS;
    }
}
