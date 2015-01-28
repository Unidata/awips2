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
package com.raytheon.viz.grid.rsc.general;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 * A pool of {@link Job} that executes {@link GridDataRequestRunner}s with a
 * scheduling pattern that is ideal for smooth user interaction. This object
 * expects {@link #schedule(GridDataRequestRunner)} to be called each time data
 * is needed that is not yet available(primarily when the frame is displayed).
 * Calling {@link #schedule(GridDataRequestRunner)} causes the runner to be
 * bumped to the front of the queue so that data will be ready as soon as
 * possible. This causes runners that are not being displayed to frequently get
 * bumped to the back of the queue, which is desired since the user does not
 * need that data yet.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 29, 2014  3668     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridDataRequestJobPool {

    private static final int POOL_SIZE = Integer.getInteger(
            "grid.request.pool.size", 10);
    
    private static final GridDataRequestJobPool instance = new GridDataRequestJobPool();

    public static void schedule(GridDataRequestRunner runner) {
        instance.scheduleRunner(runner);
    }

    protected LinkedBlockingQueue<Job> jobQueue = new LinkedBlockingQueue<Job>();

    protected List<GridDataRequestRunner> runners = new LinkedList<>();
    
    private GridDataRequestJobPool(){
        for (int i = 0; i <POOL_SIZE; i++) {
            jobQueue.add(new GridDataRequestJob());
        }
    }
    
    protected void scheduleRunner(GridDataRequestRunner runner) {
        synchronized (runners) {
            if (runners.isEmpty() || !(runners.get(0) == runner)) {
                runners.remove(runner);
                runners.add(0, runner);
            }
        }
        Job job = jobQueue.poll();
        if (job != null) {
            job.schedule();
        }
    }
    
    protected void reschedule(GridDataRequestRunner runner){
        synchronized (runners) {
            if(!runners.contains(runner)){
                runners.add(runner);
            }
        }
    }
    
    protected GridDataRequestRunner getNextRunner(){
        synchronized (runners) {
            if(runners.isEmpty()){
                return null;
            }else{
                return runners.remove(0);
            }
        }
    }
    
    
    private class GridDataRequestJob extends Job{

        public GridDataRequestJob() {
            super("Requesting Grid Data");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            jobQueue.offer(this);
            GridDataRequestRunner runner = getNextRunner();
            while(runner != null){
                if(runner.processOneRequest()){
                    reschedule(runner);
                }
                runner = getNextRunner();
            }
            return Status.OK_STATUS;
        }
    }
    
}
