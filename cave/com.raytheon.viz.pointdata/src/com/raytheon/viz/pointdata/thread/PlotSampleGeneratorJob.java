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
package com.raytheon.viz.pointdata.thread;

import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PlotModelFactory2;

/**
 * Job that uses the provided factory to generate a sampling message for a
 * particular plot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotSampleGeneratorJob extends Job {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotSampleGeneratorJob.class);

    private PlotModelFactory2 plotFactory;

    private ConcurrentLinkedQueue<PlotInfo[]> queue = new ConcurrentLinkedQueue<PlotInfo[]>();

    private IPlotModelGeneratorCaller caller;

    public PlotSampleGeneratorJob(PlotModelFactory2 factory,
            IPlotModelGeneratorCaller caller) {
        super("Generating samples");
        this.plotFactory = factory;
        this.caller = caller;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        while (!queue.isEmpty()) {
            try {
                PlotInfo[] infos = queue.poll();
                String message = plotFactory.getStationMessage(infos[0].pdv,
                        infos[0].dataURI);

                caller.messageGenerated(infos[0].dataURI, message);
            } catch (Exception e) {
                statusHandler.error("Error creating plot", e);
            }
        }

        plotFactory.disposeSampleScript();
        return Status.OK_STATUS;
    }

    public void enqueue(PlotInfo[] task) {
        this.queue.add(task);
        if (this.getState() != Job.RUNNING) {
            this.schedule();
        }
    }

}
