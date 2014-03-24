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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PlotModelFactory2;

/**
 * Job that uses the provided plot model factory to generate a sampling message
 * for a particular plot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2011            njensen     Initial creation
 * Jun 25, 2013 1869       bsteffen    Fix plot sampling.
 * Mar 21, 2014 2868       njensen     Major refactor
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotSampleGeneratorJob extends AbstractPlotCreationJob {

    private PlotModelFactory2 plotFactory;

    public PlotSampleGeneratorJob(PlotThreadOverseer parent,
            IPlotModelGeneratorCaller caller, PlotModelFactory2 factory) {
        super("Generating samples", parent, caller);
        this.plotFactory = factory;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        while (!overseer.sampleTextQueue.isEmpty()) {
            try {
                PlotInfo[] infos = overseer.sampleTextQueue.poll();
                if (infos == null) {
                    // possibility another thread got it first
                    continue;
                }
                String message = plotFactory.getStationMessage(infos[0].pdv,
                        infos[0].dataURI);
                listener.messageGenerated(infos, message);
            } catch (Exception e) {
                statusHandler.error("Error creating sample with plotModel "
                        + plotFactory.getPlotModelFilename(), e);
            }
        }

        return Status.OK_STATUS;
    }

    @Override
    public boolean shutdown() {
        boolean result = super.shutdown();
        plotFactory.dispose();
        return result;
    }
}
