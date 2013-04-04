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
package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;

import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;

import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.NcPlotModelDataRequestJob.PlotImageInfo;
/**
 * Job separated from PlotModelGenerator2 that creates the plot images.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            njensen     Initial creation
 * 09/2012      896        sgurung     Refactored raytheon's class PlotModelGeneratorJoband added
 * 									   code from ncep's PlotModelGenerator2	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcPlotModelGeneratorJob extends Job {

    private ConcurrentLinkedQueue<PlotImageInfo> taskQueue = new ConcurrentLinkedQueue<PlotImageInfo>();

    private PlotModelFactory2 plotCreator;

    private IPlotModelGeneratorCaller caller;

    private IGraphicsTarget target;

    protected NcPlotModelGeneratorJob(PlotModelFactory2 plotCreator2,
            IPlotModelGeneratorCaller caller, 
            IGraphicsTarget target) {
        super("Creating plots...");
        this.plotCreator = plotCreator2;
        this.caller = caller;
        this.target = target; 
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
    	
    	long t0 = System.currentTimeMillis();
        
        int i= 0 ;
        while (!taskQueue.isEmpty()) {
            try {
            	PlotImageInfo info = taskQueue.poll();
            	++i;
                  
            	BufferedImage bImage = plotCreator.getStationPlot( info.paramsToPlot, info.allMetParamsMap);
            	
                IImage image = null;
                if (bImage != null) {
                        image = target.initializeRaster(new IODataPreparer(
                                bImage, UUID.randomUUID().toString(), 0), null);
                }
                caller.modelGenerated(new PlotInfo[] {info.info}, image);
               /* DataBuffer buff = bImage.getData().getDataBuffer();
                int bytes = buff.getSize() * DataBuffer.getDataTypeSize(buff.getDataType()) / 8;
                System.out.println(" ~~~~~~~ bImage.memory usage: " + bytes);*/
                bImage.flush();
            } catch (Exception e) {
            	System.out.println("Error creating plot: " + e);
            }
        }

        System.out.println(" ~~~~~~ No. of stations: " +i +" ~~~ Time spent creating plots: "+ (System.currentTimeMillis() - t0));
        return Status.OK_STATUS;
    }

    protected void enqueue(PlotImageInfo infos) {
        this.taskQueue.add(infos);
        if (this.getState() != Job.RUNNING) {
            this.schedule();
        }
    }

    protected int getQueueSize() {
        return taskQueue.size();
    }

    protected void shutdown() {
        cancel();
        taskQueue.clear();
    }    
  
}
