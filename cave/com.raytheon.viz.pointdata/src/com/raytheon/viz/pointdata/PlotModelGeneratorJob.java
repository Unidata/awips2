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
package com.raytheon.viz.pointdata;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.apache.commons.collections.map.LRUMap;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.ISingleColorImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;

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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotModelGeneratorJob extends Job {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelDataRequestJob.class);

    private ConcurrentLinkedQueue<PlotInfo[]> taskQueue = new ConcurrentLinkedQueue<PlotInfo[]>();

    private PlotModelFactory2 plotCreator;

    private IPlotModelGeneratorCaller caller;

    private IGraphicsTarget target;

    @SuppressWarnings("unchecked")
    private Map<BufferedImage, IImage> imageCache = new LRUMap(1000);

    protected PlotModelGeneratorJob(PlotModelFactory2 plotCreator,
            IPlotModelGeneratorCaller caller, IGraphicsTarget target) {
        super("Creating plots");
        this.plotCreator = plotCreator;
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
        while (!taskQueue.isEmpty()) {
            try {
                PlotInfo[] infos = taskQueue.poll();
                final BufferedImage bImage = plotCreator.getStationPlot(
                        infos[0].pdv, infos[0].latitude, infos[0].longitude);
                IImage image = null;
                if (bImage != null) {
                    if (imageCache.containsKey(bImage)) {
                        image = imageCache.get(bImage);
                        if (image.getStatus() == IImage.Status.FAILED
                                || image.getStatus() == IImage.Status.INVALID
                                || image.getStatus() == IImage.Status.UNLOADED) {
                            image = null;
                        }
                    }
                    if (image == null) {
                        image = target.getExtension(
                                ISingleColorImageExtension.class)
                                .constructImage(new IRenderedImageCallback() {
                                    @Override
                                    public RenderedImage getImage()
                                            throws VizException {
                                        return bImage;
                                    }
                                }, null);
                        if (plotCreator.isCachingImages()) {
                            imageCache.put(bImage, image);
                        }
                    }
                }
                caller.modelGenerated(infos, image);
            } catch (Exception e) {
                statusHandler.error("Error creating plot", e);
            }
        }

        plotCreator.disposeScript();
        System.out.println("Time spent creating plots: "
                + (System.currentTimeMillis() - t0));
        return Status.OK_STATUS;
    }

    protected void enqueue(PlotInfo[] task) {
        this.taskQueue.add(task);
        if (this.getState() != Job.RUNNING) {
            this.schedule();
        }
    }

    protected int getQueueSize() {
        return taskQueue.size();
    }

    protected void clearImageCache() {
        imageCache.clear();
    }

    public boolean isDone() {
        return getState() != Job.RUNNING && getState() != Job.WAITING;
    }

    protected void shutdown() {
        cancel();
        taskQueue.clear();
    }
}
