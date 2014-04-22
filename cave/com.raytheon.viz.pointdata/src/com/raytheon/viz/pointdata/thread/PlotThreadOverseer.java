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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PlotModelFactory2;
import com.raytheon.viz.pointdata.PlotModelFactory2.PlotModelElement;

/**
 * Oversees a variety of threads used to concurrently and quickly get plots on
 * screen. There should be only one PlotThreadOverseer for each PlotResource2.
 * 
 * This class oversees the pipeline of threads necessary for requesting plot
 * data, generating plot images, and generating plot samples. Note that it does
 * not involve itself with progressive disclosure, paint, or python threads.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2014 2868       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotThreadOverseer {

    private static final int DATA_THREADS = 2;

    private static final int IMAGE_THREADS = DATA_THREADS;

    private static final int SAMPLE_THREADS = 1;

    protected ConcurrentLinkedQueue<GetDataTask> dataRetrievalQueue = new ConcurrentLinkedQueue<GetDataTask>();

    protected List<PlotModelDataRequestJob> dataRetrievalJobList = new ArrayList<PlotModelDataRequestJob>(
            DATA_THREADS);

    protected ConcurrentLinkedQueue<PlotInfo[]> imageCreationQueue = new ConcurrentLinkedQueue<PlotInfo[]>();

    protected List<PlotModelGeneratorJob> imageCreationJobList = new ArrayList<PlotModelGeneratorJob>(
            IMAGE_THREADS);

    protected List<PlotModelFactory2> plotCreatorList = new ArrayList<PlotModelFactory2>(
            IMAGE_THREADS);

    protected ConcurrentLinkedQueue<PlotInfo[]> sampleTextQueue = new ConcurrentLinkedQueue<PlotInfo[]>();

    protected List<PlotSampleGeneratorJob> sampleTextJobList = new ArrayList<PlotSampleGeneratorJob>(
            SAMPLE_THREADS);

    private int plotModelWidth;

    /**
     * Constructor
     * 
     * @param target
     * @param mapDescriptor
     * @param plotModelFile
     * @param levelKey
     * @param plugin
     * @param constraintMap
     * @param caller
     * @throws VizException
     */
    public PlotThreadOverseer(IGraphicsTarget target,
            MapDescriptor mapDescriptor, String plotModelFile, String levelKey,
            String plugin, HashMap<String, RequestConstraint> constraintMap,
            IPlotModelGeneratorCaller caller) throws VizException {

        /*
         * Set up image generation jobs first because the PlotModelFactory2
         * initialization will parse the SVG to determine the parameters to
         * request
         */
        List<PlotModelElement> plotFields = null;
        List<PlotModelElement> sampleFields = null;
        for (int i = 0; i < IMAGE_THREADS; i++) {
            PlotModelFactory2 plotModelFactory = new PlotModelFactory2(
                    mapDescriptor, plotModelFile);
            plotCreatorList.add(plotModelFactory);
            imageCreationJobList.add(new PlotModelGeneratorJob(this, caller,
                    plotModelFactory, target));
            if (plotFields == null) {
                /*
                 * data retrieval jobs need to know the plot and sample fields
                 * to request, which have now been parsed out of the SVG
                 */
                plotFields = plotModelFactory.getPlotFields();
                sampleFields = plotModelFactory.getSampleFields();
                plotModelWidth = plotModelFactory.getDefinedPlotModelWidth();
            }
        }

        /*
         * Set up the data retrieval jobs
         */
        for (int i = 0; i < DATA_THREADS; i++) {
            dataRetrievalJobList.add(new PlotModelDataRequestJob(this, caller,
                    plotFields, sampleFields, levelKey, plugin, constraintMap));
        }

        /*
         * Set up the sample jobs. We could probably reuse the plot model
         * factories of the image jobs, regardless, if it's a sample that uses
         * python it will reuse the python job coordinator underneath, and if
         * it's not...it's not clear if
         * PlotModelFactory2.processSampleDirective() is thread safe
         * 
         * TODO do we care?
         */
        for (int i = 0; i < SAMPLE_THREADS; i++) {
            PlotModelFactory2 plotModelFactory = new PlotModelFactory2(
                    mapDescriptor, plotModelFile);
            sampleTextJobList.add(new PlotSampleGeneratorJob(this, caller,
                    plotModelFactory));
        }
    }

    /**
     * Enqueues a task of a batch of stations to retrieve data for
     * 
     * @param task
     * 
     */
    public void enqueueDataRetrieval(GetDataTask task) {
        List<PlotInfo[]> station = task.getStations();

        Iterator<PlotInfo[]> itr = station.iterator();
        while (itr.hasNext()) {
            PlotInfo[] infos = itr.next();
            boolean allQueued = true;
            for (PlotInfo info : infos) {
                switch (task.getRequestType()) {
                case PLOT_ONLY:
                    if (!info.plotQueued) {
                        allQueued = false;
                        info.plotQueued = true;
                    }

                    break;
                case SAMPLE_ONLY:
                    if (!info.sampleQueued) {
                        allQueued = false;
                        info.sampleQueued = true;
                    }
                    break;
                case PLOT_AND_SAMPLE:
                    if (!info.sampleQueued) {
                        allQueued = false;
                        info.sampleQueued = true;
                    }
                    if (!info.plotQueued) {
                        allQueued = false;
                        info.plotQueued = true;
                    }
                }
            }
            if (allQueued) {
                itr.remove();
            }
        }

        if (station.size() > 0) {
            task.setStations(station);
            dataRetrievalQueue.add(task);
            for (PlotModelDataRequestJob job : dataRetrievalJobList) {
                if (job.getState() != Job.RUNNING) {
                    job.schedule();
                }
            }
        }
    }

    /**
     * Adds a batch of PlotInfos to generate images for
     * 
     * @param plotInfo
     */
    public void enqueueImageGeneration(PlotInfo[] plotInfo) {
        if (plotInfo.length > 0) {
            imageCreationQueue.add(plotInfo);
            for (PlotModelGeneratorJob job : imageCreationJobList) {
                if (job.getState() != Job.RUNNING) {
                    job.schedule();
                }
            }
        }
    }

    /**
     * Adds a batch of PlotInfos to generate sample messages for
     * 
     * @param plotInfo
     */
    public void enqueueSamplePlot(PlotInfo[] plotInfo) {
        if (plotInfo.length > 0) {
            sampleTextQueue.add(plotInfo);
            for (PlotSampleGeneratorJob job : sampleTextJobList) {
                if (job.getState() != Job.RUNNING) {
                    job.schedule();
                }
            }
        }
    }

    public void setPlotModelColor(RGB color) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setColor(color);
        }
        // don't clear the image cache, it's magic
    }

    public void setPlotModelLineStyle(LineStyle lineStyle) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setLineStyle(lineStyle);
        }
        for (PlotModelGeneratorJob job : imageCreationJobList) {
            job.clearImageCache();
        }
    }

    public void setPlotModelLineWidth(int outlineWidth) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setLineWidth(outlineWidth);
        }
        for (PlotModelGeneratorJob job : imageCreationJobList) {
            job.clearImageCache();
        }
    }

    public void setPlotMissingData(boolean plotMissingData) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setPlotMissingData(plotMissingData);
        }
    }

    public void setLowerLimit(double lowerLimit) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setLowerLimit(lowerLimit);
        }
    }

    public void setUpperLimit(double upperLimit) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setUpperLimit(upperLimit);
        }
    }

    public double getOriginalPlotModelWidth() {
        return plotModelWidth;
    }

    public void setPlotModelSize(long round) {
        for (PlotModelFactory2 pmf : plotCreatorList) {
            pmf.setPlotDimensions(round, round);
        }
        for (PlotModelGeneratorJob job : imageCreationJobList) {
            job.clearImageCache();
        }
    }

    /**
     * Checks if all the jobs related to requesting data, creating plot images,
     * and creating samples are done
     * 
     * @return
     */
    public boolean isDone() {
        for (AbstractPlotCreationJob job : dataRetrievalJobList) {
            if (!job.isDone()) {
                return false;
            }
        }

        for (AbstractPlotCreationJob job : imageCreationJobList) {
            if (!job.isDone()) {
                return false;
            }
        }

        for (AbstractPlotCreationJob job : sampleTextJobList) {
            if (!job.isDone()) {
                return false;
            }
        }

        return true;
    }

    /**
     * Shuts down all the threads and clears their queues in a safe manner
     */
    public void shutdown() {
        /*
         * by clearing the job lists first, nothing else can get queued, but we
         * still need a copy of them to ensure they are shut down correctly
         */
        List<AbstractPlotCreationJob> jobListCopy = null;

        // shut down data requests first
        jobListCopy = new ArrayList<AbstractPlotCreationJob>(
                dataRetrievalJobList);
        dataRetrievalJobList.clear();
        for (AbstractPlotCreationJob job : jobListCopy) {
            job.shutdown();
        }
        dataRetrievalQueue.clear();

        // then shut down images
        jobListCopy = new ArrayList<AbstractPlotCreationJob>(
                imageCreationJobList);
        imageCreationJobList.clear();
        for (AbstractPlotCreationJob job : jobListCopy) {
            job.shutdown();
        }
        imageCreationQueue.clear();

        // and finally sampling
        jobListCopy = new ArrayList<AbstractPlotCreationJob>(sampleTextJobList);
        sampleTextJobList.clear();
        for (AbstractPlotCreationJob job : jobListCopy) {
            job.shutdown();
        }
        sampleTextQueue.clear();
    }

}
