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
import com.raytheon.viz.pointdata.IPlotModelElement;
import com.raytheon.viz.pointdata.IPlotModelFactory;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PlotModelFactory;
import com.raytheon.viz.pointdata.PlotModelFactoryDefault;
import com.raytheon.viz.pointdata.def.Condition;

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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 21, 2014  2868     njensen     Initial creation
 * Jun 06, 2014  2061     bsteffen    Remove old PlotResource
 * Nov 01, 2019  71272    ksunil      tweaks to accommodate new plot
 *                                     customization changes
 * Dec 10, 2019  72280    ksunil      Added condition filter
 * Jan 07, 2020  73083    ksunil      Moved the setConditionalFilter() call from here
 *
 * </pre>
 *
 * @author njensen
 */

public class PlotThreadOverseer {

    private static final int DATA_THREADS = 2;

    private static final int IMAGE_THREADS = DATA_THREADS;

    private static final int SAMPLE_THREADS = 1;

    protected ConcurrentLinkedQueue<GetDataTask> dataRetrievalQueue = new ConcurrentLinkedQueue<>();

    protected List<PlotModelDataRequestJob> dataRetrievalJobList = new ArrayList<>(
            DATA_THREADS);

    protected ConcurrentLinkedQueue<PlotInfo[]> imageCreationQueue = new ConcurrentLinkedQueue<>();

    protected List<PlotModelGeneratorJob> imageCreationJobList = new ArrayList<>(
            IMAGE_THREADS);

    protected List<IPlotModelFactory> plotCreatorList = new ArrayList<>(
            IMAGE_THREADS);

    protected ConcurrentLinkedQueue<PlotInfo[]> sampleTextQueue = new ConcurrentLinkedQueue<>();

    protected List<PlotSampleGeneratorJob> sampleTextJobList = new ArrayList<>(
            SAMPLE_THREADS);

    private int plotModelWidth;

    private Condition filterCondition;

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
        List<IPlotModelElement> plotFields = null;
        List<IPlotModelElement> sampleFields = null;
        boolean newType = PlotModelFactory.isNewSVGFormat(plotModelFile);

        for (int i = 0; i < IMAGE_THREADS; i++) {
            IPlotModelFactory plotModelFactory = null;

            if (newType) {
                plotModelFactory = new PlotModelFactory(mapDescriptor,
                        plotModelFile);
            } else {
                plotModelFactory = new PlotModelFactoryDefault(mapDescriptor,
                        plotModelFile);
            }

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
        newType = PlotModelFactory.isNewSVGFormat(plotModelFile);

        for (int i = 0; i < SAMPLE_THREADS; i++) {
            IPlotModelFactory plotModelFactorySample = null;
            if (newType) {
                plotModelFactorySample = new PlotModelFactory(mapDescriptor,
                        plotModelFile);
                plotModelFactorySample.setConditionFilter(filterCondition);
            } else {
                plotModelFactorySample = new PlotModelFactoryDefault(
                        mapDescriptor, plotModelFile);
            }
            sampleTextJobList.add(new PlotSampleGeneratorJob(this, caller,
                    plotModelFactorySample));
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

    public void clearImages() {
        for (PlotModelGeneratorJob job : imageCreationJobList) {
            job.clearImageCache();
        }

        // TODO these weren't cleared before...is it needed?
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.clearImageCache();
        }
    }

    public void setPlotModelColor(RGB color) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setColor(color);
        }
        clearImages();
    }

    public void setPlotModelLineStyle(LineStyle lineStyle) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setLineStyle(lineStyle);
        }
        clearImages();
    }

    public void setPlotModelLineWidth(int outlineWidth) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setLineWidth(outlineWidth);
        }
        clearImages();
    }

    public void setPlotMissingData(boolean plotMissingData) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setPlotMissingData(plotMissingData);
        }
    }

    public void setLowerLimit(double lowerLimit) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setLowerLimit(lowerLimit);
        }
    }

    public void setUpperLimit(double upperLimit) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setUpperLimit(upperLimit);
        }
    }

    public double getOriginalPlotModelWidth() {
        return plotModelWidth;
    }

    public void setPlotModelSize(long round) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setPlotDimensions(round, round);
        }
        clearImages();
    }

    public void setFilterCondition(Condition filterCondition) {
        for (IPlotModelFactory pmf : plotCreatorList) {
            pmf.setConditionFilter(filterCondition);
        }
        clearImages();
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
        jobListCopy = new ArrayList<>(dataRetrievalJobList);
        dataRetrievalJobList.clear();
        for (AbstractPlotCreationJob job : jobListCopy) {
            job.shutdown();
        }
        dataRetrievalQueue.clear();

        // then shut down images
        jobListCopy = new ArrayList<>(imageCreationJobList);
        imageCreationJobList.clear();
        for (AbstractPlotCreationJob job : jobListCopy) {
            job.shutdown();
        }
        imageCreationQueue.clear();

        // and finally sampling
        jobListCopy = new ArrayList<>(sampleTextJobList);
        sampleTextJobList.clear();
        for (AbstractPlotCreationJob job : jobListCopy) {
            job.shutdown();
        }
        sampleTextQueue.clear();
    }
}
