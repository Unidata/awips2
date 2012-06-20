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
import com.raytheon.viz.pointdata.thread.GetDataTask;

/**
 * Thread pool for requesting data for plots. Each data request job is tied to a
 * plot generator job.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2011            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotDataThreadPool {

    private static final int THREADS = 2;

    protected ConcurrentLinkedQueue<GetDataTask> stationQueue = new ConcurrentLinkedQueue<GetDataTask>();

    protected List<PlotModelDataRequestJob> jobList = new ArrayList<PlotModelDataRequestJob>();

    public PlotDataThreadPool(IGraphicsTarget aTarget,
            MapDescriptor mapDescriptor, String plotModelFile, String levelKey,
            String plugin, HashMap<String, RequestConstraint> constraintMap,
            IPlotModelGeneratorCaller caller) throws VizException {
        for (int i = 0; i < THREADS; i++) {
            jobList.add(new PlotModelDataRequestJob(aTarget, mapDescriptor,
                    plotModelFile, levelKey, plugin, constraintMap, caller,
                    this));
        }
    }

    /**
     * Adds a batch of stations to the queue
     * 
     * @param station
     * 
     */
    public void queueStation(GetDataTask task) {
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
            stationQueue.add(task);
            for (PlotModelDataRequestJob job : jobList) {
                if (job.getState() != Job.RUNNING) {
                    job.schedule();
                }
            }
        }

    }

    public void setPlotModelColor(RGB color) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setPlotModelColor(color);
        }
    }

    public void setPlotModelLineStyle(LineStyle lineStyle) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setPlotModelLineStyle(lineStyle);
        }
    }

    public void setPlotModelLineWidth(int outlineWidth) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setPlotModelLineWidth(outlineWidth);
        }
    }

    public void setPlotMissingData(boolean plotMissingData) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setPlotMissingData(plotMissingData);
        }
    }

    public void setLowerLimit(double lowerLimit) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setLowerLimit(lowerLimit);
        }
    }

    public void setUpperLimit(double upperLimit) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setUpperLimit(upperLimit);
        }
    }

    public double getPlotModelWidth() {
        return jobList.get(0).getPlotModelWidth();
    }

    public void setPlotModelSize(long round) {
        for (PlotModelDataRequestJob job : jobList) {
            job.setPlotModelSize(round);
        }
    }

    public boolean isDone() {
        for (PlotModelDataRequestJob job : jobList) {
            if (!job.isDone()) {
                return false;
            }
        }
        return true;
    }

    public void shutdown() {
        stationQueue.clear();
        for (PlotModelDataRequestJob job : jobList) {
            job.shutdown();
        }
    }

}
