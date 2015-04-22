package gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels;
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

import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotInfo;

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
 * 09/2012      896        sgurung     Refactored raytheon's class PlotDataThreadPool
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcPlotDataThreadPool {

    private static final int THREADS = 2;

    protected ConcurrentLinkedQueue<List<PlotInfo>> stationQueue = new ConcurrentLinkedQueue<List<PlotInfo>>();

    protected List<NcPlotModelDataRequestJob> jobList = new ArrayList<NcPlotModelDataRequestJob>();

    public NcPlotDataThreadPool(IGraphicsTarget aTarget,
    		IMapDescriptor mapDescriptor, PlotModel plotModel, String levelKey,
    		HashMap<String, RequestConstraint> constraintMap,
            ConditionalFilter condFilter,
            IPlotModelGeneratorCaller caller) throws VizException {
        for (int i = 0; i < THREADS; i++) {
            jobList.add(new NcPlotModelDataRequestJob(aTarget, mapDescriptor,
            		plotModel, levelKey, plotModel.getPlugin(), constraintMap, condFilter, caller,
                    this));
        }
    }

    /**
     * Adds a batch of stations to the queue
     * 
     * @param station
     * 
     */
    public void queueStation(List<PlotInfo> stations) {
    	
        Iterator<PlotInfo> itr = stations.iterator();
        while (itr.hasNext()) {
            PlotInfo info = itr.next();
            boolean allQueued = true;
            if (!info.plotQueued) {
                allQueued = false;
                info.plotQueued = true;
            }
            if (allQueued) {
                itr.remove();
            }
        }

        if (stations.size() > 0) {
            stationQueue.add(stations);
            int i = 0;
            for (NcPlotModelDataRequestJob job : jobList) {
                if (job.getState() != Job.RUNNING) {
                    job.schedule();
                }
                i++;
            }
        }
    }

    public void setPlotMissingData(boolean plotMissingData) {
        for (NcPlotModelDataRequestJob job : jobList) {
            job.setPlotMissingData(plotMissingData);
        }
    }

    public double getPlotModelWidth() {
        return jobList.get(0).getPlotModelWidth();
    }

    public void setPlotModelSize(long round) {
        for (NcPlotModelDataRequestJob job : jobList) {
            job.setPlotModelSize(round);
        }
    }

    public void shutdown() {
        stationQueue.clear();
        for (NcPlotModelDataRequestJob job : jobList) {
            job.shutdown();
        }
    }
    

}
