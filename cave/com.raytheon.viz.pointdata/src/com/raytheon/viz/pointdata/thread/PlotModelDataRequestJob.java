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
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotData;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PlotModelFactory2.PlotModelElement;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;

/**
 * Job that requests plot data based on a constraintMap and the parameters
 * specified inside the plot model SVG file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            njensen     Initial creation
 * May 14, 2013 1869       bsteffen    Get plots working without dataURI
 * Mar 21, 2014 2868       njensen     Major refactor
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotModelDataRequestJob extends AbstractPlotCreationJob {

    private Map<String, RequestConstraint> constraintMap;

    private final String plugin;

    private final String levelKey;

    private final List<PlotModelElement> plotFields;

    private final List<PlotModelElement> sampleFields;

    public PlotModelDataRequestJob(PlotThreadOverseer parent,
            IPlotModelGeneratorCaller caller,
            List<PlotModelElement> plotFields,
            List<PlotModelElement> sampleFields, String levelKey,
            String plugin, Map<String, RequestConstraint> constraintMap)
            throws VizException {
        super("Requesting Plot Data...", parent, caller);
        this.plotFields = plotFields;
        this.sampleFields = sampleFields;
        this.plugin = plugin;
        this.levelKey = levelKey;
        this.constraintMap = constraintMap;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        while (overseer.dataRetrievalQueue.size() > 0) {
            List<PlotInfo[]> stationQuery = new ArrayList<PlotInfo[]>();

            GetDataTask task = null;
            synchronized (this) {
                task = overseer.dataRetrievalQueue.poll();
                if (task == null) {
                    // possibility another thread got it first
                    continue;
                }
                List<PlotInfo[]> batch = task.getStations();
                for (PlotInfo[] infos : batch) {
                    stationQuery.add(infos);
                }
            }

            List<PlotModelElement> pme = null;
            switch (task.getRequestType()) {
            case PLOT_ONLY:
                pme = plotFields;
                break;
            case SAMPLE_ONLY:
                pme = sampleFields;
                break;
            case PLOT_AND_SAMPLE:
                pme = new ArrayList<PlotModelElement>();
                pme.addAll(plotFields);
                pme.addAll(sampleFields);
            default:
                break;
            }

            // pme could be size 0 if it's sample only and there were no sample
            // parameters that weren't already part of the requested parameters
            // that have already been retrieved
            if (pme.size() > 0) {
                requestData(stationQuery, pme);
            }

            // TODO need to determine if this type of plot is a combination or
            // not
            combineData(stationQuery);
            synchronized (this) {
                if (monitor.isCanceled()) {
                    break;
                }

                for (PlotInfo[] infos : stationQuery) {
                    // schedule next work for other jobs
                    // TODO investigate further, shouldn't be possible to get a
                    // null
                    // here, but somehow we do
                    if (infos[0].pdv != null) {
                        switch (task.getRequestType()) {
                        case PLOT_ONLY:
                            overseer.enqueueImageGeneration(infos);
                            break;
                        case SAMPLE_ONLY:
                            overseer.enqueueSamplePlot(infos);
                            break;
                        case PLOT_AND_SAMPLE:
                            overseer.enqueueImageGeneration(infos);
                            overseer.enqueueSamplePlot(infos);
                            break;
                        }
                    }
                }
            }

        } // end of while !stationQueue.isEmpty()

        return Status.OK_STATUS;
    }

    private void requestData(List<PlotInfo[]> stationQuery,
            List<PlotModelElement> pme) {
        Map<String, PlotInfo> plotMap = new HashMap<String, PlotInfo>();
        List<String> params = new ArrayList<String>();

        for (PlotModelElement p : pme) {
            String param = p.getParameter();
            if (!param.equals("") && !param.contains(",")) {
                params.add(param);
            } else if (param.contains(",")) {
                String[] individualParams = param.split(",");
                for (String paramToRequest : individualParams) {
                    params.add(paramToRequest);
                }
            }
        }

        boolean hasDistinctStationId = PlotResourceData
                .getPluginProperties(plugin).hasDistinctStationId;
        String uniquePointDataKey = "stationId";
        String uniqueQueryKey = "location.stationId";
        if (!hasDistinctStationId) {
            uniquePointDataKey = "dataURI";
            uniqueQueryKey = uniquePointDataKey;

        }
        if (!params.contains(uniquePointDataKey)) {
            params.add(uniquePointDataKey);
        }

        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.putAll(this.constraintMap);
        RequestConstraint rc = new RequestConstraint();
        rc.setConstraintType(ConstraintType.IN);
        List<String> str = new ArrayList<String>(stationQuery.size());
        DataTime start = null;
        DataTime end = null;
        for (PlotInfo[] infos : stationQuery) {
            for (PlotInfo info : infos) {
                String key = null;
                if (hasDistinctStationId) {
                    key = info.stationId;
                } else {
                    key = info.dataURI;
                }
                str.add(key);
                if (!plotMap.containsKey(key)) {
                    plotMap.put(key, info);
                }
                if (start == null
                        || start.getValidTime().after(
                                info.dataTime.getValidTime())) {
                    start = info.dataTime;
                }
                if (end == null
                        || end.getValidTime().before(
                                info.dataTime.getValidTime())) {
                    end = info.dataTime;
                }
            }
        }

        if (start.equals(end)) {
            map.put("dataTime", new RequestConstraint(start.toString()));
        } else {
            RequestConstraint r = new RequestConstraint(null,
                    ConstraintType.BETWEEN);
            r.setBetweenValueList(new String[] { start.toString(),
                    end.toString() });
            map.put("dataTime.refTime", r);

        }

        int index = 0;
        int j = 0;
        int numOfValues = 500;

        while (index < str.size()) {
            while (index < str.size() && j < numOfValues) {
                rc.addToConstraintValueList(str.get(index));
                index++;
                j++;
            }
            map.put(uniqueQueryKey, rc);
            try {
                // Try and get data from datacube
                long t0 = System.currentTimeMillis();
                PointDataContainer pdc = DataCubeContainer.getPointData(
                        this.plugin, params.toArray(new String[params.size()]),
                        levelKey, map);

                if (pdc == null) {
                    // Datacube didn't have proper plugin; going
                    // directly to the data store
                    pdc = PointDataRequest.requestPointDataAllLevels(
                            this.plugin,
                            params.toArray(new String[params.size()]), null,
                            map);
                }
                System.out
                        .println("Time spent waiting on server for pointdata params: "
                                + (System.currentTimeMillis() - t0));
                if (pdc != null) {
                    pdc.setCurrentSz(pdc.getAllocatedSz());
                    for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                        PointDataView pdv = pdc.readRandom(uriCounter);
                        if (pdv != null) {
                            String unique = pdv.getString(uniquePointDataKey);
                            PlotInfo info = plotMap.get(unique);
                            // If the id doesn't match, try to match by
                            // location
                            if (info == null) {
                                // TODO verify if any code is still
                                // using this or if it's dead
                                for (PlotInfo pi : plotMap.values()) {
                                    double diffLat = Math.abs(pi.latitude
                                            - pdv.getFloat("latitude"));
                                    double diffLon = Math.abs(pi.longitude
                                            - pdv.getFloat("longitude"));
                                    if (diffLat < 0.01 && diffLon < 0.01) {
                                        info = pi;
                                    }
                                }
                            }
                            if (info != null) {
                                synchronized (info) {
                                    if (info.pdv == null) {
                                        info.pdv = new PlotData();
                                    }
                                    info.pdv.addData(pdv);
                                }
                            }
                        }
                    }
                }
            } catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error in Point Data request.", e1);
            }
            // reset in case there's more
            j = 0;
            rc.setConstraintValue(null);
        }

    }

    private void combineData(List<PlotInfo[]> stationQuery) {
        for (PlotInfo[] infos : stationQuery) {
            synchronized (infos) {
                PlotData pd = null;
                for (PlotInfo i : infos) {
                    if (pd == null) {
                        pd = i.pdv;
                        continue;
                    }
                    if (i.pdv != null) {
                        pd.addData(i.pdv);
                        i.pdv = null; // free the memory since we just combined
                                      // them
                    }
                }
                if (pd != null) {
                    infos[0].pdv = pd;
                }
            }
        }
    }

}
