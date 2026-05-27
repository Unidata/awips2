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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.pointdata.IPlotModelElement;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotData;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.PlotModelFactory.DisplayType;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;
import com.raytheon.viz.pointdata.util.MetarPrecipDataContainer;
import com.raytheon.viz.pointdata.util.MetarPrecipDataContainer.PrecipData;

/**
 * Job that requests plot data based on a constraintMap and the parameters
 * specified inside the plot model SVG file.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 22, 2011           njensen   Initial creation
 * May 14, 2013  1869     bsteffen  Get plots working without dataURI
 * Mar 21, 2014  2868     njensen   Major refactor
 * Jun 06, 2014  2061     bsteffen  Remove old PlotResource
 * Nov 01, 2019  71272    ksunil    tweaks to accommodate new plot customization
 *                                  changes
 * Mar 24, 2020  75529    ksunil    added code to handle metarPrcp fields
 * Apr 10, 2020  77336    ksunil    Minor code enhancement + don't send MARKER
 *                                  type to PDC container
 * Dec 07, 2021  8341     randerso  Move plot performance logging into perf log.
 *                                  Add additional info to performance logging.
 *
 * </pre>
 *
 * @author njensen
 */

public class PlotModelDataRequestJob extends AbstractPlotCreationJob {

    private final String plugin;

    private final List<IPlotModelElement> plotFields;

    private final List<IPlotModelElement> sampleFields;

    private final PlotResourceData rData;

    public PlotModelDataRequestJob(PlotThreadOverseer parent,
            IPlotModelGeneratorCaller caller,
            List<IPlotModelElement> plotFields,
            List<IPlotModelElement> sampleFields, String plugin,
            PlotResourceData rData)

            throws VizException {
        super("Requesting Plot Data...", parent, caller);
        this.plotFields = plotFields;
        this.sampleFields = sampleFields;
        this.plugin = plugin;
        this.rData = rData;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        while (!overseer.dataRetrievalQueue.isEmpty()) {
            List<PlotInfo[]> stationQuery = new ArrayList<>();

            GetDataTask task = null;
            synchronized (this) {
                task = overseer.dataRetrievalQueue.poll();
                if (task == null) {
                    /* possibility another thread got it first */
                    continue;
                }
                List<PlotInfo[]> batch = task.getStations();
                stationQuery.addAll(batch);
            }

            List<IPlotModelElement> pme = null;
            switch (task.getRequestType()) {
            case PLOT_ONLY:
                pme = plotFields;
                break;
            case SAMPLE_ONLY:
                pme = sampleFields;
                break;
            case PLOT_AND_SAMPLE:
                pme = new ArrayList<>(plotFields);
                pme.addAll(sampleFields);
                break;
            default:
                break;
            }

            /*
             * pme could be size 0 if it's sample only and there were no sample
             * parameters that weren't already part of the requested parameters
             * that have already been retrieved
             */
            if (!pme.isEmpty()) {
                /*
                 * Now split the PMEs into 2 lots. One for metarPrcp data and
                 * the other for regular
                 */
                List<IPlotModelElement> regularPme = new ArrayList<>();
                List<IPlotModelElement> metarPme = new ArrayList<>();
                for (IPlotModelElement element : pme) {
                    if (!StringUtils
                            .isEmpty(element.getParamDef().getPtype())) {
                        metarPme.add(element);
                    } else if (element.getParamDef()
                            .getDisplayType() != DisplayType.MARKER) {
                        regularPme.add(element);
                    }
                }
                if (!regularPme.isEmpty()) {
                    requestData(stationQuery, regularPme);
                }
                if (!metarPme.isEmpty()) {
                    try {
                        requestMetarPrcpData(stationQuery, metarPme);
                    } catch (VizException e) {
                        statusHandler.error(
                                "Metar Precip data request exception", e);
                    }
                }
            }
            combineData(stationQuery);
            synchronized (this) {
                if (monitor.isCanceled()) {
                    break;
                }

                for (PlotInfo[] infos : stationQuery) {
                    /* schedule next work for other jobs */

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

        }

        return Status.OK_STATUS;
    }

    private void requestData(List<PlotInfo[]> stationQuery,
            List<IPlotModelElement> pme) {
        Map<String, PlotInfo> plotMap = new HashMap<>();
        List<String> params = new ArrayList<>();

        for (IPlotModelElement p : pme) {
            String param = p.getParam();

            if (!StringUtils.isEmpty(param)) {
                if (param.contains(",")) {
                    String[] individualParams = param.split(",");
                    Collections.addAll(params, individualParams);
                } else {
                    params.add(param);
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

        Map<String, RequestConstraint> map = new HashMap<>(
                rData.getMetadataMap());
        RequestConstraint rc = new RequestConstraint();
        rc.setConstraintType(ConstraintType.IN);
        List<String> str = new ArrayList<>(stationQuery.size());
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
                if (start == null || start.getValidTime()
                        .after(info.dataTime.getValidTime())) {
                    start = info.dataTime;
                }
                if (end == null || end.getValidTime()
                        .before(info.dataTime.getValidTime())) {
                    end = info.dataTime;
                }
            }
        }

        if (start.equals(end)) {
            map.put("dataTime", new RequestConstraint(start.toString()));
        } else {
            RequestConstraint r = new RequestConstraint(null,
                    ConstraintType.BETWEEN);
            r.setBetweenValueList(
                    new String[] { start.toString(), end.toString() });
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
                /* Try and get data from datacube */
                long t0 = System.currentTimeMillis();
                PointDataContainer pdc = DataCubeContainer.getPointData(
                        this.plugin, params.toArray(new String[params.size()]),
                        rData.getLevelKey(), map);

                if (pdc == null) {
                    /*
                     * Datacube didn't have proper plugin; going directly to the
                     * data store
                     */
                    pdc = PointDataRequest.requestPointDataAllLevels(
                            this.plugin,
                            params.toArray(new String[params.size()]), null,
                            map);
                }
                perfLog.logDuration(String.format(
                        "Retrieving pointdata params for [%s] [%s]",
                        rData.getPlotSource(), rData.getPlotModelFile()),
                        (System.currentTimeMillis() - t0));
                if (pdc != null) {
                    pdc.setCurrentSz(pdc.getAllocatedSz());
                    for (int uriCounter = 0; uriCounter < pdc
                            .getAllocatedSz(); uriCounter++) {
                        PointDataView pdv = pdc.readRandom(uriCounter);
                        if (pdv != null) {
                            String unique = pdv.getString(uniquePointDataKey);
                            PlotInfo info = plotMap.get(unique);
                            /*
                             * If the id doesn't match, try to match by location
                             */
                            if (info == null) {
                                /*
                                 * TODO: verify if any code is still using this
                                 * or if it's dead
                                 */
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
            } catch (DataCubeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error making Point Data request.", e);
            }
            /* reset in case there's more */
            j = 0;
            rc.setConstraintValue(null);
        }

    }

    private void requestMetarPrcpData(List<PlotInfo[]> stationQuery,
            List<IPlotModelElement> pme) throws VizException {

        Map<String, PlotInfo> plotMap = new HashMap<>();

        boolean hasDistinctStationId = PlotResourceData
                .getPluginProperties(plugin).hasDistinctStationId;
        String uniqueQueryKey = "location.stationId";
        if (!hasDistinctStationId) {
            throw new VizException(
                    "Metar Precip data request with no distinct stationID ");
        }

        DataTime end = null;
        Map<String, RequestConstraint> map = new HashMap<>(
                rData.getMetadataMap());
        List<String> str = new ArrayList<>(stationQuery.size());

        for (PlotInfo[] infos : stationQuery) {
            for (PlotInfo info : infos) {
                String key = null;
                if (hasDistinctStationId) {
                    key = info.stationId;
                } else {
                    key = info.dataURI;
                }
                str.add(key);
                plotMap.putIfAbsent(key, info);
                if (end == null || end.getValidTime()
                        .before(info.dataTime.getValidTime())) {
                    end = info.dataTime;
                }
            }
        }

        RequestConstraint rc = new RequestConstraint(str);
        rc.setConstraintType(ConstraintType.IN);

        map.put(uniqueQueryKey, rc);

        for (IPlotModelElement ele : pme) {

            try {
                /* Try and get data from MetarPrecipDataContainer */
                long t0 = System.currentTimeMillis();
                MetarPrecipDataContainer container = new MetarPrecipDataContainer(
                        ele.getParamDef().getDuration(),
                        ele.getParamDef().getPtype(), map);
                String paramName = ele.getParamDef().getParamName();
                String unit = ele.getParamDef().getUnit();

                List<PrecipData> baseData = container.getBasePrecipData(end);
                baseData.addAll(container.getDerivedPrecipData(end));
                perfLog.logDuration(String.format(
                        "Retrieving METAR pointdata params for [%s] [%s]",
                        rData.getPlotSource(), rData.getPlotModelFile()),

                        (System.currentTimeMillis() - t0));

                for (PrecipData data : baseData) {
                    data.setParamName(paramName);
                    data.setUnit(unit);
                    String unique = data.getStationName();
                    PlotInfo info = plotMap.get(unique);
                    if (info != null) {
                        synchronized (info) {
                            if (info.pdv == null) {
                                info.pdv = new PlotData();
                            }
                            info.pdv.addData(data);
                        }
                    }
                }
            }

            catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error in Metar Container Data request.", e1);
            }
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
                        /*
                         * free the memory since we just combined them
                         */
                        i.pdv = null;
                    }
                }
                if (pd != null) {
                    infos[0].pdv = pd;
                }
            }
        }
    }

}
