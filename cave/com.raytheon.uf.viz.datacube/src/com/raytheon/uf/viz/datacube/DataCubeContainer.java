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
package com.raytheon.uf.viz.datacube;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;

/**
 * The DataCubeContainer is responsible for handling requests for data times,
 * the records of data, and the raw data. This class also maintains a cache of
 * records in memory for ten minutes from the last time it was requested. This
 * class is an initial step towards derived parameters and will be generalized
 * in a future release. For now, it is heavily oriented towards grids.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 22, 2008             brockwoo    Initial creation
 * Sep  9, 2013       2277  mschenke    Got rid of ScriptCreator references
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class DataCubeContainer {

    private static Map<String, DataCubeContainer> containers = new HashMap<String, DataCubeContainer>();

    private IDataCubeAdapter adapter;

    private String pluginName;

    private static DataCubeContainer getInstance(String plugin) {
        DataCubeContainer container = null;
        synchronized (DataCubeContainer.class) {
            container = containers.get(plugin);
            if (container == null) {
                container = new DataCubeContainer(plugin);
                containers.put(plugin, container);
            }
        }

        if (container.adapter != null) {
            DataCubeAdapters.getInstance().initIfNeeded(container.adapter);
        }

        return container;
    }

    private DataCubeContainer(String plugin) {
        this.pluginName = plugin;
        this.adapter = DataCubeAdapters.getInstance().getAdapterForPlugin(
                plugin);
        if (adapter == null) {
            // Construct default adapter for plugin if none found
            adapter = new DefaultDataCubeAdapter(plugin);
        }
    }

    /**
     * Returns the record associated with the plugindataobject passed to this
     * method. The actual record will be retrieved, stored into the cache and a
     * copy will be given to the requesting class.
     * 
     * @param obj
     *            The plugin data object being requested
     * @return The data record
     * @throws FileNotFoundException
     * @throws StorageException
     * @throws VizDataCubeException
     */
    public static IDataRecord[] getDataRecord(PluginDataObject obj)
            throws DataCubeException {
        return getInstance(obj.getPluginName()).adapter.getRecord(obj);
    }

    /**
     * Returns the record associated with the plugindataobject passed to this
     * method. The actual record will be retrieved, stored into the cache and a
     * copy will be given to the requesting class.
     * 
     * @param obj
     *            The plugin data object being requested
     * @param obj
     *            The plugin data object being requested
     * @return The data record
     * @throws FileNotFoundException
     * @throws StorageException
     * @throws VizDataCubeException
     */
    public static IDataRecord[] getDataRecord(PluginDataObject obj,
            Request req, String dataset) throws DataCubeException {
        return getInstance(obj.getPluginName()).adapter.getRecord(obj, req,
                dataset);
    }

    /**
     * For each PluginDataObject requests the DataRecords specified by Request
     * and dataSet and stores those DataRecords in the PluginDataObject message
     * Data.This method is very similar to getDataRecord but plugins might
     * provide optimizations to make retrieval faster.
     * 
     * @param objs
     * @param req
     * @param dataset
     * @throws VizDataCubeException
     */
    public static void getDataRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws DataCubeException {
        if (objs == null || objs.isEmpty()) {
            return;
        }
        String pluginName = objs.get(0).getPluginName();
        if (pluginName == null) {
            throw new IllegalArgumentException(
                    "PluginDataObjects must have a plugin name set.");
        }
        for (PluginDataObject obj : objs) {
            if (!pluginName.equals(obj.getPluginName())) {
                throw new IllegalArgumentException(
                        "All PluginDataObjects must be for the same plugin");
            }
        }
        getInstance(pluginName).adapter.getRecords(objs, req, dataset);
    }

    public static PointDataContainer getPointData(String plugin,
            String[] params, Map<String, RequestConstraint> map)
            throws DataCubeException {
        DataCubeContainer container = getInstance(plugin);
        return container.adapter.getPoints(container.pluginName, params, map);
    }

    public static PointDataContainer getPointData(String plugin,
            String[] params, String levelKey, Map<String, RequestConstraint> map)
            throws DataCubeException {
        DataCubeContainer container = getInstance(plugin);
        if (levelKey == null) {
            return getPointData(container.pluginName, params, map);
        }
        return container.adapter.getPoints(container.pluginName, params,
                levelKey, map);
    }

    public static DataTime[] performTimeQuery(
            Map<String, RequestConstraint> queryParams, boolean latestOnly)
            throws DataCubeException {
        return performTimeQuery(queryParams, latestOnly, null);
    }

    /**
     * Examines the query parameters and returns the times available for that
     * map. In the case of a derived parameter, this method will analyze the
     * required base parameters and compute the valid time range.
     * 
     * @param queryParams
     *            A map of request constraints
     * @return an array of datatimes
     * @throws VizException
     */
    public static DataTime[] performTimeQuery(
            Map<String, RequestConstraint> queryParams, boolean latestOnly,
            BinOffset binOffset) throws DataCubeException {
        TimeQueryRequest request = new TimeQueryRequest();
        request.setQueryTerms(queryParams);
        request.setBinOffset(binOffset);
        request.setMaxQuery(latestOnly);
        String pluginName = null;
        RequestConstraint plugin = queryParams.get("pluginName");
        if (plugin != null) {
            pluginName = plugin.getConstraintValue();
        }
        request.setPluginName(pluginName);
        return performTimeQueries(pluginName, request).get(0).toArray(
                new DataTime[0]);
    }

    /**
     * Perform a bulk time query request when all requests have the same plugin
     * type.
     * 
     * @param requests
     * @return
     */
    public static List<List<DataTime>> performTimeQueries(String pluginName,
            List<TimeQueryRequest> requests) throws DataCubeException {
        if (requests.isEmpty()) {
            return Collections.emptyList();
        }
        return getInstance(pluginName).adapter.timeQuery(requests);
    }

    /**
     * Perform a bulk time query request when all requests have the same plugin
     * type.
     * 
     * @param requests
     * @return
     */
    public static List<List<DataTime>> performTimeQueries(String pluginName,
            TimeQueryRequest... requests) throws DataCubeException {
        return getInstance(pluginName).adapter.timeQuery(Arrays
                .asList(requests));
    }

    /**
     * Perform a bulk time query request, which may be faster than individual
     * requests. If it is known that all requests have the same plugin type,
     * then call the version of this method which also takes a String arg.
     * 
     * @param requests
     * @return
     */
    public static List<List<DataTime>> performTimeQueries(
            TimeQueryRequest... requests) throws DataCubeException {
        return performTimeQueries(Arrays.asList(requests));
    }

    /**
     * Perform a bulk time query request, which may be faster than individual
     * requests. If it is known that all requests have the same plugin type,
     * then call the version of this method which also takes a String arg.
     * 
     * @param requests
     * @return
     */
    public static List<List<DataTime>> performTimeQueries(
            List<TimeQueryRequest> requests) throws DataCubeException {
        Map<String, List<TimeQueryRequest>> requestMap = new HashMap<String, List<TimeQueryRequest>>();
        for (TimeQueryRequest request : requests) {
            String pluginName = request.getPluginName();
            List<TimeQueryRequest> requestList = requestMap.get(pluginName);
            if (requestList == null) {
                requestList = new ArrayList<TimeQueryRequest>();
                requestMap.put(pluginName, requestList);
            }
            requestList.add(request);
        }
        Map<TimeQueryRequest, List<DataTime>> resultMap = new HashMap<TimeQueryRequest, List<DataTime>>();
        for (Entry<String, List<TimeQueryRequest>> entry : requestMap
                .entrySet()) {
            List<TimeQueryRequest> requestList = entry.getValue();
            List<List<DataTime>> times = performTimeQueries(entry.getKey(),
                    requestList);
            for (int i = 0; i < times.size(); i++) {
                TimeQueryRequest request = requestList.get(i);
                List<DataTime> timeList = times.get(i);
                resultMap.put(request, timeList);
            }
        }
        List<List<DataTime>> result = new ArrayList<List<DataTime>>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            result.add(resultMap.get(request));
        }
        return result;
    }

    /**
     * Returns {@link PluginDataObject}s for the specified request constraints
     * 
     * @param constraints
     * @return
     * @throws VizException
     */
    public static PluginDataObject[] getData(
            Map<String, RequestConstraint> constraints)
            throws DataCubeException {
        return getData(constraints, (DataTime[]) null);
    }

    /**
     * Returns {@link PluginDataObject}s for the specified request constraints
     * and selected time
     * 
     * @param constraints
     * @return
     * @throws VizException
     */
    public static PluginDataObject[] getData(
            Map<String, RequestConstraint> constraints, DataTime time)
            throws DataCubeException {
        return getData(constraints, time != null ? new DataTime[] { time }
                : null);
    }

    /**
     * Returns {@link PluginDataObject}s for the specified request constraints
     * and selected times
     * 
     * @param constraints
     * @return
     * @throws VizException
     */
    public static PluginDataObject[] getData(
            Map<String, RequestConstraint> constraints, DataTime[] times)
            throws DataCubeException {
        String pluginName = constraints.get(PluginDataObject.PLUGIN_NAME_ID)
                .getConstraintValue();
        return getInstance(pluginName).adapter.getData(constraints, times);
    }

    public static Object getInventory(String plugin) {
        return getInstance(plugin).adapter.getInventory();
    }

    public static List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        RequestConstraint pluginRC = constraints
                .get(PluginDataObject.PLUGIN_NAME_ID);
        String plugin = null;
        if (pluginRC != null
                && pluginRC.getConstraintType() == ConstraintType.EQUALS) {
            plugin = pluginRC.getConstraintValue();
        }
        return getInstance(plugin).adapter
                .getBaseUpdateConstraints(constraints);
    }

}
