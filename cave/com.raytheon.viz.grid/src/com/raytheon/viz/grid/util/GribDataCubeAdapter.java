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
package com.raytheon.viz.grid.util;

import java.awt.Point;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequestSet;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.CubeUtil;
import com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode.Dependency;
import com.raytheon.viz.grid.data.GribRequestableData;
import com.raytheon.viz.grid.inv.GridInventory;
import com.raytheon.viz.grid.record.RequestableDataRecord;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009            brockwoo    Initial creation
 * Nov 21, 2009 #3576      rjpeter     Refactored use of DerivParamDesc.
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class GribDataCubeAdapter implements IDataCubeAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribDataCubeAdapter.class);

    private static final String DERIVED = "DERIVED";

    private GridInventory gridInventory;

    protected void getTimeQuery(
            AbstractRequestableLevelNode req,
            boolean latestOnly,
            LinkedHashMap<AbstractRequestableLevelNode, TimeQueryRequest> queries,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        List<Dependency> depends = req.getDependencies();
        if (depends.isEmpty()) {
            // is source node
            TimeQueryRequest myQ = req.getTimeQuery(latestOnly, cache,
                    latestOnlyCache);
            if (myQ != null) {
                // no need to merge timeQueries
                queries.put(req, myQ);
            }
        } else {
            for (Dependency dep : depends) {
                // TODO: Optimize dTime/fTime to use bulk query mechanism,
                // small case that is a not easy to get right with a bulk
                // query
                if (dep.timeOffset == 0 || !latestOnly) {
                    getTimeQuery(dep.node, latestOnly, queries, cache,
                            latestOnlyCache);
                }
            }
        }
    }

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "grib" };
    }

    @Override
    public String recordKeyGenerator(PluginDataObject pdo) {
        if (pdo instanceof GribRecord) {
            GribModel modelInfo = ((GribRecord) pdo).getModelInfo();
            return DERIVED + modelInfo.getModelName()
                    + modelInfo.getParameterName() + modelInfo.getLevelName()
                    + modelInfo.getLevelInfo() + pdo.getDataTime().toString();
        }
        return null;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws VizDataCubeException {
        return getRecord(obj, Request.ALL, null);

    }

    @Override
    public List<Object> getData(LayerProperty property, int timeOut)
            throws VizException {
        List<AbstractRequestableLevelNode> requests = gridInventory
                .evaluateRequestConstraints(property
                        .getEntryQueryParameters(false));
        int mapSize = (int) (requests.size() * 1.25) + 1;
        Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache = new HashMap<AbstractRequestableLevelNode, List<AbstractRequestableData>>(
                mapSize);
        LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest> queries = new LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest>(
                mapSize);
        for (AbstractRequestableLevelNode req : requests) {
            getDataQuery(req, property, timeOut, queries, cache);
        }
        DbQueryRequestSet reqSet = new DbQueryRequestSet();
        reqSet.setQueries(queries.values().toArray(
                new DbQueryRequest[queries.size()]));
        DbQueryResponseSet qSetResponse = (DbQueryResponseSet) ThriftClient
                .sendRequest(reqSet);
        DbQueryResponse[] qResponses = qSetResponse.getResults();
        int index = 0;
        for (AbstractRequestableLevelNode node : queries.keySet()) {
            // put results into cache
            node.setDataQueryResults(qResponses[index++], cache);
        }

        // pull the actual results from the cache
        List<AbstractRequestableData> requesters = new ArrayList<AbstractRequestableData>(
                requests.size());
        for (AbstractRequestableLevelNode request : requests) {
            requesters.addAll(request.getData(property, timeOut, cache));
        }

        List<Object> results = new ArrayList<Object>(requesters.size());

        for (AbstractRequestableData requester : requesters) {
            if (requester.getDataTime() == null) {
                DataTime[] entryTime = property.getSelectedEntryTime();
                if (entryTime != null && entryTime.length > 0) {
                    List<DataTime> entryTimes = new ArrayList<DataTime>(
                            Arrays.asList(entryTime));
                    for (DataTime time : entryTimes) {
                        GribRecord rec = new RequestableDataRecord(requester);
                        rec.setDataTime(time.clone());
                        try {
                            rec.setDataURI(null);
                            rec.constructDataURI();
                        } catch (PluginException e) {
                            throw new VizException(e);
                        }
                        boolean n = true;
                        for (Object result : results) {
                            if (((GribRecord) result).getDataURI().equals(
                                    rec.getDataURI())) {
                                n = false;
                            }
                        }
                        if (n) {
                            results.add(rec);
                        }
                    }
                } else {
                    GribRecord rec = new RequestableDataRecord(requester);
                    rec.setDataTime(new DataTime(Calendar.getInstance()));
                    results.add(rec);
                }
            } else {
                GribRecord rec = new RequestableDataRecord(requester);
                results.add(rec);
            }
        }
        if (property.getEntryQueryParameters(false).containsKey(
                GridInventory.PERT_QUERY)) {
            String pert = property.getEntryQueryParameters(false)
                    .get(GridInventory.PERT_QUERY).getConstraintValue();
            if (pert != null) {
                for (Object rec : results) {
                    ((GribRecord) rec).getModelInfo().setPerturbationNumber(
                            Integer.parseInt(pert));
                }
            }
        }
        return results;
    }

    protected void getDataQuery(
            AbstractRequestableLevelNode req,
            LayerProperty property,
            int timeOut,
            LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest> queries,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        List<Dependency> depends = req.getDependencies();
        if (depends.isEmpty()) {
            // is source node
            DbQueryRequest myQ = req.getDataQuery(property, timeOut, cache);
            if (myQ != null) {
                addDataQuery(req, myQ, queries);
            }
        } else {
            for (Dependency dep : depends) {
                // TODO: Optimize dTime/fTime to use bulk query mechanism,
                // small case that is a not easy to get right with a bulk
                // query
                if (dep.timeOffset == 0) {
                    getDataQuery(dep.node, property, timeOut, queries, cache);
                }
            }
        }
    }

    private void addDataQuery(AbstractRequestableLevelNode req,
            DbQueryRequest query,
            LinkedHashMap<AbstractRequestableLevelNode, DbQueryRequest> queries) {
        DbQueryRequest curQuery = queries.get(req);
        if (curQuery == null) {
            queries.put(req, query);
        } else {
            // merge
            // assume same DB, fields, etc, should only be different
            // time constraints since its the same node
            RequestConstraint curDTs = curQuery.getConstraints()
                    .get("dataTime");
            RequestConstraint myDTs = query.getConstraints().get("dataTime");
            if (curDTs != null && myDTs != null) {
                // only merge if both require dataTimes, otherwise one
                // would be constrained when it needs everything, also
                // assuming both to be in lists and needing to be merged
                curDTs.setConstraintType(ConstraintType.IN);
                Pattern split = Pattern.compile(",");

                String[] curVals = split.split(curDTs.getConstraintValue());
                String[] myVals = split.split(myDTs.getConstraintValue());
                HashSet<String> dups = new HashSet<String>(curVals.length
                        + myVals.length);
                dups.addAll(Arrays.asList(curVals));
                dups.addAll(Arrays.asList(myVals));
                curDTs.setConstraintValueList(dups.toArray(new String[dups
                        .size()]));
            }
        }
    }

    @Override
    public void initInventory() {
        if (gridInventory == null) {
            GridInventory gridInventory = new GridInventory();
            try {
                gridInventory.initTree(DerivedParameterGenerator
                        .getDerParLibrary());
                this.gridInventory = gridInventory;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

    }

    @Override
    public Object getInventory() {
        if (gridInventory == null) {
            GridInventory gridInventory = new GridInventory();
            try {
                gridInventory.initTree(DerivedParameterGenerator
                        .getDerParLibrary());
                this.gridInventory = gridInventory;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return gridInventory;
    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) {
        // TODO Someday we should put objective analysis code
        // into this area
        return null;
    }

    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams) {
        // TODO Someday we should put objective analysis code
        // into this area
        return null;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws VizDataCubeException {
        if (obj instanceof RequestableDataRecord) {
            try {
                getRecords(Arrays.asList(obj), req, dataset);
                IDataRecord[] result = (IDataRecord[]) obj.getMessageData();
                obj.setMessageData(null);
                return result;
            } catch (VizException e) {
                throw new VizDataCubeException("Error retrieving grid record.",
                        e);
            }
        }
        try {
            IDataRecord record = null;
            if (((GribRecord) obj).getModelInfo().getParameterAbbreviation()
                    .equals("staticTopo")) {
                IDataStore ds = DataStoreFactory.getDataStore(HDF5Util
                        .findHDF5Location(obj));
                try {
                    record = ds.retrieve("/", "staticTopo", req);
                } catch (Exception e) {
                    throw new VizException("Error retrieving staticTopo data!");
                }
            } else {
                record = CubeUtil.retrieveData(obj, obj.getPluginName(), req,
                        dataset);
            }
            return new IDataRecord[] { record };

        } catch (VizException e) {
            throw new VizDataCubeException("Error retrieving grid record.", e);
        }
    }

    private IDataRecord[] getRecord(PluginDataObject obj, Request[] requests)
            throws VizException {
        if (requests == null) {
            return ((RequestableDataRecord) obj).getDataRecord(Request.ALL);
        }
        Request retrieveRequest = requests[0];
        Request sliceRequest = requests[1];

        IDataRecord[] recs = ((RequestableDataRecord) obj)
                .getDataRecord(retrieveRequest);
        IDataRecord[] newRecs = new IDataRecord[recs.length];
        for (int i = 0; i < recs.length; i++) {
            if (recs[i] instanceof FloatDataRecord) {
                newRecs[i] = SliceUtil.slice((FloatDataRecord) recs[i],
                        sliceRequest);
            } else {
                throw new VizDataCubeException("Error processing slab of type"
                        + recs[i].getClass().getSimpleName());
            }
        }
        return newRecs;
    }

    private Request[] generateRequests(Request req, ISpatialObject area) {
        int BUFFER_WIDTH = 9;
        Request retrieveRequest;
        Request sliceRequest;
        int[] minIndex;
        int[] maxIndex;
        // We need to add a buffer region around all derived parameters
        // so that parameters which rely on neighboring points are
        // derived properly.
        switch (req.getType()) {
        case POINT:
            Point[] points = req.getPoints();
            minIndex = new int[] { Integer.MAX_VALUE, Integer.MAX_VALUE };
            maxIndex = new int[] { Integer.MIN_VALUE, Integer.MIN_VALUE };
            for (Point point : points) {
                if (minIndex[0] > point.x) {
                    minIndex[0] = point.x;
                }
                if (maxIndex[0] < point.x) {
                    maxIndex[0] = point.x;
                }
                if (minIndex[1] > point.y) {
                    minIndex[1] = point.y;
                }
                if (maxIndex[1] < point.y) {
                    maxIndex[1] = point.y;
                }
            }
            minIndex[0] -= BUFFER_WIDTH + 1;
            minIndex[1] -= BUFFER_WIDTH + 1;
            maxIndex[0] += BUFFER_WIDTH;
            maxIndex[1] += BUFFER_WIDTH;
            if (minIndex[0] < 0) {
                minIndex[0] = 0;
            }
            if (maxIndex[0] >= area.getNx()) {
                maxIndex[0] = area.getNx();
            }
            if (minIndex[1] < 0) {
                minIndex[1] = 0;
            }
            if (maxIndex[1] >= area.getNy()) {
                maxIndex[1] = area.getNy();
            }
            retrieveRequest = Request.buildSlab(minIndex, maxIndex);

            Point[] newPoints = new Point[points.length];
            for (int i = 0; i < points.length; i++) {
                newPoints[i] = (Point) points[i].clone();
                newPoints[i].x -= minIndex[0];
                newPoints[i].y -= minIndex[1];
            }
            sliceRequest = Request.buildPointRequest(newPoints);
            break;
        case SLAB:
            minIndex = req.getMinIndexForSlab();
            maxIndex = req.getMaxIndexForSlab();
            minIndex = Arrays.copyOf(minIndex, minIndex.length);
            maxIndex = Arrays.copyOf(maxIndex, maxIndex.length);
            int[] sliceMaxIndex = Arrays.copyOf(maxIndex, maxIndex.length);
            int[] sliceMinIndex = Arrays.copyOf(minIndex, minIndex.length);
            minIndex[0] -= BUFFER_WIDTH + 1;
            minIndex[1] -= BUFFER_WIDTH + 1;
            maxIndex[0] += BUFFER_WIDTH;
            maxIndex[1] += BUFFER_WIDTH;
            if (minIndex[0] < 0) {
                minIndex[0] = 0;
            }
            if (maxIndex[0] >= area.getNx()) {
                maxIndex[0] = area.getNx();
            }
            if (minIndex[1] < 0) {
                minIndex[1] = 0;
            }
            if (maxIndex[1] >= area.getNy()) {
                maxIndex[1] = area.getNy();
            }
            retrieveRequest = Request.buildSlab(minIndex, maxIndex);
            sliceMinIndex[0] -= minIndex[0];
            sliceMinIndex[1] -= minIndex[1];
            sliceMaxIndex[0] -= minIndex[0];
            sliceMaxIndex[1] -= minIndex[1];
            sliceRequest = Request.buildSlab(sliceMinIndex, sliceMaxIndex);
            break;
        case XLINE:
        case YLINE:
            // TODO this is very inefficient, Should make a buffer
            // around each line
            retrieveRequest = Request.ALL;
            sliceRequest = req;
            break;
        case ALL:
        default:
            return null;
        }
        return new Request[] { retrieveRequest, sliceRequest };
    }

    /**
     * Attempts to travel through all the derived levels and prefetch all the
     * grib records since a single bulk hdf5 read should be faster than lots of
     * little reads.
     */
    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws VizDataCubeException {
        Set<GribRequestableData> realData = new HashSet<GribRequestableData>();
        ISpatialObject area = null;
        for (PluginDataObject obj : objs) {
            if (area == null) {
                area = ((ISpatialEnabled) obj).getSpatialObject();
            }
            if (obj instanceof RequestableDataRecord) {
                realData.addAll(((RequestableDataRecord) obj).getGribRequests());
            }
        }

        Map<String, List<GribRequestableData>> fileMap = new HashMap<String, List<GribRequestableData>>();
        for (GribRequestableData data : realData) {
            if (data.getGribSource().getModelInfo().getParameterAbbreviation()
                    .equals("staticTopo")) {
                continue;
            }
            GribRecord record = data.getGribSource();
            area = record.getSpatialObject();
            String file = HDF5Util.findHDF5Location(record).getAbsolutePath();
            if (file != null) {
                List<GribRequestableData> list = fileMap.get(file);
                if (list == null) {
                    list = new LinkedList<GribRequestableData>();
                    fileMap.put(file, list);
                }
                list.add(data);
            }
        }
        Request[] requests = generateRequests(req, area);
        Request request = null;
        if (requests == null) {
            request = Request.ALL;
        } else {
            request = requests[0];
        }

        List<IDataRecord[]> references = new ArrayList<IDataRecord[]>(
                realData.size());
        if (!realData.isEmpty()) {
            // The values are held weakly in cache, so we hold them strongly
            // here to prevent them from getting garbage collected to soon
            for (Entry<String, List<GribRequestableData>> entry : fileMap
                    .entrySet()) {
                List<GribRequestableData> list = entry.getValue();
                Iterator<GribRequestableData> iter = list.iterator();
                while (iter.hasNext()) {
                    GribRequestableData data = iter.next();
                    if (!data.needsRequest(request)) {
                        iter.remove();
                    }
                }

                if (list.size() > 0) {
                    List<String> groups = new ArrayList<String>(list.size());
                    for (GribRequestableData data : list) {
                        groups.add(data.getGribSource().getDataURI());
                    }

                    IDataStore ds = DataStoreFactory.getDataStore(new File(
                            entry.getKey()));
                    try {

                        IDataRecord[] records = ds.retrieveGroups(
                                groups.toArray(new String[groups.size()]),
                                request);
                        for (int i = 0; i < list.size(); i++) {
                            GribRequestableData data = list.get(i);
                            IDataRecord[] value = new IDataRecord[] { records[i] };
                            references.add(value);
                            data.setDataValue(request, value);
                        }
                    } catch (Exception e) {
                        throw new VizDataCubeException(e);
                    }
                }
            }
        }

        for (PluginDataObject obj : objs) {
            IDataRecord[] records = null;
            if (obj instanceof RequestableDataRecord) {

                try {
                    if (requests == null) {
                        records = ((RequestableDataRecord) obj)
                                .getDataRecord(req);
                    } else {
                        records = getRecord(obj, requests);
                    }
                } catch (VizException e) {
                    throw new VizDataCubeException(e);
                }
            } else {
                records = getRecord(obj, req, dataset);
            }

            obj.setMessageData(records);
        }

        references.clear();
    }

    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<Map<String, RequestConstraint>> result = new ArrayList<Map<String, RequestConstraint>>(
                1);
        result.add(constraints);
        return result;
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException {
        int mapSize = (int) (requests.size() * 1) + 1;
        Map<AbstractRequestableLevelNode, Set<DataTime>> cache = new HashMap<AbstractRequestableLevelNode, Set<DataTime>>(
                mapSize);
        LinkedHashMap<AbstractRequestableLevelNode, TimeQueryRequest> queries = new LinkedHashMap<AbstractRequestableLevelNode, TimeQueryRequest>(
                mapSize);

        for (TimeQueryRequest request : requests) {
            List<AbstractRequestableLevelNode> requestNodes = gridInventory
                    .evaluateRequestConstraints(request.getQueryTerms());
            // pull out time queries and bulk submit
            for (AbstractRequestableLevelNode requestNode : requestNodes) {
                getTimeQuery(requestNode, false, queries, cache, null);
            }
        }

        // set the results back into the cache's
        TimeQueryRequestSet reqSet = new TimeQueryRequestSet();
        reqSet.setRequests(queries.values().toArray(
                new TimeQueryRequest[queries.size()]));
        @SuppressWarnings("unchecked")
        List<List<DataTime>> qResponses = (List<List<DataTime>>) ThriftClient
                .sendRequest(reqSet);
        int index = 0;
        for (AbstractRequestableLevelNode node : queries.keySet()) {
            // put results into cache
            node.setTimeQueryResults(false, qResponses.get(index++), cache,
                    null);
        }
        List<List<DataTime>> finalResponse = new ArrayList<List<DataTime>>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            List<AbstractRequestableLevelNode> requestNodes = gridInventory
                    .evaluateRequestConstraints(request.getQueryTerms());
            // pull the actual results from the cache
            Set<DataTime> results = new HashSet<DataTime>(64);
            for (AbstractRequestableLevelNode requestNode : requestNodes) {
                Set<DataTime> times = requestNode.timeQuery(false, cache, null);
                if (times == AbstractRequestableLevelNode.TIME_AGNOSTIC) {
                    // TODO: include time agnostic query in main bulk query as
                    // each
                    // pressure level will cause a separate query
                    List<DataTime> temp = gridInventory
                            .timeAgnosticQuery(request.getQueryTerms());
                    if (temp != null) {
                        results.addAll(temp);
                    }
                    break;
                } else {
                    results.addAll(times);
                }
            }
            if (!request.isMaxQuery() || results.isEmpty()) {
                finalResponse.add(new ArrayList<DataTime>(results));
            } else {
                ArrayList<DataTime> response = new ArrayList<DataTime>(results);
                Collections.sort(response);
                finalResponse
                        .add(Arrays.asList(response.get(response.size() - 1)));
            }
        }
        return finalResponse;
    }
}
