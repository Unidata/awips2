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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataaccess.DataFactoryRegistry;
import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridPathProvider;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridCacheUpdater;
import com.raytheon.uf.common.dataplugin.grid.derivparam.daf.DerivedGridDataAccessFactory;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.GridRequestableData;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.SliceUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.inv.MetadataContainer;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.datacube.AbstractDataCubeAdapter;
import com.raytheon.uf.viz.datacube.CubeUtil;
import com.raytheon.viz.grid.inv.GridMetadataContainer;
import com.raytheon.viz.grid.inv.VizGridInventory;
import com.raytheon.viz.grid.record.RequestableDataRecord;

/**
 * DataCubeAdapter for Grid, the primary role is to link the grid datatype into
 * derived parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 16, 2009  2149     brockwoo  Initial creation
 * Nov 21, 2009  3576     rjpeter   Refactored use of DerivParamDesc.
 * Jun 04, 2013  2041     bsteffen  Improve exception handing in grid resources.
 * Apr 04, 2014  2973     bsteffen  Use correct area for expanding subgrid
 *                                  requests.
 * Nov 08, 2016  5976     bsteffen  Remove VizDataCubeException
 * Apr 20, 2017  6046     bsteffen  Avoid using messageData for temporary
 *                                  internal storage.
 * 
 * </pre>
 * 
 * @author brockwoo
 */
public class GridDataCubeAdapter extends AbstractDataCubeAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridDataCubeAdapter.class);

    private VizGridInventory gridInventory;

    public GridDataCubeAdapter() {
        super(new String[] { GridConstants.GRID });
    }

    @Override
    public void initInventory() {
        if (gridInventory == null) {
            GridCacheUpdater updater = GridCacheUpdater.getInstance();
            if (!updater.isEnabled()) {
                NotificationManagerJob.addObserver("edex.alerts",
                        updater.getObserver());
                updater.enable();
            }
            VizGridInventory gridInventory = new VizGridInventory();
            try {
                gridInventory
                        .initTree(DerivedParameterGenerator.getDerParLibrary());
                this.gridInventory = gridInventory;
                DerivedGridDataAccessFactory daf = new DerivedGridDataAccessFactory(
                        gridInventory);
                DataFactoryRegistry.getInstance().register(GridConstants.GRID,
                        daf);
            } catch (DataCubeException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

    }

    @Override
    public VizGridInventory getInventory() {
        initInventory();
        return gridInventory;
    }

    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws DataCubeException {
        if (obj instanceof RequestableDataRecord) {
            Map<PluginDataObject, IDataRecord[]> map = getRecordsAsMap(
                    Arrays.asList(obj), req, dataset);
            IDataRecord[] result = map.get(obj);
            if (result == null) {
                /*
                 * This should never happen, but detecting it now makes it
                 * easier to trace bugs.
                 */
                throw new DataCubeException(
                        "Failed to get grid data for " + obj.getDataURI());
            }
            return result;
        }
        IDataRecord record = null;
        if (GridPathProvider.STATIC_PARAMETERS.contains(
                ((GridRecord) obj).getParameter().getAbbreviation())) {
            GridRecord gridRec = (GridRecord) obj;
            IDataStore ds = DataStoreFactory
                    .getDataStore(HDF5Util.findHDF5Location(obj));
            try {
                record = ds.retrieve("/" + gridRec.getLocation().getId(),
                        gridRec.getParameter().getAbbreviation(), req);
            } catch (Exception e) {
                throw new DataCubeException("Error retrieving staticTopo data!",
                        e);
            }
        } else {
            record = CubeUtil.retrieveData(obj, obj.getPluginName(), req,
                    dataset);
        }
        return new IDataRecord[] { record };
    }

    private IDataRecord[] getRecord(PluginDataObject obj, Request[] requests)
            throws VizException {
        try {
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
                    throw new VizException("Error processing slab of type "
                            + recs[i].getClass().getSimpleName());
                }
            }
            return newRecs;
        } catch (DataCubeException e) {
            throw new VizException(e);
        }
    }

    private Request[] generateRequests(Request req, ISpatialObject area) {
        int BUFFER_WIDTH = 9;
        Request retrieveRequest;
        Request sliceRequest;
        int[] minIndex;
        int[] maxIndex;
        /*
         * We need to add a buffer region around all derived parameters so that
         * parameters which rely on neighboring points are derived properly.
         */
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
            /*
             * This is not very efficient, it would be faster to make a buffer
             * around each line, however no one is using line requests right now
             * so there is no need to complicate the code.
             */
            retrieveRequest = Request.ALL;
            sliceRequest = req;
            break;
        case ALL:
        default:
            return null;
        }
        return new Request[] { retrieveRequest, sliceRequest };
    }

    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws DataCubeException {
        Map<PluginDataObject, IDataRecord[]> map = getRecordsAsMap(objs, req,
                dataset);
        /*
         * copy the map into the messageData since that is what the
         * DataCubeContainer API calls for.
         */
        for (Entry<PluginDataObject, IDataRecord[]> entry : map.entrySet()) {
            entry.getKey().setMessageData(entry.getValue());
        }
    }

    /**
     * Attempts to travel through all the derived levels and prefetch all the
     * grib records since a single bulk hdf5 read should be faster than lots of
     * little reads.
     */
    private Map<PluginDataObject, IDataRecord[]> getRecordsAsMap(
            List<PluginDataObject> objs, Request req, String dataset)
            throws DataCubeException {
        Set<GridRequestableData> realData = new HashSet<>();
        ISpatialObject area = null;
        for (PluginDataObject obj : objs) {
            if (area == null) {
                area = ((ISpatialEnabled) obj).getSpatialObject();
            }
            if (obj instanceof RequestableDataRecord) {
                realData.addAll(
                        ((RequestableDataRecord) obj).getGridRequests());
            }
        }

        Map<String, List<GridRequestableData>> fileMap = new HashMap<>();
        for (GridRequestableData data : realData) {
            if (GridPathProvider.STATIC_PARAMETERS.contains(
                    data.getGridSource().getParameter().getAbbreviation())) {
                continue;
            }
            GridRecord record = data.getGridSource();
            String file = HDF5Util.findHDF5Location(record).getPath();
            if (file != null) {
                List<GridRequestableData> list = fileMap.get(file);
                if (list == null) {
                    list = new LinkedList<>();
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

        List<IDataRecord[]> references = new ArrayList<>(realData.size());
        if (!realData.isEmpty()) {
            /*
             * The values are held weakly in cache, so we hold them strongly
             * here to prevent them from getting garbage collected to soon
             */
            for (Entry<String, List<GridRequestableData>> entry : fileMap
                    .entrySet()) {
                List<GridRequestableData> list = entry.getValue();
                Iterator<GridRequestableData> iter = list.iterator();
                while (iter.hasNext()) {
                    GridRequestableData data = iter.next();
                    if (!data.needsRequest(request)) {
                        iter.remove();
                    }
                }

                if (!list.isEmpty()) {
                    List<String> groups = new ArrayList<>(list.size());
                    for (GridRequestableData data : list) {
                        groups.add(data.getGridSource().getDataURI());
                    }

                    IDataStore ds = DataStoreFactory
                            .getDataStore(new File(entry.getKey()));
                    try {

                        IDataRecord[] records = ds.retrieveGroups(
                                groups.toArray(new String[groups.size()]),
                                request);
                        for (int i = 0; i < list.size(); i++) {
                            GridRequestableData data = list.get(i);
                            IDataRecord[] value = new IDataRecord[] {
                                    records[i] };
                            references.add(value);
                            data.setDataValue(request, value);
                        }
                    } catch (Exception e) {
                        throw new DataCubeException(e);
                    }
                }
            }
        }

        Map<PluginDataObject, IDataRecord[]> map = new LinkedHashMap<>();

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
                    throw new DataCubeException(e.getMessage(), e);
                }
            } else {
                records = getRecord(obj, req, dataset);
            }

            map.put(obj, records);
        }

        references.clear();
        return map;
    }

    @Override
    protected List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints) {
        return gridInventory.evaluateRequestConstraints(constraints);
    }

    @Override
    protected List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> queryTerms)
            throws DataCubeException {
        return gridInventory.timeAgnosticQuery(queryTerms);
    }

    @Override
    protected List<PluginDataObject> getData(
            Map<String, RequestConstraint> constraints,
            DataTime[] selectedTimes, List<AbstractRequestableData> requesters)
            throws DataCubeException {
        try {
            List<PluginDataObject> results = new ArrayList<>(requesters.size());
            for (AbstractRequestableData requester : requesters) {
                List<RequestableDataRecord> records = new ArrayList<>();
                if (requester.getDataTime() == null
                        || requester.getTimeAndSpace().isTimeAgnostic()) {
                    DataTime[] entryTime = selectedTimes;
                    if (entryTime != null && entryTime.length > 0) {
                        List<DataTime> entryTimes = new ArrayList<>(
                                Arrays.asList(entryTime));
                        for (DataTime time : entryTimes) {
                            RequestableDataRecord rec = new RequestableDataRecord(
                                    requester);
                            rec.setDataTime(time.clone());
                            rec.setDataURI(null);
                            boolean newRecord = true;
                            for (PluginDataObject result : results) {
                                if (result.getDataURI()
                                        .equals(rec.getDataURI())) {
                                    newRecord = false;
                                    break;
                                }
                            }
                            if (newRecord) {
                                records.add(rec);
                            }
                        }
                    } else {
                        RequestableDataRecord rec = new RequestableDataRecord(
                                requester);
                        rec.setDataTime(new DataTime(Calendar.getInstance()));
                        records.add(rec);
                    }
                } else {
                    RequestableDataRecord rec = new RequestableDataRecord(
                            requester);
                    records.add(rec);
                }
                if (requester.getSpace() == null
                        || requester.getTimeAndSpace().isSpaceAgnostic()) {
                    Collection<GridCoverage> coverages = CoverageUtils
                            .getInstance().getCoverages(requester.getSource());
                    if (coverages != null && !coverages.isEmpty()) {
                        List<RequestableDataRecord> spaceRecords = new ArrayList<>();
                        for (RequestableDataRecord record : records) {
                            for (GridCoverage coverage : coverages) {
                                record = new RequestableDataRecord(record);
                                record.setLocation(coverage);
                                record.setDataURI(null);
                                spaceRecords.add(record);
                            }
                        }
                        records = spaceRecords;
                    }
                }
                results.addAll(records);
            }
            if (constraints.containsKey(GridConstants.ENSEMBLE_ID)) {
                String ensemble = constraints.get(GridConstants.ENSEMBLE_ID)
                        .getConstraintValue();
                if (ensemble != null) {
                    for (Object rec : results) {
                        ((GridRecord) rec).setEnsembleId(ensemble);
                    }
                }
            }
            return results;
        } catch (VizException e) {
            throw new DataCubeException(e);
        }
    }

    @Override
    protected MetadataContainer createMetadataContainer(
            Map<String, RequestConstraint> constraints) {
        return new GridMetadataContainer(constraints,
                createAvailabilityContainer(constraints));
    }

    @Override
    protected AvailabilityContainer createAvailabilityContainer(
            Map<String, RequestConstraint> constraints) {
        /*
         * using a grid specific container which is able to merge constraints
         * will result in faster database queries, however the extra processing
         * time it takes to route the times to the correct nodes is larger than
         * the time saved.
         */
        return super.createAvailabilityContainer(constraints);
    }

}
