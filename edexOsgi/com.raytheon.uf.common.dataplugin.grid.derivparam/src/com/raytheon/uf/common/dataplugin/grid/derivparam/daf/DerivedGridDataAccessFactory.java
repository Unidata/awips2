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
package com.raytheon.uf.common.dataplugin.grid.derivparam.daf;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.EnvelopeProjectionException;
import com.raytheon.uf.common.dataaccess.exception.InvalidIdentifiersException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory;
import com.raytheon.uf.common.dataaccess.impl.DefaultGeometryData;
import com.raytheon.uf.common.dataaccess.impl.FactoryUtil;
import com.raytheon.uf.common.dataaccess.util.DataWrapperUtil;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataaccess.GridDataAccessFactory;
import com.raytheon.uf.common.dataplugin.grid.dataaccess.GridDataAccessFactory.GridGeometryKey;
import com.raytheon.uf.common.dataplugin.grid.derivparam.CommonGridInventory;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.GridRequestableNode;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.Request.Type;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.inv.MetadataContainer;
import com.raytheon.uf.common.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * 
 * An {@link AbstractDataFactory} implementation that can request grid data and
 * also use the {@link DerivedParameterGenerator} to calculate new data on the
 * fly. This is intended as a replacement for the {@link GridDataAccessFactory},
 * it should be able to return all the same data as well as additional derived
 * data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 03, 2016  5439     bsteffen  Initial creation
 * May 31, 2016  5587     bsteffen  Implement getIdentifierValues()
 * Jun 07, 2016  5587     tgurney   Change get*Identifiers() to take
 *                                  IDataRequest
 * Jun 14, 2016  5587     tgurney   Fix NPE in getAvailableValues()
 * Oct 13, 2016  5942     bsteffen  Fix subgrid geometry.
 * Nov 17, 2016  6000     bsteffen  Do not bother calculating availability when
 *                                  source is time agnostic.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DerivedGridDataAccessFactory extends AbstractDataFactory {

    private final CommonGridInventory inventory;

    public DerivedGridDataAccessFactory() {
        this.inventory = new CommonGridInventory();
        try {
            this.inventory.initTree(DerivedParameterGenerator
                    .getDerParLibrary());
        } catch (DataCubeException e) {
            throw new IllegalStateException(
                    "Failed to initialize grid inventory.", e);
        }
    }

    public DerivedGridDataAccessFactory(CommonGridInventory inventory) {
        this.inventory = inventory;
    }

    @Override
    public String[] getOptionalIdentifiers(IDataRequest request) {
        return GridDataAccessFactory.VALID_IDENTIFIERS;
    }

    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            boolean refTimeOnly) throws TimeAgnosticDataException {
        Map<String, RequestConstraint> query = GridDataAccessFactory
                .buildGridConstraintsFromRequest(request);
        try {
            Set<DataTime> results = new HashSet<>(64);

            if (!containsLevelOrParameterConstraints(query)) {
                /*
                 * If the request is just looking for all the model times, then
                 * just return all times in the database. The default behavior
                 * of individually querying all nodes can take hours for some
                 * models and is usually identical to the times in the database.
                 */
                results.addAll(inventory.timeAgnosticQuery(query));
            } else {
                AvailabilityContainer container = new AvailabilityContainer(
                        query);
                List<AbstractRequestableNode> nodes = inventory
                        .evaluateRequestConstraints(query);
                for (AbstractRequestableNode node : nodes) {
                    container.prepareRequests(node);
                }
                for (AbstractRequestableNode requestNode : nodes) {
                    Set<TimeAndSpace> availability = container
                            .getAvailability(requestNode);
                    for (TimeAndSpace ast : availability) {
                        if (ast.isTimeAgnostic()) {
                            List<DataTime> temp = inventory
                                    .timeAgnosticQuery(query);
                            if (temp != null) {
                                results.addAll(temp);
                            }
                            break;
                        } else {
                            results.add(ast.getTime());
                        }
                    }
                }
            }
            if (refTimeOnly) {
                Set<DataTime> refTimes = new HashSet<>();
                for (DataTime time : results) {
                    refTimes.add(new DataTime(time.getRefTime()));
                }
                results = refTimes;
            }
            return results.toArray(new DataTime[0]);
        } catch (DataCubeException | InterruptedException e) {
            throw new DataRetrievalException(
                    "Error occurred during time query.", e);
        }
    }

    private static boolean containsLevelOrParameterConstraints(
            Map<String, RequestConstraint> query) {
        if (query.containsKey(GridConstants.PARAMETER_ABBREVIATION)) {
            return true;
        } else if (query.containsKey(GridConstants.LEVEL_ID)) {
            return true;
        } else if (query.containsKey(GridConstants.LEVEL_ID)) {
            return true;
        } else if (query.containsKey(GridConstants.MASTER_LEVEL_NAME)) {
            return true;
        } else if (query.containsKey(GridConstants.LEVEL_ONE)) {
            return true;
        } else if (query.containsKey(GridConstants.LEVEL_TWO)) {
            return true;
        }
        return false;
    }

    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        return FactoryUtil.getAvailableTimes(this, request, binOffset);
    }

    private DataTime[] getTimesInRange(IDataRequest request, TimeRange timeRange) {
        DataTime[] allTimes = getAvailableTimes(request, false);
        if (timeRange == null) {
            return allTimes;
        }
        List<DataTime> timesInRange = new ArrayList<>();
        for (DataTime time : allTimes) {
            if (timeRange.contains(time.getValidTimeAsDate())) {
                timesInRange.add(time);
            }
        }
        return timesInRange.toArray(new DataTime[0]);
    }

    @Override
    public IGridData[] getGridData(IDataRequest request, TimeRange timeRange) {
        return getGridData(request, getTimesInRange(request, timeRange));
    }

    private List<AbstractRequestableData> getData(IDataRequest request,
            DataTime... times) throws DataCubeException {
        Map<String, RequestConstraint> query = GridDataAccessFactory
                .buildGridConstraintsFromRequest(request);
        List<AbstractRequestableNode> requests;
        try {
            requests = inventory.evaluateRequestConstraints(query);
        } catch (InterruptedException e) {
            throw new DataRetrievalException(
                    "Unexpected interrupt getting grid constraints", e);
        }
        Set<TimeAndSpace> availability = new HashSet<>();
        for (DataTime time : times) {
            availability.add(new TimeAndSpace(time));
        }

        // pull the actual results from the cache
        List<AbstractRequestableData> requesters = new ArrayList<>();
        MetadataContainer container = new MetadataContainer(query,
                new AvailabilityContainer(query));
        for (AbstractRequestableNode node : requests) {
            container.prepareRequests(node, availability);
        }
        for (AbstractRequestableNode node : requests) {
            requesters.addAll(container.getData(node, availability));
        }
        return requesters;
    }

    @Override
    public IGridData[] getGridData(IDataRequest request, DataTime... times) {
        try {
            List<AbstractRequestableData> requesters = getData(request, times);
            Map<AbstractRequestableData, Request> requestersMap = calcDataStoreRequests(
                    requesters, request.getEnvelope(), false);

            List<IGridData> result = new ArrayList<>();
            for (Entry<AbstractRequestableData, Request> entry : requestersMap
                    .entrySet()) {
                AbstractRequestableData data = entry.getKey();
                Request dataStoreRequest = entry.getValue();

                Object obj = data.getDataValue(dataStoreRequest);
                DataSource source = getDataSource(obj);

                /*
                 * Create a GridRecord full of metadata that we can hand off to
                 * the GridDataAccessFactory to handle all the namespace mapping
                 * things.
                 */
                GridGeometry2D gridGeometry = data.getSpace().getGridGeometry();
                if (data.getTimeAndSpace().isSpaceAgnostic()) {
                    /*
                     * Choose an arbitrary grid for space agnostic data. The
                     * value is a constant and this is probably user error but
                     * handle it gracefully if possible.
                     */
                    Collection<GridCoverage> gridGeometries = CoverageUtils
                            .getInstance().getCoverages(data.getSource());
                    if (!gridGeometries.isEmpty()) {
                        gridGeometry = gridGeometries.iterator().next()
                                .getGridGeometry();
                    } else {
                        throw new DataRetrievalException(
                                "Unable to determine coverage area for " + data);
                    }
                } else if (dataStoreRequest.getType() == Type.SLAB) {
                    int[] minIndices = dataStoreRequest.getMinIndexForSlab();
                    int[] maxIndices = dataStoreRequest.getMaxIndexForSlab();
                    GridEnvelope2D range = new GridEnvelope2D(minIndices[0],
                            minIndices[1], maxIndices[0] - minIndices[0],
                            maxIndices[1] - minIndices[1]);
                    Envelope2D envelope = gridGeometry.gridToWorld(range);
                    /*
                     * The data source will be indexed starting at 0 so adjust
                     * the grid range to match.
                     */
                    range.x = 0;
                    range.y = 0;
                    gridGeometry = new GridGeometry2D((GridEnvelope) range,
                            envelope);
                }
                GridRecord record = new GridRecord();
                record.setDatasetId(data.getSource());
                if (!data.getTimeAndSpace().isTimeAgnostic()) {
                    record.setDataTime(data.getDataTime());
                }
                record.setLevel(data.getLevel());
                Parameter parameter = new Parameter();
                parameter.setAbbreviation(data.getParameter());
                parameter.setName(data.getParameterName());
                parameter.setUnit(data.getUnit());
                record.setParameter(parameter);
                result.add(GridDataAccessFactory.constructGridDataResponse(
                        request, record, gridGeometry, source));
            }
            return result.toArray(new IGridData[0]);
        } catch (DataCubeException | TransformException e) {
            throw new DataRetrievalException("Error retrieving grid data.", e);
        }
    }

    private static DataSource getDataSource(Object obj)
            throws DataCubeException {
        if (obj instanceof IDataRecord[]) {
            IDataRecord[] arr = (IDataRecord[]) obj;
            if (arr.length == 1) {
                obj = arr[0];
            } else {
                throw new DataCubeException(
                        "Array has an unexpected length of " + arr.length);
            }
        }
        if (obj instanceof IDataRecord) {
            IDataRecord dataRecord = (IDataRecord) obj;
            long[] sizes = dataRecord.getSizes();
            if (sizes.length == 1 && sizes[0] == 1) {
                BufferWrapper source = BufferWrapper.wrapArray(
                        dataRecord.getDataObject(), 1, 1);
                final double value = source.getDataValue(0, 0);
                return new ConstantDataSource(value);
            } else {
                return DataWrapperUtil.constructArrayWrapper(dataRecord);
            }
        }
        if (obj instanceof Number) {
            final double value = ((Number) obj).doubleValue();
            return new ConstantDataSource(value);
        }
        if (obj == null) {
            return null;
        }
        throw new DataCubeException("Unexpected data of type "
                + obj.getClass().getSimpleName());
    }

    @Override
    public String[] getAvailableLocationNames(IDataRequest request) {
        Map<String, RequestConstraint> query = GridDataAccessFactory
                .buildGridConstraintsFromRequest(request);
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<>();
        try {
            inventory.checkSources(query, returnQueue);
        } catch (InterruptedException e) {
            throw new DataRetrievalException("Unable to query grid location", e);
        }
        return returnQueue.toArray(new String[0]);
    }

    @Override
    public Level[] getAvailableLevels(IDataRequest request) {
        Map<String, RequestConstraint> query = GridDataAccessFactory
                .buildGridConstraintsFromRequest(request);
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<>();
        try {
            inventory.checkLevels(query, returnQueue);
        } catch (InterruptedException e) {
            throw new DataRetrievalException("Unable to query grid levels", e);
        }
        LevelFactory levelFactory = LevelFactory.getInstance();
        List<Level> result = new ArrayList<>();
        for (String levelid : returnQueue) {
            result.add(levelFactory.getLevel(levelid));
        }
        return result.toArray(new Level[0]);
    }

    /**
     * Get the available parameter abbreviations.
     */
    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        Map<String, RequestConstraint> query = GridDataAccessFactory
                .buildGridConstraintsFromRequest(request);
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<>();
        try {
            inventory.checkParameters(query, false, returnQueue);
        } catch (InterruptedException e) {
            throw new DataRetrievalException("Unable to query grid parameters",
                    e);
        }
        return returnQueue.toArray(new String[0]);
    }

    protected Map<AbstractRequestableData, Request> calcDataStoreRequests(
            Collection<AbstractRequestableData> requesters, Envelope envelope,
            boolean includePoint) {
        Map<AbstractRequestableData, Request> results = new HashMap<>(
                requesters.size());
        SubGridGeometryCalculator subGrid = null;
        for (AbstractRequestableData data : requesters) {
            Request dataStoreRequest = Request.ALL;
            if (envelope != null && !data.getTimeAndSpace().isSpaceAgnostic()) {
                GridGeometry2D gridGeometry = data.getSpace().getGridGeometry();
                if (subGrid != null) {
                    /*
                     * This is nulled if the geometry is different, since
                     * usually the geometry is the same it is faster to just
                     * reuse the same subgrid.
                     */
                    if (!subGrid.getGridGeometry().equals(gridGeometry)) {
                        subGrid = null;
                    }
                }
                if (subGrid == null) {
                    ReferencedEnvelope requestEnv = new ReferencedEnvelope(
                            envelope, DefaultGeographicCRS.WGS84);
                    try {
                        subGrid = new SubGridGeometryCalculator(requestEnv,
                                gridGeometry);
                    } catch (TransformException e) {
                        throw new EnvelopeProjectionException(
                                "Error determining subgrid from envelope: "
                                        + envelope, e);
                    }
                }
                if (subGrid.isEmpty()) {
                    if (!includePoint) {
                        continue;
                    }
                    Point p = GridDataAccessFactory.findRequestPoint(
                            gridGeometry, envelope);
                    if (p == null) {
                        continue;
                    }
                    dataStoreRequest = Request.buildPointRequest(p);
                } else if (!subGrid.isFull()) {
                    dataStoreRequest = Request.buildSlab(
                            subGrid.getGridRangeLow(true),
                            subGrid.getGridRangeHigh(false));
                }
            }
            results.put(data, dataStoreRequest);
        }
        return results;
    }

    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) {
        try {
            GeometryFactory geometryFactory = new GeometryFactory();
            List<AbstractRequestableData> requesters = getData(request, times);
            Map<AbstractRequestableData, Request> requestersMap = calcDataStoreRequests(
                    requesters, request.getEnvelope(), true);
            Map<GridGeometryKey, List<DefaultGeometryData>> resultMap = new HashMap<>();
            for (Entry<AbstractRequestableData, Request> entry : requestersMap
                    .entrySet()) {
                AbstractRequestableData data = entry.getKey();
                Request dataStoreRequest = entry.getValue();
                Object obj = data.getDataValue(dataStoreRequest);
                if (obj instanceof IDataRecord[]) {
                    IDataRecord[] arr = (IDataRecord[]) obj;
                    if (arr.length == 1) {
                        obj = arr[0];
                    } else {
                        throw new DataCubeException(
                                "Array has an unexpected length of "
                                        + arr.length);
                    }
                }
                DataSource source = getDataSource(obj);
                GridGeometry2D gridGeometry = data.getSpace().getGridGeometry();
                if (data.getTimeAndSpace().isSpaceAgnostic()) {
                    Collection<GridCoverage> geometries = CoverageUtils
                            .getInstance().getCoverages(data.getSource());
                    gridGeometry = geometries.iterator().next()
                            .getGridGeometry();
                }

                /*
                 * Figure out the request bounds to calculate the Lon/Lat
                 * location of every point geometry.
                 */
                int xMin = 0;
                int xMax = 0;
                int yMin = 0;
                int yMax = 0;
                if (dataStoreRequest.getType() == Type.POINT) {
                    Point p = dataStoreRequest.getPoints()[0];
                    xMin = p.x;
                    xMax = xMin + 1;
                    yMin = p.y;
                    yMax = yMin + 1;
                } else if (dataStoreRequest.getType() == Type.ALL) {
                    xMax = gridGeometry.getGridRange().getHigh(0);
                    yMax = gridGeometry.getGridRange().getHigh(1);
                } else if (dataStoreRequest.getType() == Type.SLAB) {
                    int[] min = dataStoreRequest.getMinIndexForSlab();
                    int[] max = dataStoreRequest.getMaxIndexForSlab();
                    xMin = min[0];
                    yMin = min[1];
                    xMax = max[0];
                    yMax = max[1];
                }

                GridGeometryKey key = new GridGeometryKey(data.getLevel(),
                        data.getDataTime(), data.getSource(), gridGeometry);
                List<DefaultGeometryData> results = resultMap.get(key);
                if (results == null) {
                    results = new ArrayList<>();
                    for (int y = yMin; y < yMax; y += 1) {
                        for (int x = xMin; x < xMax; x += 1) {
                            DefaultGeometryData geometryData = key
                                    .toGeometryData();
                            if (geometryData.getDataTime() == TimeAndSpace.TIME_AGNOSTIC) {
                                geometryData.setDataTime(null);
                            }
                            DirectPosition2D llPoint = GridDataAccessFactory
                                    .findResponsePoint(key.getGridGeometry(),
                                            x, y);
                            geometryData.setGeometry(geometryFactory
                                    .createPoint(new Coordinate(llPoint.x,
                                            llPoint.y)));
                            geometryData.setLocationName(geometryData
                                    .getLocationName() + "-" + x + "," + y);
                            results.add(geometryData);
                        }
                    }
                    resultMap.put(key, results);
                }
                int resultIndex = 0;
                for (int y = yMin; y < yMax; y += 1) {
                    for (int x = xMin; x < xMax; x += 1) {
                        results.get(resultIndex).addData(data.getParameter(),
                                source.getDataValue(x, y), data.getUnit());
                        resultIndex += 1;
                    }
                }
            }
            List<IGeometryData> results = new ArrayList<>();
            for (List<DefaultGeometryData> result : resultMap.values()) {
                results.addAll(result);
            }
            return results.toArray(new IGeometryData[0]);
        } catch (DataCubeException e) {
            throw new DataRetrievalException("Error retrieving grid data.", e);
        }
    }

    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) {
        return getGeometryData(request, getTimesInRange(request, timeRange));
    }

    /**
     * Determine all the raw data nodes that are necessary to fulfill a given
     * request and query the database for all values of the identifierKey that
     * are available for all the raw data nodes. This method will only work if
     * the field type of the identifierKey in the database is a {@link String}.
     */
    public String[] getAvailableValues(IDataRequest request,
            String identifierKey) {
        Map<String, RequestConstraint> query = GridDataAccessFactory
                .buildGridConstraintsFromRequest(request);
        try {
            List<AbstractRequestableNode> nodes = inventory
                    .evaluateRequestConstraints(query);
            List<DbQueryRequest> requests = new ArrayList<>();
            for (GridRequestableNode node : getBaseDataNodes(nodes)) {
                DbQueryRequest dbRequest = new DbQueryRequest();
                dbRequest.setEntityClass(GridRecord.class.getName());
                Map<String, RequestConstraint> rcMap = node
                        .getRequestConstraintMap();
                dbRequest.setConstraints(new HashMap<>(rcMap));
                for (Entry<String, RequestConstraint> e : query.entrySet()) {
                    if (!rcMap.containsKey(e.getKey())) {
                        dbRequest.addConstraint(e.getKey(), e.getValue());
                    }
                }
                dbRequest.setDistinct(true);
                dbRequest.addRequestField(identifierKey);
                requests.add(dbRequest);
            }
            DbQueryRequestSet requestSet = new DbQueryRequestSet();
            requestSet.setQueries(requests.toArray(new DbQueryRequest[0]));
            DbQueryResponseSet responseSet = (DbQueryResponseSet) RequestRouter
                    .route(requestSet);
            Set<String> unionSet = null;
            for (DbQueryResponse response : responseSet.getResults()) {
                String[] values = response.getFieldObjects(identifierKey,
                        String.class);
                if (unionSet == null) {
                    unionSet = new HashSet<>(Arrays.asList(values));
                } else {
                    unionSet.retainAll(Arrays.asList(values));
                }
            }
            if (unionSet == null) {
                unionSet = Collections.emptySet();
            }
            return unionSet.toArray(new String[0]);
        } catch (Exception e) {
            throw new DataRetrievalException("Error querying values.", e);
        }
    }

    private static Collection<GridRequestableNode> getBaseDataNodes(
            List<AbstractRequestableNode> nodes) {
        Set<GridRequestableNode> baseDataNodes = new HashSet<>();
        for (AbstractRequestableNode node : nodes) {
            if (node instanceof GridRequestableNode) {
                baseDataNodes.add((GridRequestableNode) node);
            } else if (node instanceof AbstractDerivedDataNode) {
                List<AbstractRequestableNode> depNodes = new ArrayList<>();
                for (Dependency dep : ((AbstractDerivedDataNode) node)
                        .getDependencies()) {
                    depNodes.add(dep.node);
                }
                baseDataNodes.addAll(getBaseDataNodes(depNodes));
            }
        }
        return baseDataNodes;
    }

    @Override
    public String[] getIdentifierValues(IDataRequest request,
            String identifierKey) {
        if (!Arrays.asList(getRequiredIdentifiers(request)).contains(
                identifierKey)
                && !Arrays.asList(getOptionalIdentifiers(request)).contains(
                        identifierKey)) {
            throw new InvalidIdentifiersException(request.getDatatype(), null,
                    Arrays.asList(identifierKey));
        }
        if (GridConstants.DATASET_ID.equals(identifierKey)) {
            return getAvailableLocationNames(request);
        } else if (GridConstants.SECONDARY_ID.equals(identifierKey)) {
            return getAvailableValues(request, identifierKey);
        } else if (GridConstants.ENSEMBLE_ID.equals(identifierKey)) {
            return getAvailableValues(request, identifierKey);
        } else if (GridConstants.MASTER_LEVEL_NAME.equals(identifierKey)) {
            Level[] levels = getAvailableLevels(request);
            Set<String> values = new HashSet<>();
            for (Level level : levels) {
                values.add(level.getMasterLevel().getName());
            }
            return values.toArray(new String[0]);
        } else if (GridConstants.LEVEL_ONE.equals(identifierKey)) {
            Level[] levels = getAvailableLevels(request);
            Set<String> values = new HashSet<>();
            for (Level level : levels) {
                values.add(level.getLevelOneValueAsString());
            }
            return values.toArray(new String[0]);
        } else if (GridConstants.LEVEL_TWO.equals(identifierKey)) {
            Level[] levels = getAvailableLevels(request);
            Set<String> values = new HashSet<>();
            for (Level level : levels) {
                values.add(level.getLevelTwoValueAsString());
            }
            return values.toArray(new String[0]);
        } else {
            throw new UnsupportedOperationException(
                    "Unable to query available " + identifierKey
                            + " identifiers.");
        }
    }

    private static final class ConstantDataSource implements DataSource {

        private final double value;

        public ConstantDataSource(double value) {
            this.value = value;
        }

        @Override
        public double getDataValue(int x, int y) {
            return value;
        }

    }

}
