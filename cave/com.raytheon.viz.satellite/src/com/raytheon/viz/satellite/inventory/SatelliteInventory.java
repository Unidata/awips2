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
package com.raytheon.viz.satellite.inventory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.derivparam.tree.LatLonDataLevelNode.LatOrLon;
import com.raytheon.uf.common.derivparam.tree.ValidTimeDataLevelNode;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.ParameterNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Inventory of available satellite data. sectorID is used for source and
 * physicalElement for parameter, the level is always the entire atmosphere.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2014  2947     bsteffen    Initial creation
 * May 06, 2014  3117     bsteffen    Update for new data.
 * Sep 09, 2014  3356     njensen     Remove CommunicationException
 * Apr 06, 2014  #17215   D. Friedman Use ReentrantLock
 * Jul 17, 2017  6345     bsteffen    Add support for latitude, longitude, validTime
 * Feb 10, 2021 20421  mgamazaychikov Add support for centalWaveLength handling
 * Oct 29, 2022  8959     mapeters    Update how data time levels are set
 * Mar 16, 2023 23414  mgamazaychikov Fix evaluateRequestConstraints for
 *                                    centralWaveLength handling
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class SatelliteInventory extends AbstractInventory
        implements IAlertObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatelliteInventory.class);

    public static final String SATELLITE = "satellite";

    public static final String SECTOR_ID = "sectorID";

    public static final String PHYSICALELEMENT = "physicalElement";

    public static final String GID = "coverage.gid";

    private static final String DATA_TIME_FIELD = "dataTime";

    private static final String LATEST_DATA_TIME_FIELD = "dataTime.refTime";

    public static final String CENTRAL_WAVELENGTH_FIELD = "centralWavelength";

    private SatelliteCoverageCache coverages;

    private Level level;

    public SatelliteInventory() {
        ProductAlertObserver.addObserver(SATELLITE, this);
    }

    @Override
    public List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> query) throws DataCubeException {
        Map<String, RequestConstraint> newQuery = new HashMap<>(query);
        newQuery.remove(PHYSICALELEMENT);
        TimeQueryRequest req = new TimeQueryRequest();
        req.setPluginName(SATELLITE);
        req.setQueryTerms(newQuery);

        try {
            @SuppressWarnings("unchecked")
            List<DataTime> result = (List<DataTime>) RequestRouter.route(req);
            return result;
        } catch (Exception e) {
            throw new DataCubeException(e);
        }
    }

    @Override
    public void initTree(Map<String, DerivParamDesc> derParLibrary)
            throws DataCubeException {
        lock.lock();
        try {
            level = LevelFactory.getInstance().getLevel("EA", 0.0);
            coverages = new SatelliteCoverageCache();
            super.initTree(derParLibrary);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected DataTree createBaseTree() {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(SatelliteRecord.class);
        request.setDistinct(true);
        request.addRequestField(SECTOR_ID);
        request.addRequestField(PHYSICALELEMENT);

        DbQueryResponse response;
        try {
            response = (DbQueryResponse) RequestRouter.route(request);
        } catch (Exception e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }

        String levelId = Long.toString(level.getId());

        DataTree tree = new DataTree();
        for (Map<String, Object> result : response.getResults()) {
            String sectorID = (String) result.get(SECTOR_ID);
            String physicalElement = (String) result.get(PHYSICALELEMENT);
            SourceNode sourceNode = tree.getSourceNode(sectorID);
            if (sourceNode == null) {
                sourceNode = new SourceNode();
                sourceNode.setValue(sectorID);
                tree.getSourceNodes().put(sectorID, sourceNode);
            }

            ParameterNode paramNode = sourceNode.getChildNode(physicalElement);
            if (paramNode == null) {
                paramNode = new ParameterNode();
                paramNode.setValue(physicalElement);
                paramNode.setParameterName(physicalElement);
                sourceNode.addChildNode(paramNode);
            }

            LevelNode levelNode = paramNode.getChildNode(levelId);
            if (levelNode == null) {
                Map<String, RequestConstraint> requestConstraints = new HashMap<>();
                requestConstraints.put(SECTOR_ID,
                        new RequestConstraint(sectorID));
                requestConstraints.put(PHYSICALELEMENT,
                        new RequestConstraint(physicalElement));
                requestConstraints.put(PluginDataObject.PLUGIN_NAME_ID,
                        new RequestConstraint(SATELLITE));
                levelNode = new SatelliteRequestableLevelNode(coverages,
                        requestConstraints);
                levelNode.setValue(levelId);
                paramNode.addChildNode(levelNode);
            }
        }
        return tree;
    }

    @Override
    protected LevelNode getCubeNode(SourceNode sNode, DerivParamField field,
            Deque<StackEntry> stack, Set<StackEntry> nodata) {
        /* Returning null means cubes is not supported. */
        return null;
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableData nodeToImport, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        /* Returning null means import is not supported. */
        return null;
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableNode nodeToImport, String nodeToImportSourceName,
            SourceNode destSourceNode, DerivParamDesc desc,
            DerivParamMethod method, Level level) {
        /* Returning null means import is not supported. */
        return null;
    }

    @Override
    protected Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        String param = field.getParam();
        if (LatOrLon.LATITUDE.toString().toLowerCase().equals(param)) {
            return new SatLatLonDataLevelNode(sNode.getValue(),
                    LatOrLon.LATITUDE, level);
        } else if (LatOrLon.LONGITUDE.toString().toLowerCase().equals(param)) {
            return new SatLatLonDataLevelNode(sNode.getValue(),
                    LatOrLon.LONGITUDE, level);
        } else if ("validTime".equals(param)) {
            return new ValidTimeDataLevelNode(this, sNode.getValue(), level);
        }
        return null;
    }

    protected List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints) {
        Collection<String> sources = getAllSources();
        RequestConstraint sectorLimiter = constraints.get(SECTOR_ID);
        boolean derive = true;
        if (sectorLimiter != null) {
            Iterator<String> it = sources.iterator();
            while (it.hasNext()) {
                if (!sectorLimiter.evaluate(it.next())) {
                    it.remove();
                }
            }
        }
        Collection<String> parameters = getAllParameters();
        RequestConstraint peLimiter = constraints.get(PHYSICALELEMENT);
        if (peLimiter != null) {
            Iterator<String> it = parameters.iterator();
            while (it.hasNext()) {
                if (!peLimiter.evaluate(it.next())) {
                    it.remove();
                }
            }
        } else {
            derive = false;
            Map<String, RequestConstraint> newConstraints = new HashMap<>(constraints);
            newConstraints.remove("pluginName");
            newConstraints.remove("creatingEntity");
            String newConstraintKey = null;
            String constraintSectorValue = null;
            String newConstraintValue = null;
            for (Map.Entry<String, RequestConstraint> entry : newConstraints.entrySet()) {
                if (entry.getKey().equals(CENTRAL_WAVELENGTH_FIELD)) {
                    newConstraintKey = entry.getKey();
                    newConstraintValue = entry.getValue().getConstraintValue();
                } else if (entry.getKey().equals(SECTOR_ID)) {
                    constraintSectorValue = entry.getValue().getConstraintValue();
                }
            }
            newConstraints.remove(SECTOR_ID);
            Map<String, SourceNode> srcNodes = dataTree.getSourceNodes();
            for (Map.Entry<String, SourceNode> entry : srcNodes.entrySet()) {
                SourceNode srcNode = dataTree.getSourceNode(entry.getKey());
                if (srcNode.containsChildNode(constraintSectorValue)) {
                    if (newConstraintValue != null && newConstraintKey != null
                            && !srcNode.containsChildNode(newConstraintValue)) {
                        peLimiter = constraints.get(newConstraintKey);
                        ParameterNode paramNode = new ParameterNode();
                        paramNode.setValue(newConstraintValue);
                        paramNode.setParameterName(newConstraintValue);
                        LevelNode levelNode = new SatelliteRequestableLevelNode(coverages, constraints);
                        String levelId = Long.toString(level.getId());
                        levelNode.setValue(levelId);
                        paramNode.addChildNode(levelNode);
                        srcNode.addChildNode(paramNode);
                        dataTree.getSourceNodes().put(sectorLimiter.getConstraintValue(), srcNode);
                        parameters = getAllParameters();
                        Iterator<String> it = parameters.iterator();
                        while (it.hasNext()) {
                            if (!peLimiter.evaluate(it.next())) {
                                it.remove();
                            }
                        }
                    }
                }
            }
        }
        Collection<Level> levels = Collections.singleton(level);
        try {
            List<AbstractRequestableNode> result = walkTree(null, sources,
                    parameters, levels, derive, true, null);
            return result;
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return Collections.emptyList();
        }
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        if (dataTree == null) {
            return;
        }
        for (AlertMessage message : alertMessages) {
            String sector = message.decodedAlert.get(SECTOR_ID).toString();
            String pe = message.decodedAlert.get(PHYSICALELEMENT).toString();
            if (dataTree.getParameterNode(sector, pe) == null) {
                /*
                 * When a sector or element arrives that is not known reinit the
                 * tree to ensure no nodes are missing.
                 */
                try {
                    initTree(derParLibrary);
                } catch (DataCubeException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                return;
            }
        }
    }

    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws DataCubeException {
        List<DbQueryRequest> dbRequests = new ArrayList<>(requests.size());
        for (TimeQueryRequest request : requests) {
            dbRequests.add(getTimeQueryRequest(request.getQueryTerms(),
                    request.isMaxQuery()));
        }
        DbQueryRequestSet requestSet = new DbQueryRequestSet();
        requestSet.setQueries(dbRequests.toArray(new DbQueryRequest[0]));
        DbQueryResponseSet responseSet;
        try {
            responseSet = (DbQueryResponseSet) RequestRouter.route(requestSet);
        } catch (Exception e) {
            throw new DataCubeException(e);
        }
        List<List<DataTime>> result = new ArrayList<>(requests.size());
        for (int i = 0; i < requests.size(); i++) {
            DbQueryResponse response = responseSet.getResults()[i];
            TimeQueryRequest request = requests.get(i);
            Collection<DataTime> times = processTimeQueryResponse(response,
                    request.isMaxQuery(), request.getBinOffset());

            result.add(new ArrayList<>(times));
        }

        return result;
    }

    private Collection<DataTime> processTimeQueryResponse(
            DbQueryResponse response, boolean latestOnly, BinOffset binOffset) {
        String dataTimefield = DATA_TIME_FIELD;
        if (latestOnly) {
            dataTimefield = LATEST_DATA_TIME_FIELD;
        }
        Collection<DataTime> results = new HashSet<>();
        for (Map<String, Object> map : response.getResults()) {
            DataTime time = null;
            if (latestOnly) {
                time = new DataTime((Date) map.get(dataTimefield), 0);
            } else {
                time = (DataTime) map.get(dataTimefield);
                Number cwl = (Number) map.get(CENTRAL_WAVELENGTH_FIELD);
                if (cwl != null) {
                    time.setLevel(cwl.doubleValue(), CENTRAL_WAVELENGTH_FIELD);
                }
            }
            // Best res requests need this because they span a time period
            if (time.getRefTime()
                    .before(SimulatedTime.getSystemTime().getTime())) {
                results.add(time);
            }
        }

        if (binOffset != null) {
            Set<DataTime> scaledDates = new TreeSet<>();
            for (DataTime dt : results) {
                scaledDates.add(binOffset.getNormalizedTime(dt));
            }
            results = scaledDates;
        }

        return results;
    }

    private DbQueryRequest getTimeQueryRequest(
            Map<String, RequestConstraint> queryParams, boolean latestOnly) {
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(queryParams);

        String dataTimefield = DATA_TIME_FIELD;
        if (latestOnly) {
            dataTimefield = LATEST_DATA_TIME_FIELD;
        }
        request.addRequestField(dataTimefield, latestOnly);
        if (!latestOnly) {
            request.addRequestField(CENTRAL_WAVELENGTH_FIELD);
        }
        request.setDistinct(true);
        return request;
    }
}
