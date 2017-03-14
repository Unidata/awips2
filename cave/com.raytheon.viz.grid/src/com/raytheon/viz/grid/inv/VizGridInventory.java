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
package com.raytheon.viz.grid.inv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.grid.derivparam.CommonGridInventory;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.CoverageUtils;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.GridRequestableNode;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod.MethodType;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * Inventory object for calculating and managing the available grid data,
 * including any data that can be derived.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Mar 25, 2009  2149     brockwoo    Initial creation
 * Nov 20, 2009  3387     jelkins     Use derived script's variableId instead of
 *                                    filename
 * Nov 21, 2009  3576     rjpeter     Refactored use of DerivParamDesc.
 * Feb 26, 2013  1659     bsteffen    Add time agnostic caching to grid derived
 *                                    parameters.
 * Jan 30, 2014  2725     ekladstrup  updated exception handling during move of
 *                                    derived parameters to common
 * Sep 09, 2014  3356     njensen     Remove CommunicationException
 * Mar 03, 2016  5439     bsteffen    Split grid inventory into common and viz
 * 
 * </pre>
 * 
 * @author brockwoo
 */

public class VizGridInventory extends CommonGridInventory implements
        IPointChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VizGridInventory.class);

    public static final String CUBE_MASTER_LEVEL_NAME = "MB";

    private GridUpdater updater;

    private List<String> modelsWithPerts = new ArrayList<>();

    private List<Map<String, RequestConstraint>> failedRequests = new ArrayList<>();

    @Override
    public void initTree(Map<String, DerivParamDesc> derParLibrary)
            throws DataCubeException {
        super.initTree(derParLibrary);
        if (updater == null) {
            updater = new GridUpdater(this);
            updater.startObserving();
            /*
             * Currently only one instance of GridInventory is created by the
             * GribDataCubeAdapter and lasts for the life of CAVE. Thus no need
             * for removal of this listener.
             */
            PointsDataManager.getInstance().addHomeChangedListener(this);
        } else {
            updater.refreshNodes();
        }
    }

    public void reinitTree() {
        try {
            initTree(derParLibrary);
            /*
             * reprocess all failed requests to see if data has become
             * available.
             */
            List<Map<String, RequestConstraint>> constraintsToTry = this.failedRequests;
            this.failedRequests = new ArrayList<>(failedRequests.size());
            for (Map<String, RequestConstraint> constraints : constraintsToTry) {
                evaluateRequestConstraints(constraints);
            }
        } catch (DataCubeException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    private void initGatherModels(DataTree tree) {
        long startTime = System.currentTimeMillis();
        modelsWithPerts.clear();

        /*
         * Construct request. Looking for every modelName that has
         * perturbationNumber >= 0
         */
        DbQueryRequest request = new DbQueryRequest();
        request.setDistinct(true);
        request.addRequestField(GridInfoConstants.DATASET_ID);
        request.addRequestField(GridInfoConstants.PARAMETER_ABBREVIATION);
        request.addRequestField(GridInfoConstants.LEVEL_ID);
        request.addRequestField(GridInfoConstants.ENSEMBLE_ID);
        request.addConstraint(GridInfoConstants.ENSEMBLE_ID,
                new RequestConstraint("", ConstraintType.NOT_EQUALS));
        request.setEntityClass(GridInfoRecord.class.getName());

        /* Send request and process response */
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            for (Map<String, Object> objMap : response.getResults()) {
                String model = String.valueOf(objMap
                        .get(GridInfoConstants.DATASET_ID));
                String parameter = String.valueOf(objMap
                        .get(GridInfoConstants.PARAMETER_ABBREVIATION));
                String levelId = String.valueOf(objMap
                        .get(GridInfoConstants.LEVEL_ID));
                String ensemble = (String) objMap
                        .get(GridInfoConstants.ENSEMBLE_ID);
                LevelNode node = tree.getLevelNode(model, parameter,
                        levelId.toString());
                if (node instanceof GridRequestableNode) {
                    GridRequestableNode gribNode = (GridRequestableNode) node;
                    List<String> ensembles = gribNode.getEnsembles();
                    if (ensembles == null) {
                        ensembles = new ArrayList<>(1);
                        ensembles.add(ensemble);
                    } else if (!ensembles.contains(ensemble)) {
                        ensembles = new ArrayList<>(ensembles);
                        ensembles.add(ensemble);
                    }
                    gribNode.setEnsembles(ensembles);
                }
                if (!modelsWithPerts.contains(model)) {
                    modelsWithPerts.add(model);
                }
            }
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error determining ensemble information, Gather function may be broken",
                            e);
        }
        long endTime = System.currentTimeMillis();
        System.out.println("Time processing gather nodes = "
                + (endTime - startTime));
    }

    /**
     * the source, parameter, and level are from an update from edex and if no
     * such database node exists will trigger a new of the grid tree
     * 
     * @param source
     * @param parameter
     * @param level
     */
    public LevelNode getNode(String source, String parameter, Level level) {
        try {
            List<AbstractRequestableNode> nodes = walkTree(null,
                    Arrays.asList(source), Arrays.asList(parameter),
                    Arrays.asList(level), true, true, null);
            if (nodes == null || nodes.isEmpty()) {
                return null;
            }
            if (nodes.get(0) instanceof AbstractDerivedDataNode) {
                updater.addNode((AbstractDerivedDataNode) nodes.get(0));
            }
            return nodes.get(0);
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured in Grid Inventory: Cannot retrieve node: "
                            + source + "/" + parameter + "/" + level, e);
        }
        return null;
    }

    @Override
    protected DataTree createBaseTree() throws DataCubeException {
        DataTree newTree = super.createBaseTree();
        initGatherModels(newTree);
        initAliasModels(newTree);
        RadarAdapter.getInstance().addRadarBaseTree(newTree, derParLibrary);
        return newTree;
    }

    @Override
    public List<DataTime> timeAgnosticQuery(Map<String, RequestConstraint> query)
            throws DataCubeException {
        List<DataTime> rval = null;
        List<String> sources = getSourcesToProcess(query);
        boolean processRadar = sources != null && sources.contains("radar");
        boolean processGrid = !processRadar || sources.size() > 1;

        if (processGrid) {
            rval = super.timeAgnosticQuery(query);
        }
        if (processRadar) {
            Set<DataTime> times;
            try {
                times = RadarAdapter.getInstance().timeInvariantQuery();
            } catch (VizException e) {
                throw new DataCubeException(e);
            }
            if (rval == null && times != null) {
                rval = new ArrayList<>(times.size());
            }

            if (times != null) {
                rval.addAll(times);
            }
        }
        return rval;
    }

    /**
     * Prepare an alias map, from a modelName to all modelNames that it
     * includes, from highest res to lowest res
     * 
     * @param newGridTree
     */
    private void initAliasModels(DataTree newGridTree) {
        sourceAliases.clear();
        DatasetInfoLookup lookup = DatasetInfoLookup.getInstance();
        for (String modelName : newGridTree.getSources()) {
            DatasetInfo info = lookup.getInfo(modelName);
            if (info != null && info.getAlias() != null) {
                SourceNode source = newGridTree.getSourceNode(modelName);
                SourceNode dest = newGridTree.getSourceNode(info.getAlias());
                if (source != null && dest != null) {
                    List<String> aliases = sourceAliases.get(dest.getValue());
                    if (aliases == null) {
                        aliases = new ArrayList<>();
                        aliases.add(dest.getValue());
                        sourceAliases.put(dest.getValue(), aliases);
                    }
                    aliases.add(source.getValue());
                }
            }
        }
        for (Entry<String, List<String>> aliases : sourceAliases.entrySet()) {
            Collections.sort(aliases.getValue(), new Comparator<String>() {

                @Override
                public int compare(String model1, String model2) {
                    try {
                        // attempt to figure out which model is the highest
                        // resolution.
                        Collection<GridCoverage> coverages1 = CoverageUtils
                                .getInstance().getCoverages(model1);
                        Collection<GridCoverage> coverages2 = CoverageUtils
                                .getInstance().getCoverages(model2);
                        if (coverages1.isEmpty()) {
                            return 1;
                        } else if (coverages2.isEmpty()) {
                            return -1;
                        }
                        double total1 = 0;
                        double total2 = 0;
                        for (GridCoverage coverage : coverages1) {
                            total1 += coverage.getDx();
                            total1 += coverage.getDy();
                        }
                        for (GridCoverage coverage : coverages2) {
                            total2 += coverage.getDx();
                            total2 += coverage.getDy();
                        }
                        Double res1 = total1 / coverages1.size();
                        Double res2 = total2 / coverages2.size();
                        return res1.compareTo(res2);
                    } catch (DataCubeException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to create model aliases, problems with "
                                        + model1 + " and " + model2, e);
                        return 0;
                    }
                }

            });
        }
    }

    public Set<Level> getAvailableLevels(Map<String, RequestConstraint> query) {
        Set<Level> levels = new HashSet<>();
        List<AbstractRequestableNode> nodes = evaluateRequestConstraints(query);
        for (AbstractRequestableNode node : nodes) {
            levels.add(node.getLevel());
        }
        return levels;
    }

    /**
     * Determine all level nodes that meet constraints, these nodes can than be
     * used to timeQuery or retrieveData.
     * 
     * @param query
     * @return
     * @throws DataCubeException
     */
    @Override
    public List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> query) {
        try {
            List<AbstractRequestableNode> nodes = super
                    .evaluateRequestConstraints(query);
            for (AbstractRequestableNode node : nodes) {
                if (node instanceof AbstractDerivedDataNode) {
                    updater.addNode((AbstractDerivedDataNode) node);
                }
            }
            return nodes;
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing request constraints", e);
            return Collections.emptyList();
        }

    }

    public List<String> getEnsembles(Map<String, RequestConstraint> query)
            throws VizException {
        RequestConstraint nameRC = query.get(GridConstants.DATASET_ID);
        if (nameRC == null) {
            // Only bother grabbing nodes with perts
            nameRC = new RequestConstraint(null, ConstraintType.IN);
            nameRC.setConstraintValueList(modelsWithPerts
                    .toArray(new String[0]));
            query = new HashMap<>(query);
            query.put(GridConstants.DATASET_ID, nameRC);
        } else {
            boolean hasPerts = false;
            for (String modelName : modelsWithPerts) {
                if (nameRC.evaluate(modelName)) {
                    hasPerts = true;
                    break;
                }
            }
            // If this query is not valid for any models with perts then it has
            // no perts, don't bother with a query.
            if (!hasPerts) {
                return Collections.emptyList();
            }
        }
        Set<String> ensembles = new HashSet<>();
        for (AbstractRequestableNode node : evaluateRequestConstraints(query)) {
            ensembles.addAll(getEnsembles(node));
        }
        return new ArrayList<>(ensembles);

    }

    protected static List<String> getEnsembles(AbstractRequestableNode node)
            throws VizException {

        if (node instanceof GridRequestableNode) {
            GridRequestableNode gNode = (GridRequestableNode) node;
            if (gNode.getEnsembles() != null) {
                return gNode.getEnsembles();
            }
            Map<String, RequestConstraint> rcMap = gNode
                    .getRequestConstraintMap();
            DbQuery dbQuery = new DbQuery(GridConstants.GRID);
            dbQuery.setDistinctField(GridConstants.ENSEMBLE_ID);
            for (Entry<String, RequestConstraint> entry : rcMap.entrySet()) {
                dbQuery.addConstraint(entry.getKey(), entry.getValue());
            }
            List<String> ensembles = new ArrayList<>();
            for (Object[] ensemble : dbQuery.performQuery()) {
                ensembles.add((String) ensemble[0]);
            }
            gNode.setEnsembles(ensembles);
            return ensembles;
        } else if (node instanceof GatherLevelNode) {
            return Collections.emptyList();
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode dataNode = (AbstractDerivedDataNode) node;
            Set<String> ensembles = new HashSet<>();

            for (Dependency dep : dataNode.getDependencies()) {
                ensembles.addAll(getEnsembles(dep.node));
            }
            return new ArrayList<>(ensembles);
        }
        return Collections.emptyList();
    }

    @Override
    public String get3DMasterLevel(String model) {
        if (model.equals(RadarAdapter.RADAR_SOURCE)) {
            return RadarAdapter.CUBE_MASTER_LEVEL_NAME;
        }
        return super.get3DMasterLevel(model);
    }

    @Override
    protected LevelNode getCubeNode(
            String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels) {
        if (modelName.equals(RadarAdapter.RADAR_SOURCE)) {
            return new RadarCubeLevelNode(cubeLevels, modelName);
        }
        return super.getCubeNode(modelName, cubeLevels);
    }

    @Override
    protected Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        String fieldParamAbbrev = field.getParam();
        /*
         * Check to see if we can set the field from the masterlevel name
         */
        if (level.getMasterLevel().getName().equals(fieldParamAbbrev)) {
            if ("TILT".equals(fieldParamAbbrev)) {
                return new TiltGridDataLevelNode(sNode.getValue(),
                        fieldParamAbbrev, level);
            }
        }
        return super.resolvePluginStaticData(sNode, field, level);
    }

    @Override
    protected AbstractDerivedDataNode createDerivedNode(DerivParamDesc desc,
            DerivParamMethod method, Level level, List<Object> fields,
            SourceNode source) {
        if (method.getMethodType() == MethodType.OTHER
                && method.getName().equalsIgnoreCase("Gather")) {
            AbstractRequestableNode lNode = (AbstractRequestableNode) fields
                    .get(0);

            try {
                if (modelsWithPerts.contains(source.getValue())
                        && getEnsembles(lNode).size() > 1) {
                    return new GatherLevelNode(lNode, desc, method,
                            source.getValue(), level);
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error implementing gather function", e);
            }
            return null;
        }
        return super.createDerivedNode(desc, method, level, fields, source);
    }

    @Override
    public void pointChanged() {
        reinitTree();
    }
}
