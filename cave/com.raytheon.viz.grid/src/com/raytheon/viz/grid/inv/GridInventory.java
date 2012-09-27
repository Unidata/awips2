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
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableSet;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.grid.request.GetGridTreeRequest;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridDataType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.derivparam.tree.ParameterNode;
import com.raytheon.uf.common.derivparam.tree.SourceNode;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.level.LevelUtilities;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AbstractInventory;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod.MethodType;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;
import com.raytheon.uf.viz.derivparam.tree.StaticDataLevelNode;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.grid.data.ImportRequestableData;
import com.raytheon.viz.grid.util.CoverageUtils;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * Inventory object for calculating and managing the available grid data,
 * including any data that can be derived.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2009            brockwoo    Initial creation
 * Nov 20, 2009 #3387      jelkins     Use derived script's variableId instead of filename
 * Nov 21, 2009 #3576      rjpeter     Refactored use of DerivParamDesc.
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class GridInventory extends AbstractInventory implements
        IPointChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridInventory.class);

    public static final String PLUGIN_NAME_QUERY = GridConstants.PLUGIN_NAME;

    public static final String MODEL_NAME_QUERY = GridConstants.DATASET_ID;

    public static final String PARAMETER_QUERY = GridConstants.PARAMETER_ABBREVIATION;

    public static final String LEVEL_ID_QUERY = GridConstants.LEVEL_ID;

    public static final String MASTER_LEVEL_QUERY = GridConstants.MASTER_LEVEL_NAME;

    public static final String LEVEL_ONE_QUERY = GridConstants.LEVEL_ONE;

    public static final String LEVEL_TWO_QUERY = GridConstants.LEVEL_TWO;

    public static final String ENSEMBLE_QUERY = GridConstants.ENSEMBLE_ID;

    public static final String CUBE_MASTER_LEVEL_NAME = "MB";

    public static final String PLUGIN_NAME = GridConstants.GRID;

    private GridUpdater updater;

    private List<String> modelsWithPerts = new ArrayList<String>();

    private List<Map<String, RequestConstraint>> failedRequests = new ArrayList<Map<String, RequestConstraint>>();

    @Override
    public void initTree(Map<String, DerivParamDesc> derParLibrary)
            throws VizException {
        super.initTree(derParLibrary);
        if (updater == null) {
            updater = new GridUpdater(this);
            updater.startObserving();
            // Currently only one instance of GridInventory is created by
            // the GribDataCubeAdapter and lasts for the life of
            // CAVE. Thus no need for removal of this listener.
            PointsDataManager.getInstance().addHomeChangedListener(this);
        } else {
            updater.refreshNodes();
        }

    }

    public void reinitTree() {
        try {
            initTree(derParLibrary);
            // reprocess all failed requests to see if data has become
            // available.
            List<Map<String, RequestConstraint>> constraintsToTry = this.failedRequests;
            this.failedRequests = new ArrayList<Map<String, RequestConstraint>>(
                    failedRequests.size());
            for (Map<String, RequestConstraint> constraints : constraintsToTry) {
                evaluateRequestConstraints(constraints);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    private DataTree getTreeFromEdex() {
        GetGridTreeRequest request = new GetGridTreeRequest();
        Object tree = null;
        try {
            tree = ThriftClient.sendRequest(request);
        } catch (VizCommunicationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error communicating with server.", e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred while retrieving grid tree.", e);
        }
        return (DataTree) tree;
    }

    private void initGatherModels(DataTree tree) {
        long startTime = System.currentTimeMillis();
        modelsWithPerts.clear();

        // Construct request. Looking for every modelName that has
        // perturbationNumber >= 0
        DbQueryRequest request = new DbQueryRequest();
        request.setDistinct(true);
        request.addRequestField(GridInfoConstants.DATASET_ID);
        request.addRequestField(GridInfoConstants.PARAMETER_ABBREVIATION);
        request.addRequestField(GridInfoConstants.LEVEL_ID);
        request.addRequestField(GridInfoConstants.ENSEMBLE_ID);
        request.addConstraint(GridInfoConstants.ENSEMBLE_ID,
                new RequestConstraint("", ConstraintType.NOT_EQUALS));
        request.setEntityClass(GridInfoRecord.class.getName());

        // Send request and process response
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
                        ensembles = new ArrayList<String>(1);
                        ensembles.add(ensemble);
                    } else if (!ensembles.contains(ensemble)) {
                        ensembles = new ArrayList<String>(ensembles);
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
                try {
                    updater.addNode((AbstractDerivedDataNode) nodes.get(0));
                } catch (VizException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error occured in Grid Inventory: Grid Update may be broken",
                                    e);
                    return null;
                }
            }
            return nodes.get(0);
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured in Grid Inventory: Cannot retrieve node: "
                            + source + "/" + parameter + "/" + level, e);
        }
        return null;
    }

    protected DataTree createBaseTree() throws VizException {
        DataTree newTree = getTreeFromEdex();
        if (newTree == null) {
            return newTree;
        }
        for (SourceNode sNode : newTree.getSourceNodes().values()) {
            for (ParameterNode pNode : sNode.getChildNodes().values()) {
                for (Entry<String, LevelNode> lEntry : pNode.getChildNodes()
                        .entrySet()) {
                    LevelNode lNode = new LevelNode();
                    lNode.setLevel(lEntry.getValue().getLevel());
                    Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
                    rcMap.put(PLUGIN_NAME_QUERY, new RequestConstraint(
                            PLUGIN_NAME));
                    rcMap.put(MODEL_NAME_QUERY,
                            new RequestConstraint(sNode.getValue()));
                    rcMap.put(PARAMETER_QUERY,
                            new RequestConstraint(pNode.getValue()));
                    rcMap.put(MASTER_LEVEL_QUERY, new RequestConstraint(lNode
                            .getLevel().getMasterLevel().getName()));
                    rcMap.put(
                            LEVEL_ONE_QUERY,
                            new RequestConstraint(Double.toString(lNode
                                    .getLevel().getLevelonevalue())));
                    rcMap.put(
                            LEVEL_TWO_QUERY,
                            new RequestConstraint(Double.toString(lNode
                                    .getLevel().getLeveltwovalue())));
                    lNode = new GridRequestableNode(lNode, rcMap);
                    pNode.getChildNodes().put(lEntry.getKey(), lNode);
                }
                sNode.addChildNode(pNode);
            }
        }
        initGatherModels(newTree);
        initAliasModels(newTree);
        RadarAdapter.getInstance().addRadarBaseTree(newTree, derParLibrary);
        return newTree;
    }

    @Override
    public List<DataTime> timeAgnosticQuery(Map<String, RequestConstraint> query)
            throws VizException {
        List<DataTime> rval = null;
        List<String> sources = getSourcesToProcess(query);
        boolean processRadar = sources != null && sources.contains("radar");
        boolean processGrid = !processRadar || sources.size() > 1;

        if (processGrid) {
            Map<String, RequestConstraint> newQuery = new HashMap<String, RequestConstraint>(
                    query);
            newQuery.remove(GridInventory.PARAMETER_QUERY);
            newQuery.remove(GridInventory.LEVEL_ID_QUERY);
            newQuery.remove(GridInventory.LEVEL_ONE_QUERY);
            newQuery.remove(GridInventory.LEVEL_TWO_QUERY);
            newQuery.remove(GridInventory.MASTER_LEVEL_QUERY);
            // Hopefully this results in querying all times for this model.
            DataTime[] times = CatalogQuery.performTimeQuery(newQuery, false,
                    null);

            if (times != null) {
                rval = new ArrayList<DataTime>(Arrays.asList(times));
            }
        }
        if (processRadar) {
            Set<DataTime> times = RadarAdapter.getInstance()
                    .timeInvariantQuery();
            if (rval == null && times != null) {
                rval = new ArrayList<DataTime>(times.size());
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
                        aliases = new ArrayList<String>();
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
                    } catch (VizException e) {
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
        Set<Level> levels = new HashSet<Level>();
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
     */
    public List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> query) {
        List<String> sourcesToProcess = getSourcesToProcess(query);
        List<String> paramsToProcess = getParametersToProcess(query);
        List<Level> levelsToProcess = getLevelsToProcess(query);
        if (!levelsToProcess.isEmpty() && !sourcesToProcess.isEmpty()
                && !paramsToProcess.isEmpty()) {
            try {
                List<AbstractRequestableNode> nodes = walkTree(null,
                        sourcesToProcess, paramsToProcess, levelsToProcess,
                        true, true, null);
                try {

                    for (AbstractRequestableNode node : nodes) {
                        if (node instanceof AbstractDerivedDataNode) {
                            updater.addNode((AbstractDerivedDataNode) node);
                        }
                    }
                } catch (VizException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Error occured in Grid Inventory: Grid Update may be broken",
                                    e);
                }
                return nodes;
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error processing request constraints", e);
            }
        } else {
            if (!failedRequests.contains(query)) {
                failedRequests
                        .add(new HashMap<String, RequestConstraint>(query));
            }
        }

        return new ArrayList<AbstractRequestableNode>(0);
    }

    public List<String> getEnsembles(Map<String, RequestConstraint> query)
            throws VizException {
        RequestConstraint nameRC = query.get(MODEL_NAME_QUERY);
        if (nameRC == null) {
            // Only bother grabbing nodes with perts
            nameRC = new RequestConstraint(null, ConstraintType.IN);
            nameRC.setConstraintValueList(modelsWithPerts
                    .toArray(new String[0]));
            query = new HashMap<String, RequestConstraint>(query);
            query.put(MODEL_NAME_QUERY, nameRC);
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
        Set<String> ensembles = new HashSet<String>();
        for (AbstractRequestableNode node : evaluateRequestConstraints(query)) {
            ensembles.addAll(getEnsembles(node));
        }
        return new ArrayList<String>(ensembles);

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
            DbQuery dbQuery = new DbQuery(PLUGIN_NAME);
            dbQuery.setDistinctField(ENSEMBLE_QUERY);
            for (Entry<String, RequestConstraint> entry : rcMap.entrySet()) {
                dbQuery.addConstraint(entry.getKey(), entry.getValue());
            }
            List<String> ensembles = new ArrayList<String>();
            for (Object[] ensemble : dbQuery.performQuery()) {
                ensembles.add((String) ensemble[0]);
            }
            gNode.setEnsembles(ensembles);
            return ensembles;
        } else if (node instanceof GatherLevelNode) {
            return Collections.emptyList();
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode dataNode = (AbstractDerivedDataNode) node;
            Set<String> ensembles = new HashSet<String>();

            for (Dependency dep : dataNode.getDependencies()) {
                ensembles.addAll(getEnsembles(dep.node));
            }
            return new ArrayList<String>(ensembles);
        }
        return Collections.emptyList();
    }

    /**
     * For a set of constraints, get all sources which meet them
     * 
     * @param query
     * @return
     */
    private List<String> getSourcesToProcess(
            Map<String, RequestConstraint> query) {
        RequestConstraint sRC = query.get(MODEL_NAME_QUERY);
        if (sRC != null) {
            List<String> sourcesToProcess = new ArrayList<String>();
            for (String source : getAllSources()) {
                if (sRC.evaluate(source)) {
                    List<String> aliases = sourceAliases.get(source);
                    if (aliases == null) {
                        sourcesToProcess.add(source);
                    } else {
                        sourcesToProcess.addAll(aliases);
                    }
                }
            }
            return sourcesToProcess;
        } else {
            return new ArrayList<String>(getAllSources());
        }
    }

    /**
     * For a set of constraints, get all parameters which meet them, these
     * parameters may already have nodes on the tree or they might need to be
     * checked for derivability.
     * 
     * @param query
     * @return
     */
    private List<String> getParametersToProcess(
            Map<String, RequestConstraint> query) {
        RequestConstraint pRC = query.get(PARAMETER_QUERY);
        if (pRC != null) {
            List<String> paramsToProcess = new ArrayList<String>();
            for (String param : getAllParameters()) {
                if (pRC.evaluate(param)) {
                    paramsToProcess.add(param);
                }
            }
            return paramsToProcess;
        } else {
            return new ArrayList<String>(getAllParameters());
        }
    }

    /**
     * For a set of constraints, get all levels which meet them
     * 
     * @param query
     * @return
     */
    private List<Level> getLevelsToProcess(Map<String, RequestConstraint> query) {

        RequestConstraint masterLevelRC = query.get(MASTER_LEVEL_QUERY);
        RequestConstraint levelOneRC = query.get(LEVEL_ONE_QUERY);
        RequestConstraint levelTwoRC = query.get(LEVEL_TWO_QUERY);

        if (masterLevelRC != null || levelOneRC != null || levelTwoRC != null) {
            List<Level> levelsToProcess = new ArrayList<Level>();
            for (Level level : getAllLevels()) {
                boolean flag = true;
                if (masterLevelRC != null
                        && !masterLevelRC.evaluate(level.getMasterLevel()
                                .getName())) {
                    flag = false;
                }
                if (levelOneRC != null
                        && !levelOneRC.evaluate(level.getLevelonevalue())) {
                    flag = false;
                }
                if (levelTwoRC != null
                        && !levelTwoRC.evaluate(level.getLeveltwovalue())) {
                    flag = false;
                }
                if (flag) {
                    levelsToProcess.add(level);
                }
            }
            return levelsToProcess;
        } else {
            return new ArrayList<Level>(getAllLevels());
        }
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableData nodeToImport, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        AbstractRequestableData data = new ImportRequestableData(nodeToImport,
                null, null);
        data.setLevel(level);
        data.setParameter(desc.getAbbreviation());
        data.setParameterName(desc.getName());
        data.setSource(destSourceNode.getValue());
        data.setUnit(desc.getUnit());
        return new StaticDataLevelNode(level, desc, method, data,
                destSourceNode.getValue());
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableNode nodeToImport,
            String nodeToImportSourceName, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        return new ImportLevelNode(nodeToImport, nodeToImportSourceName, desc,
                method, destSourceNode.getValue(), level);
    }

    public String get3DMasterLevel(String model) {
        if (model.equals(RadarAdapter.RADAR_SOURCE)) {
            return RadarAdapter.CUBE_MASTER_LEVEL_NAME;
        }
        return CUBE_MASTER_LEVEL_NAME;
    }

    @Override
    protected LevelNode getCubeNode(SourceNode sNode, DerivParamField field,
            Deque<StackEntry> stack, Set<StackEntry> nodata)
            throws VizCommunicationException {
        StackEntry se = new StackEntry(sNode.getValue(), field.getParam(),
                Long.MIN_VALUE);
        if (stack.contains(se)) {
            return null;
        }
        ParameterNode pNode = sNode.getChildNode(field.getParam());
        LevelNode cNode = pNode == null ? null : pNode.getChildNode("3D");
        if (cNode != null) {
            return cNode;
        }
        stack.push(se);
        String masterLevelName = get3DMasterLevel(sNode.getValue());
        boolean isRadar = sNode.getValue().equals(RadarAdapter.RADAR_SOURCE);

        NavigableSet<Level> levels = LevelUtilities
                .getOrderedSetOfStandardLevels(masterLevelName);
        List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels = new ArrayList<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>>(
                levels.size());

        for (Level fieldLevel : levels) {
            AbstractRequestableNode pressure = resolveNode(sNode, "P",
                    fieldLevel, stack, nodata);
            if (pressure != null) {
                AbstractRequestableNode param = resolveNode(sNode,
                        field.getParam(), fieldLevel, stack, nodata);

                if (param != null) {
                    CubeLevel<AbstractRequestableNode, AbstractRequestableNode> cl = new CubeLevel<AbstractRequestableNode, AbstractRequestableNode>(
                            pressure, param);
                    cubeLevels.add(cl);
                }
            }
        }
        if (cubeLevels.size() >= 3) {
            cNode = null;
            if (isRadar) {
                cNode = new RadarCubeLevelNode(cubeLevels, sNode.getValue(),
                        field);
            } else {
                cNode = new CubeLevelNode(cubeLevels, sNode.getValue());
            }
            if (pNode == null) {
                pNode = sNode.getChildNode(field.getParam());
            }
            pNode.addChildNode(cNode);
            stack.pop();
            return cNode;
        }
        stack.pop();
        return null;
    }

    protected Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        String fieldParamAbbrev = field.getParam();
        if (StaticGridDataType.getStringValues().contains(fieldParamAbbrev)) {
            return new StaticGridDataLevelNode(sNode.getValue(),
                    fieldParamAbbrev);
        }
        // Check to see if we can set the field from the
        // masterlevel name
        if (level.getMasterLevel().getName().equals(fieldParamAbbrev)) {
            if ("TILT".equals(fieldParamAbbrev)) {
                return new StaticGridDataLevelNode(sNode.getValue(),
                        fieldParamAbbrev, level);
            }
        }
        return null;
    }

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.IHomeChangedListener#homeLocationChanged()
     */
    @Override
    public void pointChanged() {
        reinitTree();
    }
}
