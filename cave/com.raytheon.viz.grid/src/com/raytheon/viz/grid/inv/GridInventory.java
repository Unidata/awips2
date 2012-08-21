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

import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.dataplugin.grib.util.StaticGridDataType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.derivparam.tree.ParameterNode;
import com.raytheon.uf.common.derivparam.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.comm.Connector;
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
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode.Dependency;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;
import com.raytheon.uf.viz.derivparam.tree.StaticDataLevelNode;
import com.raytheon.uf.viz.points.IPointChangedListener;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.grid.data.ImportRequestableData;
import com.raytheon.viz.grid.data.StaticGridRequestableData;
import com.raytheon.viz.grid.data.TiltRequestableData;
import com.raytheon.viz.grid.util.CoverageUtils;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * TODO Add Description
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

    public static String MODEL_NAME = "modelName";

    public static String PERT_NUMBER = "perturbationNumber";

    public static String PARAMETER = "parameterAbbreviation";

    public static String LEVEL_ID = "level.id";

    public static final String PLUGIN_NAME_QUERY = "pluginName";

    public static final String MODEL_NAME_QUERY = "modelInfo." + MODEL_NAME;

    public static final String PARAMETER_QUERY = "modelInfo." + PARAMETER;

    public static final String LEVEL_ID_QUERY = "modelInfo." + LEVEL_ID;

    public static final String MASTER_LEVEL_QUERY = "modelInfo.level.masterLevel.name";

    public static final String LEVEL_ONE_QUERY = "modelInfo.level.levelonevalue";

    public static final String LEVEL_TWO_QUERY = "modelInfo.level.leveltwovalue";

    public static final String PERT_QUERY = "modelInfo." + PERT_NUMBER;

    public static final String CUBE_MASTER_LEVEL_NAME = "MB";

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

    private DataTree getTreeFromEdex() throws VizException {
        String request = "from com.raytheon.edex.uengine.tasks.grib import GridCatalog\n"
                + "from com.raytheon.uf.common.message.response import ResponseMessageGeneric\n"
                + "test = GridCatalog()\n"
                + "return ResponseMessageGeneric(test.execute())";
        Object[] tree = null;
        tree = Connector.getInstance().connect(request, null, 60000);
        if (tree != null) {
            return (DataTree) tree[0];
        }
        return null;
    }

    private void initGatherModels(DataTree tree) {
        long startTime = System.currentTimeMillis();
        modelsWithPerts.clear();

        // Construct request. Looking for every modelName that has
        // perturbationNumber >= 0
        DbQueryRequest request = new DbQueryRequest();
        request.setDistinct(true);
        request.addRequestField(MODEL_NAME);
        request.addRequestField(PARAMETER);
        request.addRequestField(LEVEL_ID);
        request.addRequestField(PERT_NUMBER);
        request.addConstraint(PERT_NUMBER, new RequestConstraint("0",
                ConstraintType.GREATER_THAN_EQUALS));
        request.setEntityClass(GribModel.class.getName());

        // Send request and process response
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            for (Map<String, Object> objMap : response.getResults()) {
                String model = String.valueOf(objMap.get(MODEL_NAME));
                String parameter = String.valueOf(objMap.get(PARAMETER));
                String levelId = String.valueOf(objMap.get(LEVEL_ID));
                Integer pert = ((Number) objMap.get(PERT_NUMBER)).intValue();
                LevelNode node = tree.getLevelNode(model, parameter,
                        levelId.toString());
                if (node instanceof GribRequestableLevelNode) {
                    GribRequestableLevelNode gribNode = (GribRequestableLevelNode) node;
                    List<Integer> perts = gribNode.getPerts();
                    if (perts == null) {
                        perts = new ArrayList<Integer>(1);
                        perts.add(pert);
                    } else if (!perts.contains(pert)) {
                        perts = new ArrayList<Integer>(perts);
                        perts.add(pert);
                    }
                    gribNode.setPerts(perts);
                }
                if (!modelsWithPerts.contains(model)) {
                    modelsWithPerts.add(model);
                }
            }
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error determining perterbations, Gather function may be broken",
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
            List<AbstractRequestableLevelNode> nodes = walkTree(null,
                    Arrays.asList(source), Arrays.asList(parameter),
                    Arrays.asList(level), true, true, null);
            if (nodes == null || nodes.isEmpty()) {
                return null;
            }
            if (nodes.get(0) instanceof AbstractDerivedLevelNode) {
                try {
                    updater.addNode((AbstractDerivedLevelNode) nodes.get(0));
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
                    rcMap.put(PLUGIN_NAME_QUERY, new RequestConstraint("grib"));
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
                    lNode = new GribRequestableLevelNode(lNode, rcMap);
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
        Set<String> allAliasModels = new HashSet<String>();
        sourceAliases.clear();
        GribModelLookup lookup = GribModelLookup.getInstance();
        for (String modelName : newGridTree.getSources()) {
            GridModel model = lookup.getModelByName(modelName);
            if (model != null && model.getAlias() != null) {
                SourceNode source = newGridTree.getSourceNode(modelName);
                SourceNode dest = newGridTree.getSourceNode(model.getAlias());
                if (source != null && dest != null) {
                    allAliasModels.add(source.getValue());
                    allAliasModels.add(dest.getValue());
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
        // Requesting coverages all at once is more efficient
        try {
            final Map<String, GridCoverage> coverages = CoverageUtils
                    .getInstance().getCoverages(allAliasModels);

            for (Entry<String, List<String>> aliases : sourceAliases.entrySet()) {
                Collections.sort(aliases.getValue(), new Comparator<String>() {

                    @Override
                    public int compare(String model1, String model2) {
                        GridCoverage coverage1 = coverages.get(model1);
                        Integer res1 = coverage1.getNx() * coverage1.getNy();
                        GridCoverage coverage2 = coverages.get(model2);
                        Integer res2 = coverage2.getNx() * coverage2.getNy();
                        return res2.compareTo(res1);
                    }

                });
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to create model aliases", e);
            return;
        }
    }

    public Set<Level> getAvailableLevels(Map<String, RequestConstraint> query) {
        Set<Level> levels = new HashSet<Level>();
        List<AbstractRequestableLevelNode> nodes = evaluateRequestConstraints(query);
        for (AbstractRequestableLevelNode node : nodes) {
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
    public List<AbstractRequestableLevelNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> query) {
        List<String> sourcesToProcess = getSourcesToProcess(query);
        List<String> paramsToProcess = getParametersToProcess(query);
        List<Level> levelsToProcess = getLevelsToProcess(query);
        if (!levelsToProcess.isEmpty() && !sourcesToProcess.isEmpty()
                && !paramsToProcess.isEmpty()) {
            try {
                List<AbstractRequestableLevelNode> nodes = walkTree(null,
                        sourcesToProcess, paramsToProcess, levelsToProcess,
                        true, true, null);
                try {

                    for (AbstractRequestableLevelNode node : nodes) {
                        if (node instanceof AbstractDerivedLevelNode) {
                            updater.addNode((AbstractDerivedLevelNode) node);
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

        return new ArrayList<AbstractRequestableLevelNode>(0);
    }

    public List<Integer> getPerts(Map<String, RequestConstraint> query)
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
        Set<Integer> perts = new HashSet<Integer>();
        for (AbstractRequestableLevelNode node : evaluateRequestConstraints(query)) {
            perts.addAll(getPerts(node));
        }
        return new ArrayList<Integer>(perts);

    }

    protected static List<Integer> getPerts(AbstractRequestableLevelNode node)
            throws VizException {
        if (node instanceof GribRequestableLevelNode) {
            GribRequestableLevelNode gNode = (GribRequestableLevelNode) node;
            if (gNode.getPerts() != null) {
                return gNode.getPerts();
            }
            Map<String, RequestConstraint> rcMap = gNode
                    .getRequestConstraintMap();
            DbQuery dbQuery = new DbQuery("grib");
            dbQuery.setDistinctField(PERT_QUERY);
            for (Entry<String, RequestConstraint> entry : rcMap.entrySet()) {
                dbQuery.addConstraint(entry.getKey(), entry.getValue());
            }
            List<Integer> perts = new ArrayList<Integer>();
            for (Object[] pert : dbQuery.performQuery()) {
                perts.add((Integer) pert[0]);
            }
            gNode.setPerts(perts);
            return perts;
        } else if (node instanceof GatherLevelNode) {
            return Collections.emptyList();
        } else {
            Set<Integer> perts = new HashSet<Integer>();

            for (Dependency dep : node.getDependencies()) {
                perts.addAll(getPerts(dep.node));
            }
            return new ArrayList<Integer>(perts);
        }
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
    protected AbstractDerivedLevelNode getImportNode(
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
    protected AbstractDerivedLevelNode getImportNode(
            AbstractRequestableLevelNode nodeToImport,
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

        NavigableSet<Level> levels = null;
        try {
            levels = LevelUtilities
                    .getOrderedSetOfStandardLevels(masterLevelName);
        } catch (VizCommunicationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }
        List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> cubeLevels = new ArrayList<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>>(
                levels.size());

        for (Level fieldLevel : levels) {
            AbstractRequestableLevelNode pressure = resolveNode(sNode, "P",
                    fieldLevel, stack, nodata);
            if (pressure != null) {
                AbstractRequestableLevelNode param = resolveNode(sNode,
                        field.getParam(), fieldLevel, stack, nodata);

                if (param != null) {
                    CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode> cl = new CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>(
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
            StaticGridDataType staticGridDataType = StaticGridDataType
                    .valueOf(fieldParamAbbrev);
            return new StaticGridRequestableData(staticGridDataType,
                    sNode.getValue());
        }
        // Check to see if we can set the field from the
        // masterlevel name
        if (level.getMasterLevel().getName().equals(fieldParamAbbrev)) {
            if ("TILT".equals(fieldParamAbbrev)) {
                return new TiltRequestableData(sNode.getValue(), level);
            }
        }
        return null;
    }

    protected AbstractDerivedLevelNode createDerivedNode(DerivParamDesc desc,
            DerivParamMethod method, Level level, List<Object> fields,
            SourceNode source) {
        if (method.getMethodType() == MethodType.OTHER
                && method.getName().equalsIgnoreCase("Gather")) {
            AbstractRequestableLevelNode lNode = (AbstractRequestableLevelNode) fields
                    .get(0);

            try {
                if (modelsWithPerts.contains(source.getValue())
                        && getPerts(lNode).size() > 1) {
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
