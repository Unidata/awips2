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
package com.raytheon.uf.common.dataplugin.grid.derivparam;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.derivparam.cache.GridTimeCache;
import com.raytheon.uf.common.dataplugin.grid.derivparam.data.ImportRequestableData;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.CubeLevelNode;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.GridRequestableNode;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.ImportLevelNode;
import com.raytheon.uf.common.dataplugin.grid.derivparam.tree.StaticGridDataLevelNode;
import com.raytheon.uf.common.dataplugin.grid.request.GetGridTreeRequest;
import com.raytheon.uf.common.dataplugin.grid.util.StaticGridDataType;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.util.LevelUtilities;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.derivparam.tree.StaticDataLevelNode;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.ParameterNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;

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
public class CommonGridInventory extends AbstractInventory {

    public static final String CUBE_MASTER_LEVEL_NAME = "MB";

    @Override
    protected DataTree createBaseTree() throws DataCubeException {
        DataTree tree = null;
        try {
            tree = (DataTree) RequestRouter.route(new GetGridTreeRequest());
        } catch (Exception e) {
            throw new DataCubeException("Error requesting the Grid DataTree", e);
        }
        for (SourceNode sNode : tree.getSourceNodes().values()) {
            for (ParameterNode pNode : sNode.getChildNodes().values()) {
                for (Entry<String, LevelNode> lEntry : pNode.getChildNodes()
                        .entrySet()) {
                    LevelNode lNode = lEntry.getValue();
                    Level level = lEntry.getValue().getLevel();
                    Map<String, RequestConstraint> rcMap = new HashMap<>();
                    rcMap.put(PluginDataObject.PLUGIN_NAME_ID,
                            new RequestConstraint(GridConstants.GRID));
                    rcMap.put(GridConstants.DATASET_ID, new RequestConstraint(
                            sNode.getValue()));
                    rcMap.put(GridConstants.PARAMETER_ABBREVIATION,
                            new RequestConstraint(pNode.getValue()));
                    rcMap.put(GridConstants.MASTER_LEVEL_NAME,
                            new RequestConstraint(level.getMasterLevel()
                                    .getName()));
                    rcMap.put(GridConstants.LEVEL_ONE, new RequestConstraint(
                            Double.toString(level.getLevelonevalue())));
                    rcMap.put(GridConstants.LEVEL_TWO, new RequestConstraint(
                            Double.toString(level.getLeveltwovalue())));
                    lNode = new GridRequestableNode(lNode, rcMap);
                    pNode.getChildNodes().put(lEntry.getKey(), lNode);
                }
            }
        }
        return tree;
    }

    public List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> query) throws InterruptedException {
        List<String> sourcesToProcess = getSourcesToProcess(query);
        List<String> paramsToProcess = getParametersToProcess(query);
        List<Level> levelsToProcess = getLevelsToProcess(query);
        if (!levelsToProcess.isEmpty() && !sourcesToProcess.isEmpty()
                && !paramsToProcess.isEmpty()) {
            List<AbstractRequestableNode> nodes = walkTree(null,
                    sourcesToProcess, paramsToProcess, levelsToProcess, true,
                    true, null);
            return nodes;
        }

        return new ArrayList<>(0);
    }

    /**
     * For a set of constraints, get all sources which meet them
     * 
     * @param query
     * @return
     */
    protected List<String> getSourcesToProcess(
            Map<String, RequestConstraint> query) {
        RequestConstraint sRC = query.get(GridConstants.DATASET_ID);
        if (sRC != null) {
            List<String> sourcesToProcess = new ArrayList<>();
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
            return new ArrayList<>(getAllSources());
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
    protected List<String> getParametersToProcess(
            Map<String, RequestConstraint> query) {
        RequestConstraint pRC = query.get(GridConstants.PARAMETER_ABBREVIATION);
        if (pRC != null) {
            List<String> paramsToProcess = new ArrayList<>();
            for (String param : getAllParameters()) {
                if (pRC.evaluate(param)) {
                    paramsToProcess.add(param);
                }
            }
            return paramsToProcess;
        } else {
            return new ArrayList<>(getAllParameters());
        }
    }

    /**
     * For a set of constraints, get all levels which meet them
     * 
     * @param query
     * @return
     */
    protected List<Level> getLevelsToProcess(
            Map<String, RequestConstraint> query) {

        RequestConstraint masterLevelRC = query
                .get(GridConstants.MASTER_LEVEL_NAME);
        RequestConstraint levelOneRC = query.get(GridConstants.LEVEL_ONE);
        RequestConstraint levelTwoRC = query.get(GridConstants.LEVEL_TWO);

        if (masterLevelRC != null || levelOneRC != null || levelTwoRC != null) {
            List<Level> levelsToProcess = new ArrayList<>();
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
            return new ArrayList<>(getAllLevels());
        }
    }

    @Override
    public List<DataTime> timeAgnosticQuery(Map<String, RequestConstraint> query)
            throws DataCubeException {
        List<DataTime> rval = null;
        Map<String, RequestConstraint> newQuery = new HashMap<>(query);
        newQuery.remove(GridConstants.PARAMETER_ABBREVIATION);
        newQuery.remove(GridConstants.LEVEL_ID);
        newQuery.remove(GridConstants.LEVEL_ONE);
        newQuery.remove(GridConstants.LEVEL_TWO);
        newQuery.remove(GridConstants.MASTER_LEVEL_NAME);
        String modelName = null;
        Collection<DataTime> times = null;
        if (newQuery.size() == 2
                && newQuery.containsKey(PluginDataObject.PLUGIN_NAME_ID)
                && newQuery.containsKey(GridConstants.DATASET_ID)) {
            // Only use the cache if the only constraint left are pluginName
            // and datasetId and datasetId is an Equals constraint. This is
            // almost always the case.
            RequestConstraint modelRc = newQuery.get(GridConstants.DATASET_ID);
            if (modelRc.getConstraintType() == ConstraintType.EQUALS) {
                modelName = modelRc.getConstraintValue();
                times = GridTimeCache.getInstance().getModelTimes(modelName);
            }
        }
        if (times == null) {
            // This should query all times for this model.
            DataTime[] timesArray;
            try {
                timesArray = performTimeQuery(newQuery, false, null);
            } catch (Exception e) {
                throw new DataCubeException(e);
            }
            times = Arrays.asList(timesArray);
            if (modelName != null) {
                GridTimeCache.getInstance().setModelTimes(modelName,
                        new HashSet<>(times));
            }
        }
        if (times != null) {
            rval = new ArrayList<>(times);
        }
        return rval;
    }

    public static DataTime[] performTimeQuery(
            Map<String, RequestConstraint> constraintMap, boolean max,
            BinOffset binOffset) throws Exception {
        RequestConstraint plugin = constraintMap
                .get(PluginDataObject.PLUGIN_NAME_ID);

        String pluginName = plugin.getConstraintValue();
        TimeQueryRequest req = new TimeQueryRequest();
        req.setMaxQuery(max);
        req.setPluginName(pluginName);
        req.setBinOffset(binOffset);
        req.setQueryTerms(constraintMap);

        List<?> result = (List<?>) RequestRouter.route(req);
        return result.toArray(new DataTime[0]);
    }

    /**
     * Performs the same functionality as
     * {@link #checkSources(Collection, Collection, Collection, BlockingQueue)}
     * but uses the provided map of request constraints to limit the sources,
     * parameters and levels to check.
     */
    public void checkSources(Map<String, RequestConstraint> query,
            BlockingQueue<String> returnQueue) throws InterruptedException {
        List<String> sourcesToCheck = getSourcesToProcess(query);
        List<String> paramsToProcess = getParametersToProcess(query);
        List<Level> levelsToProcess = getLevelsToProcess(query);
        super.checkSources(sourcesToCheck, paramsToProcess, levelsToProcess,
                returnQueue);
    }

    /**
     * Performs the same functionality as
     * {@link #checkParameters(Collection, Collection, Collection, boolean, BlockingQueue)}
     * but uses the provided map of request constraints to limit the sources,
     * parameters and levels to check.
     */
    public void checkParameters(Map<String, RequestConstraint> query,
            boolean includeConstant, BlockingQueue<String> returnQueue)
            throws InterruptedException {
        List<String> sourcesToProcess = getSourcesToProcess(query);
        List<String> paramsToCheck = getParametersToProcess(query);
        List<Level> levelsToProcess = getLevelsToProcess(query);
        super.checkParameters(sourcesToProcess, paramsToCheck, levelsToProcess,
                includeConstant, returnQueue);
    }

    /**
     * Performs the same functionality as
     * {@link #checkLevels(Collection, Collection, Collection, BlockingQueue)}
     * but uses the provided map of request constraints to limit the sources,
     * parameters and levels to check.
     */
    public void checkLevels(Map<String, RequestConstraint> query,
            BlockingQueue<String> returnQueue) throws InterruptedException {
        List<String> sourcesToProcess = getSourcesToProcess(query);
        List<String> paramsToProcess = getParametersToProcess(query);
        List<Level> levelsToCheck = getLevelsToProcess(query);
        super.checkLevels(sourcesToProcess, paramsToProcess, levelsToCheck,
                returnQueue);
    }

    @Override
    protected LevelNode getCubeNode(SourceNode sNode, DerivParamField field,
            Deque<StackEntry> stack, Set<StackEntry> nodata) {
        StackEntry se = new StackEntry(sNode.getValue(), field.getParam(),
                Long.MIN_VALUE);
        if (stack.contains(se)) {
            /* Prevent infinite recursion. */
            return null;
        }
        ParameterNode pNode = sNode.getChildNode(field.getParam());
        /* First check if the node already exists and return early if it does. */
        LevelNode cNode = pNode == null ? null : pNode.getChildNode("3D");
        if (cNode != null) {
            return cNode;
        }
        stack.push(se);
        String masterLevelName = get3DMasterLevel(sNode.getValue());

        NavigableSet<Level> levels;
        /* All the possible levels that could make up the cube. */
        levels = LevelUtilities.getOrderedSetOfStandardLevels(masterLevelName);
        List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels = new ArrayList<>(
                levels.size());

        /*
         * A level is only valid if we have data for pressure and for the
         * specific field passed in.
         */
        for (Level fieldLevel : levels) {
            AbstractRequestableNode pressure = resolveNode(sNode, "P",
                    fieldLevel, stack, nodata);
            if (pressure != null) {
                AbstractRequestableNode param = resolveNode(sNode,
                        field.getParam(), fieldLevel, stack, nodata);

                if (param != null) {
                    CubeLevel<AbstractRequestableNode, AbstractRequestableNode> cl = new CubeLevel<>(
                            pressure, param);
                    cubeLevels.add(cl);
                }
            }
        }
        /* There must be at least three valid levels or the cube is not valid. */
        if (cubeLevels.size() >= 3) {
            cNode = getCubeNode(sNode.getValue(), cubeLevels);
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

    /**
     * Create a source specific cube node given a list of valid
     * {@link CubeLevel}s. The default implementation just makes a standard
     * {@link CubeLevelNode} but subclasses may want to override this behavior
     * for certain models.
     */
    protected LevelNode getCubeNode(
            String modelName,
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> cubeLevels) {
        return new CubeLevelNode(cubeLevels, modelName);
    }

    /**
     * Get the name of the master level that should be used for a specific
     * source. The default is always MB but subclasses may want to override it
     * for a specific source.
     */
    public String get3DMasterLevel(String model) {
        return CUBE_MASTER_LEVEL_NAME;
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
        return new ImportLevelNode(this, nodeToImport, nodeToImportSourceName,
                desc, method, destSourceNode.getValue(), level);
    }


    @Override
    protected Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        String fieldParamAbbrev = field.getParam();
        if (StaticGridDataType.getStringValues().contains(fieldParamAbbrev)) {
            return new StaticGridDataLevelNode(sNode.getValue(),
                    fieldParamAbbrev);
        }
        return null;
    }

}
