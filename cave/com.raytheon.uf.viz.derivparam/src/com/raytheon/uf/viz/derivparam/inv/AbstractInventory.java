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
package com.raytheon.uf.viz.derivparam.inv;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.concurrent.BlockingQueue;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.AbstractNode;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.derivparam.tree.ParameterNode;
import com.raytheon.uf.common.derivparam.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.core.level.LevelUtilities;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.FloatRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamConstantField;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod.FrameworkMethod;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator.DerivParamUpdateListener;
import com.raytheon.uf.viz.derivparam.library.IDerivParamField;
import com.raytheon.uf.viz.derivparam.library.LevelType;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.uf.viz.derivparam.tree.AliasLevelNode;
import com.raytheon.uf.viz.derivparam.tree.CompositeAverageLevelNode;
import com.raytheon.uf.viz.derivparam.tree.DerivedLevelNode;
import com.raytheon.uf.viz.derivparam.tree.OrLevelNode;
import com.raytheon.uf.viz.derivparam.tree.StaticDataLevelNode;
import com.raytheon.uf.viz.derivparam.tree.TimeRangeLevelNode;
import com.raytheon.uf.viz.derivparam.tree.UnionLevelNode;

/**
 * 
 * The Inventory is responsible for managing what parameters can be derived. It
 * maintains a dataTree which contains all the base parameter as well as any
 * previously used derived parameters. It can dynamically calculate what
 * parameters can be derived using the walk tree method.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractInventory implements DerivParamUpdateListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractInventory.class);

    protected class StackEntry {

        public StackEntry(String source, String parameter, long level) {
            super();
            this.source = source;
            this.parameter = parameter;
            this.level = level;
        }

        final public String source;

        final public String parameter;

        final public long level;

        public boolean recursive = false;

        public boolean autoAverage = false;

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + (int) (level ^ (level >>> 32));
            result = prime * result
                    + ((source == null) ? 0 : source.hashCode());
            result = prime * result
                    + ((parameter == null) ? 0 : parameter.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            StackEntry other = (StackEntry) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (level != other.level)
                return false;
            if (source == null) {
                if (other.source != null)
                    return false;
            } else if (!source.equals(other.source))
                return false;
            if (parameter == null) {
                if (other.parameter != null)
                    return false;
            } else if (!parameter.equals(other.parameter))
                return false;
            return true;
        }

        private AbstractInventory getOuterType() {
            return AbstractInventory.this;
        }

    };

    protected DataTree dataTree;

    protected Map<String, DerivParamDesc> derParLibrary;

    protected Map<String, List<String>> sourceAliases = new HashMap<String, List<String>>();

    protected List<String> allSources;

    protected List<String> allParameters;

    protected List<Level> allLevels;

    /**
     * A call to this method assigns the passed grid tree to the original grid
     * tree and populates it with available derived parameters based on what is
     * available from the base parameters.
     * 
     * @throws VizException
     * 
     * 
     */
    public synchronized void initTree(Map<String, DerivParamDesc> derParLibrary)
            throws VizException {
        DerivedParameterGenerator.registerUpdateListener(this);
        if (derParLibrary == null) {
            this.derParLibrary = new HashMap<String, DerivParamDesc>(0);
        } else {
            this.derParLibrary = derParLibrary;
        }
        long startTime = System.currentTimeMillis();
        DataTree newTree = null;
        newTree = createBaseTree();
        if (newTree == null) {
            return;
        }
        allSources = null;
        allParameters = null;
        allLevels = null;
        dataTree = newTree;
        for (SourceNode sourceNode : dataTree.getSourceNodes().values()) {
            try {
                doSupplement(sourceNode);
            } catch (VizCommunicationException e) {
                // TODO need to recover from this
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            for (ParameterNode parameterNode : sourceNode.getChildNodes()
                    .values()) {
                String value = parameterNode.getValue();
                if (this.derParLibrary.containsKey(value)) {
                    DerivParamDesc derivParamDesc = this.derParLibrary
                            .get(value);
                    parameterNode.setParameterName(derivParamDesc.getName());
                }
            }
        }
        System.out.println("Time to initialize "
                + this.getClass().getSimpleName() + ": "
                + (System.currentTimeMillis() - startTime) + "ms");
    }

    @Override
    public void updateDerParLibrary(Map<String, DerivParamDesc> derParLibrary) {
        try {
            initTree(derParLibrary);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public String getParameterUnit(String source, String parameter) {
        ParameterNode node = dataTree.getParameterNode(source, parameter);
        if (node != null) {
            return node.getParameterUnit();
        }
        return null;
    }

    public String getParameterName(String source, String parameter) {
        ParameterNode node = dataTree.getParameterNode(source, parameter);
        if (node != null) {
            return node.getParameterName();
        }
        return null;
    }

    /**
     * Attempt to derive every possible parameter on every possible level for
     * all sources, this may take awhile.
     */
    protected void deriveCompleteTree() {
        try {
            walkTree(null, getAllSources(), getAllParameters(), getAllLevels(),
                    true, true, null);
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error calculating derived parameters for grid tree", e);
        }

    }

    /**
     * Resolve any Supplement Derived Parameters.
     * 
     * @param sNode
     * @throws VizCommunicationException
     */
    private void doSupplement(SourceNode sNode)
            throws VizCommunicationException {
        Set<StackEntry> nodata = new HashSet<StackEntry>();
        Deque<StackEntry> stack = new ArrayDeque<StackEntry>();
        for (DerivParamDesc desc : derParLibrary.values()) {
            List<DerivParamMethod> methods = desc.getMethods();
            if (methods == null || methods.isEmpty()) {
                continue;
            }
            ParameterNode pNode = sNode.getChildNode(desc.getAbbreviation());
            if (pNode == null) {
                continue;
            }
            for (DerivParamMethod method : methods) {
                if (method.getFrameworkMethod() != FrameworkMethod.SUPPLEMENT) {
                    continue;
                }
                List<String> validModels = method.getValidModels();
                if (validModels != null
                        && !validModels.contains(sNode.getValue())) {
                    continue;
                }
                for (LevelNode lNode : pNode.getChildNodes().values()) {
                    Set<Level> validLevels = method.getValidLevels();
                    if (validLevels != null
                            && !validLevels.contains(lNode.getLevel())) {
                        continue;
                    }
                    List<Object> request = new ArrayList<Object>();
                    request.add(lNode);
                    for (IDerivParamField ifield : method.getFields()) {
                        Object result = resolveField(sNode, lNode.getLevel(),
                                ifield, stack, nodata);
                        if (result != null) {
                            request.add(result);
                        }
                    }
                    AbstractDerivedDataNode newNode = createDerivedNode(desc,
                            method, lNode.getLevel(), request, sNode);
                    pNode.getChildNodes().put(lNode.getValue(), newNode);
                }

            }
        }
    }

    protected abstract DataTree createBaseTree() throws VizException;

    /**
     * Handle a query for a time agnostic node.
     * 
     * @param query
     * @return
     */
    public abstract List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> query) throws VizException;

    /**
     * Returns the allSources field if it is set, if it is unset will return all
     * sources regardless of whether any usable data exists for that source
     * 
     * @return
     */
    protected Collection<String> getAllSources() {
        if (allSources == null) {
            if (dataTree == null) {
                return Collections.emptyList();
            } else {
                return new ArrayList<String>(dataTree.getSources());
            }
        } else {
            return allSources;
        }
    }

    /**
     * Adds any sources which have data on any level/parameter combination to
     * the returnQueue. If any of the source/level/parameter lists is null
     * assume all sources/levels/parameters. Upon completion, any parameters
     * added to the returnQueue will no longer be in sourcesToCheck.
     * 
     * @param sourcesToCheck
     * @param paramsToProcess
     * @param levelsToProcess
     * @param returnQueue
     * @throws InterruptedException
     */
    public void checkSources(Collection<String> sourcesToCheck,
            Collection<String> paramsToProcess,
            Collection<Level> levelsToProcess, BlockingQueue<String> returnQueue)
            throws InterruptedException {
        boolean genAllSources = false;
        if (sourcesToCheck == null && paramsToProcess == null
                && levelsToProcess == null) {
            if (allSources != null) {
                for (String source : allSources) {
                    returnQueue.put(source);
                }
                return;
            } else {
                genAllSources = true;
            }
        }
        if (sourcesToCheck == null) {
            sourcesToCheck = new ArrayList<String>(getAllSources());
        }
        if (paramsToProcess == null) {
            paramsToProcess = getAllParameters();
        }
        if (levelsToProcess == null) {
            levelsToProcess = getAllLevels();
        }
        // Walking the tree once without deriving is a quick way to avoid
        // failing at deriving many parameters, often paramsToProcess will be
        // empty after this, or at least significantly smaller.
        walkTree(SourceNode.class, sourcesToCheck, paramsToProcess,
                levelsToProcess, false, false, returnQueue);
        walkTree(SourceNode.class, sourcesToCheck, paramsToProcess,
                levelsToProcess, true, false, returnQueue);
        if (genAllSources == true) {
            // Setting allSources allows us to rule out any sources for which
            // there is no data on any level/parameter combination
            List<String> allSources = new ArrayList<String>(getAllSources());
            allSources.removeAll(sourcesToCheck);
            this.allSources = allSources;
        }
    }

    /**
     * See get allSources, same function but for parameters
     * 
     * @return
     */
    protected Collection<String> getAllParameters() {
        if (allParameters == null) {
            synchronized (this) {
                Collection<String> allParameters = new HashSet<String>(
                        derParLibrary.keySet());
                for (String source : getAllSources()) {
                    allParameters.addAll(dataTree.getParameters(source));
                }
                return allParameters;
            }
        } else {
            return allParameters;
        }
    }

    /**
     * See checkSources, same function but for parameters
     * 
     * @param sourcesToProcess
     * @param paramsToCheck
     * @param levelsToProcess
     * @param returnQueue
     * @throws InterruptedException
     */
    public void checkParameters(Collection<String> sourcesToProcess,
            Collection<String> paramsToCheck,
            Collection<Level> levelsToProcess, boolean includeConstant,
            BlockingQueue<String> returnQueue) throws InterruptedException {
        boolean genAllParams = false;
        if (sourcesToProcess == null && paramsToCheck == null
                && levelsToProcess == null) {
            if (allParameters != null) {
                for (String parameter : allParameters) {
                    returnQueue.put(parameter);
                }
                return;
            } else {
                genAllParams = true;
            }
        }
        if (sourcesToProcess == null) {
            sourcesToProcess = getAllSources();
        }
        if (paramsToCheck == null) {
            paramsToCheck = new ArrayList<String>(getAllParameters());
        }
        if (levelsToProcess == null) {
            levelsToProcess = getAllLevels();
        }
        walkTree(ParameterNode.class, sourcesToProcess, paramsToCheck,
                levelsToProcess, false, includeConstant, returnQueue);
        walkTree(ParameterNode.class, sourcesToProcess, paramsToCheck,
                levelsToProcess, true, includeConstant, returnQueue);
        if (genAllParams == true) {
            List<String> allParameters = new ArrayList<String>(
                    getAllParameters());
            allParameters.removeAll(paramsToCheck);
            this.allParameters = allParameters;
        }
    }

    /**
     * See get allSources, same function but for levels
     * 
     * @return
     * @throws VizCommunicationException
     */
    protected Collection<Level> getAllLevels() {
        if (allLevels == null) {
            try {
                return LevelMappingFactory.getInstance().getAllLevels();
            } catch (VizCommunicationException e) {
                // TODO recover from this.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return Collections.emptyList();
            }
        } else {
            return allLevels;
        }
    }

    /**
     * See checkSources, same function but for levels
     * 
     * @param sourcesToProcess
     * @param paramsToProcess
     * @param levelsToCheck
     * @param returnQueue
     * @throws InterruptedException
     */
    public void checkLevels(Collection<String> sourcesToProcess,
            Collection<String> paramsToProcess,
            Collection<Level> levelsToCheck, BlockingQueue<String> returnQueue)
            throws InterruptedException {
        boolean genAllLevels = false;
        if (sourcesToProcess == null && paramsToProcess == null
                && levelsToCheck == null) {
            if (allLevels != null) {
                for (Level level : allLevels) {
                    returnQueue.put(Long.toString(level.getId()));
                }
                return;
            } else {
                genAllLevels = true;
            }
        }
        if (sourcesToProcess == null) {
            sourcesToProcess = getAllSources();
        }
        if (paramsToProcess == null) {
            paramsToProcess = getAllParameters();
        }
        if (levelsToCheck == null) {
            levelsToCheck = new ArrayList<Level>(getAllLevels());
        }
        walkTree(LevelNode.class, sourcesToProcess, paramsToProcess,
                levelsToCheck, false, false, returnQueue);
        walkTree(LevelNode.class, sourcesToProcess, paramsToProcess,
                levelsToCheck, true, false, returnQueue);
        if (genAllLevels == true) {
            List<Level> allLevels = new ArrayList<Level>(getAllLevels());
            allLevels.removeAll(levelsToCheck);
            this.allLevels = allLevels;
        }
    }

    /**
     * Walks the tree in several different ways, before calling this, check if
     * one of the other functions which calls this one and is more clear in its
     * functionality will work(such as checkSources, checkParameters,
     * checkLevels, evaluateConstraints).
     * 
     * The clazz argument should specify an AbstractNode class (source,
     * parameter, level) or null. If it is null this function will return all
     * AbstractRequestableLevelNodes it finds which have any combination of the
     * provided sources/parameters/levels, returnQueue is ignored. If clazz is
     * non-null the return value will be worthless and instead the returnQueue
     * is used, the value of all nodes of clazz will be added to the return
     * queue and removed from the list to process. For example, if ParameterNode
     * is selected then all parameters from paramsToProcess that have any valid
     * source/level from those lists will be removed from paramsToProcess and
     * added to the ReturnQueue. For levels, the returnQueue will contain the
     * level id as a string.
     * 
     * The derive argument determines if the walk will attempt to derive
     * parameters, if true then it will attempt to derive all parameters for the
     * given source/parameter/level combinations. If derived=false, only values
     * which are already determined to exist will be returned and the return
     * value is not a comprehensive list of the available values but rather a
     * quick heuristic which can be used to speed up other processing.
     * 
     * @param clazz
     * @param sourcesToProcess
     * @param paramsToProcess
     * @param levelsToProcess
     * @param derive
     * @param returnQueue
     * @return
     * @throws InterruptedException
     *             thrown if the thread was interupted during execution.
     */
    protected synchronized List<AbstractRequestableNode> walkTree(
            Class<? extends AbstractNode<?>> clazz,
            Collection<String> sourcesToProcess,
            Collection<String> paramsToProcess,
            Collection<Level> levelsToProcess, boolean derive,
            boolean includeConstant, BlockingQueue<String> returnQueue)
            throws InterruptedException {
        if (sourcesToProcess == null || paramsToProcess == null
                || levelsToProcess == null || sourcesToProcess.isEmpty()
                || paramsToProcess.isEmpty() || levelsToProcess.isEmpty()
                || derParLibrary == null) {
            return Collections.emptyList();
        }
        if (clazz != null) {
            // when clazz == null we need to link the aliases to the source
            // which is handled later on, rendering this unnecessary
            List<String> aliasSources = new ArrayList<String>();
            for (String source : sourcesToProcess) {
                SourceNode node = dataTree.getSourceNode(source);
                if (node != null && sourceAliases.containsKey(node.getValue())) {
                    aliasSources.addAll(sourceAliases.get(node.getValue()));
                }
            }
            // Make a new collection for sourcesToProcess if we modify it.
            boolean cloneSources = true;
            for (String aliasSource : aliasSources) {
                if (!sourcesToProcess.contains(aliasSource)) {
                    if (cloneSources) {
                        sourcesToProcess = new ArrayList<String>(
                                sourcesToProcess);
                        cloneSources = false;
                    }
                    sourcesToProcess.add(aliasSource);
                }
            }
        }
        List<AbstractRequestableNode> results = new ArrayList<AbstractRequestableNode>();
        Set<StackEntry> nodata = new HashSet<StackEntry>(derParLibrary.size());
        Deque<StackEntry> stack = new ArrayDeque<StackEntry>();
        Iterator<String> sit = sourcesToProcess.iterator();
        SOURCE_LOOP: while (sit.hasNext()) {
            String source = sit.next();
            SourceNode node = dataTree.getSourceNode(source);
            if (node == null) {
                continue;
            }
            nodata.clear();
            Iterator<String> pit = paramsToProcess.iterator();
            while (pit.hasNext()) {
                if (Thread.interrupted()) {
                    throw new InterruptedException();
                }
                String param = pit.next();
                DerivParamDesc desc = derParLibrary.get(param);
                // If there is no way to derive it and there are no nodes to
                // composite average skip it for all levels
                if (node.containsChildNode(param)
                        || (desc != null && desc.getMethods() != null && !desc
                                .getMethods().isEmpty())) {
                    Iterator<Level> lit = levelsToProcess.iterator();
                    while (lit.hasNext()) {
                        Level level = lit.next();
                        AbstractRequestableNode result = null;
                        if (derive) {
                            try {
                                result = resolveNode(node, param, level, stack,
                                        nodata);
                            } catch (VizCommunicationException e) {
                                statusHandler.handle(Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                            }
                        } else {
                            ParameterNode pNode = node.getChildNode(param);
                            result = (AbstractRequestableNode) (pNode == null ? null
                                    : pNode.getChildNode(Long.toString(level
                                            .getId())));
                        }
                        if (result == null) {
                            continue;
                        }

                        if (!includeConstant) {
                            if (result.isConstant()) {
                                continue;
                            } else if (result instanceof AbstractDerivedDataNode) {
                                // if we don't want constant fields I assume we
                                // also don't want internal fields
                                if (((AbstractDerivedDataNode) result)
                                        .getDesc().isInternal()) {
                                    continue;
                                }
                            }
                        }

                        if (!includeConstant && result.isConstant()) {
                            continue;
                        }
                        if (clazz != null) {
                            if (clazz == SourceNode.class) {
                                returnQueue.put(source);
                                sit.remove();
                                continue SOURCE_LOOP;
                            } else if (clazz == ParameterNode.class) {
                                returnQueue.put(param);
                                pit.remove();
                                break;
                            } else if (clazz == LevelNode.class
                                    || result.getClass().isInstance(clazz)) {
                                returnQueue.put(result.getValue());
                                lit.remove();
                                continue;
                            }
                        } else {
                            if (sourceAliases.containsKey(source)) {
                                boolean supplement = false;
                                List<AbstractRequestableNode> choices = new ArrayList<AbstractRequestableNode>();
                                for (String aliasModel : sourceAliases
                                        .get(source)) {
                                    AbstractRequestableNode alias = null;
                                    if (derive) {
                                        try {
                                            alias = resolveNode(
                                                    dataTree.getSourceNode(aliasModel),
                                                    param, level, stack, nodata);
                                        } catch (VizCommunicationException e) {
                                            statusHandler.handle(
                                                    Priority.PROBLEM,
                                                    e.getLocalizedMessage(), e);
                                        }
                                    } else {
                                        ParameterNode pNode = node
                                                .getChildNode(param);
                                        alias = (AbstractRequestableNode) (pNode == null ? null
                                                : pNode.getChildNode(Long
                                                        .toString(level.getId())));
                                    }
                                    if (alias != null) {
                                        choices.add(alias);
                                        if (aliasModel.equals(source)) {
                                            supplement = true;
                                        }
                                    }

                                }
                                if (!choices.isEmpty()) {
                                    DerivParamDesc d = desc;
                                    DerivParamMethod method = null;
                                    if (d == null) {
                                        // We should at least provide an
                                        // abbreviation.
                                        d = new DerivParamDesc();
                                        d.setAbbreviation(param);
                                    }
                                    if (supplement) {
                                        // This Or node contains the original
                                        // node defined for this
                                        // model/parameter/level so it can be
                                        // treated as a Supplement rather than
                                        // an Or
                                        method = new DerivParamMethod();
                                        method.setName("Supplement");
                                    }
                                    OrLevelNode or = new OrLevelNode(level, d,
                                            method, source, choices, false);
                                    results.add(or);
                                }
                            } else {
                                results.add(result);
                            }
                        }
                    }
                }
            }
        }
        return results;
    }

    /**
     * For the given model/param/level attempt to derive a parameter if it is
     * possible. The stack is used to detect and respond to recursion. nodata is
     * used to make the algorithm more efficient by preventing attempts at
     * deriving the same parameters multiple times, however it should be cleared
     * occasionally to avoid getting huge. THis function will either return a
     * node which can request data for the given source, parameter, and level or
     * null if there is no way to derive the parameter.
     * 
     * @param sourceNode
     * @param param
     * @param level
     * @param stack
     * @param nodata
     * @return
     * @throws VizCommunicationException
     */
    protected synchronized AbstractRequestableNode resolveNode(
            SourceNode sourceNode, String param, Level level,
            Deque<StackEntry> stack, Set<StackEntry> nodata)
            throws VizCommunicationException {
        ParameterNode pNode = sourceNode.getChildNode(param);
        LevelNode lNode = pNode == null ? null : pNode.getChildNode(Long
                .toString(level.getId()));
        if (lNode != null) {
            if (lNode.getClass() == CompositeAverageLevelNode.class
                    && !stack.isEmpty()) {
                stack.getFirst().autoAverage = true;
            }
            return (AbstractRequestableNode) lNode;
        }
        DerivParamDesc desc = derParLibrary.get(param);
        if (desc == null) {
            return null;
        }
        StackEntry se = new StackEntry(sourceNode.getValue(), param,
                level.getId());
        if (nodata.contains(se)) {
            return null;
        }
        if (stack.contains(se)) {
            Iterator<StackEntry> it = stack.descendingIterator();
            while (it.hasNext()) {
                StackEntry next = it.next();
                if (next.equals(se)) {
                    break;
                } else {
                    next.recursive = true;
                }
            }
            return null;
        }
        // Any time this function returns after this point it must pop se, or
        // else!
        AbstractDerivedDataNode autoAveragedNode = null;
        stack.push(se);
        List<DerivParamMethod> methods = desc.getMethods();
        if (methods != null) {
            List<Object> request = new ArrayList<Object>();
            for (DerivParamMethod method : methods) {
                try {
                    // verify valid models
                    List<String> validModels = method.getValidModels();
                    if (validModels != null && validModels.size() > 0) {
                        if (!validModels.contains(sourceNode.getValue())) {
                            continue;
                        }
                    }

                    Set<Level> validLevels = method.getValidLevels();

                    if (validLevels != null && !validLevels.contains(level)) {
                        continue;
                    }
                    request.clear();
                    se.autoAverage = false;
                    if (method.getName().equalsIgnoreCase("Supplement")) {
                        continue;
                    } else if (method.getFrameworkMethod() == FrameworkMethod.NODERIVATION) {
                        stack.pop();
                        return null;
                    } else if (method.getFrameworkMethod() == FrameworkMethod.UNION
                            && level.isRangeLevel()) {
                        if (method.getFields().size() == 1) {
                            // No Levels specified, use all in range
                            SortedSet<Level> levels = LevelUtilities
                                    .getOrderedSetOfStandardLevels(
                                            level.getMasterLevel().getName())
                                    .subSet(level.getLowerLevel(), true,
                                            level.getUpperLevel(), true);
                            for (Level fieldLevel : levels) {
                                Object target = resolveField(sourceNode,
                                        fieldLevel, method.getFields().get(0),
                                        stack, nodata);
                                if (target != null) {
                                    request.add(target);
                                }
                            }
                        } else {
                            // Only use specific Levels
                            for (IDerivParamField field : method.getFields()) {
                                Object target = resolveField(sourceNode, level,
                                        field, stack, nodata);
                                if (target != null) {
                                    request.add(target);
                                }
                            }
                        }
                        if (request.size() < 3) {
                            request.clear();
                        }
                    } else {
                        for (IDerivParamField ifield : method.getFields()) {
                            Object result = resolveField(sourceNode, level,
                                    ifield, stack, nodata);
                            if (result != null) {
                                request.add(result);
                            } else if (method.getFrameworkMethod() != FrameworkMethod.OR) {
                                break;
                            }
                        }// field loop
                    }
                    if (request.size() == method.getFields().size()
                            || ((method.getFrameworkMethod() == FrameworkMethod.UNION || method
                                    .getFrameworkMethod() == FrameworkMethod.OR) && request
                                    .size() > 0)) {
                        AbstractDerivedDataNode newNode = createDerivedNode(
                                desc, method, level, request, sourceNode);
                        if (newNode != null) {
                            pNode = sourceNode.getChildNode(param);
                            if (pNode == null) {
                                pNode = new ParameterNode();
                                pNode.setValue(desc.getAbbreviation());
                                pNode.setParameterName(desc.getName());
                                pNode.setParameterUnit(desc.getUnit()
                                        .toString());
                                sourceNode.addChildNode(pNode);
                            }
                            if (!se.autoAverage) {
                                pNode.addChildNode(newNode);
                                stack.pop();
                                return newNode;
                            } else if (autoAveragedNode == null) {
                                autoAveragedNode = newNode;
                            }
                        }
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Population of gridTree for Derived Parameter ["
                                    + desc.getAbbreviation() + "], method ["
                                    + method.getName() + "] failed", e);
                }
            }// method loop
        }
        if (level.isRangeLevel()) {
            Level upperLevel;
            Level lowerLevel;
            try {
                upperLevel = level.getUpperLevel();
                lowerLevel = level.getLowerLevel();
            } catch (CommunicationException e) {
                throw new VizCommunicationException(e);
            }

            List<AbstractRequestableNode> nodes = new ArrayList<AbstractRequestableNode>();
            int endCount = 0;
            AbstractRequestableNode target = resolveNode(sourceNode, param,
                    upperLevel, stack, nodata);
            if (target != null) {
                endCount += 1;
                nodes.add(target);
            }
            if (level.getLevelonevalue() != level.getLeveltwovalue()) {
                target = resolveNode(sourceNode, param, lowerLevel, stack,
                        nodata);
                if (target != null) {
                    endCount += 1;
                    nodes.add(target);
                }
                SortedSet<Level> levels = LevelUtilities
                        .getOrderedSetOfStandardLevels(
                                level.getMasterLevel().getName()).subSet(
                                lowerLevel, false, upperLevel, false);
                for (Level fieldLevel : levels) {
                    target = resolveNode(sourceNode, param, fieldLevel, stack,
                            nodata);
                    if (target != null) {
                        nodes.add(target);
                    }
                }
            } else {
                endCount *= 2;
            }
            if (endCount == 2 || nodes.size() > 2) {
                pNode = sourceNode.getChildNode(param);
                CompositeAverageLevelNode newNode = new CompositeAverageLevelNode(
                        level, desc, sourceNode.getValue(), nodes);

                pNode.addChildNode(newNode);
                stack.pop();
                if (!stack.isEmpty()) {
                    stack.getFirst().autoAverage = true;
                }
                return newNode;
            }
        }
        if (autoAveragedNode != null) {
            pNode.addChildNode(autoAveragedNode);
            stack.pop();
            /*
             * The check following this comment existed so that definitions that
             * can be derived without auto-averaging will always be used before
             * definitions that need auto-averaging, even if the auto-average
             * happens in dependencies or dependiencies of dependencies etc.
             * There are two problems with this, first if a user over rides a
             * definition that does not use auto-average with a definition that
             * uses auto-average the user override will never be used which can
             * be very confusing if the auto-average is several layers deep. The
             * second problem is that we don't save off the fact that it was
             * derived using auto-average so if a parameter is requested later
             * that uses this same node it will not be marked as auto-averaged
             * so it will use this node even if an alternative exists that is
             * not auto-average. The problem with this is that you derive the
             * parameter differently depending on the order parameters are
             * derived, which is a completely random order. This non-determinism
             * is very bad so this check has been commented out with the
             * possible side effect that sometimes auto-average may be used when
             * it could have been avoided. Now auto-averaging will only be
             * avoided if it happens in one of the direct dependencies of this
             * definition.
             */

            // if (!stack.isEmpty()) {
            // stack.getFirst().autoAverage = true;
            // }
            return autoAveragedNode;
        }
        if (!se.recursive) {
            nodata.add(se);
        }
        stack.pop();
        return null;
    }

    /**
     * A utility function used by resolveNode which will use an ifield in place
     * of a specific param, using that ifield to determine the true
     * level/parameter desired.
     * 
     * @param sourceNode
     * @param level
     * @param ifield
     * @param stack
     * @param nodata
     * @return Should be Either some IStaticData or an
     *         AbstractRequestableLevelNode
     * @throws VizCommunicationException
     */
    private synchronized Object resolveField(SourceNode sourceNode,
            Level level, IDerivParamField ifield, Deque<StackEntry> stack,
            Set<StackEntry> nodata) throws VizCommunicationException {
        // process the next field
        if (ifield.getClass() == DerivParamConstantField.class) {
            return new FloatRequestableData(
                    (float) ((DerivParamConstantField) ifield).getValue());
        }
        DerivParamField field = (DerivParamField) ifield;
        String fieldParamAbbrev = field.getParam();

        // check static grid fields

        Object pStatic = resolvePluginStaticData(sourceNode, field, level);
        if (pStatic != null) {
            return pStatic;
        }

        // Check to see if we can set the field from the
        // masterlevel name
        if (level.getMasterLevel().getName().equals(fieldParamAbbrev)) {

            FloatRequestableData data = new FloatRequestableData(
                    (float) level.getLevelonevalue());
            data.setUnit(level.getMasterLevel().getUnit());
            return data;
        }

        String validSource = field.getValidSource();
        SourceNode fieldSourceNode = sourceNode;

        if (validSource != null && validSource.length() > 0) {
            fieldSourceNode = dataTree.getSourceNode(validSource);
            if (fieldSourceNode == null) {
                return null;
            }
        }

        LevelType type = field.getLevelType();
        if (type == null || type == LevelType.Upper || type == LevelType.Lower) {

            // By default, no mapping
            Level fieldLevel = null;
            LevelNode target = null;
            if (type == null) {
                fieldLevel = level;
            } else if (level.isRangeLevel()) {
                try {
                    if (type == LevelType.Upper) {
                        fieldLevel = level.getUpperLevel();
                    } else {
                        fieldLevel = level.getLowerLevel();
                    }
                } catch (CommunicationException e) {
                    throw new VizCommunicationException(e);
                }
            } else {
                SortedSet<Level> levels = null;
                if (type == LevelType.Upper) {
                    levels = LevelUtilities.getOrderedSetOfStandardLevels(
                            level.getMasterLevel().getName()).tailSet(level,
                            false);
                } else {
                    levels = LevelUtilities
                            .getOrderedSetOfStandardLevels(
                                    level.getMasterLevel().getName())
                            .headSet(level, false).descendingSet();
                }
                for (Level l : levels) {
                    target = resolveNode(fieldSourceNode, fieldParamAbbrev, l,
                            stack, nodata);
                    if (target != null) {
                        fieldLevel = l;
                        break;
                    }
                }
            }

            // If that level is defined than add a
            // request to the map
            if (fieldLevel != null && target == null) {
                target = resolveNode(fieldSourceNode, fieldParamAbbrev,
                        fieldLevel, stack, nodata);
            }
            if (target != null) {
                return target;
            }

            // Level mapping must be handled seperately
            // because it is valid for all requests
        } else if (type == LevelType.LevelMapping) {
            LevelNode target = null;
            for (Level fieldLevel : field.getLevelMapping().getLevels()) {
                target = resolveNode(fieldSourceNode, fieldParamAbbrev,
                        fieldLevel, stack, nodata);
                if (target != null) {
                    break;
                }
            }
            if (target != null) {
                return target;
            }

            // Cube's are very different from other level
            // mappings
        } else if (type == LevelType.Cube) {
            return getCubeNode(fieldSourceNode, field, stack, nodata);

        }
        return null;
    }

    protected abstract LevelNode getCubeNode(SourceNode sNode,
            DerivParamField field, Deque<StackEntry> stack,
            Set<StackEntry> nodata) throws VizCommunicationException;

    protected abstract AbstractDerivedDataNode getImportNode(
            AbstractRequestableData nodeToImport, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level);

    protected abstract AbstractDerivedDataNode getImportNode(
            AbstractRequestableNode nodeToImport,
            String nodeToImportSourceName, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level);

    protected abstract Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level);

    /**
     * This method is responsible for actually creating the derived nodes
     * 
     * @param desc
     * @param method
     * @param level
     * @param fields
     * @param source
     * @return
     */
    protected AbstractDerivedDataNode createDerivedNode(DerivParamDesc desc,
            DerivParamMethod method, Level level, List<Object> fields,
            SourceNode source) {
        switch (method.getMethodType()) {
        case PYTHON:
            if ((method.isDtime() || method.isFtime()) && source.getDt() <= 0) {
                return null;
            }
            DerivedLevelNode node = new DerivedLevelNode(level, desc, method,
                    source.getValue(), source.getDt());
            for (int k = 0; k < method.getFields().size(); k++) {
                Object obj = fields.get(k);
                if (obj instanceof AbstractRequestableData) {
                    node.putStaticField(method.getFields().get(k),
                            (AbstractRequestableData) obj);
                }
                if (obj instanceof AbstractRequestableNode) {
                    node.putField((DerivParamField) method.getFields().get(k),
                            (AbstractRequestableNode) obj);
                }
            }
            return node;
        case FRAMEWORK:
            switch (method.getFrameworkMethod()) {
            case ALIAS: {
                Object field = fields.get(0);
                if (field instanceof AbstractRequestableData) {
                    AbstractRequestableData data = (AbstractRequestableData) field;
                    data.setLevel(level);
                    data.setParameter(desc.getAbbreviation());
                    data.setParameterName(desc.getName());
                    data.setSource(source.getValue());
                    data.setUnit(desc.getUnit());
                    return new StaticDataLevelNode(level, desc, method, data,
                            source.getValue());
                } else if (field instanceof AbstractRequestableNode) {
                    return new AliasLevelNode((AbstractRequestableNode) field,
                            desc, method, source.getValue(), level);
                }
                return null;
            }
            case IMPORT: {
                String importSource = null;
                IDerivParamField derivParamField = method.getFields().get(0);

                if (derivParamField instanceof DerivParamField) {
                    importSource = ((DerivParamField) derivParamField)
                            .getValidSource();
                }

                if (importSource == null || importSource.trim().length() == 0) {
                    importSource = source.getValue();
                }

                Object field = fields.get(0);
                if (field instanceof AbstractRequestableData) {
                    return getImportNode((AbstractRequestableData) field,
                            source, desc, method, level);
                } else if (field instanceof AbstractRequestableNode) {
                    return getImportNode((AbstractRequestableNode) field,
                            importSource, source, desc, method, level);
                }
                return null;
            }
            case MODELRUN:
                if (source.getDt() <= 0) {
                    return null;
                }
                AbstractRequestableNode sourceLevelNode = (AbstractRequestableNode) fields
                        .get(0);
                return new TimeRangeLevelNode(sourceLevelNode, desc, method,
                        source.getValue(), (Integer) null, 0, source.getDt(),
                        level);
            case TIMERANGE:
                if (source.getDt() <= 0) {
                    return null;
                }
                AbstractRequestableNode sourceNode = (AbstractRequestableNode) fields
                        .get(0);
                Float startTime = ((FloatRequestableData) fields.get(1))
                        .getDataValue();
                Float endTime = ((FloatRequestableData) fields.get(2))
                        .getDataValue();
                return new TimeRangeLevelNode(sourceNode, desc, method,
                        source.getValue(), startTime.intValue(),
                        endTime.intValue(), source.getDt(), level);
            case SUPPLEMENT:
            case OR:
                List<AbstractRequestableNode> choices = new ArrayList<AbstractRequestableNode>(
                        fields.size());
                for (Object obj : fields) {
                    choices.add((AbstractRequestableNode) obj);
                }
                return new OrLevelNode(level, desc, method, source.getValue(),
                        choices);
            case UNION:
                List<AbstractRequestableNode> nodes = new ArrayList<AbstractRequestableNode>(
                        fields.size());
                for (Object obj : fields) {
                    nodes.add((AbstractRequestableNode) obj);
                }
                return new UnionLevelNode(level, desc, method,
                        source.getValue(), nodes);
            default:
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Unimplemented framework method [" + method.getName()
                                + "] in definition of ["
                                + desc.getAbbreviation() + "]");
                return null;
            }
        case OTHER:
            return null;
        default:
            statusHandler.handle(Priority.PROBLEM,
                    "Unknown method type [" + method.getMethodType()
                            + "] for method [" + method.getName()
                            + "] in definition of [" + desc.getAbbreviation()
                            + "]");
            return null;
        }
    }

}
