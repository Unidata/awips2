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
package com.raytheon.viz.pointdata.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.derivparam.tree.ParameterNode;
import com.raytheon.uf.common.derivparam.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AbstractInventory;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod.MethodType;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;

/**
 * Abstract implementation of a point data inventory that can be used by
 * datatypes that have point data but don't adhere fully to the point data api.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractPointDataInventory extends AbstractInventory {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractPointDataInventory.class);

    public static String PLUGIN_NAME = "pluginName";

    protected List<String> plugins;

    public AbstractPointDataInventory(List<String> plugins) {
        this.plugins = plugins;
    }

    public String getTypeKey(String pluginName) {
        return PLUGIN_NAME;
    }

    public String[] getAvailableTypes(String pluginName) throws VizException {
        String typeKey = getTypeKey(pluginName);
        if (!typeKey.equals(PLUGIN_NAME)) {
            Map<String, RequestConstraint> queryTerms = new HashMap<String, RequestConstraint>();
            queryTerms.put(PLUGIN_NAME, new RequestConstraint(pluginName));
            return CatalogQuery.performQuery(typeKey, queryTerms);
        } else {
            return new String[] { pluginName };
        }
    }

    public List<AbstractRequestableNode> getNodes(String source,
            List<String> parameters, List<Level> levels) throws VizException {
        parameters = new ArrayList<String>(parameters);
        try {
            return walkTree(null, Arrays.asList(source), parameters, levels,
                    true, true, null);

        } catch (InterruptedException e) {
            throw new VizException("Errro walking PointData Tree", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#createBaseTree()
     */
    @Override
    protected DataTree createBaseTree() {
        DataTree tree = getInitialTree();
        if (tree == null) {
            return tree;
        }
        for (SourceNode sNode : tree.getSourceNodes().values()) {
            for (ParameterNode pNode : sNode.getChildNodes().values()) {
                for (Entry<String, LevelNode> lEntry : pNode.getChildNodes()
                        .entrySet()) {
                    lEntry.setValue(new PointDataLevelNode(lEntry.getValue(),
                            pNode.getValue()));
                }
            }
        }
        return tree;
    }

    protected DataTree getInitialTree() {
        DataTree tree = new DataTree();
        String stationId = Long.toString(getStationLevel().getId());
        for (String pluginName : plugins) {
            try {
                String[] types = getAvailableTypes(pluginName);
                for (String type : types) {
                    String source = pluginName;
                    if (!PLUGIN_NAME.equals(getTypeKey(pluginName))) {
                        source += type;
                    }
                    for (String param : getBaseParams(pluginName, type)) {
                        tree.addBranch(source, param, null, null, stationId);
                    }
                }
            } catch (VizException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Error creating point data inventory for "
                                        + pluginName, e);
            }
        }
        return tree;

    }

    protected abstract List<String> getBaseParams(String pluginName, String type)
            throws VizException;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getCubeNode(com.
     * raytheon.uf.viz.derivparam.inv.SourceNode,
     * com.raytheon.uf.viz.derivparam.library.DerivParamField, java.util.Deque,
     * java.util.Set)
     */
    @Override
    protected LevelNode getCubeNode(SourceNode node, DerivParamField field,
            Deque<StackEntry> stack, Set<StackEntry> nodata) {
        return null;
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableData nodeToImport, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        return null;
    }

    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableNode nodeToImport,
            String nodeToImportSourceName, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        return null;
    }

    @Override
    public List<DataTime> timeAgnosticQuery(Map<String, RequestConstraint> query)
            throws VizException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#resolvePluginStaticData
     * (com.raytheon.uf.viz.derivparam.inv.SourceNode,
     * com.raytheon.uf.viz.derivparam.library.DerivParamField,
     * com.raytheon.uf.common.dataplugin.level.Level)
     */
    @Override
    protected Object resolvePluginStaticData(SourceNode node,
            DerivParamField field, Level level) {
        return null;
    }

    @Override
    protected AbstractDerivedDataNode createDerivedNode(DerivParamDesc desc,
            DerivParamMethod method, Level level, List<Object> fields,
            SourceNode source) {
        if (method.getMethodType() == MethodType.OTHER) {
            if (method.getName().equalsIgnoreCase("PointAccum")) {
                int index = fields.size() - 1;
                index -= 4; // The last 4 fields in order are time, paramName,
                            // totalTime, time increment
                AbstractRequestableNode tNode = (AbstractRequestableNode) fields
                        .get(index + 1);
                List<AbstractRequestableNode> idNodes = new ArrayList<AbstractRequestableNode>(
                        index + 1);
                for (int i = 0; i <= index; i++) {
                    idNodes.add((AbstractRequestableNode) fields.get(i));
                }
                return new PointAccumLevelNode(desc, method, idNodes, tNode,
                        source.getValue());
            } else if (method.getName().equalsIgnoreCase("HeightOf")) {
                AbstractRequestableNode latNode = (AbstractRequestableNode) fields
                        .get(0);
                AbstractRequestableNode lonNode = (AbstractRequestableNode) fields
                        .get(1);
                AbstractRequestableNode timeNode = null;
                if (fields.size() > 2) {
                    timeNode = (AbstractRequestableNode) fields.get(2);
                }
                return new HeightOfLevelNode(level, desc, method, latNode,
                        lonNode, timeNode);

            }
        }
        return super.createDerivedNode(desc, method, level, fields, source);
    }

    /**
     * @param queryParams
     * @return
     * @throws VizException
     */
    public String getType(Map<String, RequestConstraint> queryParams)
            throws VizException {
        String plugin = queryParams.get(PLUGIN_NAME).getConstraintValue();
        String type = plugin;
        String typeKey = getTypeKey(plugin);
        if (queryParams.containsKey(typeKey)) {
            type = queryParams.get(typeKey).getConstraintValue();
        } else if (queryParams.containsKey("dataURI")) {
            String dataURI = queryParams.get("dataURI").getConstraintValue()
                    .split(",")[0];
            Map<String, Object> paramMap = RecordFactory.getInstance()
                    .loadMapFromUri(dataURI);
            type = paramMap.get(typeKey).toString();
        }
        return type;
    }

    /**
     * @return the plugins
     */
    public List<String> getPlugins() {
        return plugins;
    }

    public static Level getStationLevel() {
        try {
            return LevelMappingFactory.getInstance()
                    .getLevelMappingForKey("Station").getLevels().get(0);
        } catch (VizCommunicationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return null;
        }
    }

}
