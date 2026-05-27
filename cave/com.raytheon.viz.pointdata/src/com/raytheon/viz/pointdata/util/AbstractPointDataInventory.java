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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.derivparam.library.DerivParamDesc;
import com.raytheon.uf.common.derivparam.library.DerivParamField;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod;
import com.raytheon.uf.common.derivparam.library.DerivParamMethod.MethodType;
import com.raytheon.uf.common.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.DataTree;
import com.raytheon.uf.common.inventory.tree.LevelNode;
import com.raytheon.uf.common.inventory.tree.ParameterNode;
import com.raytheon.uf.common.inventory.tree.SourceNode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Abstract implementation of a point data inventory that can be used by
 * datatypes that have point data but don't adhere fully to the point data api.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen    Initial creation
 * Sep 09, 2014  3356      njensen     Remove CommunicationException
 * Aug 20, 2018  7019      bsteffen    Add support for ImportGrid method
 * 
 * </pre>
 * 
 * @author bsteffen
 */

public abstract class AbstractPointDataInventory extends AbstractInventory {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractPointDataInventory.class);

    public static final String PLUGIN_NAME = PluginDataObject.PLUGIN_NAME_ID;

    private static final String GRID_IMPORT = "ImportGrid";

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
            Map<String, RequestConstraint> queryTerms = new HashMap<>();
            queryTerms.put(PLUGIN_NAME, new RequestConstraint(pluginName));
            return CatalogQuery.performQuery(typeKey, queryTerms);
        } else {
            return new String[] { pluginName };
        }
    }

    public List<AbstractRequestableNode> getNodes(String source,
            List<String> parameters, List<Level> levels) throws VizException {
        parameters = new ArrayList<>(parameters);
        try {
            return walkTree(null, Arrays.asList(source), parameters, levels,
                    true, true, null);

        } catch (InterruptedException e) {
            throw new VizException("Error walking PointData Tree", e);
        }
    }

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
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating point data inventory for " + pluginName,
                        e);
            }
        }
        return tree;

    }

    protected abstract List<String> getBaseParams(String pluginName,
            String type) throws VizException;

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
            AbstractRequestableNode nodeToImport, String nodeToImportSourceName,
            SourceNode destSourceNode, DerivParamDesc desc,
            DerivParamMethod method, Level level) {
        return null;
    }

    @Override
    public List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> query) throws DataCubeException {
        return null;
    }

    @Override
    protected Object resolvePluginSpecifiedField(SourceNode sourceNode,
            Level level, DerivParamMethod method, DerivParamField field) {
        if (GRID_IMPORT.equals(method.getName())) {
            if (field.getValidSource() != null) {
                if (GridImportLevelNode.isAvailable(field, level)) {
                    /*
                     * The field isn't used for much, but we have to return
                     * something to signal that this field has data.
                     */
                    return field;
                }
            }
        }
        return super.resolvePluginSpecifiedField(sourceNode, level, method,
                field);
    }

    @Override
    protected AbstractDerivedDataNode createDerivedNode(DerivParamDesc desc,
            DerivParamMethod method, Level level, List<Object> fields,
            SourceNode source) {
        if (method.getMethodType() == MethodType.OTHER) {
            if ("PointAccum".equalsIgnoreCase(method.getName())) {
                int index = fields.size() - 1;
                /*
                 * The last 4 fields in order are time, paramName, totalTime,
                 * time increment
                 */
                index -= 4;
                AbstractRequestableNode tNode = (AbstractRequestableNode) fields
                        .get(index + 1);
                List<AbstractRequestableNode> idNodes = new ArrayList<>(
                        index + 1);
                for (int i = 0; i <= index; i++) {
                    idNodes.add((AbstractRequestableNode) fields.get(i));
                }
                return new PointAccumLevelNode(desc, method, idNodes, tNode,
                        source.getValue());
            } else if ("HeightOf".equalsIgnoreCase(method.getName())) {
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

            } else if (GRID_IMPORT.equalsIgnoreCase(method.getName())) {
                /*
                 * Grid import allows between 1 and 4 fields, it requires 4
                 * pieces of data but for convenience if you specify less fields
                 * it will figure out some reasonable defaults. The first field
                 * is required and must specify which grid model and parameter
                 * to import and may also specify a level. The optional fields
                 * are latitude, the longitude, and the valid time of the point
                 * data which is used to import the closest available grid data.
                 */
                if (fields.isEmpty()) {
                    return null;
                }
                Object firstArg = fields.get(0);
                if (!(firstArg instanceof DerivParamField)) {
                    return null;
                }
                AbstractRequestableNode latNode;
                AbstractRequestableNode lonNode;
                AbstractRequestableNode timeNode;

                Set<StackEntry> nodata = new HashSet<>();
                Deque<StackEntry> stack = new ArrayDeque<>();

                if (fields.size() > 1) {
                    latNode = (AbstractRequestableNode) fields.get(1);
                } else {
                    latNode = resolveNode(source, "latitude", getStationLevel(),
                            stack, nodata);
                    if (latNode == null) {
                        return null;
                    }
                }
                if (fields.size() > 2) {
                    lonNode = (AbstractRequestableNode) fields.get(2);
                } else {
                    lonNode = resolveNode(source, "longitude",
                            getStationLevel(), stack, nodata);
                    if (lonNode == null) {
                        return null;
                    }
                }
                if (fields.size() > 3) {
                    timeNode = (AbstractRequestableNode) fields.get(3);
                } else {
                    timeNode = resolveNode(source, "refTime", getStationLevel(),
                            stack, nodata);
                    if (timeNode == null) {
                        return null;
                    }
                }
                return new GridImportLevelNode(level, desc, method, latNode,
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
        } else if (queryParams.containsKey(PluginDataObject.DATAURI_ID)) {
            String dataURI = queryParams.get(PluginDataObject.DATAURI_ID)
                    .getConstraintValue().split(",")[0];
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
        return LevelMappingFactory
                .getInstance(
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                .getLevelMappingForKey("Station").getLevels().get(0);
    }

}
