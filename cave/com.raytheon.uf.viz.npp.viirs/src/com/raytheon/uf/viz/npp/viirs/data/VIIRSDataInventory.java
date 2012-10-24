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
package com.raytheon.uf.viz.npp.viirs.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
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
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AbstractInventory;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * Inventory for viirs data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDataInventory extends AbstractInventory implements
        IAlertObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VIIRSDataInventory.class);

    public static final String PLUGIN_NAME = "viirs";

    private static final String PLUGIN_NAME_QUERY = "pluginName";

    private static final String CHANNEL_TYPE = "channelType";

    private static final String PARAMETER = "parameter";

    private static final String WAVELENGTH = "wavelength";

    private static final String VIIRS_LEVEL_NAME = "EA";

    private Level entireAtmosphere;

    private Map<Number, List<String>> wavelengthParameterMap;

    private Map<String, String> parameterSourceMap;

    private List<String> viirsSources = new ArrayList<String>();

    private List<String> viirsParameters = new ArrayList<String>();

    private List<Level> viirsLevels = new ArrayList<Level>();

    public VIIRSDataInventory() throws CommunicationException {
        entireAtmosphere = LevelFactory.getInstance().getLevel(
                VIIRS_LEVEL_NAME, 0.0);
        ProductAlertObserver.addObserver(PLUGIN_NAME, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getAllSources()
     */
    @Override
    protected Collection<String> getAllSources() {
        return viirsSources;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getAllParameters()
     */
    @Override
    protected Collection<String> getAllParameters() {
        return viirsParameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getAllLevels()
     */
    @Override
    protected Collection<Level> getAllLevels() {
        return viirsLevels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#createBaseTree()
     */
    @Override
    protected DataTree createBaseTree() {
        wavelengthParameterMap = new HashMap<Number, List<String>>();
        parameterSourceMap = new HashMap<String, String>();
        viirsSources = new ArrayList<String>();
        viirsParameters = new ArrayList<String>();
        viirsLevels = new ArrayList<Level>();
        DataTree tree = new DataTree();
        DbQueryRequest request = new DbQueryRequest();
        request.addFields(new String[] { CHANNEL_TYPE, WAVELENGTH, PARAMETER });
        request.setDistinct(true);
        request.addConstraint(PLUGIN_NAME_QUERY, new RequestConstraint("viirs"));
        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            String viirsLevel = String.valueOf(entireAtmosphere.getId());
            viirsLevels.add(entireAtmosphere);
            for (Map<String, Object> result : response.getResults()) {
                String channelType = (String) result.get(CHANNEL_TYPE);
                Number wavelength = (Number) result.get(WAVELENGTH);
                String parameter = (String) result.get(PARAMETER);
                addToTree(tree, channelType, parameter, wavelength, viirsLevel);
            }
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error requesting base tree: "
                            + e.getLocalizedMessage(), e);
        }

        return tree;
    }

    public void addToTree(DataTree tree, String channelType, String parameter,
            Number wavelength, String level) {
        Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
        rcMap.put(PLUGIN_NAME_QUERY, new RequestConstraint(
                VIIRSDataInventory.PLUGIN_NAME));
        rcMap.put(CHANNEL_TYPE, new RequestConstraint(channelType));
        rcMap.put(PARAMETER, new RequestConstraint(parameter));

        SourceNode source = tree.getSourceNode(channelType);
        if (source == null) {
            source = new SourceNode();
            source.setValue(channelType);
            tree.getSourceNodes().put(channelType, source);
            viirsSources.add(channelType);
        }

        // Add node for parameter with wavelength and one for just parameter

        // ParameterNode for parameter only:
        String parameterName = parameter;
        ParameterNode paramNode = source.getChildNode(parameterName);
        if (paramNode == null) {
            paramNode = new ParameterNode();
            paramNode.setParameterName(parameterName);
            paramNode.setValue(parameterName);
            source.addChildNode(paramNode);
        }

        LevelNode levelNode = paramNode.getChildNode(level);
        if (levelNode == null) {
            levelNode = new VIIRSRequestableLevelNode(
                    new HashMap<String, RequestConstraint>(rcMap));
            levelNode.setValue(level);
            paramNode.addChildNode(levelNode);
        }

        // ParameterNode for parameter+wavelength
        rcMap.put(WAVELENGTH, new RequestConstraint(String.valueOf(wavelength)));
        parameterName = VIIRSDynamicParameters.createParameter(wavelength,
                parameter);

        paramNode = source.getChildNode(parameterName);
        if (paramNode == null) {
            paramNode = new ParameterNode();
            paramNode.setParameterName(parameterName);
            paramNode.setValue(parameterName);
            source.addChildNode(paramNode);
            List<String> parameters = wavelengthParameterMap.get(wavelength);
            if (parameters == null) {
                parameters = new ArrayList<String>();
                wavelengthParameterMap.put(wavelength, parameters);
            }
            parameters.add(parameter);
            viirsParameters.add(parameterName);
            parameterSourceMap.put(parameterName, channelType);
        }

        levelNode = paramNode.getChildNode(level);
        if (levelNode == null) {
            levelNode = new VIIRSRequestableLevelNode(rcMap);
            levelNode.setValue(level);
            paramNode.addChildNode(levelNode);
        }
    }

    public List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints) {
        List<String> sources = new ArrayList<String>();
        List<String> parameters = extractParameters(constraints);
        for (String parameter : parameters) {
            // Ensure parameter definition exists for each parameter
            VIIRSDynamicParameters.createParameterDefinitions(parameter, this,
                    sources);
        }
        RequestConstraint sourceRC = constraints.get(CHANNEL_TYPE);
        if (sourceRC != null) {
            // Specific source constraint, clear ones from parameters
            sources.clear();
            for (String source : getAllSources()) {
                if (sourceRC.evaluate(source)) {
                    sources.add(source);
                }
            }
        } else {
            if (sources.size() > 1) {
                // Limit to one source, use first
                String toUse = sources.get(0);
                sources.clear();
                sources.add(toUse);
            }
        }
        List<Level> levels = extractLevels(constraints);
        try {
            return walkTree(null, sources, parameters, levels, true, true, null);
        } catch (InterruptedException e) {
        }
        return new ArrayList<AbstractRequestableNode>(0);
    }

    /**
     * @param constraints
     * @return
     */
    private List<Level> extractLevels(Map<String, RequestConstraint> constraints) {
        return Arrays.asList(entireAtmosphere);
    }

    public Level getParameterLevel(String parameter) {
        return entireAtmosphere;
    }

    public String getParameterSource(String parameter) {
        return parameterSourceMap.get(parameter);
    }

    public DerivParamDesc getParameterDescription(String parameter) {
        return derParLibrary.get(parameter);
    }

    public void addParameterDescription(String parameter,
            DerivParamDesc description) {
        derParLibrary.put(parameter, description);
    }

    /**
     * @param constraints
     * @return
     */
    private List<String> extractParameters(
            Map<String, RequestConstraint> constraints) {
        List<String> parameters = new ArrayList<String>();
        RequestConstraint wavelengthRC = constraints.get(WAVELENGTH);
        RequestConstraint parameterRC = constraints.get(PARAMETER);
        if (parameterRC == null) {
            // No parameter, add any base parameters for the wavelength or all
            // parameters if no wavelength either
            if (wavelengthRC == null) {
                parameters.addAll(getAllParameters());
            } else {
                for (Number wavelength : wavelengthParameterMap.keySet()) {
                    if (wavelengthRC.evaluate(wavelength)) {
                        List<String> params = wavelengthParameterMap
                                .get(wavelength);
                        for (String param : params) {
                            parameters.add(VIIRSDynamicParameters
                                    .createParameter(wavelength, param));
                        }
                    }
                }
            }
        } else {
            ConstraintType parameterType = parameterRC.getConstraintType();
            if (parameterType != ConstraintType.EQUALS) {
                throw new RuntimeException("ConstriantType of " + parameterType
                        + " not supported for viirs dynamic parameters");
            }
            String parameter = parameterRC.getConstraintValue();
            for (Number wavelength : wavelengthParameterMap.keySet()) {
                if (wavelengthRC == null || wavelengthRC.evaluate(wavelength)) {
                    String combinedParam = VIIRSDynamicParameters
                            .createParameter(wavelength, parameter);
                    if (parameters.contains(combinedParam) == false) {
                        // Add parameter to list
                        parameters.add(combinedParam);
                    }
                }
            }
        }
        return parameters;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.IAlertObserver#alertArrived(java.util.Collection)
     */
    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        String level = String.valueOf(entireAtmosphere.getId());
        for (AlertMessage message : alertMessages) {
            String channelType = (String) message.decodedAlert
                    .get(CHANNEL_TYPE);
            String parameter = (String) message.decodedAlert.get(PARAMETER);
            Number wavelength = (Number) message.decodedAlert.get(WAVELENGTH);
            addToTree(dataTree, channelType, parameter, wavelength, level);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#timeAgnosticQuery
     * (java.util.Map)
     */
    @Override
    public List<DataTime> timeAgnosticQuery(Map<String, RequestConstraint> query)
            throws VizException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getCubeNode(com.
     * raytheon.uf.common.derivparam.tree.SourceNode,
     * com.raytheon.uf.viz.derivparam.library.DerivParamField, java.util.Deque,
     * java.util.Set)
     */
    @Override
    protected LevelNode getCubeNode(SourceNode sNode, DerivParamField field,
            Deque<StackEntry> stack, Set<StackEntry> nodata) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getImportNode(com
     * .raytheon.uf.viz.derivparam.data.AbstractRequestableData,
     * com.raytheon.uf.common.derivparam.tree.SourceNode,
     * com.raytheon.uf.viz.derivparam.library.DerivParamDesc,
     * com.raytheon.uf.viz.derivparam.library.DerivParamMethod,
     * com.raytheon.uf.common.dataplugin.level.Level)
     */
    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableData nodeToImport, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#getImportNode(com
     * .raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode,
     * java.lang.String, com.raytheon.uf.common.derivparam.tree.SourceNode,
     * com.raytheon.uf.viz.derivparam.library.DerivParamDesc,
     * com.raytheon.uf.viz.derivparam.library.DerivParamMethod,
     * com.raytheon.uf.common.dataplugin.level.Level)
     */
    @Override
    protected AbstractDerivedDataNode getImportNode(
            AbstractRequestableNode nodeToImport,
            String nodeToImportSourceName, SourceNode destSourceNode,
            DerivParamDesc desc, DerivParamMethod method, Level level) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.inv.AbstractInventory#resolvePluginStaticData
     * (com.raytheon.uf.common.derivparam.tree.SourceNode,
     * com.raytheon.uf.viz.derivparam.library.DerivParamField,
     * com.raytheon.uf.common.dataplugin.level.Level)
     */
    @Override
    protected Object resolvePluginStaticData(SourceNode sNode,
            DerivParamField field, Level level) {
        return null;
    }

}
