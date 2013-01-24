package com.raytheon.viz.grid;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;
import com.raytheon.viz.grid.inv.GridInventory;
import com.raytheon.viz.grid.rsc.GridLoadProperties;
import com.raytheon.viz.grid.rsc.GridResourceData;

/**
 * Product browser implementation for grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2010            bsteffens    Initial creation
 * May 26, 2010            mnash        Used ProductBrowserLabel implementation instead of requery
 * 
 * </pre>
 * 
 * @author bsteffens
 * @version 1.0
 */
public class GridProductBrowserDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<GridResourceData> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridProductBrowserDataDefinition.class);

    private static final String SHOW_DERIVED_PARAMS = "Show Derived Parameters";

    private static final Comparator<Level> levelComparator = new Comparator<Level>() {

        @Override
        public int compare(Level o1, Level o2) {
            if (o1.isRangeLevel() == o2.isRangeLevel()) {
                int val = Double.compare(o1.getLevelonevalue(),
                        o2.getLevelonevalue());
                if (val == 0) {
                    val = Double.compare(o1.getLeveltwovalue(),
                            o2.getLeveltwovalue());
                }
                return val;
            }
            if (o1.isRangeLevel()) {
                return 1;
            } else {
                return -1;
            }
        }

    };

    public GridProductBrowserDataDefinition() {
        productName = GridInventory.PLUGIN_NAME;
        displayName = "Grid";
        order = new String[] { GridInventory.MODEL_NAME_QUERY,
                GridInventory.PARAMETER_QUERY,
                GridInventory.MASTER_LEVEL_QUERY, GridInventory.LEVEL_ID_QUERY };
        order = getOrder();
        loadProperties = new GridLoadProperties();
        loadProperties.setResourceType(getResourceType());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #getResourceData()
     */
    @Override
    public GridResourceData getResourceData() {
        return new GridResourceData();
    }

    @Override
    protected String[] queryData(String param,
            HashMap<String, RequestConstraint> queryList) {
        try {
            if (getInventory() == null) {
                return super.queryData(param, queryList);
            } else {
                Collection<String> sources = null;
                Collection<String> params = null;
                Collection<Level> levels = null;
                BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
                for (Entry<String, RequestConstraint> queryParam : queryList
                        .entrySet()) {
                    String key = queryParam.getKey();
                    String value = queryParam.getValue().getConstraintValue();
                    if (key.equals(GridInventory.MODEL_NAME_QUERY)) {
                        sources = Arrays.asList(value);
                    } else if (key.equals(GridInventory.PARAMETER_QUERY)) {
                        params = Arrays.asList(value);
                    } else if (key.equals(GridInventory.MASTER_LEVEL_QUERY)) {
                        if (levels == null) {
                            levels = new ArrayList<Level>(LevelMappingFactory
                                    .getInstance().getAllLevels());
                        }
                        Iterator<Level> iter = levels.iterator();
                        while (iter.hasNext()) {
                            if (!iter.next().getMasterLevel().getName()
                                    .equals(value)) {
                                iter.remove();
                            }
                        }

                    } else if (key.equals(GridInventory.LEVEL_ONE_QUERY)) {
                        double doubleValue = Double.parseDouble(value);
                        if (levels == null) {
                            levels = new ArrayList<Level>(LevelMappingFactory
                                    .getInstance().getAllLevels());
                        }
                        Iterator<Level> iter = levels.iterator();
                        while (iter.hasNext()) {
                            if (iter.next().getLevelonevalue() != doubleValue) {
                                iter.remove();
                            }
                        }
                    } else if (key.equals(GridInventory.LEVEL_TWO_QUERY)) {
                        double doubleValue = Double.parseDouble(value);
                        if (levels == null) {
                            levels = new ArrayList<Level>(LevelMappingFactory
                                    .getInstance().getAllLevels());
                        }
                        Iterator<Level> iter = levels.iterator();
                        while (iter.hasNext()) {
                            if (iter.next().getLeveltwovalue() != doubleValue) {
                                iter.remove();
                            }
                        }
                    } else if (key.equals(GridInventory.LEVEL_ID_QUERY)) {
                        levels = Arrays.asList(LevelFactory.getInstance()
                                .getLevel(value));
                    }
                }

                if (param.equals(GridInventory.MODEL_NAME_QUERY)) {
                    try {
                        getInventory().checkSources(sources, params, levels,
                                returnQueue);
                    } catch (InterruptedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    return returnQueue.toArray(new String[0]);
                } else if (param.equals(GridInventory.PARAMETER_QUERY)) {
                    try {
                        getInventory().checkParameters(sources, params, levels,
                                false, returnQueue);
                    } catch (InterruptedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    return returnQueue.toArray(new String[0]);
                } else if (param.equals(GridInventory.MASTER_LEVEL_QUERY)) {
                    try {
                        getInventory().checkLevels(sources, params, levels,
                                returnQueue);
                    } catch (InterruptedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    Set<String> masterlevels = new HashSet<String>();
                    LevelFactory lf = LevelFactory.getInstance();
                    for (String levelid : returnQueue) {
                        Level level = lf.getLevel(levelid);
                        masterlevels.add(level.getMasterLevel().getName());
                    }
                    return masterlevels.toArray(new String[0]);
                } else if (param.equals(GridInventory.LEVEL_ID_QUERY)) {
                    try {
                        getInventory().checkLevels(sources, params, levels,
                                returnQueue);
                    } catch (InterruptedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                    return returnQueue.toArray(new String[0]);
                }
            }
        } catch (VizCommunicationException e) {
            statusHandler.handle(Priority.ERROR, "Unable to query data for "
                    + productName, e);
        } catch (CommunicationException e) {
            statusHandler.handle(Priority.ERROR, "Unable to query data for "
                    + productName, e);
        }
        return new String[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition
     * #formatData(java.lang.String, java.lang.String[])
     */
    @Override
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
        try {
            if (GridInventory.MODEL_NAME_QUERY.equals(param)) {
                DatasetInfoLookup lookup = DatasetInfoLookup.getInstance();
                for (int i = 0; i < parameters.length; i++) {
                    DatasetInfo info = lookup.getInfo(parameters[i]);
                    if (info == null) {
                        labels.add(new ProductBrowserLabel(parameters[i],
                                parameters[i]));
                    } else {
                        labels.add(new ProductBrowserLabel(info.getTitle()
                                + " (" + parameters[i] + ")", parameters[i]));
                    }
                }
                Collections.sort(labels);
                return labels;
            } else if (GridInventory.PARAMETER_QUERY.equals(param)) {
                Map<String, DerivParamDesc> library = DerivedParameterGenerator
                        .getDerParLibrary();
                for (int i = 0; i < parameters.length; i++) {
                    DerivParamDesc desc = library.get(parameters[i]);
                    if (desc == null || desc.getName().isEmpty()) {
                        labels.add(new ProductBrowserLabel(parameters[i],
                                parameters[i]));
                    } else {
                        labels.add(new ProductBrowserLabel(desc.getName()
                                + " (" + parameters[i] + ")", parameters[i]));
                    }
                }
                Collections.sort(labels);
                return labels;
            } else if (GridInventory.LEVEL_ID_QUERY.equals(param)) {
                Level[] levels = new Level[parameters.length];
                LevelFactory lf = LevelFactory.getInstance();
                for (int i = 0; i < levels.length; i++) {
                    levels[i] = lf.getLevel(parameters[i]);
                }
                Arrays.sort(levels, levelComparator);
                for (int i = 0; i < parameters.length; i++) {
                    String levelName = levels[i].toString().replace("_", "-");
                    levelName = levelName.replace(levels[i].getMasterLevel()
                            .getName(), " "
                            + levels[i].getMasterLevel().getName());
                    labels.add(new ProductBrowserLabel(levelName, Long
                            .toString(levels[i].getId())));
                }
                return labels;
            } else if (GridInventory.MASTER_LEVEL_QUERY.equals(param)) {
                LevelFactory lf = LevelFactory.getInstance();
                for (int i = 0; i < parameters.length; i++) {
                    MasterLevel masterLevel = lf.getMasterLevel(parameters[i]);
                    labels.add(new ProductBrowserLabel(masterLevel
                            .getDescription()
                            + " ("
                            + masterLevel.getName()
                            + ")", masterLevel.getName()));
                }
                Collections.sort(labels);
                return labels;
            }
        } catch (CommunicationException e) {
            statusHandler.handle(Priority.ERROR, "Unable to format data for "
                    + productName, e);
        }
        return super.formatData(param, parameters);
    }

    @Override
    public HashMap<String, RequestConstraint> getProductParameters(
            String[] selection, String[] order) {
        if (getInventory() == null) {
            return super.getProductParameters(selection, order);
        }
        HashMap<String, RequestConstraint> queryList = super
                .getProductParameters(selection, order);
        if (queryList.containsKey(GridInventory.LEVEL_ID_QUERY)) {
            RequestConstraint levelRC = queryList
                    .remove(GridInventory.LEVEL_ID_QUERY);
            // Convert Level id to level one and level two values.
            try {
                Level level = LevelFactory.getInstance().getLevel(
                        levelRC.getConstraintValue());
                queryList
                        .put(GridInventory.LEVEL_ONE_QUERY,
                                new RequestConstraint(level
                                        .getLevelOneValueAsString()));
                queryList
                        .put(GridInventory.LEVEL_TWO_QUERY,
                                new RequestConstraint(level
                                        .getLevelTwoValueAsString()));
                queryList
                        .put(GridInventory.MASTER_LEVEL_QUERY,
                                new RequestConstraint(level.getMasterLevel()
                                        .getName()));
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to get product parameters for " + productName,
                        e);
            }
        }
        return queryList;
    }

    private GridInventory getInventory() {
        if ((Boolean) getPreference(SHOW_DERIVED_PARAMS).getValue()) {
            return (GridInventory) DataCubeContainer
                    .getInventory(GridConstants.GRID);
        } else {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #getDisplayTypes()
     */
    @Override
    public Map<ResourceType, List<DisplayType>> getDisplayTypes() {
        Map<ResourceType, List<DisplayType>> type = new HashMap<ResourceType, List<DisplayType>>();
        List<DisplayType> types = new ArrayList<DisplayType>();
        types.add(DisplayType.CONTOUR);
        types.add(DisplayType.IMAGE);
        type.put(ResourceType.PLAN_VIEW, types);
        return type;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.xml.IProductBrowserPreferences#
     * configurePreferences()
     */
    @Override
    public List<ProductBrowserPreference> configurePreferences() {
        List<ProductBrowserPreference> widgets = super.configurePreferences();
        ProductBrowserPreference derivedParameterPref = new ProductBrowserPreference();
        derivedParameterPref.setLabel(SHOW_DERIVED_PARAMS);
        derivedParameterPref.setPreferenceType(PreferenceType.BOOLEAN);
        derivedParameterPref
                .setTooltip("Show derived parameters in the Product Browser");
        derivedParameterPref.setValue(true);
        widgets.add(derivedParameterPref);
        return widgets;
    }
}
