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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grib.util.GribModelLookup;
import com.raytheon.uf.common.dataplugin.grib.util.GridModel;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;
import com.raytheon.viz.grid.inv.GridInventory;
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

    private static final String SHOW_UNKNOWN_MODELS = "Show Unknown Models";

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
        productName = "grib";
        displayName = "Grid";
        order = new String[] { GridInventory.PLUGIN_NAME_QUERY,
                GridInventory.MODEL_NAME_QUERY, GridInventory.PARAMETER_QUERY,
                GridInventory.MASTER_LEVEL_QUERY, "modelInfo.level.id" };
        order = getOrder();
        loadProperties = new LoadProperties();
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
    public List<ProductBrowserLabel> populateData(String[] selection) {
        // inventory and cannot label each level of product browser
        if (getInventory() == null) {
            return super.populateData(selection);
        }
        Collection<String> sources = null;
        Collection<String> params = null;
        Collection<Level> levels = null;
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        if (selection.length > 1) {
            sources = Arrays.asList(selection[1]);
        } else {
            try {
                getInventory().checkSources(sources, params, levels,
                        returnQueue);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            List<ProductBrowserLabel> results = formatData(
                    GridInventory.MODEL_NAME_QUERY,
                    returnQueue.toArray(new String[returnQueue.size()]));
            Collections.sort(results);
            return results;
        }
        if (selection.length > 2) {
            params = Arrays.asList(selection[2]);
        } else {
            try {
                getInventory().checkParameters(sources, params, levels, false,
                        returnQueue);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            List<ProductBrowserLabel> results = formatData(
                    GridInventory.PARAMETER_QUERY,
                    returnQueue.toArray(new String[returnQueue.size()]));
            Collections.sort(results);
            return results;
        }
        try {
            getInventory().checkLevels(sources, params, levels, returnQueue);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        LevelFactory lf = LevelFactory.getInstance();
        try {
            if (selection.length > 3) {
                List<String> availLevels = new ArrayList<String>();
                for (String levelid : returnQueue) {
                    Level level = lf.getLevel(levelid);
                    if (level.getMasterLevel().getName().equals(selection[3])) {
                        availLevels.add(levelid);
                    }
                }
                return formatData(GridInventory.LEVEL_ID_QUERY,
                        availLevels.toArray(new String[availLevels.size()]));
            } else {
                Set<String> masterLevels = new HashSet<String>();
                for (String levelid : returnQueue) {
                    Level level = lf.getLevel(levelid);
                    masterLevels.add(level.getMasterLevel().getName());
                }
                List<ProductBrowserLabel> results = formatData(
                        GridInventory.MASTER_LEVEL_QUERY,
                        masterLevels.toArray(new String[masterLevels.size()]));
                Collections.sort(results);
                return results;
            }
        } catch (CommunicationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return super.populateData(selection);
        }

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
        if (!(Boolean) getPreference(FORMAT_DATA).getValue()) {
            boolean isProduct = false;
            if (GridInventory.LEVEL_ID_QUERY.equals(param)) {
                isProduct = true;
            }
            for (String string : parameters) {
                ProductBrowserLabel label = new ProductBrowserLabel(string,
                        string);
                label.setProduct(isProduct);
                labels.add(label);
            }
            return labels;
        }
        if (GridInventory.MODEL_NAME_QUERY.equals(param)) {
            GribModelLookup lookup = GribModelLookup.getInstance();
            for (int i = 0; i < parameters.length; i++) {
                GridModel model = lookup.getModelByName(parameters[i]);
                if (model == null) {
                    if (!(Boolean) getPreference(SHOW_UNKNOWN_MODELS)
                            .getValue()) {
                        continue;
                    }
                    labels.add(new ProductBrowserLabel(parameters[i],
                            parameters[i]));
                } else {
                    labels.add(new ProductBrowserLabel(model.getTitle() + " ("
                            + parameters[i] + ")", parameters[i]));
                }
            }
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
                    labels.add(new ProductBrowserLabel(desc.getName(),
                            parameters[i]));
                }
            }
            return labels;
        } else if (GridInventory.LEVEL_ID_QUERY.equals(param)) {
            Level[] levels = new Level[parameters.length];
            LevelFactory lf = LevelFactory.getInstance();
            try {
                for (int i = 0; i < levels.length; i++) {
                    levels[i] = lf.getLevel(parameters[i]);
                }
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            Arrays.sort(levels, levelComparator);
            for (int i = 0; i < parameters.length; i++) {
                String levelName = levels[i].toString().replace("_", "-");
                levelName = levelName.replace(levels[i].getMasterLevel()
                        .getName(), " " + levels[i].getMasterLevel().getName());
                labels.add(new ProductBrowserLabel(levelName, Long
                        .toString(levels[i].getId())));
                labels.get(i).setProduct(true);
            }
            return labels;
        } else if (GridInventory.MASTER_LEVEL_QUERY.equals(param)) {
            LevelFactory lf = LevelFactory.getInstance();
            try {
                for (int i = 0; i < parameters.length; i++) {
                    MasterLevel masterLevel = lf.getMasterLevel(parameters[i]);
                    labels.add(new ProductBrowserLabel(masterLevel
                            .getDescription(), masterLevel.getName()));
                }
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
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
        try {
            // Convert Level id to level one and level two values.
            Level level = LevelFactory.getInstance().getLevel(selection[4]);
            queryList.put(GridInventory.LEVEL_ONE_QUERY, new RequestConstraint(
                    level.getLevelOneValueAsString()));
            queryList.put(GridInventory.LEVEL_TWO_QUERY, new RequestConstraint(
                    level.getLevelTwoValueAsString()));
            queryList.remove(order[4]);
        } catch (CommunicationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);

        }
        return queryList;
    }

    private GridInventory getInventory() {
        if ((Boolean) getPreference(SHOW_DERIVED_PARAMS).getValue()) {
            return (GridInventory) DataCubeContainer.getInventory("grib");
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
        ProductBrowserPreference unknownPreference = new ProductBrowserPreference();
        unknownPreference.setLabel(SHOW_UNKNOWN_MODELS);
        unknownPreference.setPreferenceType(PreferenceType.BOOLEAN);
        unknownPreference
                .setTooltip("Show unknown models in the Product Browser");
        unknownPreference.setValue(true);
        widgets.add(unknownPreference);
        return widgets;
    }
}
