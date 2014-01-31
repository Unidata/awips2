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
package com.raytheon.uf.viz.datadelivery;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionType;
import com.raytheon.uf.common.datadelivery.registry.handlers.IAdhocSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;
import com.raytheon.viz.grid.GridProductBrowserDataFormatter;
import com.raytheon.viz.grid.inv.GridInventory;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.pointdata.PlotModels;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;
import com.raytheon.viz.pointdata.util.PointDataInventory;

/**
 * Product browser implementation for data delivery data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2013  2391      mpduff      Initial creation
 * Sept 22, 2013 2246      dhladky     Setup binoffset for time into +-5 min intervals
 * Oct 13,  2013 2460      dhladky     Added display of Adhoc subscriptions
 * Nov 19, 2013  2458      mpduff      Only pull subscriptions for the local site
 * Nov 21, 2013  2554      dhladky     Restored ADHOC's to working.
 * Jan 14, 2014  2459      mpduff      Change Subscription status code
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataDeliveryProductBrowserDataDefinition
        extends
        AbstractRequestableProductBrowserDataDefinition<AbstractRequestableResourceData> {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataDeliveryProductBrowserDataDefinition.class);

    /** Constant */
    private final String MADIS = "madis";

    /** Plot model file */
    private final String SVG = "madisObsDesign.svg";

    /** Constant */
    private final String PLUGIN_NAME = "pluginName";

    /** Constant */
    private final String DATA_DELIVERY = "Data Delivery";

    /** Constant */
    private final String SHOW_DERIVED_PARAMS = "Show Derived Parameters";

    /** Active subscription list */
    private final List<Subscription> activeSubList = new ArrayList<Subscription>();

    /** Selected subscription name */
    private String selectedSubscriptionName;

    /** Selected data type */
    private String selectedDataType;

    /** Point order */
    private final String[] POINT_ORDER = new String[] { "type", "subscription",
            "level" };

    private final String[] GRID_ORDER = new String[] {
            GridInventory.MODEL_NAME_QUERY, GridInventory.PARAMETER_QUERY,
            GridInventory.MASTER_LEVEL_QUERY, GridInventory.LEVEL_ID_QUERY };

    /**
     * Setup as 5 mins +- (60x5=300) from a reference time
     */
    private final int frameOffset = 300;

    /**
     * Constructor.
     */
    public DataDeliveryProductBrowserDataDefinition() {
        this.productName = DATA_DELIVERY;
        this.displayName = DATA_DELIVERY;
        loadProperties = new LoadProperties();
        order = POINT_ORDER;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void constructResource(String[] selection, ResourceType type) {
        selectedSubscriptionName = selection[2];
        selectedDataType = selection[1];
        super.constructResource(selection, type);
        if (selection[1].equalsIgnoreCase("Point")) {
            ((PlotResourceData) resourceData)
                    .setPlotSource(selection[selection.length - 3] + " "
                            + selection[selection.length - 2] + " "
                            + selection[selection.length - 1]);
            ((PlotResourceData) resourceData).setPlotModelFile(SVG);
            ((PlotResourceData) resourceData)
                    .setLevelKey(selection[selection.length - 1]);
        }
        constructResource();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ProductBrowserLabel> populateData(String[] selection) {
        // Set the order based on data type
        if (selection.length > 2) {
            if (selection[1].equalsIgnoreCase(DataType.GRID.name())) {
                order = GRID_ORDER;
                productName = DataType.GRID.name().toLowerCase();
                selectedDataType = DataType.GRID.name();
            } else if (selection[1].equalsIgnoreCase(DataType.POINT.name())) {
                order = POINT_ORDER;
                productName = DataType.POINT.name().toLowerCase();
                selectedDataType = DataType.POINT.name();
            }
        }
        if (selection.length == 1) {
            // Get provider names
            String[] dataTypes = getDataTypes();
            return formatData("Data Type", dataTypes);
        }

        if (selection.length == 2) {
            String source = selection[1];
            String[] subs = getSubscriptions(source);
            return formatData("SubscriptionName", subs);
        }

        if (selection.length == 3) {
            if (selectedDataType.equalsIgnoreCase(DataType.POINT.name())) {
                String[] results = PlotModels.getInstance().getLevels(MADIS,
                        SVG);
                String param = order[2];
                return formatData(param, results);
            } else if (selectedDataType.equalsIgnoreCase(DataType.GRID.name())) {
                this.productName = DataType.GRID.name().toLowerCase();
                // Must remove the first selection so this matches with the grid
                // version
                String[] usedSelection = realignSelection(selection);

                return super.populateData(usedSelection);
            }
        }

        if (selection.length >= 4) {
            if (selection[1].equalsIgnoreCase(DataType.GRID.name())) {
                // Must remove the first selection so this matches with the grid
                // version
                String[] usedSelection = realignSelection(selection);

                return super.populateData(usedSelection);
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String populateInitial() {
        return DATA_DELIVERY;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.productbrowser.AbstractProductBrowserDataDefinition
     * #formatData(java.lang.String, java.lang.String[])
     */
    @Override
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        if (selectedDataType == null
                || selectedDataType.equalsIgnoreCase(DataType.POINT.name())) {
            List<ProductBrowserLabel> temp = new ArrayList<ProductBrowserLabel>();
            for (int i = 0; i < parameters.length; i++) {
                ProductBrowserLabel label = new ProductBrowserLabel(
                        parameters[i], parameters[i]);
                temp.add(label);
                label.setProduct(param == order[2]);
                label.setData(parameters[i]);
            }
            Collections.sort(temp);
            return temp;
        } else if (selectedDataType.equalsIgnoreCase(DataType.GRID.name())) {
            try {
                return GridProductBrowserDataFormatter.formatGridData(param,
                        parameters);
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AbstractRequestableResourceData getResourceData() {
        if (selectedDataType.equalsIgnoreCase(DataType.POINT.name())) {
            resourceData = new PlotResourceData();
            resourceData.setBinOffset(new BinOffset(frameOffset, frameOffset));
            resourceData.setRetrieveData(false);
            resourceData.setUpdatingOnMetadataOnly(true);
            ((PlotResourceData) resourceData).setTopOfTheHour(false);
        } else if (selectedDataType.equalsIgnoreCase(DataType.GRID.name())) {
            resourceData = new GridResourceData();
        }
        return resourceData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HashMap<String, RequestConstraint> getProductParameters(
            String[] selection, String[] order) {
        if (selection[1].equalsIgnoreCase(DataType.POINT.name())) {
            return getPointProductParameters(selection, order);
        } else if (selection[0].equalsIgnoreCase(DataType.GRID.name())
                || selection[1].equalsIgnoreCase(DataType.GRID.name())) {
            return getGridProductParameters(selection, order);
        } else {
            throw new IllegalArgumentException("Invalid data type: "
                    + selection[1]);
        }
    }

    /**
     * Get the grid parameters.
     * 
     * @param selection
     *            The selected node
     * 
     * @param order
     *            The order
     * @return The constraint map
     */
    private HashMap<String, RequestConstraint> getGridProductParameters(
            String[] selection, String[] order) {
        if (selection.length > 5) {
            // Must remove the first selection so this matches with the grid
            // version
            String[] tmpSelection = realignSelection(selection);

            return super.getProductParameters(tmpSelection, order);
        } else {
            return super.getProductParameters(selection, order);
        }
    }

    /**
     * Get the point parameters.
     * 
     * @param selection
     *            The selected node
     * 
     * @param order
     *            The order
     * @return The constraint map
     */
    private HashMap<String, RequestConstraint> getPointProductParameters(
            String[] selection, String[] order) {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(PLUGIN_NAME, new RequestConstraint(MADIS));
        PointDataInventory inv = PlotModels.getInstance().getInventory();
        if (!inv.getTypeKey(selection[1])
                .equals(PointDataInventory.PLUGIN_NAME)) {
            queryList.put(inv.getTypeKey(selection[1]), new RequestConstraint(
                    selection[2]));
        }

        List<RequestConstraint> spatialCons = getSpatialConstraint();
        if (spatialCons != null && !spatialCons.isEmpty()) {
            queryList.put("location.longitude", spatialCons.get(0));
            queryList.put("location.latitude", spatialCons.get(1));
        }

        return queryList;
    }

    /**
     * Get the spatial constraints.
     * 
     * @return List of spatial constraints, or empty list if none
     */
    private List<RequestConstraint> getSpatialConstraint() {
        if (activeSubList == null || activeSubList.isEmpty()) {
            getSubscriptions(selectedDataType);
        }

        List<RequestConstraint> cons = new ArrayList<RequestConstraint>();

        Coverage cov = null;
        for (Subscription s : activeSubList) {
            if (s.getName().equals(this.selectedSubscriptionName)) {
                cov = s.getCoverage();
                break;
            }
        }

        if (cov != null) {
            RequestConstraint lonConstraint = new RequestConstraint(
                    cov.getRequestUpperLeft().x + "--"
                            + cov.getRequestLowerRight().x,
                    ConstraintType.BETWEEN);
            RequestConstraint latConstraint = new RequestConstraint(
                    cov.getRequestLowerRight().y + "--"
                            + cov.getRequestUpperLeft().y,
                    ConstraintType.BETWEEN);
            cons.add(lonConstraint);
            cons.add(latConstraint);
        }

        return cons;
    }

    /**
     * Get the subscriptions for the specified data type.
     * 
     * @param dataType
     *            The data type
     * 
     * @return Array of subscription names for the specified data type
     */
    private String[] getSubscriptions(String dataType) {
        DataType dt = DataType.valueOf(dataType.toUpperCase());
        List<String> subscriptionList = getSubscriptions(dt);
        return subscriptionList.toArray(new String[subscriptionList.size()]);
    }

    /**
     * Get the subscriptions for the specified data type.
     * 
     * @param dataType
     *            The data type
     * @return
     */
    private List<String> getSubscriptions(DataType dataType) {
        activeSubList.clear();
        final List<String> subNames = new ArrayList<String>();

        List<Subscription> subList = getSubscriptions();
        for (Subscription s : subList) {
            if (s.isActive()
                    || s.getSubscriptionType().equals(SubscriptionType.QUERY)) {
                if (s.getDataSetType() == dataType) {
                    activeSubList.add(s);
                    subNames.add(s.getName());
                }
            }
        }

        return subNames;
    }

    private String[] getDataTypes() {
        List<Subscription> subList = getSubscriptions();
        List<String> dataTypes = new ArrayList<String>();
        for (Subscription s : subList) {
            String dt = StringUtils.capitalize(s.getDataSetType().name()
                    .toLowerCase());
            if (!dataTypes.contains(dt)) {
                dataTypes.add(dt);
            }
        }

        return dataTypes.toArray(new String[dataTypes.size()]);
    }

    /**
     * Get a list of subscriptions from the registry.
     * 
     * @return List of subscriptions
     */
    private List<Subscription> getSubscriptions() {
        List<Subscription> subList = new ArrayList<Subscription>();
        final ISubscriptionHandler handler = RegistryObjectHandlers
                .get(ISubscriptionHandler.class);
        try {
            subList = handler.getByFilters(null, LocalizationManager
                    .getInstance().getCurrentSite());
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        final IAdhocSubscriptionHandler adhochandler = RegistryObjectHandlers
                .get(IAdhocSubscriptionHandler.class);
        List<AdhocSubscription> adhocSubs = null;

        try {
            adhocSubs = adhochandler.getAll();
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        if (adhocSubs != null) {
            subList.addAll(adhocSubs);
        }

        return subList;
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
        if (selectedDataType.equalsIgnoreCase(DataType.GRID.name())) {
            Map<ResourceType, List<DisplayType>> type = new HashMap<ResourceType, List<DisplayType>>();
            List<DisplayType> types = new ArrayList<DisplayType>();
            types.add(DisplayType.CONTOUR);
            types.add(DisplayType.IMAGE);
            type.put(ResourceType.PLAN_VIEW, types);
            return type;
        }
        return super.getDisplayTypes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition#configurePreferences()
     */
    @Override
    protected List<ProductBrowserPreference> configurePreferences() {
        if (selectedDataType.equalsIgnoreCase(DataType.GRID.name())) {
            List<ProductBrowserPreference> widgets = super
                    .configurePreferences();
            ProductBrowserPreference derivedParameterPref = new ProductBrowserPreference();
            derivedParameterPref.setLabel(SHOW_DERIVED_PARAMS);
            derivedParameterPref.setPreferenceType(PreferenceType.BOOLEAN);
            derivedParameterPref
                    .setTooltip("Show derived parameters in the Product Browser");
            derivedParameterPref.setValue(true);
            widgets.add(derivedParameterPref);
            return widgets;

        }
        return super.configurePreferences();
    }
}
