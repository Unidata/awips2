package com.raytheon.viz.pointdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;
import com.raytheon.viz.pointdata.util.PointDataInventory;

/**
 * Product browser implementation for grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2013  2391      mpduff      refactored some methods to common class.
 * 
 * </pre>
 */
public class PlotModelDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<PlotResourceData> {

    /**
     * Constructor.
     */
    public PlotModelDataDefinition() {
        productName = "plotModels";
        displayName = "Plot Models";
        order = new String[] { "type", "svg", "level" };
        loadProperties = new LoadProperties();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.
     * AbstractRequestableProductBrowserDataDefinition
     * #constructResource(java.lang.String[],
     * com.raytheon.uf.viz.core.rsc.ResourceType)
     */
    @Override
    public void constructResource(String[] selection, ResourceType type) {
        super.constructResource(selection, type);
        resourceData.setPlotSource(selection[selection.length - 3] + " "
                + selection[selection.length - 2] + " "
                + selection[selection.length - 1]);
        resourceData.setPlotModelFile(selection[selection.length - 2]);
        resourceData.setLevelKey(selection[selection.length - 1]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public PlotResourceData getResourceData() {
        resourceData = new PlotResourceData();
        resourceData.setBinOffset(new BinOffset(1800, 1800));
        resourceData.setRetrieveData(false);
        resourceData.setUpdatingOnMetadataOnly(true);
        return resourceData;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public HashMap<String, RequestConstraint> getProductParameters(
            String[] selection, String[] order) {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(PLUGIN_NAME, new RequestConstraint(selection[1]));
        PointDataInventory inv = PlotModels.getInstance().getInventory();
        if (!inv.getTypeKey(selection[1])
                .equals(PointDataInventory.PLUGIN_NAME)) {
            queryList.put(inv.getTypeKey(selection[1]), new RequestConstraint(
                    selection[2]));

        }
        return queryList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> buildProductList(List<String> historyList) {
        // TODO something here
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String populateInitial() {
        if (!isEnabled()) {
            return null;
        }

        PointDataInventory inv = PlotModels.getInstance().getInventory();
        try {
            if (inv == null || inv.getPlugins() == null) {
                return null;
            }
        } catch (Exception e) {
            return null;
        }
        return displayName;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ProductBrowserLabel> populateData(String[] selection) {
        String[] results = null;
        PlotModels plotModels = PlotModels.getInstance();
        PointDataInventory inv = plotModels.getInventory();
        String param = null;
        if (selection.length == 1) {
            results = inv.getPlugins().toArray(new String[0]);
            param = PLUGIN_NAME;
            return formatData(param, results);
        }
        String source = selection[1];

        boolean hasType = !inv.getTypeKey(selection[1]).equals(
                PointDataInventory.PLUGIN_NAME);
        if (hasType) {
            if (selection.length == 2) {
                try {
                    results = inv.getAvailableTypes(selection[1]);
                } catch (VizException e) {
                    throw new RuntimeException(e);
                }
                param = order[1];
                return formatData(param, results);
            } else {
                source += selection[2];
            }
        }
        if ((hasType && selection.length == 3) || selection.length == 2) {
            results = getModels(source);
            param = order[1];
            return formatData(param, results);
        }

        if ((hasType && selection.length == 4) || selection.length == 3) {
            results = plotModels.getLevels(source,
                    selection[selection.length - 1]);
            param = order[2];
            return formatData(param, results);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ProductBrowserLabel> formatData(String param,
            String[] parameters) {
        Arrays.sort(parameters);
        List<ProductBrowserLabel> labels = new ArrayList<ProductBrowserLabel>();
        for (int i = 0; i < parameters.length; i++) {
            if (param == order[1]) {
                labels.add(new ProductBrowserLabel(parameters[i].replace(
                        ".svg", ""), parameters[i]));
            } else {
                labels.add(new ProductBrowserLabel(parameters[i], parameters[i]));
            }
            labels.get(i).setProduct(param == order[2]);
        }
        return labels;
    }

    private String[] getModels(String source) {
        PlotModels models = PlotModels.getInstance();

        List<String> validModels = new ArrayList<String>();
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        Collection<String> sourcesToProcess = Arrays.asList(source);
        try {
            PlotModels
                    .getInstance()
                    .getInventory()
                    .checkParameters(sourcesToProcess, null, null, true,
                            returnQueue);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        for (Entry<String, List<String>> entry : models.getModels().entrySet()) {
            if (returnQueue.containsAll(entry.getValue())) {
                validModels.add(entry.getKey());
            }
        }

        return validModels.toArray(new String[0]);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ProductBrowserPreference> configurePreferences() {
        List<ProductBrowserPreference> prefs = super.configurePreferences();
        for (ProductBrowserPreference pref : prefs) {
            if (pref.getLabel().equals(ENABLE_PLUGIN)) {
                pref.setValue(false);
            }
        }
        return prefs;
    }
}
