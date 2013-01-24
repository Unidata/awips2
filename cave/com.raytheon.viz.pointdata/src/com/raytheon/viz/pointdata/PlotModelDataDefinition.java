package com.raytheon.viz.pointdata;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.AbstractRequestableProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.viz.pointdata.PlotModelFactory2.PlotModelElement;
import com.raytheon.viz.pointdata.rsc.PlotResourceData;
import com.raytheon.viz.pointdata.util.PointDataInventory;

public class PlotModelDataDefinition extends
        AbstractRequestableProductBrowserDataDefinition<PlotResourceData> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PlotModelDataDefinition.class);

    private static final String PLOTLOCATION = "plotModels";

    private static Map<String, List<String>> models;

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

    @Override
    public PlotResourceData getResourceData() {
        resourceData = new PlotResourceData();
        resourceData.setBinOffset(new BinOffset(1800, 1800));
        resourceData.setRetrieveData(false);
        resourceData.setUpdatingOnMetadataOnly(true);
        return resourceData;
    }

    @Override
    public HashMap<String, RequestConstraint> getProductParameters(
            String[] selection, String[] order) {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(PLUGIN_NAME, new RequestConstraint(selection[1]));
        PointDataInventory inv = getInventory();
        if (!inv.getTypeKey(selection[1])
                .equals(PointDataInventory.PLUGIN_NAME)) {
            queryList.put(inv.getTypeKey(selection[1]), new RequestConstraint(
                    selection[2]));

        }
        return queryList;
    }

    @Override
    public List<String> buildProductList(List<String> historyList) {
        // TODO something here
        return Collections.emptyList();
    }

    @Override
    public String populateInitial() {
        if (!isEnabled()) {
            return null;
        }
        try {
            if (getInventory() == null || getInventory().getPlugins() == null) {
                return null;
            }
        } catch (Exception e) {
            return null;
        }
        return displayName;

    }

    @Override
    public List<ProductBrowserLabel> populateData(String[] selection) {
        String[] results = null;
        PointDataInventory inv = getInventory();
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
            results = getLevels(source, selection[selection.length - 1]);
            param = order[2];
            return formatData(param, results);
        }
        return null;
    }

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

    private void populateModels() {
        if (models == null) {
            models = new HashMap<String, List<String>>();
            LocalizationContext ctx = null;

            IPathManager pm = PathManagerFactory.getPathManager();

            ctx = pm.getContext(LocalizationType.CAVE_STATIC,
                    LocalizationLevel.BASE);
            File dir = pm.getFile(ctx, PLOTLOCATION);

            if (!dir.exists() || !dir.isDirectory()) {
                throw new RuntimeException("Cannot find plot model files");
            }

            FileFilter filter = new FileFilter() {

                @Override
                public boolean accept(File f) {
                    return (!f.isHidden() && f.canRead() && f.getName()
                            .contains("svg"));
                }

            };

            File[] files = dir.listFiles(filter);

            MapDescriptor fakeDescriptor = null;
            try {
                fakeDescriptor = new MapDescriptor();
            } catch (VizException e) {
                throw new RuntimeException(e);
            }

            for (File file : files) {
                try {
                    List<PlotModelElement> fields = new PlotModelFactory2(
                            fakeDescriptor, file.getName()).getPlotFields();
                    List<String> params = new ArrayList<String>();
                    for (PlotModelElement p : fields) {
                        if (!p.parameter.equals("")
                                && !p.parameter.contains(",")) {
                            params.add(p.parameter);
                        } else if (p.parameter.contains(",")) {
                            String[] individualParams = p.parameter.split(",");
                            for (String param : individualParams) {
                                params.add(param);
                            }
                        }
                    }
                    models.put(file.getName(), params);
                } catch (Exception e) {
                    ;//
                }
            }
        }
    }

    private String[] getModels(String source) {
        populateModels();
        List<String> validModels = new ArrayList<String>();
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        Collection<String> sourcesToProcess = Arrays.asList(source);
        try {
            getInventory().checkParameters(sourcesToProcess, null, null, true,
                    returnQueue);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        for (Entry<String, List<String>> entry : models.entrySet()) {
            if (returnQueue.containsAll(entry.getValue())) {
                validModels.add(entry.getKey());
            }
        }

        return validModels.toArray(new String[0]);
    }

    private String[] getLevels(String source, String model) {
        populateModels();
        Set<String> possibleLevels = null;
        for (String parameter : models.get(model)) {
            BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
            Collection<String> sourcesToProcess = Arrays.asList(source);
            try {
                getInventory().checkLevels(sourcesToProcess,
                        Arrays.asList(parameter), null, returnQueue);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            if (possibleLevels == null) {
                possibleLevels = new HashSet<String>(returnQueue);
            } else {
                possibleLevels.retainAll(returnQueue);
            }
        }
        List<String> validLevels = new ArrayList<String>();
        for (String levelid : possibleLevels) {
            try {
                Level level = LevelFactory.getInstance().getLevel(
                        Long.parseLong(levelid));
                validLevels.add(LevelMappingFactory.getInstance()
                        .getLevelMappingForLevel(level).getDisplayName());
            } catch (CommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (VizCommunicationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return validLevels.toArray(new String[0]);
    }

    private PointDataInventory getInventory() {
        return (PointDataInventory) DataCubeContainer.getInventory("obs");
    }

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
