package com.raytheon.uf.viz.productbrowser;

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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference.PreferenceType;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Product browser abstract requestable implementation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 * @param <T>
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractRequestableProductBrowserDataDefinition<T extends AbstractRequestableResourceData>
        extends AbstractProductBrowserDataDefinition<T> implements
        ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractRequestableProductBrowserDataDefinition.class);

    // for requestable products, pluginName must be part of the request
    public String PLUGIN_NAME = "pluginName";

    // name of the product for the request constraints
    public String productName;

    // order that you want the data to be in the tree, must correspond to
    // request constraints
    protected String[] order = null;

    protected static final String ORDER = "Order";

    /**
     * First population when product browser is opened, decides what data types
     * are available
     * 
     * @return
     */
    @Override
    public String populateInitial() {
        if (!isEnabled()) {
            return null;
        }
        List<Object[]> parameters = null;
        if (order.length >= 1) {
            try {
                DbQuery query = new DbQuery(productName);
                query.setMaxResults(1);
                parameters = query.performQuery();
            } catch (VizException e) {
                statusHandler.handle(Priority.ERROR,
                        "Unable to populate initial product tree", e);
            }

            if (parameters != null && !parameters.isEmpty()) {
                if (parameters.get(0).length > 0) {
                    return displayName;
                } else {
                    return null;
                }
            } else {
                return null;
            }
        } else {
            return displayName;
        }
    }

    /**
     * Populates the tree each time the tree is expanded using the selections
     * 
     * @param selection
     * @return
     */
    @Override
    public List<ProductBrowserLabel> populateData(String[] selection) {
        long time = System.currentTimeMillis();
        List<ProductBrowserLabel> parameters = null;
        boolean product = false;
        String param = order[selection.length - 1];
        HashMap<String, RequestConstraint> queryList = getProductParameters(
                selection, order);
        product = selection.length == order.length;

        String[] temp = queryData(param, queryList);
        if (temp != null) {
            if ((Boolean) getPreference(FORMAT_DATA).getValue()) {
                parameters = formatData(param, temp);
            } else {
                parameters = super.formatData(param, temp);
            }
        }

        if (parameters != null) {
            for (int i = 0; i < parameters.size(); i++) {
                parameters.get(i).setProduct(product);
            }
        }

        System.out.println("Time to query for "
                + selection[selection.length - 1] + ": "
                + (System.currentTimeMillis() - time) + "ms");
        return parameters;
    }

    /**
     * 
     * @param param
     * @param queryList
     * @return
     */
    protected String[] queryData(String param,
            HashMap<String, RequestConstraint> queryList) {
        try {
            return CatalogQuery.performQuery(param, queryList);
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Unable to perform query", e);
        }
        return null;
    }

    @Override
    public void constructResource(String[] selection, ResourceType type) {
        resourceData = getResourceData();
        (resourceData).setMetadataMap(getProductParameters(selection, order));
        if (type != null) {
            loadProperties.setResourceType(type);
        }
        constructResource();
    }

    public void constructResource(
            HashMap<String, RequestConstraint> requestConstraints) {
        resourceData = getResourceData();
        resourceData.setMetadataMap(requestConstraints);
        constructResource();
    }

    protected void constructResource() {
        ResourcePair pair = new ResourcePair();
        pair.setResourceData(resourceData);
        pair.setLoadProperties(loadProperties);
        pair.setProperties(new ResourceProperties());
        constructResource(Arrays.asList(pair));
    }

    protected void constructResource(List<ResourcePair> pairs) {
        // retrieves the correct editor
        getEditor();
        IDisplayPaneContainer container = getEditor();
        if (container == null) {
            return;
        }
        AbstractRenderableDisplay display = (AbstractRenderableDisplay) container
                .getActiveDisplayPane().getRenderableDisplay();
        display = (AbstractRenderableDisplay) display.createNewDisplay();
        for (ResourcePair pair : pairs) {
            display.getDescriptor().getResourceList().add(pair);
        }
        Bundle b = new Bundle();
        b.setDisplays(new AbstractRenderableDisplay[] { display });
        new BundleProductLoader(EditorUtil.getActiveVizContainer(), b).schedule();
    }

    /**
     * @return
     */
    @Override
    public abstract T getResourceData();

    @Override
    public List<String> buildProductList(final List<String> historyList) {
        historyList.add(displayName);
        Map<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        RequestConstraint contstraint = new RequestConstraint(productName);
        queryList.put(PLUGIN_NAME, contstraint);
        for (int i = 0; i < order.length; i++) {
            try {
                String[] items = CatalogQuery.performQuery(order[i], queryList);
                List<ProductBrowserLabel> labels = formatData(order[i], items);
                if (labels != null) {
                    for (int j = 0; j < labels.size(); j++) {
                        historyList.add(labels.get(j).getName());
                    }
                }
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
        return historyList;
    }

    /**
     * Getting the map of request constraints for populating the resource data
     * 
     * @param selection
     * @param order
     * @return
     */
    public HashMap<String, RequestConstraint> getProductParameters(
            String[] selection, String[] order) {
        HashMap<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
        queryList.put(PLUGIN_NAME, new RequestConstraint(productName));

        String[] usedSelection = realignSelection(selection);
        for (int i = 0; i < usedSelection.length; i++) {
            queryList.put(order[i], new RequestConstraint(usedSelection[i]));
        }
        return queryList;
    }

    /**
     * Reorder the selection so that it lines up with the order
     * 
     * @param selection
     * @return
     */
    protected final String[] realignSelection(String[] selection) {
        String[] usedSelection = new String[selection.length - 1];
        for (int i = 1; i < selection.length; i++) {
            usedSelection[i - 1] = selection[i];
        }
        return usedSelection;
    }

    public ResourceType getResourceType() {
        return ResourceType.PLAN_VIEW;
    }

    /**
     * Knowledge of this is for what kind of product it is, should be overridden
     * by child class to send back what are the possible ResourceTypes and the
     * names they should be given in the menu
     * 
     * @return
     */
    public List<ResourceType> getProductTypes() {
        return null;
    }

    protected IDisplayPaneContainer getEditor() {
        String id = DescriptorMap.getEditorId(getDescriptorClass().getName());
        IEditorPart editorPart = EditorUtil.getActiveEditor();
        if (editorPart != null && id.equals(editorPart.getEditorSite().getId())) {
            return (AbstractEditor) editorPart;
        }
        editorPart = EditorUtil.findEditor(id);
        if (editorPart != null) {
            return (AbstractEditor) editorPart;
        }
        return openNewEditor(id);
    }

    protected IDisplayPaneContainer openNewEditor(String editorId) {
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        AbstractVizPerspectiveManager mgr = VizPerspectiveListener.getInstance(
                window).getActivePerspectiveManager();
        if (mgr != null) {
            AbstractEditor editor = mgr.openNewEditor();
            if (editor == null) {
                return null;
            } else if (editorId.equals(editor.getEditorSite().getId())) {
                return editor;
            } else {
                window.getActivePage().closeEditor(editor, false);
            }
        }
        return null;
    }

    protected Class<? extends IDescriptor> getDescriptorClass() {
        return MapDescriptor.class;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.productbrowser.IProductBrowserPreferences#
     * configurePreferences()
     */
    @Override
    protected List<ProductBrowserPreference> configurePreferences() {
        List<ProductBrowserPreference> widgets = super.configurePreferences();
        ProductBrowserPreference preference = null;
        preference = new ProductBrowserPreference();
        preference.setLabel(ORDER);
        preference.setPreferenceType(PreferenceType.STRING_ARRAY);
        preference.setValue(order);
        preference
                .setTooltip("Change the order to make things appear in a different order in the Product Browser");
        widgets.add(preference);

        preference = new ProductBrowserPreference();
        preference.setLabel(FORMAT_DATA);
        preference.setPreferenceType(PreferenceType.BOOLEAN);
        preference.setValue(true);
        preference
                .setTooltip("Check to format the data or not (not will give exact parameter that is queried for, not the nice looking version");
        widgets.add(preference);
        return widgets;
    }

    /**
     * @return the order
     */
    protected String[] getOrder() {
        String temp = Activator.getDefault().getPreferenceStore()
                .getString(ORDER + displayName);
        if (temp != null && !temp.isEmpty()) {
            order = temp.split(",");
        }
        return order;
    }
}
