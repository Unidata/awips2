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
package com.raytheon.uf.viz.productbrowser.datalisting;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.NoPluginException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.MapRenderableDisplay;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.productbrowser.ProductBrowserDataDefinition;
import com.raytheon.uf.viz.productbrowser.ProductBrowserLabel;
import com.raytheon.uf.viz.productbrowser.ProductBrowserPreference;
import com.raytheon.uf.viz.productbrowser.ProductBrowserView;
import com.raytheon.uf.viz.productbrowser.pref.PreferenceBasedDataDefinition;
import com.raytheon.uf.viz.productbrowser.pref.ProductBrowserPreferenceConstants;
import com.raytheon.uf.viz.productbrowser.pref.ProductBrowserPreferenceListener;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * 
 * {@link ProductBrowserDataDefinition} which uses a {@link DataListing} to
 * format and dispaly items in the {@link ProductBrowserView}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 04, 2015  4153     bsteffen  Initial creation
 * Nov 03, 2015  5030     mapeters  Quietly handle CAVE & EDEX plugins being out of sync
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class DataListingProductBrowserDefinition implements
        PreferenceBasedDataDefinition {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(DataListingProductBrowserDefinition.class);

    protected final String displayName;

    protected final DataListing listing;

    protected final ProductBrowserPreference enabledPreference;

    protected final ProductBrowserPreference formatPreference;

    protected final ProductBrowserPreference orderPreference;

    public DataListingProductBrowserDefinition(DataListing listing) {
        this(createDisplayName(listing.getPluginName()), listing);
    }

    @Override
    public List<ProductBrowserPreference> getPreferences() {
        return Arrays.asList(enabledPreference, orderPreference,
                formatPreference);
    }

    protected boolean isEnabled() {
        return (Boolean) enabledPreference.getValue();
    }

    protected boolean isFormatted() {
        return (Boolean) formatPreference.getValue();
    }

    protected String[] getOrderedKeys() {
        return (String[]) orderPreference.getValue();
    }

    protected Map<String, String> createKeyValMap(String[] selection) {
        Map<String, String> keyVals = new HashMap<>();
        int index = 1;
        for (String key : getOrderedKeys()) {
            if (index < selection.length) {
                keyVals.put(key, selection[index]);
                index += 1;
            } else {
                break;
            }
        }
        return keyVals;
    }

    public DataListingProductBrowserDefinition(String displayName,
            DataListing listing) {
        this.displayName = displayName;
        this.listing = listing;
        this.enabledPreference = ProductBrowserPreferenceConstants
                .createEnabledPreference();
        this.formatPreference = ProductBrowserPreferenceConstants
                .createFormatPreference();
        this.orderPreference = ProductBrowserPreferenceConstants
                .createOrderPreference(listing.getKeys().toArray(new String[0]));

        new ProductBrowserPreferenceListener(displayName, enabledPreference);
        new ProductBrowserPreferenceListener(displayName, formatPreference);
        new ProductBrowserPreferenceListener(displayName, orderPreference);

    }

    private static String createDisplayName(String pluginName) {
        return Character.toUpperCase(pluginName.charAt(0))
                + pluginName.substring(1);
    }

    @Override
    public boolean checkAvailability() {
        if (!isEnabled()) {
            return false;
        }

        try {
            RecordFactory.getInstance().getPluginClass(listing.getPluginName());
        } catch (NoPluginException e) {
            String msg = "Unable to display "
                    + displayName
                    + " data in Product Browser because the server does not support the "
                    + listing.getPluginName() + " plugin";
            logger.debug(msg);
            return false;
        }

        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(listing.getRequestConstraints(Collections
                .<String, String> emptyMap()));
        request.setLimit(1);
        try {
            DbQueryResponse response = (DbQueryResponse) RequestRouter
                    .route(request);
            return response.getNumResults() > 0;
        } catch (Exception e) {
            logger.error("Error initializing product browser data for "
                    + displayName, e);
            return false;
        }
    }

    @Override
    public List<ProductBrowserLabel> getLabels(String[] selection) {
        if (selection.length == 0) {
            ProductBrowserLabel label = new ProductBrowserLabel(displayName,
                    listing.getPluginName());
            label.setProduct(listing.getKeys().isEmpty());
            return Collections.singletonList(label);
        }
        Map<String, String> keyVals = createKeyValMap(selection);
        String nextKey = getOrderedKeys()[keyVals.size()];
        try {
            if (isFormatted()) {
                Map<String, String> formattedMap = listing.getFormattedValues(
                        nextKey, keyVals);
                List<ProductBrowserLabel> labels = new ArrayList<>(
                        formattedMap.size());
                for (Entry<String, String> entry : formattedMap.entrySet()) {
                    ProductBrowserLabel label = new ProductBrowserLabel(
                            entry.getValue(), entry.getKey());
                    label.setProduct(selection.length >= listing.getKeys()
                            .size());
                    labels.add(label);
                }
                return labels;
            } else {
                Collection<String> values = listing.getValues(nextKey, keyVals);
                List<ProductBrowserLabel> labels = new ArrayList<>(
                        values.size());
                for (String value : values) {
                    ProductBrowserLabel label = new ProductBrowserLabel(value,
                            value);
                    label.setProduct(selection.length >= listing.getKeys()
                            .size());
                    labels.add(label);
                }
                return labels;
            }
        } catch (Exception e) {
            logger.error("Error querying " + nextKey + " for " + displayName
                    + " in the product browser.", e);
            return Collections.emptyList();
        }
    }

    @Override
    public String getProductInfo(String[] selection) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(PluginDataObject.PLUGIN_NAME_ID + " = "
                + listing.getPluginName());
        int index = 1;
        for (Entry<String, String> entry : createKeyValMap(selection)
                .entrySet()) {
            if (index >= selection.length) {
                break;
            }
            stringBuilder.append("\n");
            stringBuilder.append(entry.getKey() + " = " + entry.getValue());
            index += 1;
        }
        return stringBuilder.toString();
    }

    @Override
    public Collection<DisplayType> getValidDisplayTypes(String[] selection) {
        return Collections.emptyList();
    }

    @Override
    public void loadResource(String[] selection, DisplayType displayType) {
        Map<String, String> keyVals = createKeyValMap(selection);
        AbstractResourceData resourceData = createResourceData(keyVals);
        ResourcePair resourcePair = createResourcePair(resourceData,
                displayType);
        try {
            AbstractRenderableDisplay display = createRenderableDisplay(resourcePair);
            AbstractEditor editor = createOrOpenEditor(display);
            Bundle b = new Bundle();
            b.setDisplays(new AbstractRenderableDisplay[] { display });
            new BundleProductLoader(editor, b).schedule();
        } catch (VizException e) {
            logger.error("Failed to load resource of type "
                    + resourceData.getClass().getSimpleName(), e);
        }
    }

    /**
     * This method is very similar to
     * {@link UiUtil#createOrOpenEditor(String, IRenderableDisplay...)}. The
     * main differences are that it only works for a single display, it
     * determines the editor type from the descriptor in the display, and it
     * allows the perspective manager to open new editors.
     * 
     * The reason this is necessary is because when a new editor is opened with
     * the default {@link MapRenderableDisplay} then it is not very useful. When
     * the perspective manager loads a new map editor it will include some base
     * maps and set the map projection to something that is user friendly.
     * 
     * If the perspective manager does not load the correct type of editor then
     * this will fall back to using UiUtil.
     */
    protected AbstractEditor createOrOpenEditor(IRenderableDisplay display) {
        String editorId = DescriptorMap.getEditorId(display.getDescriptor()
                .getClass().getName());
        IEditorPart editorPart = EditorUtil.getActiveEditor();
        if (editorPart != null
                && editorId.equals(editorPart.getEditorSite().getId())) {
            return (AbstractEditor) editorPart;
        }
        editorPart = EditorUtil.findEditor(editorId);
        if (editorPart != null) {
            return (AbstractEditor) editorPart;
        }
        IWorkbenchWindow window = VizWorkbenchManager.getInstance()
                .getCurrentWindow();
        /*
         * This part allows the perspective manager to make an editor which may
         * have some customizations.
         */
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
        return UiUtil.createOrOpenEditor(editorId, display);
    }

    protected abstract AbstractResourceData createResourceData(
            Map<String, String> keyVals);

    protected ResourcePair createResourcePair(
            AbstractResourceData resourceData, DisplayType displayType) {
        ResourcePair pair = new ResourcePair();
        pair.setResourceData(resourceData);
        LoadProperties loadProperties = new LoadProperties();
        if (displayType != null) {
            loadProperties.getCapabilities()
                    .getCapability(resourceData, DisplayTypeCapability.class)
                    .setDisplayType(displayType);
        }
        pair.setLoadProperties(loadProperties);
        pair.setProperties(new ResourceProperties());
        return pair;
    }

    protected AbstractRenderableDisplay createRenderableDisplay(
            ResourcePair resourcePair) throws VizException {
        IMapDescriptor mapDescriptor = new MapDescriptor();
        mapDescriptor.getResourceList().add(resourcePair);
        return new MapRenderableDisplay(mapDescriptor);
    }

}
