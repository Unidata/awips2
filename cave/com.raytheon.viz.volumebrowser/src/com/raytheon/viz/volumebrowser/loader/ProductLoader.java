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
package com.raytheon.viz.volumebrowser.loader;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.volumebrowser.datacatalog.DataCatalogManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.raytheon.viz.volumebrowser.vbui.ProductTableComp;

/**
 * 
 * This class can be used to accumulate multiple products from the
 * {@link ProductTableComp} for loading. Each product is added using
 * {@link #addProduct(IDataCatalogEntry, DisplayType)} and when all products
 * have beens elected then they can be loaded with either {@link #load()} or
 * {@link #loadDifference()}. Instances of this class are not intended for
 * reuse.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 03, 2015  3861     bsteffen  Initial Creation
 * Jan 13, 2016  5246     bsteffen  Create editor with new display so map
 *                                  resources are preserved.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class ProductLoader {

    private List<AbstractRenderableDisplay> displaysToLoad = new ArrayList<>();

    public void addProduct(IDataCatalogEntry catalogEntry,
            DisplayType displayType) {
        IDataCatalog catalog = DataCatalogManager.getDataCatalogManager()
                .getDataCatalog(catalogEntry.getSelectedData());
        ResourceType resourceType = catalogEntry.getDialogSettings()
                .getViewSelection().getResourceType();
        HashMap<String, RequestConstraint> metadataMap = catalog
                .getProductParameters(catalogEntry);
        String pluginName = metadataMap.get(PluginDataObject.PLUGIN_NAME_ID)
                .getConstraintValue();
        ProductCreator loader = ProductCreatorManager.getInstance().getCreator(
                pluginName, resourceType);
        if (loader == null) {
            throw new RuntimeException("Unable to load product for plugin "
                    + pluginName + " of type " + resourceType);
        }
        AbstractRenderableDisplay display = loader.loadProduct(catalog,
                catalogEntry, displayType);
        if (display != null) {
            displaysToLoad.add(display);
        }
    }

    public void loadDifference() {
        if (displaysToLoad.size() < 2) {
            throw new RuntimeException(
                    "Not enough resources selected to difference");
        } else if (displaysToLoad.size() > 2) {
            throw new RuntimeException(
                    "Too many resources selected to difference");
        }
        AbstractRenderableDisplay display1 = displaysToLoad.get(0);
        AbstractRenderableDisplay display2 = displaysToLoad.get(1);
        displaysToLoad.clear();
        if (!display1.getDescriptor().isCompatible(display2.getDescriptor())) {
            throw new RuntimeException(
                    "Difference failed because of incompatible resources.");
        }
        Iterator<ResourcePair> it1 = display1.getDescriptor().getResourceList()
                .iterator();
        Iterator<ResourcePair> it2 = display2.getDescriptor().getResourceList()
                .iterator();
        while (it1.hasNext() && it2.hasNext()) {
            ResourcePair pair1 = it1.next();
            ResourcePair pair2 = it2.next();
            if (pair1.getResourceData() instanceof ICombinedResourceData) {
                ICombinedResourceData first = (ICombinedResourceData) pair1
                        .getResourceData();
                first.setSecondaryData(pair2.getResourceData());
                first.setCombineOperation(CombineOperation.DIFFERENCE);
            }
        }
        System.out
                .println("Loading a Difference Product from the Volume Browser.");
        load(display1);
    }

    public void load() {
        if (displaysToLoad.isEmpty()) {
            return;
        }
        while (!displaysToLoad.isEmpty()) {
            AbstractRenderableDisplay display = displaysToLoad.get(0);
            displaysToLoad.remove(0);
            Iterator<AbstractRenderableDisplay> it = displaysToLoad.iterator();
            while (it.hasNext()) {
                AbstractRenderableDisplay next = it.next();
                if (display.getDescriptor().isCompatible(next.getDescriptor())) {
                    display.getDescriptor().getResourceList()
                            .addAll(next.getDescriptor().getResourceList());
                    it.remove();
                }
            }
            load(display);
        }
    }

    protected void load(AbstractRenderableDisplay display) {
        String editorId = DescriptorMap.getEditorId(display.getDescriptor()
                .getClass().getName());

        IEditorPart activeEditor = EditorUtil.getActiveEditor();

        AbstractEditor editor = UiUtil.createOrOpenEditor(editorId,
                display.createNewDisplay());

        if (editor != null) {
            if (activeEditor != null && editor != activeEditor
                    && activeEditor instanceof IMultiPaneEditor
                    && editor instanceof IMultiPaneEditor) {
                IMultiPaneEditor activeMpe = (IMultiPaneEditor) activeEditor;
                IMultiPaneEditor mpe = (IMultiPaneEditor) editor;

                if (mpe.getNumberofPanes() < activeMpe.getNumberofPanes()) {
                    for (int i = mpe.getNumberofPanes(); i < activeMpe
                            .getNumberofPanes(); ++i) {
                        mpe.addPane(display.createNewDisplay());
                    }

                    IDisplayPane selectedPane = activeMpe
                            .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
                    if (selectedPane != null) {
                        IDisplayPane[] allPanes = activeMpe.getDisplayPanes();
                        for (int i = 0; i < allPanes.length; ++i) {
                            if (selectedPane == allPanes[i]) {
                                IDisplayPane newSelectedPane = mpe
                                        .getDisplayPanes()[i];
                                mpe.setSelectedPane(
                                        IMultiPaneEditor.LOAD_ACTION,
                                        newSelectedPane);
                                break;
                            }
                        }
                    }
                }
            }
            Bundle b = new Bundle();
            b.setDisplays(new AbstractRenderableDisplay[] { display });
            Job j = new BundleProductLoader(editor, b);
            j.schedule();
        }
    }

}
