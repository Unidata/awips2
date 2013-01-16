package com.raytheon.viz.ui;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * 
 * Loads a bundle as a product to a container. This will add the resources from
 * the bundle displays onto the container instead of replacing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class BundleProductLoader extends BundleLoader {

    public BundleProductLoader(IDisplayPaneContainer container, Bundle bundle) {
        super("Product Loader", container, bundle);
    }

    @Override
    protected LoadItem[] getLoadItems(IDisplayPaneContainer container,
            Bundle bundle) throws VizException {
        IDisplayPane[] containerPanes = container.getDisplayPanes();
        AbstractRenderableDisplay[] bundleDisplays = bundle.getDisplays();

        int bundleSize = bundleDisplays.length;
        int editorSize = containerPanes.length;

        IDisplayPane[] loadTo;
        IRenderableDisplay[] loadFrom;

        IDisplayPane selected = null;
        if (container instanceof IMultiPaneEditor) {
            selected = ((IMultiPaneEditor) container)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
        }

        // Figure out what panes to load to
        if (selected != null && bundleSize == 1) {
            // Only load to selected pane
            loadTo = new IDisplayPane[] { selected };
            loadFrom = new IRenderableDisplay[] { bundleDisplays[0] };
        } else if (bundleSize == 1 && editorSize >= 1) {
            loadTo = new IDisplayPane[editorSize];
            loadFrom = new IRenderableDisplay[editorSize];
            for (int i = 0; i < editorSize; ++i) {
                loadTo[i] = containerPanes[i];
                loadFrom[i] = bundleDisplays[0].cloneDisplay();
            }
        } else {
            // Load 1-1
            if (editorSize < bundleSize) {
                // If fewer container panes than bundle displays, attempt to
                // ensure 1-1 by adding panes
                ensureOneToOne(container, bundle);
                containerPanes = container.getDisplayPanes();
                editorSize = containerPanes.length;
            }
            // Load what is possible
            int maxCanLoad = Math.min(editorSize, bundleSize);
            loadTo = new IDisplayPane[maxCanLoad];
            loadFrom = new IRenderableDisplay[maxCanLoad];
            for (int i = 0; i < maxCanLoad; ++i) {
                loadTo[i] = containerPanes[i];
                loadFrom[i] = bundleDisplays[i];
            }
        }

        LoadItem[] items = new LoadItem[loadTo.length];
        for (int i = 0; i < items.length; ++i) {
            items[i] = new LoadItem(loadTo[i], loadFrom[i]);
        }
        return items;
    }

    @Override
    protected void load(IDisplayPane loadTo, IRenderableDisplay loadFrom) {
        IDescriptor existingDescriptor = loadTo.getDescriptor();
        IDescriptor fromDescriptor = loadFrom.getDescriptor();

        /**
         * Update the frame count based on what has been listed in the bundle if
         * we don't have times already loaded
         */
        FramesInfo info = existingDescriptor.getFramesInfo();
        if (info.getFrameCount() == 0) {
            existingDescriptor.setNumberOfFrames(fromDescriptor
                    .getNumberOfFrames());
        }

        // Pull out the resources to load
        ResourceList rscs = loadFrom.getDescriptor().getResourceList();
        List<ResourcePair> resourcesToLoad = new ArrayList<ResourcePair>();

        for (ResourcePair rp : rscs) {
            if (rp.getProperties().isSystemResource() == false) {
                resourcesToLoad.add(rp);
            }
        }

        rscs.clear();

        /**
         * For each resource pair in the bundle resources: Give a unique color
         * for the legend if one isn't set, add to pane's descriptor's resource
         * list
         */
        for (ResourcePair rp : resourcesToLoad) {
            AbstractResourceData ard = rp.getResourceData();

            try {
                ard.configure(rp.getLoadProperties(), existingDescriptor);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            boolean newRP = true;
            if (existingDescriptor.getResourceList().contains(rp)) {
                newRP = false;
            }
            if (newRP
                    && (rp.getProperties().isSystemResource() == false && !rp
                            .getLoadProperties().getCapabilities()
                            .hasCapability(ColorableCapability.class))) {
                rp.getLoadProperties()
                        .getCapabilities()
                        .getCapability(rp.getResourceData(),
                                ColorableCapability.class)
                        .setColor(
                                ColorUtil.getNewColor(
                                        container.getDisplayPanes(),
                                        existingDescriptor, rp));
            }

            if (newRP) {
                existingDescriptor.getResourceList().add(rp);
            }
        }
    }

}
