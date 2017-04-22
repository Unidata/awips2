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
package com.raytheon.uf.viz.d2d.core.legend;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.IMiddleClickCapableResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.ChooseColorAction;
import com.raytheon.viz.ui.cmenu.ColorEditDialogAction;
import com.raytheon.viz.ui.input.EditableManager;

/**
 * This mouse handler is responsible for picking up mouse clicks on resources in
 * the legend
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DLegendClickHandler extends AbstractD2DLegendInputHandler {

    private static final String HIDE_RESOURCE_PREF = "com.raytheon.viz.d2d.ui.hide.resource";

    private static final String SHOW_COLOR_EDIT = "com.raytheon.viz.d2d.ui.show.colorEdit";

    private static final String EDIT_RESOURCE_PREF = "com.raytheon.viz.ui.input.resource.edit";

    /**
     * @param resource
     */
    protected D2DLegendClickHandler(D2DLegendResource resource) {
        super(resource);
    }

    private int lastX, lastY;

    private ResourcePair mouseDownRsc = null;

    private boolean cancel = false;

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        if (mouseDownRsc != null) {
            cancel = true;
        }
        // eat the movement if we initially clicked on a resource
        return cancel;
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        lastX = x;
        lastY = y;
        final IDisplayPaneContainer editor = resource.getResourceContainer();
        if (resource.getLegendMode() != LegendMode.SHORT_PRODUCT
                && prefManager.handleClick(HIDE_RESOURCE_PREF, mouseButton)) {
            IDisplayPane activePane = editor.getActiveDisplayPane();
            IRenderableDisplay display = editor.getActiveDisplayPane()
                    .getRenderableDisplay();
            if (display.getDescriptor() == resource.getDescriptor()) {
                // Verify we are on our own pane
                mouseDownRsc = resource.checkLabelSpace(
                        display.getDescriptor(), activePane.getTarget(), x, y);
                return mouseDownRsc != null;
            }
        }
        return false;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        IDescriptor descriptor = resource.getDescriptor();
        final IDisplayPaneContainer container = resource.getResourceContainer();
        if (prefManager.handleClick(HIDE_RESOURCE_PREF, mouseButton)) {
            if (mouseDownRsc == null || cancel) {
                cancel = false;
                return false;
            }

            IDisplayPane activePane = container.getActiveDisplayPane();
            IRenderableDisplay display = container.getActiveDisplayPane()
                    .getRenderableDisplay();
            ResourcePair rsc = resource.checkLabelSpace(
                    display.getDescriptor(), activePane.getTarget(), x, y);

            if (rsc != null && rsc == mouseDownRsc) {
                mouseDownRsc = null;
                toggleVisibility(rsc);
                // Get the index of the toggled resource, -1 means
                // ignore blended-ness
                int idxOfToggled = -1;
                if (rsc.getResource() != null
                        && rsc.getResource().hasCapability(
                                BlendedCapability.class)) {
                    ResourcePair blended = rsc.getResource()
                            .getCapability(BlendedCapability.class)
                            .getBlendableResource();
                    if (blended != null && blended.getResource() != null) {
                        ResourceList rl = blended.getResource()
                                .getCapability(BlendableCapability.class)
                                .getResourceList();
                        int i = 0;
                        for (ResourcePair rp : rl) {
                            if (rp == rsc) {
                                idxOfToggled = i;
                                break;
                            }
                            ++i;
                        }
                    }
                }

                IExtent extent = descriptor.getRenderableDisplay().getExtent();
                double ratio = extent.getHeight()
                        / descriptor.getRenderableDisplay().getBounds().height;
                double worldY = extent.getMinY() + (y * ratio);
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    if (pane == activePane) {
                        continue;
                    }
                    if (idxOfToggled > -1) {
                        ResourcePair[] pairs = resource.checkYLabelSpace(
                                pane.getDescriptor(), pane.getTarget(), worldY,
                                ratio);
                        ResourcePair toCheck = null;
                        for (ResourcePair pair : pairs) {
                            if (pair != null
                                    && pair.getResource() != null
                                    && pair.getResource().hasCapability(
                                            BlendableCapability.class)) {
                                toCheck = pair;
                                break;
                            }
                        }

                        if (toCheck != null) {
                            ResourceList rl = toCheck.getResource()
                                    .getCapability(BlendableCapability.class)
                                    .getResourceList();
                            if (idxOfToggled < rl.size()) {
                                if (rsc.getProperties().isVisible() != rl
                                        .get(idxOfToggled).getProperties()
                                        .isVisible()) {
                                    toggleVisibility(rl.get(idxOfToggled));
                                }
                            }
                        }
                    } else {
                        AbstractResourceData ard = null;
                        if (rsc.getResource() != null) {
                            ard = rsc.getResource().getResourceData();
                        }
                        if (ard != null) {
                            for (ResourcePair rp : pane.getDescriptor()
                                    .getResourceList()) {
                                AbstractResourceData ard2 = null;
                                if (rp.getResource() != null) {
                                    ard2 = rp.getResource().getResourceData();
                                }
                                if (ard.equals(ard2)) {
                                    rp.getProperties().setVisible(
                                            rsc.getProperties().isVisible());
                                }
                            }
                        }
                    }
                }
                resource.issueRefresh();
                return true;
            }
        }

        // if (panned) {
        // panned = false;
        // }
        mouseDownRsc = null;

        final IDisplayPane activePane = container.getActiveDisplayPane();
        if (activePane == null || activePane.getDescriptor() != descriptor) {
            return false;
        }
        final ResourcePair[] rsc = new ResourcePair[] { null };
        rsc[0] = resource.checkLabelSpace(descriptor, activePane.getTarget(),
                x, y);
        if (rsc[0] != null && rsc[0].getResource() != null) {
            if (prefManager.handleClick(EDIT_RESOURCE_PREF, mouseButton)
                    && lastX == x && lastY == y) {
                if (rsc[0].getResource()
                        .hasCapability(EditableCapability.class)) {
                    // check / make editable
                    EditableManager.makeEditable(
                            rsc[0].getResource(),
                            !rsc[0].getResource()
                                    .getCapability(EditableCapability.class)
                                    .isEditable());
                    container.refresh();
                    return true;
                }
                if (rsc[0].getResource() instanceof IMiddleClickCapableResource) {
                    try {
                        ((IMiddleClickCapableResource) rsc[0].getResource())
                                .middleClicked();
                    } catch (VizException e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "An error occurred while processing the mouse action",
                                        e);
                    }
                    return true;
                }
            } else if (prefManager.handleClick(SHOW_COLOR_EDIT, mouseButton)) {
                if (rsc[0].getResource()
                        .hasCapability(ColorMapCapability.class)) {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            ColorEditDialogAction action = new ColorEditDialogAction();
                            action.setSelectedRsc(rsc[0]);
                            action.setContainer(container);
                            action.run();
                        }
                    });
                } else {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            AbstractRightClickAction parent = new AbstractRightClickAction() {

                            };
                            parent.setSelectedRsc(rsc[0]);
                            parent.setContainer(container);
                            ChooseColorAction action = new ChooseColorAction(
                                    parent);
                            action.run();
                        }
                    });
                }
                return true;
            }
        }
        return false;
    }

    /**
     * Toggle visibility of resource taking blended/blendable resources into
     * account.
     * 
     * If resource to toggle is blended, 1st check to see if parent resource is
     * visible. If not visible then make parent and all children visible.
     * 
     * @param rp
     */
    private boolean toggleVisibility(ResourcePair rp) {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc != null) {
            if (rsc.hasCapability(BlendedCapability.class)) {
                ResourcePair parentRsc = rsc.getCapability(
                        BlendedCapability.class).getBlendableResource();
                ResourceList children = parentRsc.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList();
                if (parentRsc.getProperties().isVisible() == false) {
                    parentRsc.getProperties().setVisible(true);
                    for (ResourcePair child : children) {
                        child.getProperties().setVisible(true);
                    }
                } else {
                    // topmost resource is visible, toggle us and other
                    // rsc
                    if (rp.getProperties().isVisible() == false) {
                        rp.getProperties().setVisible(true);
                        parentRsc
                                .getResource()
                                .getCapability(BlendableCapability.class)
                                .setAlphaStep(BlendableCapability.BLEND_MAX / 2);
                    } else {
                        parentRsc.getResource()
                                .getCapability(BlendableCapability.class)
                                .toggle(rp);
                    }
                }
                return rp.getProperties().isVisible();
            }
        }
        rp.getProperties().setVisible(!rp.getProperties().isVisible());
        return rp.getProperties().isVisible();
    }
}
