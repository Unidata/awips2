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
package com.raytheon.uf.viz.core.rsc.legend;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.legend.ILegendDecorator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.viz.ui.cmenu.ContextMenuManager;
import com.raytheon.viz.ui.cmenu.IContextMenuProvider;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Base legend resource class, does majority of work for drawing legends.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 * @param <T>
 */

public abstract class AbstractLegendResource<T extends AbstractResourceData>
        extends AbstractVizResource<T, IDescriptor> implements
        ILegendDecorator, IContextMenuProvider {

    private static final int BOTTOM_OFFSET_IN_PIXELS = 7;

    private static final int RIGHT_OFFSET_IN_PIXELS = 18;

    private InputAdapter resourceClickedHandler = new InputAdapter() {

        private boolean moved = false;

        private ResourcePair mouseDownRsc;

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            boolean handle = moved = false;
            IDisplayPaneContainer container = getResourceContainer();
            IDisplayPane active = container.getActiveDisplayPane();
            if (active.getDescriptor() == getDescriptor()) {
                mouseDownRsc = checkLabelSpace(getDescriptor(),
                        active.getTarget(), x, y);
                if (mouseDownRsc != null) {
                    handle = AbstractLegendResource.this.checkResourceClick(
                            mouseDownRsc, mouseButton);
                    if (!handle) {
                        mouseDownRsc = null;
                    }
                }
            }
            return handle;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (mouseDownRsc != null) {
                moved = true;
            }
            return moved;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseDownRsc != null) {
                try {
                    if (!moved) {
                        resourceClicked(mouseDownRsc, mouseButton);
                    }
                } finally {
                    mouseDownRsc = null;
                }
                return true;
            }
            return false;
        }

    };

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractLegendResource(T resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(resourceClickedHandler);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        // Get the legend data to draw
        LegendEntry[] legendData = getLegendData(descriptor);
        List<DrawableString> legendStrings = new ArrayList<DrawableString>();

        double yStart = paintProps.getCanvasBounds().height
                - (BOTTOM_OFFSET_IN_PIXELS);
        for (LegendEntry le : legendData) {
            String allText = "";
            for (LegendData ld : le.legendParts) {
                allText += ld.label;
            }

            Rectangle2D allTextBounds = target
                    .getStringBounds(le.font, allText);

            double xStart = paintProps.getCanvasBounds().width
                    - ((RIGHT_OFFSET_IN_PIXELS + allTextBounds.getWidth()));

            double maxHeight = 0.0;
            for (LegendData ld : le.legendParts) {
                String text = ld.label;
                DrawableString string = new DrawableString(text, ld.color);
                string.font = le.font;
                string.setCoordinates(xStart, yStart);

                legendStrings.add(string);

                Rectangle2D textBounds = target.getStringsBounds(string);
                xStart += textBounds.getWidth();
                if (textBounds.getHeight() > maxHeight) {
                    maxHeight = textBounds.getHeight();
                }
            }

            yStart -= maxHeight;
        }

        target.getExtension(ICanvasRenderingExtension.class).drawStrings(
                paintProps, legendStrings.toArray(new DrawableString[0]));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(resourceClickedHandler,
                    InputPriority.SYSTEM_RESOURCE);
        }
    }

    protected ResourcePair checkLabelSpace(IDescriptor descriptor,
            IGraphicsTarget target, double x, double y) {
        LegendEntry[] legendData = getLegendData(descriptor);
        if (legendData == null || legendData.length == 0) {
            return null;
        }

        // Get the ratio for pixel to gl pixel conversion
        double ratio = descriptor.getRenderableDisplay().getView().getExtent()
                .getWidth()
                / descriptor.getRenderableDisplay().getBounds().width;

        IExtent extent = descriptor.getRenderableDisplay().getView()
                .getExtent();

        x = extent.getMinX() + (x * ratio);
        y = extent.getMinY() + (y * ratio);

        double yStart = extent.getMaxY() - ((BOTTOM_OFFSET_IN_PIXELS) * ratio);

        if (y > yStart) {
            return null;
        }

        for (LegendEntry le : legendData) {
            String allText = "";
            for (LegendData ld : le.legendParts) {
                allText += ld.label;
            }

            Rectangle2D allTextBounds = target
                    .getStringBounds(le.font, allText);

            double yEnd = yStart - (allTextBounds.getHeight() * ratio);
            if (y <= yStart && y > yEnd) {
                // Found the entry, look at data
                double xEnd = extent.getMaxX()
                        - (RIGHT_OFFSET_IN_PIXELS * ratio);
                if (x > xEnd) {
                    return null;
                }
                double xStart = xEnd - (allTextBounds.getWidth() * ratio);
                if (x < xStart) {
                    return null;
                }
                for (LegendData ld : le.legendParts) {
                    String text = ld.label;
                    Rectangle2D textBounds = target.getStringBounds(le.font,
                            text);
                    xEnd = xStart + (textBounds.getWidth() * ratio);
                    if (x <= xEnd) {
                        return ld.resource;
                    }
                    xStart = xEnd;
                }
            }
            yStart = yEnd;
        }

        return null;
    }

    /**
     * Get the resources pairs in the legend that are along point y
     * 
     * @param descriptor
     * @param target
     * @param y
     *            location world grid where to look
     * @param ratio
     *            canvas to world ratio to use
     * @return
     */
    protected ResourcePair[] checkYLabelSpace(IDescriptor descriptor,
            IGraphicsTarget target, double y, double ratio) {
        LegendEntry[] legendData = getLegendData(descriptor);
        if (legendData == null || legendData.length == 0) {
            return new ResourcePair[0];
        }

        List<ResourcePair> rps = new ArrayList<ResourcePair>();

        IExtent extent = descriptor.getRenderableDisplay().getView()
                .getExtent();

        double yStart = extent.getMaxY() - (BOTTOM_OFFSET_IN_PIXELS * ratio);

        if (y > yStart) {
            return new ResourcePair[0];
        }

        for (LegendEntry le : legendData) {
            String allText = "";
            for (LegendData ld : le.legendParts) {
                allText += ld.label;
            }

            Rectangle2D allTextBounds = target
                    .getStringBounds(le.font, allText);

            double yEnd = yStart - (allTextBounds.getHeight() * ratio);
            if (y <= yStart && y > yEnd) {
                // Found the entry
                for (LegendData ld : le.legendParts) {
                    rps.add(ld.resource);
                }
            }
            yStart = yEnd;
        }

        return rps.toArray(new ResourcePair[rps.size()]);
    }

    @Override
    public ResourceOrder getResourceOrder() {
        return ResourceOrder.HIGHEST;
    }

    @Override
    public void provideContextMenuItems(IMenuManager menuManager, int x, int y) {
        IGraphicsTarget target = null;
        for (IDisplayPane pane : getResourceContainer().getDisplayPanes()) {
            if (pane.getDescriptor() == descriptor) {
                target = pane.getTarget();
                break;
            }
        }
        ResourcePair rp = checkLabelSpace(descriptor, target, x, y);
        if (rp != null && rp.getResource() != null) {
            fillContextMenu(menuManager, rp);
        }
    }

    protected void fillContextMenu(IMenuManager menuManager,
            ResourcePair selectedResource) {
        ContextMenuManager.fillContextMenu(menuManager, selectedResource,
                getResourceContainer());
    }

    protected boolean checkResourceClick(ResourcePair mouseDownRsc,
            int mouseButton) {
        // Do nothing
        return false;
    }

    protected void resourceClicked(ResourcePair resource, int mouseButton) {
        // Do nothing
    }
}
