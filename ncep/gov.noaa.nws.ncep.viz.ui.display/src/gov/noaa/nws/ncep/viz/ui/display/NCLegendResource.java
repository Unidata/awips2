/*****************************************************************************************
 * COPYRIGHT (c), 2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.IPowerLegend;

import java.awt.geom.Rectangle2D;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.legend.ILegendDecorator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.legend.AbstractLegendResource;
import com.raytheon.viz.ui.cmenu.MoveDownAction;
import com.raytheon.viz.ui.cmenu.MoveUpAction;

/**
 * Legend decorator for Natl Cntrs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2010     #259     Greg Hull    Initial Creation
 * 07/13/2001     #446     Q. Zhou      Added implements IInputHandler
 * 										Added mouse handlers, initInternal, toggleVisibility. See D2DLegendResource
 * 02/06/2011              S. Gurung    Separated/moved input handler code to class NCLegendHandler
 * 02/29/2011     651      Archana      Added the overridden method fillContextMenu()										
 * 07/27/2012	  695	   B. Yin		Added editable capability for resource legends
 * 08/18/2014       ?      B. Yin       Handle GroupResource.
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

public class NCLegendResource extends
        AbstractLegendResource<GenericResourceData> implements ILegendDecorator {

    private IInputHandler legendHandler = new NCLegendHandler(this);

    protected static final int BOTTOM_OFFSET_IN_PIXELS = 7;

    protected static final int RIGHT_OFFSET_IN_PIXELS = 18;

    private static IFont groupResourceFont;

    private double yStart;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public NCLegendResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
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

        groupResourceFont = target.initializeFont(target.getDefaultFont()
                .getFontName(), target.getDefaultFont().getFontSize(),
                new Style[] { Style.ITALIC, Style.BOLD });
        yStart = paintProps.getCanvasBounds().height
                - (BOTTOM_OFFSET_IN_PIXELS);

        List<DrawableString> legendStrings = generateLegendStrings(target,
                paintProps, legendData, RIGHT_OFFSET_IN_PIXELS);

        target.getExtension(ICanvasRenderingExtension.class).drawStrings(
                paintProps, legendStrings.toArray(new DrawableString[0]));
    }

    @SuppressWarnings("deprecation")
    private List<DrawableString> generateLegendStrings(IGraphicsTarget target,
            PaintProperties paintProps, LegendEntry[] legendData,
            int rightOffset) {
        List<DrawableString> legendStrings = new ArrayList<DrawableString>();

        for (LegendEntry le : legendData) {

            if (le.legendParts[0].resource.getResource() instanceof IPowerLegend) {

                IPowerLegend gr = (IPowerLegend) le.legendParts[0].resource
                        .getResource();
                if (gr.isNameExpanded()) {
                    LegendEntry[] legends = getLegendData(gr);
                    List<DrawableString> legendStr = generateLegendStrings(
                            target, paintProps, legends, RIGHT_OFFSET_IN_PIXELS);

                    if (legendStr != null && !legendStr.isEmpty()) {

                        for (DrawableString dstring : legendStr) {
                            dstring.font = groupResourceFont;

                            // Adjust xStart with the font.
                            String legendText = "";
                            for (String str : dstring.getText()) {
                                legendText += str;
                            }
                            Rectangle2D stringBounds = target.getStringBounds(
                                    dstring.font, legendText);

                            double xStart = paintProps.getCanvasBounds().width
                                    - ((rightOffset + stringBounds.getWidth()));
                            dstring.setCoordinates(xStart, dstring.basics.y);

                        }

                        legendStrings.addAll(legendStr);
                    }
                }
            }

            String allText = "";
            for (LegendData ld : le.legendParts) {
                allText += ld.label;
            }

            Rectangle2D allTextBounds = target
                    .getStringBounds(le.font, allText);

            double xStart = paintProps.getCanvasBounds().width
                    - ((rightOffset + allTextBounds.getWidth()));

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

        return legendStrings;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public LegendEntry[] getLegendData(IDescriptor descriptor) {

        List<LegendData> labels = new ArrayList<LegendData>();
        ResourceList resourceList = descriptor.getResourceList();
        if (resourceList != null) {
            for (int i = 0; i < resourceList.size(); i++) {
                ResourcePair resourcePair = resourceList.get(i);
                // See if resource is a system resource (does not
                // participate in legend)
                boolean system = resourcePair.getProperties()
                        .isSystemResource();
                // See if resource is visible
                boolean vis = resourcePair.getProperties().isVisible();
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                if (system) {
                    continue;
                } else {
                    LegendData legend = new LegendData();
                    if (rsc == null) {
                        continue;
                    } else if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                        continue;
                    } else {
                        legend.label = rsc.getName();
                        // if the resource is editable, add (Editable) to the
                        // legend string
                        if (rsc.hasCapability(EditableCapability.class)
                                && rsc.getCapability(EditableCapability.class)
                                        .isEditable()) {
                            legend.label += " (Enabled) ";
                        }
                        legend.resource = resourcePair;
                    }

                    if (!vis) {
                        legend.color = new RGB(50, 50, 50);
                    } else {
                        // get the color from the NatlCntrs Resource.
                        legend.color = new RGB(250, 250, 250); // default to
                                                               // white

                        try {
                            // HACK ALERT : currently there is a cyclical
                            // dependency bug
                            // that prevents the display project from
                            // referencing the
                            // resources project.
                            // get the method from INatlCntrsResource to get the
                            // legend
                            // color
                            Method[] mthds = legend.resource.getResourceData()
                                    .getClass().getMethods();

                            for (Method m : mthds) {
                                // System.out.println( m.getName() );
                                if (m.getName().equals("getLegendColor")) {
                                    if (m.getReturnType() == RGB.class) {
                                        legend.color = (RGB) m
                                                .invoke(legend.resource
                                                        .getResourceData());
                                        break;
                                    }
                                }
                            }
                        } catch (Exception e) {
                            System.out.println(e.getMessage());
                        }
                    }
                    labels.add(legend);
                }

            }
        }

        LegendEntry[] entries = new LegendEntry[labels.size()];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].legendParts = new LegendData[] { labels.get(i) };
        }
        return entries;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.legend.ILegendDecorator#getLegendData(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    private LegendEntry[] getLegendData(IResourceGroup groupResource) {

        List<LegendData> labels = new ArrayList<LegendData>();
        ResourceList resourceList = groupResource.getResourceList();
        if (resourceList != null) {
            for (int i = 0; i < resourceList.size(); i++) {
                ResourcePair resourcePair = resourceList.get(i);
                // See if resource is a system resource (does not
                // participate in legend)
                boolean system = resourcePair.getProperties()
                        .isSystemResource();
                // See if resource is visible
                boolean vis = resourcePair.getProperties().isVisible();
                AbstractVizResource<?, ?> rsc = resourcePair.getResource();
                if (system) {
                    continue;
                } else {
                    LegendData legend = new LegendData();
                    if (rsc == null) {
                        continue;
                    } else if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                        continue;
                    } else {
                        legend.label = rsc.getName();
                        // if the resource is editable, add (Editable) to the
                        // legend string
                        if (rsc.hasCapability(EditableCapability.class)
                                && rsc.getCapability(EditableCapability.class)
                                        .isEditable()) {
                            legend.label += " (Enabled) ";
                        }
                        legend.resource = resourcePair;
                    }

                    if (!vis) {
                        legend.color = new RGB(50, 50, 50);
                    } else {
                        // get the color from the NatlCntrs Resource.
                        legend.color = new RGB(250, 250, 250); // default to
                                                               // white

                        try {
                            // HACK ALERT : currently there is a cyclical
                            // dependency bug
                            // that prevents the display project from
                            // referencing the
                            // resources project.
                            // get the method from INatlCntrsResource to get the
                            // legend
                            // color
                            Method[] mthds = legend.resource.getResourceData()
                                    .getClass().getMethods();

                            for (Method m : mthds) {
                                // System.out.println( m.getName() );
                                if (m.getName().equals("getLegendColor")) {
                                    if (m.getReturnType() == RGB.class) {
                                        legend.color = (RGB) m
                                                .invoke(legend.resource
                                                        .getResourceData());
                                        break;
                                    }
                                }
                            }
                        } catch (Exception e) {
                            System.out.println(e.getMessage());
                        }
                    }
                    labels.add(legend);
                }

            }
        }

        LegendEntry[] entries = new LegendEntry[labels.size()];
        for (int i = 0; i < entries.length; ++i) {
            entries[i] = new LegendEntry();
            entries[i].legendParts = new LegendData[] { labels.get(i) };
        }
        return entries;
    }

    protected void initInternal(IGraphicsTarget target) {
        try {
            super.initInternal(target);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        target.initializeFont(NCLegendResource.class.getName());
        IDisplayPaneContainer rc = getResourceContainer();
        if (rc == null)
            return;

        rc.registerMouseHandler(legendHandler, InputPriority.SYSTEM_RESOURCE);
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();

        IDisplayPaneContainer rc = getResourceContainer();
        if (rc == null)
            return;

        rc.unregisterMouseHandler(legendHandler);
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
        ResourcePair rsc;
        List<ResourcePair> resourcesClicked = getResourceClicked(descriptor,
                target, x, y);
        if (resourcesClicked != null && !resourcesClicked.isEmpty()) {
            // Get the top level resource, otherwise some of the Raytheon code
            // cannot find the resource.
            rsc = resourcesClicked.get(resourcesClicked.size() - 1);
            // rsc = resourcesClicked.get(0);
        } else {
            rsc = null;
        }

        if (rsc != null && rsc.getResource() != null) {

            if (resourcesClicked.size() > 1) {

                // Some of Raytheon actions require the resource be in the
                // descriptor. The code below is to add then remove the resource
                // into/from the descriptor without firing listeners.
                AbstractVizResource<?, ?> originalRes = rsc.getResource();

                rsc.setResource(null);
                originalRes.getDescriptor().getResourceList().add(rsc);
                rsc.setResource(originalRes);

                fillContextMenu(menuManager, rsc);

                rsc.setResource(null);
                originalRes.getDescriptor().getResourceList().remove(rsc);
                rsc.setResource(originalRes);
            } else {
                fillContextMenu(menuManager, rsc);
            }

        }

        for (IContributionItem item : menuManager.getItems()) {
            if (item instanceof ActionContributionItem) {
                ActionContributionItem act = (ActionContributionItem) item;

                // Remove Raytheon's move-up and move-down actions.
                // They don't work with our ResourceGroup.
                if (act.getAction() instanceof MoveDownAction
                        || act.getAction() instanceof MoveUpAction) {
                    act.setVisible(false);
                }
                else if ( rsc.getResource() instanceof IPowerLegend && act.getAction().getText().contains("Attributes") ){
                    //for group resource, remove the "Edit Attributes" action
                    act.setVisible(false);
                }
            }
        }
    }

    protected List<ResourcePair> getResourceClicked(IDescriptor descriptor,
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

        List<ResourcePair> resourcesClicked = new ArrayList<ResourcePair>();
        getResourceClickedInLegendGroup(descriptor, target, x, y, extent,
                ratio, yStart, legendData, resourcesClicked);

        return resourcesClicked;
    }

    @SuppressWarnings("deprecation")
    private List<ResourcePair> getResourceClickedInLegendGroup(
            IDescriptor descriptor, IGraphicsTarget target, double x, double y,
            IExtent extent, double ratio, double yStart,
            LegendEntry[] legendData, List<ResourcePair> resourcesClicked) {
        for (LegendEntry le : legendData) {

            // Check if the entry is a group. For NC there is only one legend
            // part.
            boolean isExpandedGroup = false;
            IPowerLegend gr = null;
            ResourcePair grpPair = null;
            for (LegendData ld : le.legendParts) {
                if (ld.resource.getResource() instanceof IPowerLegend) {
                    grpPair = ld.resource;
                    gr = (IPowerLegend) ld.resource.getResource();
                    isExpandedGroup = gr.isNameExpanded();
                    break;
                }
            }

            // Calculate legend height
            String allText = "";
            for (LegendData ld : le.legendParts) {
                allText += ld.label;
            }

            Rectangle2D allTextBounds = target
                    .getStringBounds(le.font, allText);

            double legendHeight = (allTextBounds.getHeight() * ratio);
            double yEnd = yStart - legendHeight;

            // Calculate group resource legend height
            double groupHeight = 0;
            if (isExpandedGroup) {
                Rectangle2D resourceGroupBounds = target.getStringBounds(
                        groupResourceFont, allText);
                groupHeight = resourceGroupBounds.getHeight() * ratio
                        * gr.getResourceList().size();
            }

            if (isExpandedGroup && (y <= yStart && y > yStart - groupHeight)) { // in
                                                                                // group
                                                                                // resources
                resourcesClicked.add(grpPair);
                getResourceClickedInLegendGroup(descriptor, target, x, y,
                        extent, ratio, yStart, getLegendData(gr),
                        resourcesClicked);
                // if not on any member of the group resource, remove the
                // parent
                if (resourcesClicked.get(resourcesClicked.size() - 1) == grpPair) {
                    resourcesClicked.remove(resourcesClicked.size() - 1);
                }
                break;
            } else { // either not in group resource or click on the group name

                if (isExpandedGroup) { // click on the group name
                    yStart -= groupHeight;
                    yEnd = yStart - legendHeight;
                }

                if (y <= yStart && y > yEnd) {
                    // Found the entry, look at data

                    double xEnd = extent.getMaxX()
                            - (RIGHT_OFFSET_IN_PIXELS * ratio);

                    if (x > xEnd) {
                        return resourcesClicked;
                    }

                    double xStart = xEnd - (allTextBounds.getWidth() * ratio);

                    if (x < xStart) {
                        return resourcesClicked;
                    }

                    for (LegendData ld : le.legendParts) {
                        String text = ld.label;
                        Rectangle2D textBounds = target.getStringBounds(
                                le.font, text);
                        xEnd = xStart + (textBounds.getWidth() * ratio);
                        if (x <= xEnd) {

                            resourcesClicked.add(ld.resource);

                            return resourcesClicked;
                        }
                        xStart = xEnd;
                    }
                }
                yStart = yEnd;
            }
        }
        return resourcesClicked;
    }

    // @Override
    // protected void fillContextMenu(IMenuManager menuManager,
    // ResourcePair selectedResource) {
    //
    // MoveUpAction upAction = new MoveUpAction();
    // MoveDownAction downAction = new MoveDownAction();
    // EnableDisableAction enableDisableAction = new EnableDisableAction();
    // RemoveResourceAction rrAction = new RemoveResourceAction();
    // NCMapEditor container = NmapUiUtils.getActiveNatlCntrsEditor();
    //
    // upAction.setContainer(container);
    // upAction.setSelectedRsc(selectedResource);
    // downAction.setContainer(container);
    // downAction.setSelectedRsc(selectedResource);
    // enableDisableAction.setContainer(container);
    // enableDisableAction.setSelectedRsc(selectedResource);
    // rrAction.setContainer(container);
    // rrAction.setSelectedRsc(selectedResource);
    //
    // AbstractVizResource<?,?> thisResource = selectedResource.getResource();
    // //Cyclic dependancy work-around..
    // Object editRscAttrsAction = null;
    // Method[] arrayOfMethods = thisResource.getClass().getMethods();
    // for ( Method m : arrayOfMethods ){
    // if ( m.getName().compareTo("resourceAttrsModified") == 0){
    // try {
    // editRscAttrsAction =
    // Class.forName("gov.noaa.nws.ncep.viz.resources.attributes.EditResourceAttrsAction").newInstance();
    // ( ( AbstractRightClickAction ) editRscAttrsAction
    // ).setContainer(container);
    // ( ( AbstractRightClickAction ) editRscAttrsAction
    // ).setSelectedRsc(selectedResource);
    // break;
    // } catch (InstantiationException e) {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // } catch (IllegalAccessException e) {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // } catch (ClassNotFoundException e) {
    // // TODO Auto-generated catch block
    // e.printStackTrace();
    // }
    //
    // }
    // }
    //
    // if ( editRscAttrsAction != null )
    // menuManager.add( ( IAction )editRscAttrsAction);
    //
    // menuManager.add(upAction);
    // menuManager.add(downAction);
    // menuManager.add(enableDisableAction);
    // menuManager.add(rrAction);
    //
    // }

}