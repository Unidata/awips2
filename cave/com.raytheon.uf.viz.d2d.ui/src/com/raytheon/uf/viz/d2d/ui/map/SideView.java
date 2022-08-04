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

package com.raytheon.uf.viz.d2d.ui.map;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.IRenameablePart;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.panes.PaneManager;
import org.locationtech.jts.geom.Coordinate;

/**
 * The side view for "docking" displays.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 01, 2006             chammack    Initial Creation.
 * Dec 19, 2007             njensen     Support for different tab types.
 * Jul 08, 2009    830      bgonzale    use pane manager instead of managing own panes.
 * Oct 22, 2009    334      bsteffen    limit number of frames in sidepane rather than reseting
 * Jul 01, 2010    6146     bkowal      Updates the legend mode so that smaller panes will
 *                                       now include a legend
 * Nov 20, 2012 DR 15524    M.Porricelli Changed so interactive screens still editable when
 *                                        swapped to side panel
 * Mar 21, 2013    1638     mschenke    Changed map scales not tied to d2d
 * Aug 09, 2013 DR 16427    D. Friedman Swap additional input handlers.
 * Oct 10, 2013    2104     mschenke    Switched to use MapScalesManager
 * Jul 15, 2014    2954     njensen     Updated init() for MapScalesManager change
 * Aug 25, 2014    3467     mapeters    Removed changing of editability from swapPanes().
 * Mar 02, 2015    4204     njensen     Support for swapping part names
 * Apr 02, 2015    4204     njensen     Fix 4-panel swap of renamed parts
 * Sep 18, 2015 DR 17996    D. Friedman Clear editor pane's IRenderableDisplay before swap
 * Oct 21, 2015    5023     njensen     Removed unnecessary setPartName()
 * Jun 25, 2018    6660     bsteffen    Ensure outgoing display keeps background color during swap.
 * Jan 09, 2019    7526     tgurney     Make resources uneditable while they are
 *                                      in the side view
 *
 * </pre>
 *
 * @author chammack
 *
 */
public class SideView extends ViewPart implements IMultiPaneEditor,
        IBackgroundColorChangedListener, IRenameablePart {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SideView.class);

    public static final String ID = SideView.class.getName();

    private PaneManager paneManager;

    private LoopProperties loopProperties;

    private Bundle bundleToLoad = null;

    private BackgroundColor backgroundColor;

    /**
     * Remembers which resource was editable before it was swapped into this
     * side view
     */
    private AbstractVizResource<?, ?> editableRsc;

    /**
     * The constructor.
     */
    public SideView() {
        init();
    }

    public synchronized void init() {
        loopProperties = new LoopProperties();
    }

    @Override
    public void init(IViewSite site) throws PartInitException {
        super.init(site);

        backgroundColor = BackgroundColor
                .getInstance(site.getPage().getPerspective());
        backgroundColor.addListener(BGColorMode.GLOBAL, this);

        String myId = site.getId() + UiUtil.SECONDARY_ID_SEPARATOR
                + site.getSecondaryId();

        bundleToLoad = MapScalesManager.getInstance()
                .getScaleBundleForPart(myId);
    }

    @Override
    public void dispose() {
        super.dispose();
        if (backgroundColor != null) {
            backgroundColor.removeListener(BGColorMode.GLOBAL, this);
        }
    }

    @Override
    public void createPartControl(Composite parent) {
        try {
            if (paneManager == null) {
                paneManager = new PaneManager();
                paneManager.registerMouseHandler(new PanHandler(this),
                        InputPriority.LOWEST);
                paneManager.registerMouseHandler(swappingListener,
                        InputPriority.PART);
            }
            paneManager.initializeComponents(this, parent);

            parent.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseUp(MouseEvent e) {
                    if (e.button == 3) {
                        swapPanes();
                    }
                }
            });
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error creating SideView", e);
        }

        if (bundleToLoad != null) {
            new BundleLoader(this, bundleToLoad).run();
        }
    }

    /**
     * Passing the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        if (paneManager != null) {
            paneManager.setFocus();
        }
    }

    private IInputHandler swappingListener = new InputAdapter() {

        private int buttonDown = 0;

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            buttonDown = mouseButton;
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (buttonDown == 3 && buttonDown == mouseButton) {
                /* right click */
                buttonDown = -1;
                swapPanes();
                return true;
            }
            return false;
        }
    };

    @Override
    public IDisplayPane[] getDisplayPanes() {
        if (this.paneManager == null) {
            return new IDisplayPane[0];
        } else {
            return this.paneManager.getDisplayPanes();
        }
    }

    @Override
    public LoopProperties getLoopProperties() {
        return this.loopProperties;
    }

    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        this.loopProperties = loopProperties;
    }

    @Override
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    @Override
    public void refresh() {
        paneManager.refresh();
    }

    /**
     * <pre>
     * Swap View and Editor panes.
     *    View
     *       Pane
     *          set RenderableDisplay
     *          set RenderableDisplay.Extent
     *          -setZoombox()
     *          -zoomToZoombox()
     *          -refresh()
     *    Editor
     *       Active Display Pane
     *          if the editor and the pane's renderable display descriptor are the same type
     *          then
     *             set RenderableDisplay
     *             set RenderableDisplay.Extent
     *          else
     *             -closeEditor()
     *             -openEditor(with Renderable Display)
     *             set RenderableDisplay.Extent
     *          end if
     *          RenderableDisplay.Extent
     *          -setZoombox()
     *          -zoomToZoombox()
     *         -refresh()
     * </pre>
     */
    public void swapPanes() {
        IEditorPart editor = EditorUtil.getActiveEditor();
        if (editor != null && editor instanceof AbstractEditor) {
            Set<IRenderableDisplay> myHiddenDisplays = new HashSet<>();
            Set<IRenderableDisplay> editorHiddenDisplays = new HashSet<>();

            AbstractEditor theEditor = (AbstractEditor) editor;

            // swap part name
            String editorName = theEditor.getPartName();
            String viewName = this.getPartName();
            if ("D2D Side View".equals(viewName)) {
                viewName = IRenameablePart.DEFAULT_PART_NAME;
            }
            theEditor.setPartName(viewName);
            this.setPartName(editorName);

            // First thing to do, swap input handlers
            // Get editor resource handlers and unregister on editor
            final InputPriority[] SWAPPABLE_PRIORITIES = {
                    InputPriority.RESOURCE, InputPriority.SYSTEM_RESOURCE,
                    InputPriority.SYSTEM_RESOURCE_LOW };
            Map<InputPriority, IInputHandler[]> editorHandlers = new HashMap<>();
            for (InputPriority priority : SWAPPABLE_PRIORITIES) {
                IInputHandler[] handlers = theEditor.getMouseManager()
                        .getHandlersForPriority(priority);
                editorHandlers.put(priority, handlers);
                for (IInputHandler handler : handlers) {
                    theEditor.getMouseManager().unregisterMouseHandler(handler);
                }
            }

            // Store and unregister input handlers on ourself
            Map<InputPriority, IInputHandler[]> myHandlers = new HashMap<>();
            for (InputPriority priority : SWAPPABLE_PRIORITIES) {
                IInputHandler[] handlers = paneManager.getMouseManager()
                        .getHandlersForPriority(priority);
                myHandlers.put(priority, handlers);
                for (IInputHandler handler : handlers) {
                    unregisterMouseHandler(handler);
                }
            }

            // Register editor handlers on ourself
            for (InputPriority priority : SWAPPABLE_PRIORITIES) {
                IInputHandler[] handlers = editorHandlers.get(priority);
                for (IInputHandler handler : handlers) {
                    registerMouseHandler(handler, priority);
                }
            }

            IDisplayPane[] editorPanes = theEditor.getDisplayPanes();
            // Set swapping so we don't get disposed, and find an editable
            // resource if there is one
            for (IDisplayPane dPane : editorPanes) {
                dPane.getRenderableDisplay().setSwapping(true);
                if (!dPane.isVisible()) {
                    editorHiddenDisplays.add(dPane.getRenderableDisplay());
                }
            }

            IRenderableDisplay[] myRenderables = paneManager
                    .getRenderableDisplays();
            IDisplayPane[] viewPanes = getDisplayPanes();
            for (IDisplayPane myPane : viewPanes) {
                myPane.getRenderableDisplay().setSwapping(true);
                if (!myPane.isVisible()) {
                    myHiddenDisplays.add(myPane.getRenderableDisplay());
                }
            }

            RGB outgoingBackgroundColor = myRenderables[0].getBackgroundColor();

            int editorPaneCount = editorPanes.length;
            int viewPaneCount = viewPanes.length;

            try {
                boolean isCompatibleEditor = theEditor.getSite().getId()
                        .equals(DescriptorMap.getEditorId(myRenderables[0]
                                .getDescriptor().getClass().getName()));
                // I have my renderables saved off, load editor renderables
                // to me first
                if (viewPaneCount > editorPaneCount) {
                    // view has too many panes, remove and set editor's displays
                    for (int i = viewPaneCount - 1; i >= editorPaneCount; --i) {
                        removePane(viewPanes[i]);
                    }
                    for (int i = 0; i < editorPaneCount; ++i) {
                        IRenderableDisplay display = editorPanes[i]
                                .getRenderableDisplay();
                        /*
                         * TODO: This condition is currently needed because the
                         * NSHARP input handlers incorrectly retain references
                         * to VizDisplayPane instances. Should do this
                         * unconditionally when that is fixed.
                         */
                        if (isCompatibleEditor) {
                            editorPanes[i].setRenderableDisplay(null);
                        }
                        viewPanes[i].setRenderableDisplay(display);
                        if (!editorHiddenDisplays
                                .contains(editorPanes[i].getRenderableDisplay())
                                && !viewPanes[i].isVisible()) {
                            showPane(viewPanes[i]);
                        }
                    }
                } else {
                    for (int i = 0; i < editorPaneCount; ++i) {
                        IRenderableDisplay display = editorPanes[i]
                                .getRenderableDisplay();
                        boolean hide = editorHiddenDisplays.contains(display);
                        /*
                         * TODO: See note above for the isCompatibleEditor
                         * condition.
                         */
                        if (isCompatibleEditor) {
                            editorPanes[i].setRenderableDisplay(null);
                        }
                        if (i < viewPaneCount) {
                            viewPanes[i].setRenderableDisplay(display);
                            if (hide) {
                                hidePane(viewPanes[i]);
                            } else if (!viewPanes[i].isVisible()) {
                                showPane(viewPanes[i]);
                            }
                        } else {
                            IDisplayPane pane = addPane(display);
                            if (hide) {
                                hidePane(pane);
                            }
                        }
                    }
                }

                if (isCompatibleEditor) {

                    // swap loop properties
                    LoopProperties editorLoopProperties = theEditor
                            .getLoopProperties();
                    theEditor.setLoopProperties(loopProperties);
                    this.loopProperties = editorLoopProperties;

                    // load myRenderables to existing editor
                    if (theEditor instanceof IMultiPaneEditor) {
                        IMultiPaneEditor mpe = (IMultiPaneEditor) theEditor;
                        if (editorPaneCount > viewPaneCount) {
                            for (int i = editorPaneCount
                                    - 1; i >= viewPaneCount; --i) {
                                mpe.removePane(editorPanes[i]);
                            }
                            for (int i = 0; i < viewPaneCount; ++i) {
                                editorPanes[i]
                                        .setRenderableDisplay(myRenderables[i]);
                                if (!myHiddenDisplays.contains(myRenderables[i])
                                        && !editorPanes[i].isVisible()) {
                                    mpe.showPane(editorPanes[i]);
                                }
                            }
                        } else {
                            for (int i = 0; i < viewPaneCount; ++i) {
                                if (i < editorPaneCount) {
                                    editorPanes[i].setRenderableDisplay(
                                            myRenderables[i]);
                                    if (!myHiddenDisplays
                                            .contains(myRenderables[i])
                                            && !editorPanes[i].isVisible()) {
                                        mpe.showPane(editorPanes[i]);
                                    } else if (myHiddenDisplays
                                            .contains(myRenderables[i])
                                            && editorPanes[i].isVisible()) {
                                        mpe.hidePane(editorPanes[i]);
                                    }
                                } else {
                                    IDisplayPane pane = mpe
                                            .addPane(myRenderables[i]);
                                    if (myHiddenDisplays
                                            .contains(myRenderables[i])) {
                                        mpe.hidePane(pane);
                                    }
                                }
                            }
                        }
                    } else {
                        int min = Math.min(viewPaneCount, editorPaneCount);
                        for (int i = 0; i < min; ++i) {
                            editorPanes[i]
                                    .setRenderableDisplay(myRenderables[i]);
                        }
                    }
                } else {
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage().closeEditor(theEditor, false);
                    String editorId = DescriptorMap.getEditorId(myRenderables[0]
                            .getDescriptor().getClass().getName());
                    if (editorId == null) {
                        throw new VizException(
                                "No editor mapping for descriptor: "
                                        + getActiveDisplayPane()
                                                .getRenderableDisplay()
                                                .getDescriptor().getClass()
                                                .getName());
                    }
                    LoopProperties editorLoopProperties = theEditor
                            .getLoopProperties();
                    theEditor = UiUtil.createEditor(editorId, myRenderables);
                    // closed the editor above so have to set the name again
                    theEditor.setPartName(viewName);

                    theEditor.setLoopProperties(loopProperties);
                    this.loopProperties = editorLoopProperties;

                }

                // Must jump to latest frame!
                for (IDisplayPane pane : getDisplayPanes()) {
                    pane.getDescriptor().getFrameCoordinator().changeFrame(
                            FrameChangeOperation.LAST,
                            FrameChangeMode.TIME_AND_SPACE);
                }

                for (IDisplayPane pane : theEditor.getDisplayPanes()) {
                    pane.getDescriptor().getFrameCoordinator().changeFrame(
                            FrameChangeOperation.LAST,
                            FrameChangeMode.TIME_AND_SPACE);
                }

                for (IDisplayPane pane : theEditor.getDisplayPanes()) {
                    IDescriptor desc = pane.getDescriptor();
                    if (desc == null) {
                        continue;
                    }
                    if (desc.unlimitNumberOfFrames()) {
                        TimeMatchingJob.scheduleTimeMatch(desc);
                    }
                }

                for (IDisplayPane pane : getDisplayPanes()) {
                    IDescriptor desc = pane.getDescriptor();
                    if (desc == null) {
                        continue;
                    }
                    if (desc.limitNumberOfFrames(8)) {
                        TimeMatchingJob.scheduleTimeMatch(desc);
                    }
                }

                // Update the legend mode(s), if necessary
                /*
                 * Set the legend mode of our new main pane back to product if
                 * it has been set to the date/time mode.
                 */
                this.switchLegendMode(theEditor.getDisplayPanes(),
                        D2DLegendResource.LegendMode.SHORT_PRODUCT,
                        D2DLegendResource.LegendMode.PRODUCT);
                /*
                 * Set the legend mode of our new sideview pane to datetime if
                 * the mode is currently set to product.
                 */
                this.switchLegendMode(this.getDisplayPanes(),
                        D2DLegendResource.LegendMode.PRODUCT,
                        D2DLegendResource.LegendMode.SHORT_PRODUCT);

                for (InputPriority priority : SWAPPABLE_PRIORITIES) {
                    IInputHandler[] handlers = myHandlers.get(priority);
                    for (IInputHandler handler : handlers) {
                        theEditor.registerMouseHandler(handler, priority);
                    }
                }

                theEditor.getBackgroundColor().setColor(BGColorMode.EDITOR,
                        outgoingBackgroundColor);

                for (IDisplayPane pane : theEditor.getDisplayPanes()) {
                    pane.getRenderableDisplay().setSwapping(false);
                }
                for (IDisplayPane pane : getDisplayPanes()) {
                    pane.getRenderableDisplay().setSwapping(false);
                }

                for (IDisplayPane dpane : theEditor.getDisplayPanes()) {
                    for (ResourcePair rp : dpane.getDescriptor()
                            .getResourceList()) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc != null
                                && rsc.hasCapability(EditableCapability.class)
                                && rsc == editableRsc) {
                            EditableManager.makeEditable(rsc, true);
                            editableRsc = null;
                        }
                    }
                }

                for (IDisplayPane dpane : getDisplayPanes()) {
                    for (ResourcePair rp : dpane.getDescriptor()
                            .getResourceList()) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc != null
                                && rsc.hasCapability(EditableCapability.class)
                                && rsc.getCapability(EditableCapability.class)
                                        .isEditable()) {
                            EditableManager.makeEditable(rsc, false);
                            editableRsc = rsc;
                        }
                    }
                }

                refresh();

                theEditor.refresh();
                theEditor.setFocus();

                VizGlobalsManager.getCurrentInstance().updateUI(theEditor);

                HistoryList.getInstance().addBundle();
            } catch (Exception e) {
                statusHandler.handle(Priority.CRITICAL,
                        "Error during pane swap", e);
            }
        }
    }

    private void switchLegendMode(IDisplayPane[] displayPanes,
            LegendMode modeToChange, LegendMode changeModeTo) {
        for (IDisplayPane pane : displayPanes) {
            List<D2DLegendResource> decors = pane.getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(D2DLegendResource.class);
            for (D2DLegendResource rsc : decors) {
                if (rsc.getLegendMode() == modeToChange) {
                    rsc.setLegendMode(changeModeTo);
                }
            }
        }
    }

    @Override
    public Coordinate translateClick(double x, double y) {
        return paneManager.translateClick(x, y);
    }

    @Override
    public double[] translateInverseClick(Coordinate c) {
        return paneManager.translateInverseClick(c);
    }

    public IMultiPaneEditor getPaneManager() {
        return this.paneManager;
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return paneManager.addPane(renderableDisplay);
    }

    @Override
    public void clear() {
        paneManager.clear();
    }

    @Override
    public int displayedPaneCount() {
        return paneManager.displayedPaneCount();
    }

    @Override
    public int getNumberofPanes() {
        return paneManager.getNumberofPanes();
    }

    @Override
    public IDisplayPane getSelectedPane(String action) {
        return paneManager.getSelectedPane(action);
    }

    @Override
    public void hidePane(IDisplayPane pane) {
        paneManager.hidePane(pane);
    }

    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        return paneManager.isSelectedPane(action, pane);
    }

    @Override
    public void removePane(IDisplayPane pane) {
        paneManager.removePane(pane);
    }

    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        paneManager.setSelectedPane(action, pane);
    }

    @Override
    public void showPane(IDisplayPane pane) {
        paneManager.showPane(pane);
    }

    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        // no op
    }

    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {
        // no op
    }

    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        // no op
    }

    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        // no op
    }

    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        // no op
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        paneManager.registerMouseHandler(handler, priority);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        paneManager.registerMouseHandler(handler);
    }

    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        paneManager.unregisterMouseHandler(handler);
    }

    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        return paneManager.getSelectedPanes(action);
    }

    @Override
    public void setColor(BGColorMode mode, RGB newColor) {
        for (IDisplayPane pane : getDisplayPanes()) {
            IRenderableDisplay display = pane.getRenderableDisplay();
            if (display != null) {
                display.setBackgroundColor(newColor);
            }
        }
        refresh();
    }

    @Override
    public void setPartName(String partName) {
        super.setPartName(partName);
    }

}
