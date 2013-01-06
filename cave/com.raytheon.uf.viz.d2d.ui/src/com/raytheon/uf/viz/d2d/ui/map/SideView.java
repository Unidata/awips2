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

import java.io.File;
import java.util.HashSet;
import java.util.List;
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

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
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
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.uf.viz.d2d.core.map.MapScales;
import com.raytheon.uf.viz.d2d.core.map.MapScales.MapScale;
import com.raytheon.uf.viz.d2d.core.map.MapScales.PartId;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.HistoryList;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.panes.PaneManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The side view for "docking" maps
 * 
 * <pre>
 * 
 *      SOFTWARE HISTORY
 *     
 *      Date       	    Ticket#		Engineer	Description
 *      ------------	----------	-----------	--------------------------
 *      7/1/06                      chammack    Initial Creation.
 *      Dec 19, 2007                njensen     Support for different tab types.
 *      Jul  8, 2009    #830        bgonzale    use pane manager instead of managing own panes.
 *      Oct  22, 2009   #3348       bsteffen    limit number of frames in sidepane rather than reseting
 *      Jul 1, 2010     #6146       bkowal      Updates the legend mode so that smaller panes will 
 *                                              now include a legend
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class SideView extends ViewPart implements IMultiPaneEditor,
        IBackgroundColorChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SideView.class);

    public static final String ID = SideView.class.getName();

    private PaneManager paneManager;

    private LoopProperties loopProperties;

    private AbstractVizResource<?, ?> editableResource = null;

    private Bundle bundleToLoad = null;

    private BackgroundColor backgroundColor;

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

        backgroundColor = BackgroundColor.getInstance(site.getPage()
                .getPerspective());
        backgroundColor.addListener(BGColorMode.GLOBAL, this);

        String myId = site.getId() + UiUtil.SECONDARY_ID_SEPARATOR
                + site.getSecondaryId();

        for (MapScale scale : MapScales.getInstance().getScales()) {
            boolean myScale = false;
            for (PartId partId : scale.getPartIds()) {
                if (partId.isView() && myId.equals(partId.getId())) {
                    myScale = true;
                    break;
                }
            }
            if (myScale) {
                File file = scale.getFile();
                try {
                    Bundle b = (Bundle) SerializationUtil
                            .jaxbUnmarshalFromXmlFile(file);
                    b.setView(myId);
                    bundleToLoad = b;
                } catch (SerializationException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error deserializing bundle: "
                                    + file.getAbsolutePath(), e);
                }
                break;
            }
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        if (backgroundColor != null) {
            backgroundColor.removeListener(BGColorMode.GLOBAL, this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
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
            try {
                LoadSerializedXml.loadTo(this, bundleToLoad);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading bundle view", e);
            }
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

    IInputHandler swappingListener = new InputAdapter() {

        int buttonDown = 0;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            buttonDown = mouseButton;
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (buttonDown == 3 && buttonDown == mouseButton) { // right click
                buttonDown = -1;
                swapPanes();
                return true;
            }
            return false;
        }
    };

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.IDisplayPaneContainer#getDisplayPanes()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        if (this.paneManager == null) {
            return new IDisplayPane[0];
        } else {
            return this.paneManager.getDisplayPanes();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.IDisplayPaneContainer#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return this.loopProperties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.IDisplayPaneContainer#setLoopProperties(com.raytheon
     * .viz.core.datastructure.LoopProperties)
     */
    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        this.loopProperties = loopProperties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IDisplayPaneContainer#getActiveDisplayPane()
     */
    public IDisplayPane getActiveDisplayPane() {
        return paneManager.getActiveDisplayPane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IDisplayPaneContainer#refresh()
     */
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
            Set<IRenderableDisplay> myHiddenDisplays = new HashSet<IRenderableDisplay>();
            Set<IRenderableDisplay> editorHiddenDisplays = new HashSet<IRenderableDisplay>();

            AbstractEditor theEditor = (AbstractEditor) editor;

            // First thing to do, swap input handlers
            // Get editor resource handlers and unregister on editor
            IInputHandler[] editorResourceHandlers = theEditor
                    .getMouseManager().getHandlersForPriority(
                            InputPriority.RESOURCE);
            IInputHandler[] editorSystemRscHandlers = theEditor
                    .getMouseManager().getHandlersForPriority(
                            InputPriority.SYSTEM_RESOURCE);
            for (IInputHandler handler : editorResourceHandlers) {
                theEditor.getMouseManager().unregisterMouseHandler(handler);
            }
            for (IInputHandler handler : editorSystemRscHandlers) {
                theEditor.getMouseManager().unregisterMouseHandler(handler);
            }

            // Store and unregister input handlers on ourself
            IInputHandler[] myResourceHandlers = paneManager.getMouseManager()
                    .getHandlersForPriority(InputPriority.RESOURCE);
            IInputHandler[] mySystemRscHandlers = paneManager.getMouseManager()
                    .getHandlersForPriority(InputPriority.SYSTEM_RESOURCE);
            for (IInputHandler handler : myResourceHandlers) {
                unregisterMouseHandler(handler);
            }
            for (IInputHandler handler : mySystemRscHandlers) {
                unregisterMouseHandler(handler);
            }

            // Register editor handlers on ourself
            for (IInputHandler handler : editorResourceHandlers) {
                registerMouseHandler(handler, InputPriority.RESOURCE);
            }
            for (IInputHandler handler : editorSystemRscHandlers) {
                registerMouseHandler(handler, InputPriority.SYSTEM_RESOURCE);
            }

            IDisplayPane[] editorPanes = theEditor.getDisplayPanes();
            AbstractVizResource<?, ?> editableResource = null;
            // Set swapping so we don't get disposed, and find an editable
            // resource if there is one
            for (IDisplayPane dPane : editorPanes) {
                dPane.getRenderableDisplay().setSwapping(true);
                if (dPane.isVisible() == false) {
                    editorHiddenDisplays.add(dPane.getRenderableDisplay());
                }

                if (editableResource == null) {
                    for (ResourcePair rp : dPane.getDescriptor()
                            .getResourceList()) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc != null
                                && rsc.hasCapability(EditableCapability.class)) {
                            if (rsc.getCapability(EditableCapability.class)
                                    .isEditable()) {
                                editableResource = rsc;
                                break;
                            }
                        }
                    }
                }
            }

            IRenderableDisplay[] myRenderables = paneManager
                    .getRenderableDisplays();
            IDisplayPane[] viewPanes = getDisplayPanes();
            for (IDisplayPane myPane : viewPanes) {
                myPane.getRenderableDisplay().setSwapping(true);
                if (myPane.isVisible() == false) {
                    myHiddenDisplays.add(myPane.getRenderableDisplay());
                }
            }

            int editorPaneCount = editorPanes.length;
            int viewPaneCount = viewPanes.length;

            try {
                // I have my renderables saved off, load editor renderables
                // to me first
                if (viewPaneCount > editorPaneCount) {
                    // view has too many panes, remove and set editor's displays
                    for (int i = viewPaneCount - 1; i >= editorPaneCount; --i) {
                        removePane(viewPanes[i]);
                    }
                    for (int i = 0; i < editorPaneCount; ++i) {
                        viewPanes[i].setRenderableDisplay(editorPanes[i]
                                .getRenderableDisplay());
                        if (editorHiddenDisplays.contains(editorPanes[i]
                                .getRenderableDisplay()) == false
                                && viewPanes[i].isVisible() == false) {
                            showPane(viewPanes[i]);
                        }
                    }
                } else {
                    for (int i = 0; i < editorPaneCount; ++i) {
                        IRenderableDisplay display = editorPanes[i]
                                .getRenderableDisplay();
                        boolean hide = editorHiddenDisplays.contains(display);
                        if (i < viewPaneCount) {
                            viewPanes[i].setRenderableDisplay(display);
                            if (hide) {
                                hidePane(viewPanes[i]);
                            } else if (viewPanes[i].isVisible() == false) {
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

                if (theEditor
                        .getSite()
                        .getId()
                        .equals(DescriptorMap.getEditorId(myRenderables[0]
                                .getDescriptor().getClass().getName()))) {
                    LoopProperties editorLoopProperties = theEditor
                            .getLoopProperties();
                    theEditor.setLoopProperties(loopProperties);
                    this.loopProperties = editorLoopProperties;

                    // load myRenderables to existing editor
                    if (theEditor instanceof IMultiPaneEditor) {
                        IMultiPaneEditor mpe = (IMultiPaneEditor) theEditor;
                        if (editorPaneCount > viewPaneCount) {
                            for (int i = editorPaneCount - 1; i >= viewPaneCount; --i) {
                                mpe.removePane(editorPanes[i]);
                            }
                            for (int i = 0; i < viewPaneCount; ++i) {
                                editorPanes[i]
                                        .setRenderableDisplay(myRenderables[i]);
                                if (myHiddenDisplays.contains(myRenderables[i]) == false
                                        && editorPanes[i].isVisible() == false) {
                                    mpe.showPane(editorPanes[i]);
                                }
                            }
                        } else {
                            for (int i = 0; i < viewPaneCount; ++i) {
                                if (i < editorPaneCount) {
                                    editorPanes[i]
                                            .setRenderableDisplay(myRenderables[i]);
                                    if (myHiddenDisplays
                                            .contains(myRenderables[i]) == false
                                            && editorPanes[i].isVisible() == false) {
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
                    String editorId = DescriptorMap
                            .getEditorId(myRenderables[0].getDescriptor()
                                    .getClass().getName());
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

                    theEditor.setLoopProperties(loopProperties);
                    this.loopProperties = editorLoopProperties;

                }

                // Must jump to latest frame!
                for (IDisplayPane pane : getDisplayPanes()) {
                    pane.getDescriptor()
                            .getFrameCoordinator()
                            .changeFrame(FrameChangeOperation.LAST,
                                    FrameChangeMode.TIME_AND_SPACE);
                }

                for (IDisplayPane pane : theEditor.getDisplayPanes()) {
                    pane.getDescriptor()
                            .getFrameCoordinator()
                            .changeFrame(FrameChangeOperation.LAST,
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

                for (IInputHandler handler : myResourceHandlers) {
                    theEditor.registerMouseHandler(handler,
                            InputPriority.RESOURCE);
                }
                for (IInputHandler handler : mySystemRscHandlers) {
                    theEditor.registerMouseHandler(handler,
                            InputPriority.SYSTEM_RESOURCE);
                }

                // Set up editableness
                if (editableResource != null) {
                    EditableManager.makeEditable(editableResource, false);
                }
                if (this.editableResource != null) {
                    EditableManager.makeEditable(this.editableResource, true);
                }
                this.editableResource = editableResource;

                theEditor.getBackgroundColor().setColor(BGColorMode.EDITOR,
                        myRenderables[0].getBackgroundColor());

                for (IDisplayPane pane : theEditor.getDisplayPanes()) {
                    pane.getRenderableDisplay().setSwapping(false);
                }
                for (IDisplayPane pane : getDisplayPanes()) {
                    pane.getRenderableDisplay().setSwapping(false);
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IDisplayPaneContainer#translateClick(double,
     * double)
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        return paneManager.translateClick(x, y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateInverseClick(
     * com.vividsolutions.jts.geom.Coordinate)
     */
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

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.IDisplayPaneContainer#
     * addRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.IDisplayPaneContainer#
     * notifyRenderableDisplayChangedListeners
     * (com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay)
     */
    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {

    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.IDisplayPaneContainer#
     * removeRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#addSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPaneChangedListener)
     */
    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#removeSelectedPaneChangedListener
     * (com.raytheon.viz.ui.editor.ISelectedPaneChangedListener)
     */
    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
    }

    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        paneManager.registerMouseHandler(handler, priority);
    }

    public void registerMouseHandler(IInputHandler handler) {
        paneManager.registerMouseHandler(handler);
    }

    public void unregisterMouseHandler(IInputHandler handler) {
        paneManager.unregisterMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#getSelectedPanes(java.lang
     * .String)
     */
    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        return paneManager.getSelectedPanes(action);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.color.IBackgroundColorChangedListener#setColor(com
     * .raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode,
     * org.eclipse.swt.graphics.RGB)
     */
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
}
