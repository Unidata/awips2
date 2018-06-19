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
package com.raytheon.uf.viz.d2d.ui.perspectives;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.actions.NewMapEditor;
import com.raytheon.uf.viz.core.maps.scales.MapScalesManager;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.sampling.actions.LatLonReadoutAction;
import com.raytheon.uf.viz.core.rsc.sampling.actions.SampleAction;
import com.raytheon.uf.viz.d2d.core.legend.ChangeLegendModeAction;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource;
import com.raytheon.uf.viz.d2d.core.legend.D2DLegendResource.LegendMode;
import com.raytheon.uf.viz.d2d.ui.actions.BlinkToggleAction;
import com.raytheon.uf.viz.d2d.ui.map.SideView;
import com.raytheon.uf.viz.d2d.ui.map.actions.AllPanelSampleAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.D2DUnloadAllProductsAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.FourPanelLayoutMenuAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.RotatePanelLayoutMenuAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.SetBackgroundColorAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.SinglePanelLayoutMenuAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.SkipFramesAction;
import com.raytheon.uf.viz.d2d.ui.map.actions.SkipFramesAction.SkipFrameMode;
import com.raytheon.uf.viz.d2d.ui.map.actions.SwapWithLargePaneAction;
import com.raytheon.viz.ui.actions.SelectPaneAction;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.LoopingAction;
import com.raytheon.viz.ui.cmenu.StepFrameMenuAction;
import com.raytheon.viz.ui.cmenu.UnloadAllGraphicsAction;
import com.raytheon.viz.ui.cmenu.ZoomMenuAction;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import com.raytheon.viz.ui.statusline.FrameCountDisplay;

/**
 * 
 * Manages the life cycle of the D2D Perspectives
 * 
 * A default perspective manager that can be shared amongh perspectives. Just
 * loads the procedure named default-procedure.xml in the localization path.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/27/2010              mschenke    Initial Creation.
 * Mar 21, 2013       1638 mschenke    Changed map scales not tied to d2d
 * Oct 10, 2013       2104 mschenke    Switched to use MapScalesManager
 * Jan 14, 2014       2594 bclement    added low memory notification
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class D2DPerspectiveManager extends AbstractCAVEPerspectiveManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DPerspectiveManager.class);

    // Prevent instantiating these constantly by static-izing them

    private static final SampleAction sampleAction = new SampleAction();

    private static final LatLonReadoutAction readoutAction = new LatLonReadoutAction();

    private static final D2DUnloadAllProductsAction unloadAllAction = new D2DUnloadAllProductsAction();

    private static final SwapWithLargePaneAction swapAction = new SwapWithLargePaneAction();

    private static final Separator sep = new Separator();

    private static Map<Object, AbstractRightClickAction> legendActions = new HashMap<Object, AbstractRightClickAction>();

    @Override
    public void open() {
        contextActivator = new D2DContextActivator(page);
        try {
            MapScalesManager.getInstance().loadEditorScales(perspectiveWindow);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading bundles to screen", e);
        }
    }

    @Override
    public AbstractEditor openNewEditor() {
        try {
            return new NewMapEditor().execute(null);
        } catch (ExecutionException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error opening new map editor", e);
        }
        return null;
    }

    @Override
    protected List<ContributionItem> getStatusLineItems() {
        List<ContributionItem> items = super.getStatusLineItems();
        items.add(new FrameCountDisplay(perspectiveWindow));
        return items;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.AbstractVizPerspective#addContextMenuItems(org.eclipse
     * .jface.action.IMenuManager,
     * com.raytheon.uf.viz.core.IDisplayPaneContainer,
     * com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager,
            IDisplayPaneContainer container, IDisplayPane pane) {
        boolean hasImages = false;
        boolean hasGraphics = false;
        boolean hasTimeFrames = false;

        D2DLegendResource ld = null;

        List<D2DLegendResource> lds = pane.getDescriptor().getResourceList()
                .getResourcesByTypeAsType(D2DLegendResource.class);
        if (!lds.isEmpty()) {
            ld = lds.get(0);
        }
        if (ld != null) {
            hasImages = ld.hasImages();
            hasGraphics = ld.hasGraphics();
        }
        boolean hasProducts = hasImages || hasGraphics;
        IDescriptor descriptor = container.getActiveDisplayPane()
                .getRenderableDisplay().getDescriptor();
        FramesInfo currentFrameInfo = descriptor.getFramesInfo();

        hasTimeFrames = currentFrameInfo.getFrameCount() > 0;
        if (container instanceof SideView == false) {
            LegendMode mode = null;
            if (ld != null) {
                mode = ld.getLegendMode();
                if (mode != null) {
                    switch (mode) {
                    case NONE: {
                        menuManager
                                .add(getLegendAction(LegendMode.PRODUCT, ld));
                        menuManager.add(getLegendAction(LegendMode.MAP, ld));
                        break;

                    }
                    case PRODUCT: {
                        menuManager.add(getLegendAction(LegendMode.HIDE, ld));
                        menuManager.add(getLegendAction(LegendMode.MAP, ld));
                        break;
                    }
                    case MAP: {
                        menuManager.add(getLegendAction(LegendMode.HIDE, ld));
                        menuManager
                                .add(getLegendAction(LegendMode.PRODUCT, ld));
                        break;
                    }
                    }
                }
                menuManager.add(sep);
            }
        }

        // need to do this only on a multipane with multiple panes.
        if (container instanceof IMultiPaneEditor) {
            IMultiPaneEditor editor = (IMultiPaneEditor) container;

            if (editor.getNumberofPanes() > 1
                    && editor.displayedPaneCount() > 1) {
                // Set up load to pane menu
                if (editor.getSelectedPane(IMultiPaneEditor.LOAD_ACTION) == null
                        || editor.isSelectedPane(IMultiPaneEditor.LOAD_ACTION,
                                pane) == false) {
                    SelectPaneAction selectPaneAction = new SelectPaneAction(
                            pane, IMultiPaneEditor.LOAD_ACTION);
                    selectPaneAction.setContainer(container);
                    selectPaneAction.setSelectedRsc(null);
                    menuManager.add(selectPaneAction);
                }
                if (editor.getSelectedPane(IMultiPaneEditor.LOAD_ACTION) != null) {
                    SelectPaneAction selectPaneAction = new SelectPaneAction(
                            null, IMultiPaneEditor.LOAD_ACTION);
                    selectPaneAction.setContainer(container);
                    selectPaneAction.setSelectedRsc(null);
                    menuManager.add(selectPaneAction);
                }

                // Set up control color of menu
                if (editor.getSelectedPane(IMultiPaneEditor.IMAGE_ACTION) == null
                        || editor.isSelectedPane(IMultiPaneEditor.IMAGE_ACTION,
                                pane) == false) {
                    for (ResourcePair rp : pane.getDescriptor()
                            .getResourceList()) {
                        AbstractVizResource<?, ?> rsc = rp.getResource();
                        if (rsc != null
                                && rsc.hasCapability(BlendableCapability.class)) {
                            ResourceList subList = rsc.getCapability(
                                    BlendableCapability.class)
                                    .getResourceList();
                            if (subList.size() > 0) {
                                rsc = subList.get(0).getResource();
                            }
                        }

                        if (rsc != null
                                && rsc.hasCapability(ImagingCapability.class)
                                && rsc.hasCapability(ColorMapCapability.class)) {
                            SelectPaneAction selectPaneAction = new SelectPaneAction(
                                    pane, IMultiPaneEditor.IMAGE_ACTION);
                            selectPaneAction.setContainer(container);
                            selectPaneAction.setSelectedRsc(null);
                            menuManager.add(selectPaneAction);
                            break;
                        }
                    }
                }
                if (editor.getSelectedPane(IMultiPaneEditor.IMAGE_ACTION) != null) {
                    SelectPaneAction selectPaneAction = new SelectPaneAction(
                            null, IMultiPaneEditor.IMAGE_ACTION);
                    selectPaneAction.setContainer(container);
                    selectPaneAction.setSelectedRsc(null);
                    menuManager.add(selectPaneAction);
                }

                SinglePanelLayoutMenuAction singlePanelLayoutMenuAction = new SinglePanelLayoutMenuAction(
                        pane);
                singlePanelLayoutMenuAction.setContainer(container);
                menuManager.add(singlePanelLayoutMenuAction);

                RotatePanelLayoutMenuAction rotatePanelLayoutMenuAction = new RotatePanelLayoutMenuAction();
                rotatePanelLayoutMenuAction.setPaneInFocus(pane);
                rotatePanelLayoutMenuAction.setContainer(container);
                menuManager.add(rotatePanelLayoutMenuAction);
            } else {
                if (editor.getNumberofPanes() > 1) {
                    AllPanelSampleAction sample = new AllPanelSampleAction();
                    sample.setContainer(container);
                    menuManager.add(sample);

                    RotatePanelLayoutMenuAction rotatePanelLayoutMenuAction = new RotatePanelLayoutMenuAction();
                    rotatePanelLayoutMenuAction.setPaneInFocus(pane);
                    rotatePanelLayoutMenuAction.setContainer(container);
                    menuManager.add(rotatePanelLayoutMenuAction);

                    FourPanelLayoutMenuAction fourPanelLayoutMenuAction = new FourPanelLayoutMenuAction();
                    fourPanelLayoutMenuAction.setContainer(container);
                    menuManager.add(fourPanelLayoutMenuAction);
                    SelectPaneAction selectPaneAction = new SelectPaneAction(
                            pane, IMultiPaneEditor.LOAD_ACTION);
                    selectPaneAction.setContainer(container);
                    selectPaneAction.setSelectedRsc(null);
                    menuManager.add(selectPaneAction);
                } else {
                    FourPanelLayoutMenuAction fourPanelLayoutMenuAction = new FourPanelLayoutMenuAction();
                    fourPanelLayoutMenuAction.setContainer(container);
                    menuManager.add(fourPanelLayoutMenuAction);
                }
            }
            menuManager.add(sep);
        }

        if (hasProducts && hasTimeFrames) {
            AbstractRightClickAction action;
            int validFrameCount = 0;
            for (DataTime dt : currentFrameInfo.getFrameTimes()) {
                if (dt.isVisible()) {
                    ++validFrameCount;
                }
            }
            if (validFrameCount > 1) {
                action = getLegendAction(SkipFrameMode.THIS_FRAME);
                action.setContainer(container);
                menuManager.add(action);
            }
            int idx = currentFrameInfo.getFrameIndex();
            if (idx > 0) {
                action = getLegendAction(SkipFrameMode.PREVIOUS_FRAMES);
                action.setContainer(container);
                menuManager.add(action);
            }
            if (idx > -1 && idx < descriptor.getNumberOfFrames() - 1) {
                action = getLegendAction(SkipFrameMode.SUBSEQ_FRAMES);
                action.setContainer(container);
                menuManager.add(action);
            }
            if (validFrameCount < descriptor.getNumberOfFrames()) {
                action = getLegendAction(SkipFrameMode.RESTORE);
                action.setContainer(container);
                menuManager.add(action);
            }
        }

        menuManager.add(sep);

        if (container instanceof SideView) {
            swapAction.setContainer(container);
            swapAction.setSelectedRsc(null);
            menuManager.add(swapAction);
            if (container.getDisplayPanes().length > 1) {
                IMultiPaneEditor editor = ((SideView) container)
                        .getPaneManager();
                RotatePanelLayoutMenuAction rotatePanelLayoutMenuAction = new RotatePanelLayoutMenuAction();
                rotatePanelLayoutMenuAction.setPaneInFocus(pane);
                rotatePanelLayoutMenuAction.setContainer(editor);
                menuManager.add(rotatePanelLayoutMenuAction);

                if (editor.displayedPaneCount() == 1) {
                    FourPanelLayoutMenuAction fourPanelLayoutMenuAction = new FourPanelLayoutMenuAction();
                    fourPanelLayoutMenuAction.setContainer(editor);
                    menuManager.add(fourPanelLayoutMenuAction);
                }
            }

            if (hasTimeFrames) {
                menuManager.add(new StepFrameMenuAction(container));
                menuManager.add(new LoopingAction(container));
            }
        }

        menuManager.add(new ZoomMenuAction(container));

        SetBackgroundColorAction setBackGroundColor = new SetBackgroundColorAction();
        if (container instanceof SideView) {
            setBackGroundColor.setMode(null);
        } else {
            setBackGroundColor.setMode(BGColorMode.EDITOR);
        }
        setBackGroundColor.setContainer(container);
        menuManager.add(setBackGroundColor);
        menuManager.add(new Separator());

        if (hasProducts) {
            sampleAction.setContainer(container);
            sampleAction.setSelectedRsc(null);
            menuManager.add(sampleAction);
        }

        if (descriptor instanceof MapDescriptor) {
            readoutAction.setContainer(container);
            readoutAction.setSelectedRsc(null);
            menuManager.add(readoutAction);
        }

        if (pane.getRenderableDisplay().isBlinking()) {
            BlinkToggleAction bta = new BlinkToggleAction(container);
            menuManager.add(bta);
        }

        if (hasProducts && hasImages) {
            menuManager.add(sep);
            unloadAllAction.setContainer(container);
            menuManager.add(unloadAllAction);
        }

        if (hasGraphics) {
            menuManager.add(new UnloadAllGraphicsAction(container));
        }
    }

    private AbstractRightClickAction getLegendAction(SkipFrameMode mode) {
        AbstractRightClickAction action = legendActions.get(mode);
        if (action == null) {
            action = new SkipFramesAction(mode);
            legendActions.put(mode, action);
        }
        return action;
    }

    /**
     * @param none
     * @return
     */
    private ChangeLegendModeAction getLegendAction(LegendMode mode,
            D2DLegendResource rsc) {
        return new ChangeLegendModeAction(mode, rsc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#
     * getLowMemoryMessage(long)
     */
    @Override
    protected String getLowMemoryMessage(long availMemory) {
        return super.getLowMemoryMessage(availMemory)
                + "\n\nConsider closing tabs, clearing panes, or reducing the frame count to free up memory.";
    }

}
