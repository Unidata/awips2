/*
 * PgenSession
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil.PgenMode;
import gov.noaa.nws.ncep.ui.pgen.controls.PgenCommandManager;
import gov.noaa.nws.ncep.ui.pgen.filter.CategoryFilter;
import gov.noaa.nws.ncep.ui.pgen.palette.PgenPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResourceData;
import gov.noaa.nws.ncep.ui.pgen.tools.AbstractPgenTool;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.WorkbenchPage;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

/**
 * This singleton is intended to couple a PGEN Palette with a PGgenResource, so
 * that a palette can be updated and used to modify a specific PgenResource
 * 
 * @author sgilbert
 * 
 */

/**
 * Implements a drawing layer for PGEN products.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/14		R5413		B. Yin   	Added IPartListener2 and IRenderableDisplayChangedListener
 * to make the swapping in D2D work
 * 
 */

public class PgenSession implements IPartListener2,
        IRenderableDisplayChangedListener {

    /*
     * The singleton instance
     */
    private static PgenSession instance = null;

    /*
     * the current PGEN resource
     */
    private PgenResource pgenResource = null;

    /*
     * the current PGEN palette
     */
    private PgenPaletteWindow palette = null;

    private List<AbstractEditor> editors = new ArrayList<AbstractEditor>();

    /*
     * Active PGEN tool
     */
    private AbstractPgenTool pgenTool = null;

    /*
     * Hide default constructor
     */
    private PgenSession() {

    }

    /**
     * Static method to get THE PgenSession instance
     * 
     * @return PgenSession reference
     */
    public static synchronized PgenSession getInstance() {

        if (instance == null)
            instance = new PgenSession();
        return instance;
    }

    /**
     * Sets a PgenResource for the current session
     * 
     * @param rsc
     *            a Pgen Resource
     */
    public void setResource(PgenResource rsc) {

        /*
         * Remove the current PGEN Resource from the Session
         */
        removeResource();

        // set new PGEN resource
        pgenResource = rsc;
        // add the palette's stack listener to new resource's command Manager
        if (pgenResource != null && palette != null) {
            pgenResource.getCommandMgr().addStackListener(palette);
        }

    }

    /**
     * Removes the current PGEN resource from the Session
     */
    public void removeResource() {
        if (pgenResource != null) {
            // Remove the Palette's stack listener from the Resource's
            // CommandManager
            pgenResource.getCommandMgr().removeStackListener(palette);
        }
        pgenResource = null;

        /*
         * disable the palette's Undo and redo buttons.
         */
        if (palette != null)
            palette.disableUndoRedo();
    }

    /**
     * Gets an appropriate PGEN Resource. Returns the current Pgen Resource
     * registered with this PGEN Session if there is one. If not, it will look
     * for an existing resource in the current editor. If one is not found, a
     * new PgenResource will be created.
     * 
     * @return the rsc
     */
    public PgenResource getPgenResource() {

        if (pgenResource == null) {
            // PgenResource rsc =
            // PgenUtil.findPgenResource(NmapUiUtils.getActiveNatlCntrsEditor());
            PgenResource rsc = PgenUtil.findPgenResource(PgenUtil
                    .getActiveEditor());
            if (rsc != null) {
                pgenResource = rsc;
            } else {
                pgenResource = PgenUtil.createNewResource();
            }
        }

        return pgenResource;
    }

    /**
     * Get the PGEN Resource currently registered with the session
     * 
     * @return
     */
    public PgenResource getCurrentResource() {
        return pgenResource;
    }

    /**
     * Gets the Resource's Command Manager
     * 
     * @return the commandMgr
     */
    public PgenCommandManager getCommandManager() {
        return pgenResource.getCommandMgr();
    }

    /**
     * Register the given palette with the Session
     * 
     * @param pal
     */
    public void setPalette(PgenPaletteWindow pal) {
        palette = pal;
        // Register this palette's stack listener with the CommandManager, if
        // able
        if (pgenResource != null) {
            pgenResource.getCommandMgr().addStackListener(palette);
        }
    }

    /**
     * Remove the current palette from this Session
     */
    public void removePalette() {
        // Remove this palette's stack listener from the CommandManager, if able
        if (pgenResource != null)
            pgenResource.getCommandMgr().removeStackListener(palette);
        palette = null;
    }

    /**
     * Clear and disable undo/redos.
     */
    public void disableUndoRedo() {

        if (pgenResource != null)
            getCommandManager().clearStacks();

        if (palette != null) {
            palette.disableUndoRedo();
        }

    }

    public PgenResourceData getPgenResourceData() {
        if (pgenResource != null) {
            return pgenResource.getResourceData();
        } else {
            return null;
        }
    }

    /**
     * Return the palette window
     */
    public PgenPaletteWindow getPgenPalette() {
        return palette;
    }

    public void addEditor(AbstractEditor editor) {
        editors.add(editor);
    }

    public List<AbstractEditor> getEditors() {
        return editors;
    }

    /*
     * Remove PGEN handler when swapping to side view. Also open PGEN palette if
     * there is a PGEN resource when swapping to main editor.
     * 
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IRenderableDisplayChangedListener#
     * renderableDisplayChanged(com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @SuppressWarnings("restriction")
    @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {

        if (type == DisplayChangeType.ADD
                && newRenderableDisplay.getContainer() instanceof VizMapEditor) {

            VizMapEditor editorChanged = (VizMapEditor) newRenderableDisplay
                    .getContainer();

            if (PgenUtil.getPgenMode() == PgenMode.SINGLE) { // for D2d
                                                             // swapping, single
                                                             // pane mode
                if (pgenResource != null) {
                    pgenResource.removeGhostLine();
                    pgenResource.removeSelected();
                }

                PgenUtil.setSelectingMode();
            }

            else { // for D2d swapping, multi-pane mode

                // clean up current pgen resource
                if (pgenResource != null) {
                    pgenResource.closeDialogs();
                    pgenResource.deactivatePgenTools();
                    pgenResource.getCommandMgr().removeStackListener(palette);
                }

                if (palette != null) {
                    if (PgenUtil.findPgenResource(editorChanged) == null) {
                        // editor does not have PGEN, close the palette

                        PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                                .getActivePage().hideView(palette);
                        palette = null;
                    } else {
                        // editor has PGEN resource, reset to selecting mode
                        pgenResource = PgenUtil.findPgenResource(editorChanged);
                        pgenResource.setCatFilter(new CategoryFilter("Any"));
                        palette.setCurrentCategory("Any");
                        PgenUtil.setSelectingMode();
                    }
                } else {
                    // palette is closed
                    if (PgenUtil.findPgenResource(editorChanged) != null) {
                        // editor has PGEN, open the palette
                        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getActivePage();

                        IViewPart vpart = wpage.findView(PgenUtil.VIEW_ID);

                        try {

                            if (vpart == null) {

                                vpart = wpage.showView(PgenUtil.VIEW_ID);
                                IViewReference pgenViewRef = wpage
                                        .findViewReference(PgenUtil.VIEW_ID);
                                if (pgenViewRef != null
                                        && wpage instanceof WorkbenchPage) {
                                    ((WorkbenchPage) wpage)
                                            .detachView(pgenViewRef);
                                }
                            } else {

                                if (!wpage.isPartVisible(vpart)) {
                                    vpart = wpage.showView(PgenUtil.VIEW_ID);
                                    IViewReference pgenViewRef = wpage
                                            .findViewReference(PgenUtil.VIEW_ID);
                                    if (pgenViewRef != null
                                            && wpage instanceof WorkbenchPage) {
                                        ((WorkbenchPage) wpage)
                                                .detachView(pgenViewRef);
                                    }
                                }
                            }
                            this.pgenResource = PgenUtil
                                    .findPgenResource(editorChanged);
                            this.pgenResource.setCatFilter(new CategoryFilter(
                                    "Any"));
                            this.palette.setCurrentCategory("Any");
                            PgenUtil.setSelectingMode();
                        } catch (Exception e) {
                            e.printStackTrace();
                        }

                    }
                }
            }
        } else if (type == DisplayChangeType.REMOVE
                && !(newRenderableDisplay.getContainer() instanceof AbstractEditor)) {
            // remove to side view
            // unregister pgen handlers

            if (newRenderableDisplay.getContainer() instanceof IMultiPaneEditor) {
                IMultiPaneEditor sideView = (IMultiPaneEditor) newRenderableDisplay
                        .getContainer();
                if (this.getPgenTool() != null) {
                    sideView.unregisterMouseHandler(this.getPgenTool()
                            .getMouseHandler());
                }
            }
        }
    }

    @Override
    public void partActivated(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partBroughtToTop(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partClosed(IWorkbenchPartReference partRef) {
        IWorkbenchPart part = partRef.getPart(false);
        if (part instanceof VizMapEditor) { // for D2D
            if (PgenUtil.findPgenResource((VizMapEditor) part) != null) {
                ((VizMapEditor) part)
                        .removeRenderableDisplayChangedListener(this);
            }
        }
    }

    @Override
    public void partDeactivated(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partOpened(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partHidden(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partVisible(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partInputChanged(IWorkbenchPartReference partRef) {
        // TODO Auto-generated method stub

    }

    public AbstractPgenTool getPgenTool() {
        return pgenTool;
    }

    public void setPgenTool(AbstractPgenTool pgenTool) {
        this.pgenTool = pgenTool;
    }

}
