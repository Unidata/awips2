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
package com.raytheon.viz.ui.editor;

import java.awt.image.BufferedImage;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISaveablePart2;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.perspectives.AbstractCAVEPerspectiveManager;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provides the basis for editors
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------	----------- --------------------------
 * Oct 10, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractEditor extends EditorPart implements
        IDisplayPaneContainer, IBackgroundColorChangedListener, ISaveablePart2 {

    /** The set of those listening for IRenderableDisplay changes */
    private Set<IRenderableDisplayChangedListener> renderableDisplayListeners;

    /** The editor input for the editor */
    protected EditorInput editorInput;

    /** The renderable displays to load when constructed */
    protected IRenderableDisplay[] displaysToLoad;

    /**
     * If not null will prevent user from closing the editor and display this
     * Message
     */
    private String closeMessage = null;

    protected BackgroundColor backgroundColor;

    /**
     * Constructor
     */
    public AbstractEditor() {
        renderableDisplayListeners = new HashSet<IRenderableDisplayChangedListener>();
    }

    private IRenderableDisplay[] getRenderableDisplays() {
        IRenderableDisplay[] displays = new IRenderableDisplay[getDisplayPanes().length];
        int i = 0;
        for (IDisplayPane pane : getDisplayPanes()) {
            displays[i++] = pane.getRenderableDisplay();
        }
        return displays;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getDisplayPanes()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        return editorInput.getPaneManager().getDisplayPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#refresh()
     */
    @Override
    public void refresh() {
        editorInput.getPaneManager().refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return editorInput.getPaneManager().getActiveDisplayPane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#translateClick(double,
     * double)
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        return editorInput.getPaneManager().translateClick(x, y);
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
        return editorInput.getPaneManager().translateInverseClick(c);
    }

    /**
     * Validates the editor input on init, default implementation checks to make
     * sure renderable displays are not null
     * 
     * @param input
     *            the input for the editor
     * @throws PartInitException
     *             on unexpected or invalid input
     */
    protected void validateEditorInput(EditorInput input)
            throws PartInitException {
        boolean valid = false;
        if (input.getRenderableDisplays() != null) {
            valid = true;
            for (IRenderableDisplay display : input.getRenderableDisplays()) {
                if (display == null) {
                    valid = false;
                }
            }
        }

        if (!valid) {
            throw new PartInitException(
                    "Renderable displays for input must not be null");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        if (backgroundColor != null) {
            backgroundColor.removeListener(BGColorMode.EDITOR, this);
        }
        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite,
     * org.eclipse.ui.IEditorInput)
     */
    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        if (input != null) {
            if (!(input instanceof EditorInput)) {
                throw new PartInitException("Input is of wrong type");
            }
        }

        backgroundColor = BackgroundColor.getInstance(site.getPage()
                .getPerspective());
        backgroundColor.addListener(BGColorMode.EDITOR, this);

        editorInput = (EditorInput) input;
        validateEditorInput(editorInput);
        displaysToLoad = editorInput.getRenderableDisplays();

        setSite(site);
        setInput(input);

        for (IRenderableDisplay display : displaysToLoad) {
            if (display != null) {
                initDisplay(display);
            }
        }

        PaneManager paneManager = editorInput.getPaneManager();
        if (paneManager == null) {
            editorInput.setPaneManager(getNewPaneManager());
        }

        addCustomHandlers(getMouseManager());
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
        editorInput.getPaneManager().initializeComponents(this, parent);
        for (IRenderableDisplay display : displaysToLoad) {
            addPane(display);
        }

        contributePerspectiveActions();
    }

    /**
     * Contribute perspective specific actions
     * 
     * This should occur on startup and also when the perspective changes
     */
    protected void contributePerspectiveActions() {
        // Find the site of this container and it's
        // enclosing window
        IWorkbenchWindow window = getSite().getWorkbenchWindow();

        VizPerspectiveListener listener = VizPerspectiveListener
                .getInstance(window);
        if (listener != null) {
            AbstractVizPerspectiveManager manager = listener
                    .getActivePerspectiveManager();
            if (manager instanceof AbstractCAVEPerspectiveManager) {
                IInputHandler[] handlers = ((AbstractCAVEPerspectiveManager) manager)
                        .getPerspectiveInputHandlers(this);
                getMouseManager().firePerspectiveChanged(handlers);
            }
        }
    }

    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        return editorInput.getPaneManager().addPane(renderableDisplay);
    }

    /**
     * @param display
     */
    protected void initDisplay(IRenderableDisplay display) {
        display.setBackgroundColor(backgroundColor.getColor(BGColorMode.EDITOR));
        display.getDescriptor().getResourceList()
                .instantiateResources(display.getDescriptor(), true);
    }

    /**
     * Get the pane manager to use for this editor
     * 
     * @return
     */
    protected abstract PaneManager getNewPaneManager();

    /**
     * Add any custom mouse handlers in this function
     * 
     * @param manager
     */
    protected void addCustomHandlers(InputManager manager) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
     */
    @Override
    protected void setInput(IEditorInput input) {
        super.setInputWithNotify(input);
    }

    @Override
    public IEditorInput getEditorInput() {
        editorInput.setRenderableDisplays(getRenderableDisplays());
        return editorInput;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return editorInput.getLoopProperties();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#setLoopProperties(com.
     * raytheon.uf.viz.core.datastructure.LoopProperties)
     */
    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        editorInput.setLoopProperties(loopProperties);
    }

    /**
     * Set the title of the tab
     */
    public void setTabTitle(String title) {
        this.setPartName(title);
    }

    public InputManager getMouseManager() {
        return editorInput.getPaneManager().getMouseManager();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler,
     * com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        editorInput.getPaneManager().registerMouseHandler(handler, priority);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#registerMouseHandler(com
     * .raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        editorInput.getPaneManager().registerMouseHandler(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IDisplayPaneContainer#unregisterMouseHandler
     * (com.raytheon.uf.viz.core.rsc.IInputHandler)
     */
    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        editorInput.getPaneManager().unregisterMouseHandler(handler);
    }

    public BufferedImage screenshot() {
        return editorInput.getPaneManager().screenshot();
    }

    public String getDefaultTool() {
        return "com.raytheon.viz.ui.tools.nav.PanTool";
    }

    public void setColor(BGColorMode mode, RGB newColor) {
        setColor(getDisplayPanes(), newColor);
    }

    protected void setColor(IDisplayPane[] panes, RGB newColor) {
        for (IDisplayPane pane : getDisplayPanes()) {
            IRenderableDisplay disp = pane.getRenderableDisplay();
            if (disp != null) {
                disp.setBackgroundColor(newColor);
            }
        }
        this.refresh();
    }

    /**
     * Prevent users from closing an editor, if the user attempts to close the
     * editor the reason will be displayed. Use with caution and please
     * enableClose as soon as possible
     * 
     * @param reason
     *            the message to display to the user when they attempt to close
     *            the editor
     */
    public void disableClose(String reason) {
        closeMessage = reason;
        firePropertyChange(ISaveablePart2.PROP_DIRTY);
    }

    public boolean isCloseable() {
        return closeMessage == null;
    }

    @Override
    public int promptToSaveOnClose() {
        if (PlatformUI.getWorkbench().isClosing()) {
            return ISaveablePart2.NO;
        }

        Shell shell = getSite().getShell();

        if (!isCloseable()) {
            // Let the user know why we refuse to close the editor
            MessageDialog.openError(shell, "Closing Disabled", closeMessage);
            // Cancel the clsoe
            return ISaveablePart2.CANCEL;
        } else {
            boolean close = MessageDialog.openQuestion(shell, "Close Editor?",
                    "Are you sure you want to close this editor?");
            return close ? ISaveablePart2.NO : ISaveablePart2.CANCEL;
        }
    }

    @Override
    public boolean isDirty() {
        if (!isCloseable()) {
            return true;
        } else {
            for (IDisplayPane pane : getDisplayPanes()) {
                IRenderableDisplay display = pane.getRenderableDisplay();
                if (display != null) {
                    if (display.isSwapping()) {
                        // never prompt on a swap
                        return false;
                    }
                    for (ResourcePair rp : display.getDescriptor()
                            .getResourceList()) {
                        ResourceProperties props = rp.getProperties();
                        if (!props.isSystemResource() && !props.isMapLayer()) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * addRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        renderableDisplayListeners.add(displayChangedListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * removeRenderableDisplayChangedListener
     * (com.raytheon.uf.viz.core.IRenderableDisplayChangedListener)
     */
    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
        renderableDisplayListeners.remove(displayChangedListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#
     * notifyRenderableDisplayChangedListeners
     * (com.raytheon.uf.viz.core.IDisplayPane,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay,
     * com.raytheon.uf.viz
     * .core.IRenderableDisplayChangedListener.DisplayChangeType)
     */
    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {
        for (IRenderableDisplayChangedListener listener : renderableDisplayListeners) {
            listener.renderableDisplayChanged(pane, display, type);
        }
    }

    public BackgroundColor getBackgroundColor() {
        return backgroundColor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    public void doSave(IProgressMonitor monitor) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#doSaveAs()
     */
    @Override
    public void doSaveAs() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
     */
    @Override
    public boolean isSaveAsAllowed() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        editorInput.getPaneManager().setFocus();
    }
}
