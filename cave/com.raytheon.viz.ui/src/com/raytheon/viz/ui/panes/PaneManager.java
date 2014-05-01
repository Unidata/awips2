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

package com.raytheon.viz.ui.panes;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.InputManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Manages panes. If virtual cursor is not desired, override InputAdapter
 * functions functions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *      
 * Date            Ticket#     Engineer    Description
 * ------------    ----------  ----------- --------------------------
 * 07/07/09                      bgonzale    Initial Creation.
 * </pre>
 * 
 * @author bgonzale
 * 
 */
public class PaneManager extends InputAdapter implements IMultiPaneEditor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PaneManager.class);

    /** The map input manager */
    protected InputManager inputManager;

    /** The display pane */
    protected List<VizDisplayPane> displayPanes;

    /** The pane that currently has the active focus */
    protected IDisplayPane activatedPane;

    /** The pane that is currently selected for loads and operations */
    protected Map<String, IDisplayPane> selectedPanes = null;

    /** The pane that is currently used as the basis for the mouse cursor */
    protected IDisplayPane currentMouseHoverPane;

    protected IDisplayPaneContainer paneContainer;

    private int displayedPaneCount = 0;

    protected Composite composite;

    private Set<ISelectedPanesChangedListener> listeners;

    protected IDisplayPane[] lastHandledPanes = null;

    public PaneManager() {
        inputManager = new InputManager(this);

        // Add us as input handler, virtual cursor
        inputManager.registerMouseHandler(this, InputPriority.PART);

        displayPanes = new ArrayList<VizDisplayPane>();
        listeners = new HashSet<ISelectedPanesChangedListener>();
    }

    public void initializeComponents(IDisplayPaneContainer container,
            Composite comp) {
        displayPanes.clear();
        displayedPaneCount = 0;
        paneContainer = container;
        GridLayout gl = new GridLayout(0, true);
        gl.horizontalSpacing = 3;
        gl.verticalSpacing = 3;
        gl.marginHeight = 0;
        gl.marginWidth = 0;

        composite = comp;
        composite.setLayout(gl);

        composite.addListener(SWT.Resize, new Listener() {
            private boolean waiting = false;

            @Override
            public void handleEvent(Event event) {
                if (waiting) {
                    return;
                }
                if (displayPanes.size() > 0) {
                    waiting = true;
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            adjustPaneLayout(displayedPaneCount);
                            waiting = false;
                        }
                    });
                }
            }
        });

        displayPanes.clear();
    }

    protected void registerHandlers(final IDisplayPane pane) {
        pane.addListener(SWT.MouseUp, inputManager);
        pane.addListener(SWT.MouseDown, inputManager);
        pane.addListener(SWT.MouseMove, inputManager);
        pane.addListener(SWT.MouseWheel, inputManager);
        pane.addListener(SWT.MouseHover, inputManager);
        pane.addListener(SWT.MouseDoubleClick, inputManager);
        pane.addListener(SWT.KeyDown, inputManager);
        pane.addListener(SWT.KeyUp, inputManager);
        pane.addListener(SWT.MenuDetect, inputManager);
        pane.addListener(SWT.MouseExit, inputManager);
        pane.addListener(SWT.MouseEnter, inputManager);

        pane.addListener(SWT.FocusIn, new Listener() {
            @Override
            public void handleEvent(Event event) {
                activatedPane = pane;
            }
        });

        pane.addListener(SWT.MouseEnter, new Listener() {
            @Override
            public void handleEvent(Event event) {
                currentMouseHoverPane = activatedPane = pane;
            }
        });
    }

    public void setFocus() {
        IDisplayPane pane = getActiveDisplayPane();
        if (pane != null) {
            pane.setFocus();
        }
    }

    public void hidePane(IDisplayPane pane) {
        if (pane.isVisible() == true) {
            VizDisplayPane glPane = (VizDisplayPane) pane;
            --displayedPaneCount;
            pane.setVisible(false);
            ((GridData) glPane.getCanvas().getParent().getLayoutData()).exclude = true;
            if (pane == getSelectedPane(IMultiPaneEditor.IMAGE_ACTION)) {
                setSelectedPane(IMultiPaneEditor.IMAGE_ACTION, null);
            }
            adjustPaneLayout(displayedPaneCount);
        }
        refresh();
    }

    public void showPane(IDisplayPane pane) {
        if (pane.isVisible() == false) {
            VizDisplayPane glPane = (VizDisplayPane) pane;
            ++displayedPaneCount;
            pane.setVisible(true);
            ((GridData) glPane.getCanvas().getParent().getLayoutData()).exclude = false;
            setSelectedPane(IMultiPaneEditor.IMAGE_ACTION, null);
            adjustPaneLayout(displayedPaneCount);
        }
        refresh();
    }

    protected void adjustPaneLayout(int paneCount) {
        if (composite == null || composite.isDisposed()) {
            return;
        }
        int numColums = (int) Math.sqrt(paneCount);
        int numRows = (int) Math.ceil(paneCount / (double) numColums);
        GridLayout gl = new GridLayout(numColums, true);
        int width = composite.getBounds().width;
        int height = composite.getBounds().height;

        if (numColums > 0 && numRows > 0) {
            gl.horizontalSpacing = width % numColums == 0 ? 2 : 3;
            gl.verticalSpacing = height % numRows == 0 ? 2 : 3;
        }
        gl.marginHeight = 0;
        gl.marginWidth = 0;

        composite.setLayout(gl);
        composite.layout();
    }

    /**
     * Perform a refresh asynchronously
     * 
     */
    @Override
    public void refresh() {
        for (IDisplayPane pane : displayPanes) {
            pane.refresh();
        }
    }

    public IRenderableDisplay[] getRenderableDisplays() {
        List<IRenderableDisplay> rDisplays = new ArrayList<IRenderableDisplay>();
        for (IDisplayPane pane : displayPanes) {
            rDisplays.add(pane.getRenderableDisplay());
        }
        return rDisplays.toArray(new IRenderableDisplay[rDisplays.size()]);
    }

    /**
     * Translate a current (x,y) screen coordinate to world coordinates.
     * 
     * The container using this manager should not call this method as it will
     * become recursive
     * 
     * @param x
     *            a visible x screen coordinate
     * @param y
     *            a visible y screen coordinate
     * @return the lat lon value of the cooordinate
     */
    @Override
    public Coordinate translateClick(double x, double y) {
        IDisplayPane pane = getActiveDisplayPane();
        // Convert the screen coordinates to grid space
        double[] world = pane.screenToGrid(x, y, 0);
        GridEnvelope ge = pane.getDescriptor().getGridGeometry().getGridRange();
        IExtent extent = new PixelExtent(ge);
        // Verify grid space is within the extent, otherwiser return null
        if (world == null || extent.contains(world) == false) {
            return null;
        }
        // use descriptor to convert pixel world to CRS world space
        world = pane.getDescriptor().pixelToWorld(world);
        // Check for null
        if (world == null) {
            return null;
        }
        return new Coordinate(world[0], world[1], world[2]);
    }

    /**
     * Translate a world coordinate to screen coordinates (x,y).
     * 
     * The container using this manager should not call this method as it will
     * become recursive
     * 
     * @param c
     *            Coordinate to convert
     * @return the world coordinates for the display
     */
    @Override
    public double[] translateInverseClick(Coordinate c) {
        if (c == null) {
            return null;
        }
        IDisplayPane pane = getActiveDisplayPane();
        double[] grid = pane.getDescriptor().worldToPixel(
                new double[] { c.x, c.y, c.z });
        if (grid == null) {
            return null;
        }
        return pane.gridToScreen(grid);
    }

    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        inputManager.registerMouseHandler(handler, priority);
    }

    /**
     * Register a mouse handler to a map
     * 
     * @param handler
     *            the handler to register
     */
    public void registerMouseHandler(IInputHandler handler) {
        inputManager.registerMouseHandler(handler);
    }

    /**
     * Unregister a mouse handler to a map
     * 
     * @param handler
     *            the handler to unregister
     */
    public void unregisterMouseHandler(IInputHandler handler) {
        inputManager.unregisterMouseHandler(handler);
    }

    /**
     * Take a screen shot of each display pane
     * 
     * @return the screen shots
     */
    public BufferedImage[] screenshots() {
        IDisplayPane[] panes = getDisplayPanes();
        BufferedImage[] images = new BufferedImage[panes.length];
        for (int i = 0; i < panes.length; ++i) {
            images[i] = panes[i].getTarget().screenshot();
        }
        return images;
    }

    public BufferedImage screenshot() {
        if (composite == null || composite.isDisposed()) {
            return null;
        }
        int numColums = (int) Math.sqrt(displayedPaneCount);
        int numRows = (int) Math.ceil(displayedPaneCount / (double) numColums);

        BufferedImage[] screens = screenshots();
        BufferedImage retval = new BufferedImage(screens[0].getWidth()
                * numColums, screens[0].getHeight() * numRows,
                screens[0].getType());

        int column = 0;
        int row = 0;
        int shotHeight = screens[0].getHeight();
        int shotWidth = screens[0].getWidth();

        for (BufferedImage currentPane : screens) {
            retval.createGraphics().drawImage(currentPane, shotWidth * column,
                    shotHeight * row, null);
            ++column;
            if (column == numColums) {
                column = 0;
                ++row;
            }
        }
        return retval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.IDisplayContainer#getDisplayPanes()
     */
    @Override
    public IDisplayPane[] getDisplayPanes() {
        return displayPanes.toArray(new VizDisplayPane[displayPanes.size()]);
    }

    /**
     * Returns the mouse manager
     * 
     * @return
     */
    public InputManager getMouseManager() {
        return inputManager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IDisplayPaneContainer#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        if (activatedPane == null) {
            activatedPane = displayPanes.size() > 0 ? displayPanes.get(0)
                    : null;
        }
        return activatedPane;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPanelEditor#getSelectedPane()
     */
    @Override
    public IDisplayPane getSelectedPane(String action) {
        return selectedPanes == null ? null : selectedPanes.get(action);
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
        IDisplayPane pane = getSelectedPane(action);
        if (pane == null && LOAD_ACTION.equals(action)) {
            return getDisplayPanes();
        }
        return new IDisplayPane[] { pane };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.IMultiPanelEditor#getNumberofPanes()
     */
    @Override
    public int getNumberofPanes() {
        return displayPanes.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#isSelectedPane(com.raytheon
     * .viz.core.IDisplayPane)
     */
    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        return getSelectedPane(action) == pane;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.IMultiPaneEditor#setSelectedPane(com.raytheon
     * .viz.core.IDisplayPane)
     */
    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        if (pane != null && !displayPanes.contains(pane)) {
            throw new IllegalArgumentException(
                    "setSelectedPane called with pane not in this IMultiPaneEditor");
        }
        if (selectedPanes == null) {
            selectedPanes = new HashMap<String, IDisplayPane>();
        }
        selectedPanes.put(action, pane);

        for (ISelectedPanesChangedListener listener : listeners) {
            listener.selectedPanesChanged(action, new IDisplayPane[] { pane });
        }

        refresh();
    }

    protected IDisplayPane addPane(IRenderableDisplay renderableDisplay,
            Composite canvasComp) {

        if (displayPanes.size() > 0) {
            for (ResourcePair rp : renderableDisplay.getDescriptor()
                    .getResourceList()) {
                if (rp.getProperties().isMapLayer()) {
                    renderableDisplay.getDescriptor().getResourceList()
                            .remove(rp);
                }
            }

            for (IDisplayPane gp : displayPanes) {
                for (ResourcePair rp : gp.getDescriptor().getResourceList()) {
                    if (rp.getProperties().isMapLayer()) {
                        renderableDisplay.getDescriptor().getResourceList()
                                .add(rp);
                    }
                }
            }
        }

        VizDisplayPane pane = null;
        try {
            pane = (VizDisplayPane) createNewPane(renderableDisplay, canvasComp);
            registerHandlers(pane);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error adding pane", e);
        }

        if (pane != null) {
            try {
                if (activatedPane == null) {
                    activatedPane = pane;
                }
                ++displayedPaneCount;
                if (displayPanes.size() > 0) {
                    pane.getRenderableDisplay().setBackgroundColor(
                            displayPanes.get(0).getRenderableDisplay()
                                    .getBackgroundColor());
                } else if (paneContainer instanceof AbstractEditor) {
                    ((AbstractEditor) paneContainer).getBackgroundColor()
                            .setColor(
                                    BGColorMode.EDITOR,
                                    pane.getRenderableDisplay()
                                            .getBackgroundColor());
                }

                if (displayPanes.size() > 0) {
                    pane.getDescriptor().synchronizeTimeMatching(
                            displayPanes.get(0).getDescriptor());
                }
                displayPanes.add(pane);
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM, "Error adding pane", t);
            }
        }

        setSelectedPane(IMultiPaneEditor.IMAGE_ACTION, null);
        adjustPaneLayout(displayedPaneCount);
        return pane;
    }

    protected IDisplayPane createNewPane(IRenderableDisplay renderableDisplay,
            Composite canvasComp) throws VizException {
        return new VizDisplayPane(paneContainer, canvasComp, renderableDisplay,
                true);
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        renderableDisplay.getDescriptor().getResourceList()
                .instantiateResources(renderableDisplay.getDescriptor(), true);
        Composite canvasComp = new Composite(composite, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        canvasComp.setLayout(gl);
        canvasComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return addPane(renderableDisplay, canvasComp);
    }

    @Override
    public void removePane(IDisplayPane pane) {
        if (!displayPanes.contains(pane)) {
            throw new IllegalArgumentException(
                    "removePane called with pane not in this IDisplayPaneContainer");
        }
        boolean wasVisible = pane.isVisible();
        displayPanes.remove(pane);
        if (activatedPane == pane && displayPanes.size() > 0) {
            activatedPane = displayPanes.get(0);
        } else {
            activatedPane = null;
        }
        if (selectedPanes != null) {

            Iterator<IDisplayPane> it = selectedPanes.values().iterator();
            while (it.hasNext()) {
                if (it.next() == pane) {
                    it.remove();
                }
            }
        }
        pane.dispose();

        if (wasVisible) {
            --displayedPaneCount;
        }

        if (pane == getSelectedPane(IMultiPaneEditor.IMAGE_ACTION)) {
            setSelectedPane(IMultiPaneEditor.IMAGE_ACTION, null);
        }

        adjustPaneLayout(displayedPaneCount);
    }

    @Override
    public LoopProperties getLoopProperties() {
        return paneContainer.getLoopProperties();
    }

    @Override
    public void setLoopProperties(LoopProperties loopProperties) {
        paneContainer.setLoopProperties(loopProperties);
    }

    @Override
    public int displayedPaneCount() {
        return displayedPaneCount;
    }

    @Override
    public void clear() {
        while (displayPanes.size() > 1) {
            removePane(displayPanes.get(displayPanes.size() - 1));
        }
        IDisplayPane pane = displayPanes.get(0);
        showPane(pane);
        pane.clear();
    }

    public void dispose() {
        activatedPane = null;
        currentMouseHoverPane = null;
        displayedPaneCount = 0;
        selectedPanes = null;
        composite = null;
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
        listeners.add(listener);
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
        listeners.remove(listener);
    }

    /**
     * PaneManager handle mouse move, sets the virtual cursor on the display
     * pane
     */
    @Override
    public boolean handleMouseMove(int x, int y) {
        Coordinate c = translateClick(x, y);

        if (c == null) {
            return false;
        }

        lastHandledPanes = getDisplayPanes();
        for (IDisplayPane pane : lastHandledPanes) {
            if (currentMouseHoverPane != pane) {
                ((VizDisplayPane) pane).setVirtualCursor(c);
            } else {
                ((VizDisplayPane) pane).setVirtualCursor(null);
            }
        }

        if (displayedPaneCount > 1) {
            refresh();
        }

        return false;
    }

    /**
     * Default calls handle mouse move
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        handleMouseMove(x, y);
        return false;
    }

    /**
     * Sets the virtual cursor on the display panes to null
     */
    @Override
    public boolean handleMouseExit(Event event) {
        if (lastHandledPanes != null) {
            for (IDisplayPane pane : lastHandledPanes) {
                ((VizDisplayPane) pane).setVirtualCursor(null);
            }
        }
        return false;
    }

}
