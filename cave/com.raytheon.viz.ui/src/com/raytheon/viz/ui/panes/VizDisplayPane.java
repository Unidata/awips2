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

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.cmenu.IContextMenuProvider;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Creates a GL Context for drawing
 * 
 * <P>
 * Typical usage:
 * </P>
 * 
 * <pre>
 * 
 *       Composite c = ....;
 *       
 *       GLDisplayPane glDisplayPane = new GLDisplayPane(c, 
 *                                     MapDescriptor.DEFAULT_WIDTH, 
 *                                     MapDescriptor.DEFAULT_HEIGHT);
 * 
 * </pre>
 * 
 * <pre>
 * 
 *           SOFTWARE HISTORY
 *          
 *           Date         Ticket#     Engineer    Description
 *           ------------ ----------  ----------- --------------------------
 *           Oct 25, 2006             chammack    Initial Creation.
 *           20 Nov 2007              ebabin      Fix location of sample,lat/lon menu add.
 *           14 Jan 2007              ebabin      Update to remove lat/lon only for GLEditor an Gl4panelEDitor.
 *           Jul 9, 2008  #1228       chammack    Add capability for perspective contributed right click menus
 *           Oct 27, 2009 #2354       bsteffen    Configured input handler to use mouse preferences
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class VizDisplayPane implements IDisplayPane {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VizDisplayPane.class);

    protected static final String CONTEXT_MENU_PREF = "com.raytheon.viz.ui.contextmenu";

    protected static final double WHEEL_CLICK_SCALE_FACTOR = -0.04;

    protected static final double MIN_ZOOM_REQUEST = 0.01;

    protected static final double ZOOM_ANIMATION_FACTOR = 2.0;

    /** The canvas composite */
    private final Composite canvasComp;

    /** The canvas */
    private final Canvas canvas;

    /** The graphics target */
    protected IGraphicsTarget target;

    /** The renderable display to draw to the screen */
    protected IRenderableDisplay renderableDisplay;

    /** Requested zoom level change percentage */
    protected double zoomRequest = 0.0;

    /** Current zoom level */
    protected double zoomLevel = 1.0;

    /** Indicates the map editor is shutting down */
    protected boolean isShuttingDown;

    /** The last mouse x position */
    protected int lastMouseX;

    /** The last mouse y position */
    protected int lastMouseY;

    /** The last mouse clicked x position */
    protected int lastClickX;

    /** The last mouse clicked y position */
    protected int lastClickY;

    /** The virtual cursor location */
    protected Coordinate virtualCursor;

    /** The container of this pane */
    protected IDisplayPaneContainer container;

    protected Job menuJob;

    protected final Object menuLock = new Object();

    protected AbstractGraphicsFactoryAdapter graphicsAdapter;

    protected MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    /**
     * Abstract display pane constructor
     * 
     * @param container
     * @param canvasComp
     *            the composite to use with the canvas
     * @param display
     *            the initial renderable display to use in construction
     * @throws VizException
     */
    public VizDisplayPane(IDisplayPaneContainer container, Composite c,
            IRenderableDisplay display) throws VizException {
        this(container, c, display, true);
    }

    /**
     * Abstract display pane constructor
     * 
     * @param container
     * @param canvasComp
     *            the composite to use with the canvas
     * @param display
     *            the initial renderable display to use in construction
     * @param enableContextualMenus
     *            whether to enable contextual menu support
     * @throws VizException
     */
    public VizDisplayPane(final IDisplayPaneContainer container,
            Composite canvasComp, IRenderableDisplay display,
            boolean enableContextualMenus) throws VizException {
        this.container = container;
        this.canvasComp = canvasComp;
        this.canvasComp.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                VizDisplayPane.this.dispose();
            }
        });

        // create the graphics adapter
        graphicsAdapter = display.getGraphicsAdapter();
        // create the canvas
        this.canvas = graphicsAdapter.constrcutCanvas(canvasComp);
        // set the renderable display
        setRenderableDisplay(display);

        // Setup the canvas listeners
        addCanvasListeners(this.canvas);

        if (enableContextualMenus) {
            this.canvas.addMouseListener(new MouseListener() {

                @Override
                public void mouseDoubleClick(MouseEvent e) {
                    mouseDoubleClickOnCanvas(e);
                }

                @Override
                public void mouseDown(MouseEvent e) {
                    handleMouseDownOnCanvas(e);
                }

                @Override
                public void mouseUp(MouseEvent e) {
                    handleMouseUpOnCanvas(e);
                }
            });

            MenuManager menuMgr = new MenuManager("#PopupMenu");
            menuMgr.setRemoveAllWhenShown(true);

            menuMgr.addMenuListener(new IMenuListener() {
                public void menuAboutToShow(IMenuManager manager) {
                    VizDisplayPane.this.menuAboutToShow(manager);
                }
            });
            Menu menu = menuMgr.createContextMenu(canvas);
            menu.setVisible(false);
            canvasComp.setMenu(menu);
        }

        // Register ourselves with the DrawCoordinatorJob
        DrawCoordinatorJob.getInstance().registerPane(this.container, this);
    }

    /**
     * Add any kind of listeners to the canvas, defaults are refresh, resize,
     * dispose, and mouse wheel
     * 
     * @param canvas2
     */
    protected void addCanvasListeners(Canvas canvas) {
        // Add canvas refresh,resize,dispose listeners
        canvas.addListener(SWT.Paint, new Listener() {
            public void handleEvent(Event event) {
                refresh();
            }
        });

        canvas.addListener(SWT.Resize, new Listener() {
            public void handleEvent(Event event) {
                resize();
            }
        });

        canvas.addListener(SWT.MouseMove, new Listener() {
            @Override
            public void handleEvent(Event event) {
                double[] grid = screenToGrid(event.x, event.y, 0);
                for (IDisplayPane pane : container.getDisplayPanes()) {
                    if (pane instanceof VizDisplayPane) {
                        VizDisplayPane gdp = (VizDisplayPane) pane;
                        double[] screen = gdp.gridToScreen(grid);
                        gdp.lastMouseX = (int) screen[0];
                        gdp.lastMouseY = (int) screen[1];
                    }
                }
                lastMouseX = event.x;
                lastMouseY = event.y;
            }
        });
    }

    /**
     * @param display
     * @return
     */
    public AbstractGraphicsFactoryAdapter getGraphicsAdapter() {
        return graphicsAdapter;
    }

    /**
     * @param manager
     */
    protected void menuAboutToShow(IMenuManager manager) {
        manager.removeAll();

        IDescriptor descriptor = renderableDisplay.getDescriptor();
        ResourceList list = descriptor.getResourceList();

        // First thing: search for last resource that is
        // IContextMenuProvider. They will potentially provide the entire
        // context menu contents
        List<IContextMenuProvider> providers = list
                .getResourcesByTypeAsType(IContextMenuProvider.class);
        for (int i = providers.size() - 1; i >= 0; --i) {
            providers.get(i).provideContextMenuItems(manager, lastClickX,
                    lastClickY);
            if (manager.getItems().length > 0) {
                // A provider has provided us with a context menu
                return;
            }
        }

        List<IContextMenuContributor> contributors = list
                .getResourcesByTypeAsType(IContextMenuContributor.class);
        for (IContextMenuContributor contributor : contributors) {
            addContextMenuItems(contributor, manager);
        }

        // Get the contributions from the perspective
        if (container instanceof IWorkbenchPart) {
            // Find the site of this container and it's
            // enclosing window
            IWorkbenchWindow window = ((IWorkbenchPart) container).getSite()
                    .getWorkbenchWindow();

            AbstractVizPerspectiveManager perspectiveManager = VizPerspectiveListener
                    .getInstance(window).getActivePerspectiveManager();

            if (perspectiveManager != null) {
                perspectiveManager
                        .addContextMenuItems(manager, container, this);
            }

        }
    }

    /**
     * Add the context menu items for the context menu contributor
     * 
     * @param contributor
     * @param manager
     */
    protected void addContextMenuItems(IContextMenuContributor contributor,
            IMenuManager manager) {
        int len = manager.getItems().length;
        contributor.addContextMenuItems(manager, lastClickX, lastClickY);
        if (manager.getItems().length > len) {
            manager.add(new Separator());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#addListener(int,
     * org.eclipse.swt.widgets.Listener)
     */
    public void addListener(int eventType, Listener listener) {
        canvas.addListener(eventType, listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#getTarget()
     */
    public IGraphicsTarget getTarget() {
        return target;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#dispose()
     */
    public void dispose() {
        synchronized (this) {
            DrawCoordinatorJob.getInstance().unregisterPane(this.container,
                    this);

            isShuttingDown = true;

            if (this.target != null) {
                this.target.dispose();
            }

            if (this.renderableDisplay != null
                    && this.renderableDisplay.isSwapping() == false) {
                this.renderableDisplay.dispose();
                container.notifyRenderableDisplayChangedListeners(this,
                        renderableDisplay, DisplayChangeType.REMOVE);
            }

            if (canvas.isDisposed() == false) {
                canvasComp.dispose();
            }
        }
    }

    protected void draw(final boolean actualDraw) {

        if (renderableDisplay == null) {
            return;
        }

        IExtent extent = renderableDisplay.getExtent();

        if (extent == null) {
            return;
        }

        zoomLevel = renderableDisplay.recalcZoomLevel(renderableDisplay
                .getDimensions());

        Rectangle bounds = getBounds();
        if (bounds == null || bounds.width == 0 || bounds.height == 0) {
            return;
        }

        renderableDisplay.setBounds(bounds);
        boolean isZooming = false;
        // compute scale factor based on zoom request
        synchronized (this) {
            double factor = 1.0 + zoomRequest;
            if (Math.abs(zoomRequest) < MIN_ZOOM_REQUEST) {
                zoomRequest = 0;
            } else {
                double limitedZoomRequest = zoomRequest / ZOOM_ANIMATION_FACTOR;
                factor = 1.0 + limitedZoomRequest;
                zoomRequest -= limitedZoomRequest;
            }

            if (factor != 1.0) {
                /*
                 * Check if zooming past max and min zoom levels
                 */
                if (zoomLevel * factor > IRenderableDisplay.MAX_ZOOM_LEVEL) {
                    factor = IRenderableDisplay.MAX_ZOOM_LEVEL / zoomLevel;
                    zoomRequest = 0.0;
                } else if (zoomLevel * factor < IRenderableDisplay.MIN_ZOOM_LEVEL) {
                    factor = IRenderableDisplay.MIN_ZOOM_LEVEL / zoomLevel;
                    zoomRequest = 0.0;
                }
                if (lastMouseX == 0 && lastMouseY == 0) {
                    renderableDisplay.zoom(factor);
                } else {
                    renderableDisplay.scaleAndBias(factor, lastMouseX,
                            lastMouseY, target);
                }

                zoomLevel = renderableDisplay.recalcZoomLevel(renderableDisplay
                        .getDimensions());
            }

            isZooming = zoomRequest != 0.0;
        }

        if (actualDraw) {
            glDrawInternal(isZooming);
        }

        if (!isVisible()) {
            // No need to paint again if not visible
            target.setNeedsRefresh(false);
        }
    }

    protected void drawEnd() {
        if (isShuttingDown) {
            return;
        }

        this.target.endFrame();
    }

    /**
     * Actually perform the draw. This should never be called directly, and
     * should only be scheduled via the Redraw Job (preferably by calling
     * refresh()).
     * 
     * @param endFrame
     *            should the frame be terminated
     * @param is
     *            a zoom operation in process?
     */
    private void glDrawInternal(boolean isZoomInProgress) {
        if (isShuttingDown) {
            return;
        }

        synchronized (this) {
            this.target.beginFrame(renderableDisplay.getView(), true);

            try {
                PaintProperties paintProps = new PaintProperties(1.0f,
                        (float) zoomLevel, renderableDisplay.getView(),
                        getBounds(), isZoomInProgress, renderableDisplay
                                .getDescriptor().getFramesInfo());

                if (this.container != null) {
                    paintProps.setLoopProperties(this.container
                            .getLoopProperties());
                }

                renderableDisplay.paint(target, paintProps);

                if (virtualCursor != null) {
                    drawVirtualCursor(paintProps, virtualCursor);
                }

                if (isZoomInProgress) {
                    // If we are zooming, make sure we paint again
                    refresh();
                }
            } catch (RuntimeException e) {
                statusHandler.handle(
                        Priority.SIGNIFICANT,
                        "Internal exception occurred while drawing: "
                                + e.getMessage(), e);
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT, e.getMessage(), e);
            }
        }
    }

    /**
     * Paints the virtual cursor to the screen
     * 
     * @param paintProps
     * @param virtualCursor
     * @throws VizException
     */
    protected void drawVirtualCursor(PaintProperties paintProps,
            Coordinate virtualCursor) throws VizException {
        // Calculate scale for image
        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double middleValue = 5 / screenToWorldRatio;
        double outsideValue = 6 / screenToWorldRatio;
        double insideValue = 4 / screenToWorldRatio;

        this.target.drawRect(new PixelExtent(virtualCursor.x - middleValue,
                virtualCursor.x + middleValue, virtualCursor.y - middleValue,
                virtualCursor.y + middleValue), new RGB(255, 255, 255), 1.0f,
                1.0f);
        this.target.drawRect(new PixelExtent(virtualCursor.x - outsideValue,
                virtualCursor.x + outsideValue, virtualCursor.y - outsideValue,
                virtualCursor.y + outsideValue), new RGB(0, 0, 0), 0.5f, 1.0f);
        this.target.drawRect(new PixelExtent(virtualCursor.x - insideValue,
                virtualCursor.x + insideValue, virtualCursor.y - insideValue,
                virtualCursor.y + insideValue), new RGB(0, 0, 0), 0.5f, 1.0f);

        DrawableCircle circle = new DrawableCircle();
        circle.filled = true;
        circle.radius = 1.0 / screenToWorldRatio;
        circle.basics.color = new RGB(255, 255, 255);
        circle.setCoordinates(virtualCursor.x, virtualCursor.y);
        this.target.drawCircle(circle);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#shiftExtent(float, float)
     */
    public void shiftExtent(double[] startScreen, double[] endScreen) {
        if (canvas.isDisposed()) {
            return;
        }

        renderableDisplay.shiftExtent(startScreen, endScreen, this.target);

        this.target.setNeedsRefresh(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.IDisplayPane#setRenderableDisplay(com.raytheon
     * .viz.core.drawables.IRenderableDisplay)
     */
    public void setRenderableDisplay(IRenderableDisplay renderableRsc) {
        if (this.renderableDisplay == renderableRsc) {
            return;
        }

        if (this.renderableDisplay != null) {
            container.notifyRenderableDisplayChangedListeners(this,
                    this.renderableDisplay, DisplayChangeType.REMOVE);
        }

        this.renderableDisplay = renderableRsc;
        if (this.renderableDisplay == null) {
            return;
        }

        this.renderableDisplay.setContainer(container);
        graphicsAdapter = renderableDisplay.getGraphicsAdapter();

        try {
            initializeTarget();
            this.renderableDisplay.calcPixelExtent(getBounds());
            renderableDisplay.setup(this.target);
            container.notifyRenderableDisplayChangedListeners(this,
                    renderableDisplay, DisplayChangeType.ADD);
        } catch (VizException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Exception while initializing graphics target", e);
        }
    }

    private void initializeTarget() throws VizException {
        int worldWidth = renderableDisplay.getWorldWidth();
        int worldHeight = renderableDisplay.getWorldHeight();

        if (this.target != null) {
            target.dispose();
        }

        this.target = graphicsAdapter.constructTarget(canvas, worldWidth,
                worldHeight);

        // Initialize OpenGL
        this.target.init();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#getRenderableDisplay()
     */
    public IRenderableDisplay getRenderableDisplay() {
        return this.renderableDisplay;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#refresh()
     */
    public void refresh() {
        this.target.setNeedsRefresh(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#getBounds()
     */
    public Rectangle getBounds() {
        if (canvas.isDisposed()) {
            return null;
        }

        return this.canvas.getBounds();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#setFocus()
     */
    public void setFocus() {
        if (canvas != null && canvas.isDisposed() == false) {
            canvas.setFocus();
            refresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#zoom(int, int, int)
     */
    public void zoom(final int value, int mouseX, int mouseY) {
        lastMouseX = mouseX;
        lastMouseY = mouseY;

        double factor = (WHEEL_CLICK_SCALE_FACTOR * value);
        synchronized (this) {
            this.zoomRequest += factor;
        }
        refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#zoom(int)
     */
    public void zoom(final int value) {
        zoom(value, 0, 0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#getZoomLevel()
     */
    public double getZoomLevel() {
        return zoomLevel;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#setZoomLevel(double)
     */
    public void setZoomLevel(double zoomLevel) {
        this.zoomLevel = zoomLevel;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#getLastMouseX()
     */
    public int getLastMouseX() {
        return lastMouseX;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.IDisplayPane#getLastMouseY()
     */
    public int getLastMouseY() {
        return lastMouseY;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPane#getLastClickX()
     */
    public int getLastClickX() {
        return lastClickX;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPane#getLastClickY()
     */
    public int getLastClickY() {
        return lastClickY;
    }

    /**
     * Add a focus listener
     * 
     * @param listener
     *            the focus listener
     */
    public void addFocusListener(FocusListener listener) {
        canvas.addFocusListener(listener);
    }

    /**
     * Remove a focus listener
     * 
     * @param listener
     *            the focus listener
     */
    public void removeFocusListener(FocusListener listener) {
        canvas.removeFocusListener(listener);
    }

    /**
     * Add a mouse track listener
     * 
     * @param listener
     *            the mouse track listener
     */
    public void addMouseTrackListener(MouseTrackListener listener) {
        canvas.addMouseTrackListener(listener);
    }

    /**
     * @return the virtualCursor
     */
    public Coordinate getVirtualCursor() {
        return virtualCursor;
    }

    /**
     * @param virtualCursor
     *            the virtualCursor to set
     */
    public void setVirtualCursor(Coordinate virtualCursor) {
        if (virtualCursor != null && this.renderableDisplay != null) {
            double[] out = this.getDescriptor().worldToPixel(
                    new double[] { virtualCursor.x, virtualCursor.y });
            this.virtualCursor = new Coordinate(out[0], out[1]);
        } else {
            this.virtualCursor = null;
        }
    }

    /**
     * Resize the pane
     */
    public void resize() {
        synchronized (this) {

            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (canvas == null || canvas.isDisposed()) {
                        return;
                    }

                    target.resize();

                    Rectangle clientArea = canvas.getClientArea();

                    if (renderableDisplay != null
                            && renderableDisplay.getExtent() == null) {
                        scaleToClientArea();

                        zoomLevel = renderableDisplay
                                .recalcZoomLevel(renderableDisplay
                                        .getDimensions());
                        refresh();

                    } else if (renderableDisplay != null) {
                        renderableDisplay.calcPixelExtent(clientArea);
                        zoomLevel = renderableDisplay
                                .recalcZoomLevel(renderableDisplay
                                        .getDimensions());
                        refresh();
                    }
                }
            });

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IDisplayPane#getDescriptor()
     */
    @Override
    public IDescriptor getDescriptor() {
        IDescriptor desc = null;
        if (this.renderableDisplay != null) {
            desc = this.renderableDisplay.getDescriptor();
        }
        return desc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IDisplayPane#getDisplay()
     */
    @Override
    public Display getDisplay() {
        return this.canvas.getDisplay();
    }

    /**
     * Scale this pane's display to the display bounds.
     */
    public void scaleToClientArea() {
        renderableDisplay.scaleToClientArea(getBounds());
    }

    /**
     * 
     */
    public double[] screenToGrid(double x, double y, double depth) {
        return renderableDisplay.screenToGrid(x, y, depth, target);
    }

    /**
     * 
     */
    public double[] gridToScreen(double[] grid) {
        return renderableDisplay.gridToScreen(grid, target);
    }

    /**
     * 
     * @param zoomLevel
     */
    public void zoom(double zoomLevel) {
        renderableDisplay.zoom(zoomLevel);
    }

    @Override
    public void clear() {
        renderableDisplay.clear();
    }

    /**
     * @return the canvas
     */
    public Canvas getCanvas() {
        return canvas;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPane#setVisible(boolean)
     */
    @Override
    public void setVisible(boolean visible) {
        canvasComp.setVisible(visible);
        canvas.setVisible(visible);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPane#isVisible()
     */
    @Override
    public boolean isVisible() {
        return canvas.isVisible();
    }

    /**
     * 
     * @param menuJob
     * @param e
     */
    protected void handleMouseDownOnCanvas(MouseEvent e) {
        lastClickX = lastMouseX = e.x;
        lastClickY = lastMouseY = e.y;
        if (prefManager.handleLongClick(CONTEXT_MENU_PREF, e.button)) {
            canvasComp.getMenu().setVisible(false);
            synchronized (menuLock) {
                if (menuJob != null) {
                    menuJob.cancel();
                }
            }
            menuJob = new Job("RightClickMgr") {
                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            if (canvas.isDisposed() == false
                                    && canvasComp.getMenu() != null) {
                                showMenu();
                            }
                        }
                    });
                    synchronized (menuLock) {
                        menuJob = null;
                    }
                    return Status.OK_STATUS;
                }
            };
            menuJob.schedule(275);
        } else if (prefManager.handleClick(CONTEXT_MENU_PREF, e.button)) {
            canvasComp.getMenu().setVisible(false);
            showMenu();
        }
    }

    /**
     * Show the context menu
     */
    protected void showMenu() {
        Point canvasLoc = canvas.getDisplay().map(canvas, null, lastClickX,
                lastClickY);
        canvasComp.getMenu().setLocation(canvasLoc);
        canvasComp.getMenu().setVisible(true);
    }

    /**
     * mouse event was double clicked on the canvas
     * 
     * @param e
     */
    protected void mouseDoubleClickOnCanvas(MouseEvent e) {

    }

    /**
     * handle mouse up on canvas event
     * 
     * @param e
     */
    protected void handleMouseUpOnCanvas(MouseEvent e) {
        if (prefManager.handleLongClick(CONTEXT_MENU_PREF, e.button)) {
            synchronized (menuLock) {
                if (menuJob != null) {
                    menuJob.cancel();
                }
            }
        }
    }

}
