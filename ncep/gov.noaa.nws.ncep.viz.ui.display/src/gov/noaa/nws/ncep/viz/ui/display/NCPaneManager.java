package gov.noaa.nws.ncep.viz.ui.display;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener.DisplayChangeType;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Natl Cntrs implementation of IPaneManager.
 * 
 * Note that this uses a slightly different method of selecting panes.
 * IPaneManager allows for different kind of pane selections (ie actions of
 * LOAD, IMAGE, ....) but one one may be selected for each action.)
 * NCPaneManager ignores the action but will allow for more than one pane to be
 * selected at one time. selectPane() and deselectPane() should be called
 * instead of setSelectedPane().
 * 
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer     Description 
 * ------------ ----------  -----------  -------------------------- 
 * 03/07/11      R1G2-9 	Greg Hull 	 Created
 * 07/18/12      #649       Shova Gurung Fixed echo/virtual cursor display issue.
 * 09/13/12			?		B. Yin		 Refresh only for multiple panes
 * 
 * </pre>
 * 
 * @author ghull
 * 
 */

// extend IPaneManager or implement PaneManager?
// bsteffen changed to extend PaneManager
// public class NCPaneManager implements IPaneManager {
// extends PaneManager {
public class NCPaneManager extends PaneManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NCPaneManager.class);

    static public String NC_PANE_SELECT_ACTION = "NC_SELECT_PANE";

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.NONE)
    public static class PaneLayout {

        @XmlAttribute
        private int numRows;

        @XmlAttribute
        private int numColumns;

        public PaneLayout() {
            numRows = 1;
            numColumns = 1;
        }

        public PaneLayout(int r, int c) {
            numRows = r;
            numColumns = c;
        }

        public int getRows() {
            return numRows;
        }

        public int getColumns() {
            return numColumns;
        }

        public int getNumberOfPanes() {
            return numRows * numColumns;
        }

        // an index into a one dimensional array
        //
        public int getPaneIndex(PaneID pid) {
            int pIndx = (pid.getRow() * numColumns) + pid.getColumn();
            if (pIndx >= getNumberOfPanes()) {
                return -1;
            }
            return pIndx;
        }

        public int compare(PaneLayout paneLayout) {
            if (numColumns != paneLayout.getColumns()) {
                return (numColumns < paneLayout.getColumns() ? -1 : 1);
            }
            if (numRows != paneLayout.getRows()) {
                return (numRows < paneLayout.getRows() ? -1 : 1);
            } else
                return 0;
        }

        public boolean equals(PaneLayout paneLayout) {
            return (compare(paneLayout) == 0);
        }
    }

    private PaneLayout paneLayout;

    /** The map input manager */
    protected InputManager inputManager;

    /** The display pane */
    protected ArrayList<NCDisplayPane> displayPanes;

    // The pane that is currently selected. While D2D allows for mulitple
    // types of selections (actions).
    // NC only supports selected/not selected (ie NC_PANE_SELECT_ACTION).
    protected ArrayList<NCDisplayPane> selectedPanes = null;

    /** The pane that currently has the active focus */
    protected IDisplayPane activatedPane;

    /** The pane that is currently used as the basis for the mouse cursor */
    protected NCDisplayPane currentMouseHoverPane;

    private IDisplayPaneContainer paneContainer;

    protected Composite parentComposite;

    private Set<ISelectedPanesChangedListener> listeners;

    // Implement the VirtualCursor and Pane Selection
    private class NcPaneMouseHandler extends InputAdapter {

        IDisplayPane[] lastHandledPanes = null;

        NCMapEditor ncMapEditor = null;

        // NCDisplayPane thisPane = null;

        NcPaneMouseHandler(NCMapEditor ncEd) { // , NCDisplayPane pane ) {
            ncMapEditor = ncEd;
            // thisPane = pane;
        }

        public boolean handleMouseMove(int x, int y) {
            Coordinate c = translateClick(x, y);

            if (c == null) {
                return false;
            }

            lastHandledPanes = getDisplayPanes();

            boolean geoSync = true;

            if (lastHandledPanes.length > 1
                    && paneContainer instanceof NCMapEditor) {

                geoSync = ((NCMapEditor) paneContainer).arePanesGeoSynced();
            }

            for (IDisplayPane pane : lastHandledPanes) {

                if (geoSync && currentMouseHoverPane != pane) {
                    // bsteffen change to VizDisplayPane
                    // ((GLDisplayPane) pane).setVirtualCursor(c);
                    ((VizDisplayPane) pane).setVirtualCursor(c);
                } else {
                    // bsteffen change to VizDisplayPane
                    // ((GLDisplayPane) pane).setVirtualCursor(null);
                    ((VizDisplayPane) pane).setVirtualCursor(null);
                }
            }

            //Refresh only for multiple panes.
            if ( getNumberofPanes() > 1 ){
            	
            refresh();
            }

            return false;
        }

        @Override
        public boolean handleMouseExit(Event event) {
            if (lastHandledPanes != null) {
                for (IDisplayPane pane : lastHandledPanes) {
                    // bsteffen change to VizDisplayPane
                    // ((GLDisplayPane) pane).setVirtualCursor(null);
                    ((VizDisplayPane) pane).setVirtualCursor(null);
                }
            }
            return false;
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {

            return false;
        }
    }

    public NCPaneManager() {
        this(new PaneLayout(1, 1));
    }

    public NCPaneManager(PaneLayout playout) {
        paneLayout = playout;

        inputManager = new InputManager(this);

        // sgurung: moved the following code to initializeComponents(...) 
        // Enable the inspect adapters
        // handles the VirtualCursor and selecting the panes
        /*if (paneContainer instanceof NCMapEditor) {
            inputManager.registerMouseHandler(new NcPaneMouseHandler(
                    (NCMapEditor) paneContainer), InputPriority.PERSPECTIVE);
        }*/

        displayPanes = new ArrayList<NCDisplayPane>();
        selectedPanes = new ArrayList<NCDisplayPane>();
        listeners = new HashSet<ISelectedPanesChangedListener>();
    }

    public PaneLayout getPaneLayout() {
        return paneLayout;
    }

    // called from AbstractEditor.createPartControl
    @Override
    public void initializeComponents(IDisplayPaneContainer container,
            Composite parent) {
        // super.initializeComponents( container, parent );
        if (!(paneContainer instanceof NCMapEditor)) {

        }
        paneContainer = container;
        GridLayout gl = new GridLayout(paneLayout.getColumns(), true);
        gl.horizontalSpacing = 3;
        gl.verticalSpacing = 3;
        gl.marginHeight = 0;
        gl.marginWidth = 0;

        parentComposite = parent;
        parentComposite.setLayout(gl);

        // Enable the inspect adapters
        // handles the VirtualCursor and selecting the panes
        if (paneContainer instanceof NCMapEditor) {
            inputManager.registerMouseHandler(new NcPaneMouseHandler(
                    (NCMapEditor) paneContainer), InputPriority.PERSPECTIVE);
        }

        // create the Composites for the panes ahead of time be
        // Composite canvasComp = new Composite(composite, SWT.NONE);
        // GridLayout gl = new GridLayout(1, false);
        // gl.marginHeight = 0;
        // gl.marginWidth = 0;
        // canvasComp.setLayout(gl);
        // canvasComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
        // true));

        // final AbstractEditor editor = this;
        //
        // displayPanes = new ArrayList<NCDisplayPane>();
        // displayPaneMap = new HashMap<String, NCDisplayPane>();
        // selectedPanes = new HashMap<PaneID,NCDisplayPane>();
        selectedPanes = new ArrayList<NCDisplayPane>();

        // for (int r = 0; r < paneLayout.getRow(); r++) {
        // for (int c = 0; c < paneLayout.getColumn(); c++) {
        // PaneID paneId = new PaneID(r, c);
        //
        // NCMapRenderableDisplay display = displaysToLoad.get(paneId
        // .toString());
        //
        // editorOwnDisplay = display;
        // addPane(paneId, display);
        // }
        // }
        //
        // activatedPane = displayPaneMap.get(new PaneID(0, 0).toString());
        // currentMouseHoverPane = displayPaneMap.get(new PaneID(0,
        // 0).toString());
        //
        // // if there is more
        // if (displayPaneMap.size() > 0) {
        // selectPane(displayPaneMap.get(new PaneID(0, 0).toString()), true);
        // }

        //
        parentComposite.addListener(SWT.Resize, new Listener() {
            private boolean waiting = false;

            @Override
            public void handleEvent(Event event) {
                // PaneManager includes code to adjust the paneLayout here .....
            }
        });

        displayPanes.clear();

    }

    protected void registerHandlers(IDisplayPane pane) {
        // I think that this listener needs to be added before the
        // input Manager otherwise an inputHandler method may reference
        // the wrong selected pane.
        //
        final NCDisplayPane thisPane = (NCDisplayPane) pane;

        // bsteffen change to Listener
        // pane.addMouseListener(new MouseListener() {
        // @Override
        // public void mouseDoubleClick(MouseEvent e) {
        // }
        //
        // @Override
        // public void mouseDown(MouseEvent e) {
        // // TODO : Add this to the MousePreferences???
        // if (e.button == 1 || e.button == 3) {
        //
        // boolean radioBehaviour = ((e.stateMask & SWT.CONTROL) == 0);
        //
        // if (paneContainer instanceof NCMapEditor) {
        // ((NCMapEditor) paneContainer).selectPane(thisPane,
        // radioBehaviour);
        // }
        // }
        // }
        //
        // @Override
        // public void mouseUp(MouseEvent e) {
        // }
        // });
        pane.addListener(SWT.MouseDown, new Listener() {

            @Override
            public void handleEvent(Event e) {
                if (e.button == 1 || e.button == 3) {

                    boolean radioBehaviour = ((e.stateMask & SWT.CONTROL) == 0);

                    if (paneContainer instanceof NCMapEditor) {
                        ((NCMapEditor) paneContainer).selectPane(thisPane,
                                radioBehaviour);
                    }
                }
            }
        });

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
    }

    @Override
    public int getNumberofPanes() {
        return displayPanes.size();
    }

    public int getNumberofSelectedPanes() {
        return selectedPanes.size();
    }

    // public boolean isPaneManaged( IDisplayPane pane ) {
    // return displayPanes.contains( pane );
    // }

    // This interface method shouldn't be called from NC perspective.
    //
    @Override
    public void setSelectedPane(String action, IDisplayPane pane) {
        if (action == null || !action.equals(NC_PANE_SELECT_ACTION)) {
            return;
        }

        // if not already selected then
        if (!selectedPanes.contains(pane)) {
            selectedPanes.add((NCDisplayPane) pane);
        }

//        for( ISelectedPanesChangedListener lstnr : listeners ) {
//            lstnr.selectedPanesChanged(NC_PANE_SELECT_ACTION,
//                    getSelectedPanes(NC_PANE_SELECT_ACTION));
//        }
//        refresh();
    }

    public void selectPane(NCDisplayPane pane) {
//        System.out.println("NCPaneManager selecting pane " + pane.hashCode());
        setSelectedPane(NC_PANE_SELECT_ACTION, pane);
    }

    public void selectPanes( List<IDisplayPane> seldPanes ) {

    	if( seldPanes.isEmpty() ) {
    		return;
    	}
    	
    	selectedPanes.clear();
    	
    	for( IDisplayPane p : seldPanes ) {
    		setSelectedPane(NC_PANE_SELECT_ACTION, p );	
    	}

            for (ISelectedPanesChangedListener lstnr : listeners) {
                lstnr.selectedPanesChanged(NC_PANE_SELECT_ACTION,
                        getSelectedPanes(NC_PANE_SELECT_ACTION));
            }
    }

    
    // if this pane is in the list of selected panes, remove it
    // and call the listeners.
//    public void deselectPane(IDisplayPane pane) {
//        if (selectedPanes.contains(pane)) {
//            selectedPanes.remove(pane);
//
//            for (ISelectedPanesChangedListener lstnr : listeners) {
//                lstnr.selectedPanesChanged(NC_PANE_SELECT_ACTION,
//                        getSelectedPanes(NC_PANE_SELECT_ACTION));
//            }
//
//            refresh();
//        }
//    }

    // This method is part of the IPaneManager interface but
    // for the NC Perspective multiple panes may be selected.
    // Here we'll return the first selected pane in the list
    // but if the caller needs to handle cases with multiple
    // selected panes, it should call getSelectedPanes instead.
    //
    @Override
    public IDisplayPane getSelectedPane(String action) {
        if (action == null || !action.equals(NC_PANE_SELECT_ACTION)) {
            return null;
        }

        if (getNumberofSelectedPanes() > 0) {
            return selectedPanes.get(0);
        } else {
            return null;
        }
    }

    @Override
    public IDisplayPane[] getSelectedPanes(String action) {
        if (action == null || !action.equals(NC_PANE_SELECT_ACTION)) {
            return null;
        }
        NCDisplayPane seldPanes[] = selectedPanes.toArray(new NCDisplayPane[0]);

        return seldPanes;
    }

    @Override
    public boolean isSelectedPane(String action, IDisplayPane pane) {
        if (action == null || !action.equals(NC_PANE_SELECT_ACTION)) {
            return false;
        }
        return (selectedPanes.contains(pane) ? true : false);
    }

    @Override
    public void addSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        listeners.add(listener);
    }

    @Override
    public void removeSelectedPaneChangedListener(
            ISelectedPanesChangedListener listener) {
        listeners.remove(listener);
    }

    // bsteffen added return value
    // protected void addRenderableDisplayToPane(
    protected IDisplayPane addRenderableDisplayToPane(
            IRenderableDisplay renderableDisplay, Composite canvasComp) {

        // What was raytheon doing here with the MapLayer resources??
        if (displayPanes.size() > 0) {
            // for (ResourcePair rp : renderableDisplay.getDescriptor()
            // .getResourceList()) {
            // if (rp.getProperties().isMapLayer()) {
            // renderableDisplay.getDescriptor().getResourceList()
            // .remove(rp);
            // }
            // }
            //
            // for (IDisplayPane gp : displayPanes) {
            // for (ResourcePair rp : gp.getDescriptor().getResourceList()) {
            // if (rp.getProperties().isMapLayer()) {
            // renderableDisplay.getDescriptor().getResourceList()
            // .add(rp);
            // }
            // }
            // }
        }

        NCDisplayPane pane = null;
        try {
            pane = new NCDisplayPane(paneContainer, canvasComp,
                    renderableDisplay, true);
            // bsteffen function no longer exists
            // pane.setAdjustExtent(false); // what does this do?

            // register the inputManager and the mouse listener for pane
            // selection
            registerHandlers(pane);

            final NCDisplayPane thisPane = pane;

            pane.addFocusListener(new FocusListener() {
                public void focusGained(FocusEvent e) {
                    activatedPane = thisPane;
                }

                public void focusLost(FocusEvent e) {
                }
            });

            pane.addMouseTrackListener(new MouseTrackListener() {
                public void mouseEnter(MouseEvent e) {
                    activatedPane = thisPane;
                    currentMouseHoverPane = thisPane;
                }

                public void mouseExit(MouseEvent e) {
                }

                public void mouseHover(MouseEvent e) {
                }
            });
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error adding pane", e);
            if (pane != null) {
                pane.dispose();
            }
        }

        if (pane != null) {
            try {
                if (activatedPane == null) {
                    activatedPane = pane;
                }
                // ++displayedPaneCount; // not hiding panes...
                if (displayPanes.size() > 0) {
                    pane.getRenderableDisplay().setBackgroundColor(
                            displayPanes.get(0).getRenderableDisplay()
                                    .getBackgroundColor());
                } else {
                    BackgroundColor.getActivePerspectiveInstance().setColor(
                            BGColorMode.EDITOR,
                            pane.getRenderableDisplay().getBackgroundColor());
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

        // if( getNumberofSelectedPanes() == 0 ) {
        // selectPane( pane );
        // currentMouseHoverPane = pane;
        // activatedPane = pane;
        // }

        // bsteffen added return value
        return pane;
    }

    // This
    @Override
    // bsteffen added return value
    // public void addPane(IRenderableDisplay renderableDisplay) {
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        // All panes are added up front in initializeComponents
        // System.out.println("addPane not implemented for NCDisplayPane");
        Composite canvasComp = new Composite(parentComposite, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        canvasComp.setLayout(gl);
        canvasComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // bsteffen added return value
        // addRenderableDisplayToPane(renderableDisplay, canvasComp);
        return addRenderableDisplayToPane(renderableDisplay, canvasComp);

    }

    // @Override
    // public void addPanes( IRenderableDisplay[] displaysToLoad ) {
    // }

    @Override
    public void removePane(IDisplayPane pane) {
        System.out.println("removePane not implemented for NCDisplayPane");
    }

    @Override
    public void hidePane(IDisplayPane pane) {
        System.out.println("hidePane not implemented for NCDisplayPane");
    }

    @Override
    public void showPane(IDisplayPane pane) {
        System.out.println("showPane not implemented for NCDisplayPane");
    }

    // TODO : if we implement hide/show then this will need to change, but for
    // now all panes are shown.
    @Override
    public int displayedPaneCount() {
        return displayPanes.size();
    }

    @Override
    public void clear() {
        System.out.println("clearPanes not implemented for NCDisplayPane");
    }

    @Override
    public IDisplayPane[] getDisplayPanes() {
        return displayPanes.toArray(new NCDisplayPane[displayPanes.size()]);
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
    public IDisplayPane getActiveDisplayPane() {
        if (activatedPane == null) {
            activatedPane = displayPanes.size() > 0 ? displayPanes.get(0)
                    : null;
        }
        return activatedPane;
    }

    @Override
    public void refresh() {
        for (IDisplayPane pane : displayPanes) {
            pane.refresh();
        }
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
    // bsteffen Im going to let super handle this, as this recursively calls
    // NCMapEditor, which calls this
    // @Override
    // public Coordinate translateClick(double x, double y) {
    // return paneContainer != null ? paneContainer.translateClick(x, y)
    // : null;
    // }

    /**
     * Translate a world coordinate to screen coordinates (x,y).
     * 
     * The container using this manager should not call this method as it will
     * become recursive
     * 
     * @param c
     *            Coordinate to convert
     * @return the world coordinates for the display
    @Override
    public double[] translateInverseClick(Coordinate c) {
        return paneContainer != null ? paneContainer.translateInverseClick(c)
                : null;
    }
     */

    @Override
    public void addRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
    }

    @Override
    public void removeRenderableDisplayChangedListener(
            IRenderableDisplayChangedListener displayChangedListener) {
    }

    @Override
    public void notifyRenderableDisplayChangedListeners(IDisplayPane pane,
            IRenderableDisplay display, DisplayChangeType type) {
    }

    @Override
    public void registerMouseHandler(IInputHandler handler,
            InputPriority priority) {
        inputManager.registerMouseHandler(handler, priority);
    }

    @Override
    public void registerMouseHandler(IInputHandler handler) {
        inputManager.registerMouseHandler(handler);
    }

    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        inputManager.unregisterMouseHandler(handler);
    }

    @Override
    public void dispose() {
        activatedPane = null;
        currentMouseHoverPane = null;
        // displayedPaneCount = 0;
        selectedPanes = null;
        parentComposite = null;
    }

    // NCDisplayPane getCurrentMouseHoverPane() {
    // return currentMouseHoverPane;
    // }

    @Override
    public InputManager getMouseManager() {
        return inputManager;
    }

    @Override
    public void setFocus() {
        IDisplayPane pane = getActiveDisplayPane();
        if (pane != null) {
            pane.setFocus();
        }
    }

    // bsteffen removed override
    // @Override
    public IDescriptor getDescriptor() {
        IDescriptor descriptor = null;
        IRenderableDisplay display = getActiveDisplayPane()
                .getRenderableDisplay();
        if (display != null) {
            descriptor = display.getDescriptor();
        }
        return descriptor;
    }

    // from PaneManager if we want to use it.....
    public BufferedImage screenshot() {
        return getActiveDisplayPane().getTarget().screenshot();
    }

}
