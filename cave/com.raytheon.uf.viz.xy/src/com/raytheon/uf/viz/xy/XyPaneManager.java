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
package com.raytheon.uf.viz.xy;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.IGlobalChangedListener;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer;
import com.raytheon.uf.viz.xy.map.IInsetMapContainer.InsetMapUtil;
import com.raytheon.uf.viz.xy.map.IInsetMapDisplayPaneContainer;
import com.raytheon.uf.viz.xy.map.InsetMapRenderableDisplay;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;

/**
 * Pane manager for graph panes, manages the inset maps
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 13, 2011           mschenke  Initial creation
 * Jan 13, 2016  5246     bsteffen  instantiate resources when a pane is added.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class XyPaneManager extends PaneManager implements
        IInsetMapDisplayPaneContainer, IPropertyChangeListener,
        IGlobalChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(XyPaneManager.class);

    /**
     * The display panes for inset pane -> graph pane with display panes or
     * empty
     */
    protected Map<IDisplayPane, IDisplayPane> insetMapPaneMap;

    /**
     * The display panes for graph pane -> inset map with display panes or empty
     */
    protected Map<IDisplayPane, IDisplayPane> graphPaneMap;

    /**
     * Pixel width to hide inset map
     */
    private int displayInsetWidth;

    /**
     * Density used to hide inset map
     */
    protected double displayInsetDensity;

    /**
     * Current density
     */
    protected double currentDensity;

    public XyPaneManager() {
        insetMapPaneMap = new HashMap<IDisplayPane, IDisplayPane>();
        graphPaneMap = new HashMap<IDisplayPane, IDisplayPane>();
        IPersistentPreferenceStore store = Activator.getDefault()
                .getPreferenceStore();
        displayInsetWidth = store.getInt(Activator.MAP_DISPLAY_WIDTH);
        displayInsetDensity = store.getDouble(Activator.MAP_DISPLAY_DENSITY);
        store.addPropertyChangeListener(this);
        VizGlobalsManager.addListener(VizConstants.DENSITY_ID, this);
        currentDensity = (Double) VizGlobalsManager.getCurrentInstance()
                .getPropery(VizConstants.DENSITY_ID);
    }

    @Override
    public IDisplayPane[] getDisplayPanes() {
        // Checks active pane to see if active pane is inset pane or graph
        if (isInsetPane(getActiveDisplayPane())) {
            return getInsetPanes();
        } else {
            return getGraphPanes();
        }
    }

    @Override
    public void setFocus() {
        // When we set focus, make sure we focus on the graph pane
        IDisplayPane active = getActiveDisplayPane();
        if (isInsetPane(active)) {
            activatedPane = insetMapPaneMap.get(active);
        }
        super.setFocus();
    }

    /**
     * Get an array of the graph panes
     * 
     * @return the graph panes
     */
    public IDisplayPane[] getGraphPanes() {
        return super.getDisplayPanes();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.IInsetMapDisplayPaneContainer#getInsetPanes()
     */
    @Override
    public IDisplayPane[] getInsetPanes() {
        return insetMapPaneMap.keySet().toArray(
                new IDisplayPane[insetMapPaneMap.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.map.IInsetMapDisplayPaneContainer#getInsetPanes
     * (com.raytheon.uf.viz.core.IDisplayPane)
     */
    @Override
    public IDisplayPane[] getInsetPanes(IDisplayPane pane) {
        return new IDisplayPane[] { graphPaneMap.get(pane) };
    }

    /**
     * Check to see if the pane is an inset pane
     * 
     * @param pane
     * @return
     */
    public boolean isInsetPane(IDisplayPane pane) {
        return insetMapPaneMap.containsKey(pane);
    }

    /**
     * Get the inset map pane for the graph pane
     * 
     * @param pane
     * @return
     */
    public IDisplayPane getInsetMapForPane(IDisplayPane pane) {
        return graphPaneMap.get(pane);
    }

    @Override
    public IDisplayPane addPane(IRenderableDisplay renderableDisplay) {
        try {
            VizDisplayPane insetPane = null, graphPane = null;
            final Composite graphComposite = new Composite(composite, SWT.NONE);
            if (isInsetMapDisplay(renderableDisplay)) {
                final Composite insetComp = new Composite(graphComposite,
                        SWT.NONE);
                insetComp.setLayout(new FormLayout());
                insetComp
                        .setLayoutData(((IInsetMapContainer) renderableDisplay)
                                .getInsetMapLocation());
                // TODO: Need to have inset map renderable displays share
                // projection info somehow
                InsetMapRenderableDisplay insetDisplay = (InsetMapRenderableDisplay) InsetMapUtil
                        .loadInsetMap(renderableDisplay);
                for (IDisplayPane pane : insetMapPaneMap.keySet()) {
                    insetDisplay.setDescriptor(pane.getDescriptor());
                    break;
                }

                final VizDisplayPane insetMapPane = new VizDisplayPane(this,
                        insetComp, insetDisplay, true);
                insetMapPane.getCanvas().setLayoutData(getFullFormData());

                insetMapPane.getCanvas().addMouseTrackListener(
                        new MouseTrackAdapter() {
                            @Override
                            public void mouseEnter(MouseEvent e) {
                                activatedPane = insetMapPane;
                                currentMouseHoverPane = insetMapPane;
                            }

                            @Override
                            public void mouseExit(MouseEvent e) {
                                activatedPane = insetMapPaneMap
                                        .get(insetMapPane);
                            }
                        });
                insetMapPane.addListener(SWT.Resize, new Listener() {
                    @Override
                    public void handleEvent(Event event) {
                        setVisibility(insetMapPane);
                        graphComposite.layout();
                    }
                });
                insetPane = insetMapPane;
                if (currentDensity < displayInsetDensity) {
                    insetComp.setVisible(false);
                }
            }
            IDescriptor descriptor = renderableDisplay.getDescriptor();
            descriptor.getResourceList().instantiateResources(descriptor, true);
            addPane(renderableDisplay, graphComposite);
            graphPane = displayPanes.get(displayPanes.size() - 1);
            graphComposite.setLayout(new FormLayout());
            graphComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                    true));
            graphPane.getCanvas().setLayoutData(getFullFormData());
            composite.layout();

            activatedPane = graphPane;

            if (insetPane != null) {
                registerHandlers(insetPane);
                insetMapPaneMap.put(insetPane, graphPane);
                graphPaneMap.put(graphPane, insetPane);
            }
            return graphPane;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error adding pane", e);
        }
        return null;
    }

    @Override
    public void removePane(IDisplayPane pane) {
        super.removePane(pane);
        IDisplayPane inset = graphPaneMap.remove(pane);
        if (inset != null) {
            inset.dispose();
            insetMapPaneMap.remove(inset);
        }
    }

    @Override
    public void refresh() {
        super.refresh();
        for (IDisplayPane pane : insetMapPaneMap.keySet()) {
            pane.refresh();
        }
    }

    /**
     * Check to see if the renderable display supports the attachement of an
     * inset map
     * 
     * @param display
     * @return
     */
    public boolean isInsetMapDisplay(IRenderableDisplay display) {
        return ((display instanceof IInsetMapContainer) && (((IInsetMapContainer) display)
                .getInsetMapLocation() != null));
    }

    /**
     * @return
     */
    protected static FormData getFullFormData() {
        FormData fd = new FormData();
        fd.right = new FormAttachment(100, 0);
        fd.left = new FormAttachment(0, 0);
        fd.top = new FormAttachment(0, 0);
        fd.bottom = new FormAttachment(100, 0);
        return fd;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse
     * .jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (Activator.MAP_DISPLAY_DENSITY.equals(event.getProperty())) {
            displayInsetDensity = Double.parseDouble(String.valueOf(event
                    .getNewValue()));
        } else if (Activator.MAP_DISPLAY_WIDTH.equals(event.getProperty())) {
            displayInsetWidth = Integer.parseInt(String.valueOf(event
                    .getNewValue()));
        }
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                for (IDisplayPane inset : getInsetPanes()) {
                    setVisibility((VizDisplayPane) inset);
                }
                composite.layout();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.globals.IGlobalChangedListener#updateValue(org
     * .eclipse.ui.IWorkbenchWindow, java.lang.Object)
     */
    @Override
    public void updateValue(IWorkbenchWindow changedWindow, Object value) {
        if (paneContainer == EditorUtil.getActiveVizContainer(changedWindow)) {
            // Applies to us, set new density
            currentDensity = (Double) value;
            for (IDisplayPane inset : getInsetPanes()) {
                setVisibility((VizDisplayPane) inset);
            }
            composite.layout();
        }
    }

    protected void setVisibility(VizDisplayPane insetPane) {
        Composite insetComp = insetPane.getCanvas().getParent();
        boolean shouldBeVisByWidth = insetComp.getBounds().width >= displayInsetWidth;
        boolean shouldBeVisByDensity = currentDensity >= displayInsetDensity;
        boolean visible = shouldBeVisByDensity && shouldBeVisByWidth;
        insetComp.setVisible(visible);
    }
}
