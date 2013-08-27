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
package com.raytheon.viz.ghg.monitor;

import java.util.List;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.viz.gfe.rsc.zones.ZoneDbResource;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent;
import com.raytheon.viz.ghg.monitor.event.GhgMonitorZoneSelectionEvent;
import com.raytheon.viz.ghg.monitor.listener.GhgMonitorTableSelectionListener;
import com.raytheon.viz.ghg.utilities.GhgDragDetector;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.PanHandler;
import com.raytheon.viz.ui.panes.PaneManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * GHG Monitor Map Manager is an interface between the map and the Monitoring
 * dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2010            mpduff     Initial creation
 * Aug 27, 2013  #2301     dgilling   Fix mouse handler registration so they
 *                                    can't trigger events until map is loaded.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GhgMapManager extends PaneManager implements
        GhgMonitorTableSelectionListener {

    private class GhgMapInputAdapter extends InputAdapter {
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 1) {
                if (!dragDetector.isDrag()) {
                    GhgConfigData config = GhgConfigData.getInstance();
                    Coordinate c = translateClick(x, y);

                    ZoneDbResource zdbr = getZoneResource();

                    if (c != null) {
                        selectedZones = zdbr.getOverlappingZones(c);
                    } else {
                        selectedZones = new String[0];
                    }

                    setHighlightedZones(selectedZones);
                    GhgMonitorZoneSelectionEvent evt = new GhgMonitorZoneSelectionEvent(
                            this);
                    evt.setHighlightedZones(selectedZones);
                    GhgDisplayManager.getInstance().fireMapChangeEvent(evt);

                    // Set color to map selection color
                    getZoneResource().getResourceData().setSelectionColor(
                            config.getMapSelectionsColors().getBackgroundRgb());
                    getZoneResource().setDirty(true);
                    refresh();
                }
            }

            dragDetector.setDrag(false);

            return false;
        }
    }

    /**
     * Zones that the user has selected (with the mouse).
     */
    private String[] selectedZones;

    /** The looping properties */
    private LoopProperties loopProperties;

    /** The drag detector */
    private GhgDragDetector dragDetector;

    /**
     * Constructor
     */
    GhgMapManager(Composite parent) {
        selectedZones = new String[0];
        loopProperties = new LoopProperties();
        GhgDisplayManager.getInstance().addGhgMonitorTableSelectionListener(
                this);
    }

    /**
     * Create the dialog
     * 
     * @param parent
     *            The parent composite
     */
    public void createComponent(Composite parent) {
        initializeComponents(this, parent);
    }

    /**
     * Get the display resource
     * 
     * @return The ZoneDbResource
     */
    public ZoneDbResource getZoneResource() {
        IDisplayPane activePane = getActiveDisplayPane();
        List<ZoneDbResource> zoneRscs = activePane.getDescriptor()
                .getResourceList()
                .getResourcesByTypeAsType(ZoneDbResource.class);
        if (zoneRscs.size() > 0) {
            return zoneRscs.get(0);
        }
        return null;
    }

    /**
     * Set Label Zones flag.
     * 
     * @param labelZones
     *            true to label, false to not label
     */
    public void setLabelZones(boolean labelZones) {
        if (getZoneResource() != null) {
            getZoneResource().setLabelZones(labelZones);
        }
    }

    @Override
    public void dispose() {
        super.dispose();
        GhgDisplayManager.getInstance().removeGhgMonitorTableSelectionListener(
                this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ghg.monitor.listener.GhgMonitorTableSelectionListener
     * #notifyUpdate
     * (com.raytheon.viz.ghg.monitor.event.GhgMonitorTableSelectionEvent)
     */
    @Override
    public void notifyUpdate(GhgMonitorTableSelectionEvent evt) {
        String[] highlightedZones = evt.getHighlightedZones();
        setHighlightedZones(highlightedZones);
        ZoneDbResource zoneRsc = getZoneResource();
        if (zoneRsc != null) {
            zoneRsc.getResourceData()
                    .setSelectionColor(evt.getSelectionColor());
            zoneRsc.setDirty(true);
        }
        refresh();
    }

    /**
     * Set the zone IDs to highlight. This will cause the zones listed to be
     * drawn in the highlighted color the next time the zone resource is
     * painted.
     * 
     * @param highlightedZones
     *            the highlightedZones to set
     */
    public void setHighlightedZones(String[] highlightedZones) {
        if (getZoneResource() != null) {
            getZoneResource().setZoneIdsToShow(highlightedZones);
            refresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#getLoopProperties()
     */
    @Override
    public LoopProperties getLoopProperties() {
        return loopProperties;
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
        this.loopProperties = loopProperties;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IDisplayPaneContainer#refresh()
     */
    @Override
    public void refresh() {
        if (getZoneResource() != null) {
            getZoneResource().refreshSelections();
            super.refresh();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.panes.PaneManager#registerHandlers(com.raytheon.uf
     * .viz.core.IDisplayPane)
     */
    @Override
    protected void registerHandlers(IDisplayPane pane) {
        super.registerHandlers(pane);
        registerMouseHandler(new GhgMapInputAdapter());
        registerMouseHandler(new PanHandler(this));
        dragDetector = new GhgDragDetector();
        registerMouseHandler(dragDetector);
    }
}
