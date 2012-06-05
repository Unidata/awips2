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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.ui.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent.CollaborationEventType;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.drawing.DrawingToolLayer;
import com.raytheon.uf.viz.drawing.DrawingToolLayer.DrawMode;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource that uses DrawingToolLayer to render drawn data for multiple users
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDrawingResource extends
        AbstractVizResource<CollaborationDrawingResourceData, IDescriptor> {

    private SessionContainer container;

    private UserId myUser;

    private Map<UserId, DrawingToolLayer> layerMap;

    private CollaborationDrawingUIManager manager;

    private boolean lockingDrawing = false;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public CollaborationDrawingResource(
            CollaborationDrawingResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    public CollaborationDrawingResource(CollaborationDrawingResource resource) {
        super(resource.getResourceData(), resource.getLoadProperties());
        this.layerMap = new HashMap<UserId, DrawingToolLayer>(resource.layerMap);
        resource.layerMap.clear();
        this.myUser = resource.myUser;
        this.getCapabilities().addCapability(
                resource.getCapability(OutlineCapability.class));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        EditableManager.makeEditable(this, true);
        container = SharedDisplaySessionMgr.getSessionContainer(resourceData
                .getSessionId());
        if (container == null) {
            throw new VizException("Could not find container for sessionId: "
                    + resourceData.getSessionId());
        }

        if (layerMap == null) {
            // Don't reinitialized
            myUser = container.getSession().getUserID();
            layerMap = new HashMap<UserId, DrawingToolLayer>();

            OutlineCapability outline = getCapability(OutlineCapability.class);
            outline.setLineStyle(LineStyle.SOLID);
            outline.setOutlineWidth(4);
            outline.setSuppressingMenuItems(true);

            ColorableCapability colorable = getCapability(ColorableCapability.class);
            colorable.setSuppressingMenuItems(true);
        }

        manager = new CollaborationDrawingUIManager(this);
        container.getSession().registerEventHandler(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (target instanceof DispatchGraphicsTarget) {
            // Ensure we paint to our own target only
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }

        OutlineCapability outline = getCapability(OutlineCapability.class);

        synchronized (layerMap) {
            for (UserId user : layerMap.keySet()) {
                DrawingToolLayer layer = layerMap.get(user);
                if (layer != null) {
                    layer.setEraserWidth(16); // Configure?
                    layer.setLineStyle(outline.getLineStyle());
                    layer.setLineWidth(outline.getOutlineWidth());
                    layer.setColor(container.getColorManager()
                            .getColorFromUser(user));
                    layer.paint(target, paintProps);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        for (DrawingToolLayer layer : layerMap.values()) {
            layer.dispose();
        }
        layerMap.clear();
        layerMap = null;

        manager.dispose();
        container.getSession().unRegisterEventHandler(this);
    }

    /**
     * @return the myUser
     */
    public UserId getMyUser() {
        return myUser;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    public String getName() {
        return "Telestrator Drawing Tool";
    }

    /**
     * Get the DrawingToolLayer object associated with the user, one will be
     * created if none exists
     * 
     * @param user
     * @return
     */
    public DrawingToolLayer getDrawingLayerFor(UserId user) {
        if (layerMap != null) {
            synchronized (layerMap) {
                DrawingToolLayer layer = layerMap.get(user);
                if (layer == null) {
                    if (user == myUser) {
                        layer = new CollaborationDrawingToolLayer(
                                descriptor.getGridGeometry(), this);
                    } else {
                        layer = new DrawingToolLayer(
                                descriptor.getGridGeometry());
                    }
                    layerMap.put(user, layer);
                }
                return layer;
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (layerMap) {
            for (DrawingToolLayer layer : layerMap.values()) {
                layer.reproject(descriptor.getGridGeometry());
            }
        }
    }

    /**
     * @return
     */
    public boolean isSessionLeader() {
        return container.getSession().getCurrentSessionLeader().equals(myUser);
    }

    /**
     * @return
     */
    public boolean isLockingDrawing() {
        return lockingDrawing;
    }

    public void setLockingDrawing(boolean lockingDrawing) {
        this.lockingDrawing = lockingDrawing;
        if (isSessionLeader()) {
            CollaborationDrawingEvent event = new CollaborationDrawingEvent();
            event.setUserName(myUser);
            event.setType(CollaborationEventType.TOGGLE_LOCK);
            sendEvent(event);
        }
    }

    /**
     * Checks if this resource is able to tellestrate given its state
     * 
     * @return
     */
    public boolean canTellestrate() {
        return getCapability(EditableCapability.class).isEditable()
                && (isSessionLeader() || isLockingDrawing() == false);
    }

    @Subscribe
    public void participantChanged(IVenueParticipantEvent event) {
        UserId user = event.getParticipant();
        switch (event.getEventType()) {
        case DEPARTED:
            synchronized (layerMap) {
                DrawingToolLayer layer = layerMap.remove(user);
                if (layer != null) {
                    layer.dispose();
                    issueRefresh();
                }
            }
            break;
        case ARRIVED:
            // TODO: Send all your user data?
            break;
        }
    }

    @Subscribe
    public void handleDrawEvent(CollaborationDrawingEvent event) {
        UserId user = event.getUserName();
        if (user.equals(myUser)) {
            // Early exit case, don't process my own events twice
            issueRefresh();
            return;
        }

        synchronized (layerMap) {
            DrawingToolLayer layer = getDrawingLayerFor(user);
            List<Coordinate> points = event.getCoordinates();
            switch (event.getType()) {
            case CLEAR:
                layer.clear();
                break;
            case TOGGLE_LOCK:
                // Toggle locking
                setLockingDrawing(!isLockingDrawing());
                break;
            case DRAW:
                layer.setDrawMode(DrawMode.DRAW);
                if (points != null) {
                    for (Coordinate c : points) {
                        layer.addCoordinate(c);
                    }
                }
                layer.doneDrawing();
                break;
            case ERASE:
                layer.setDrawMode(DrawMode.ERASE);
                if (points != null) {
                    for (Coordinate c : points) {
                        layer.addCoordinate(c);
                    }
                }
                layer.doneErasing();
                IRenderableDisplay display = descriptor.getRenderableDisplay();
                layer.processErase(display.getExtent(), display.getBounds());
                break;
            case REDO:
                layer.redo();
                break;
            case UNDO:
                layer.undo();
                break;
            }
        }
        issueRefresh();
    }

    public void sendEvent(Object event) {
        try {
            container.getSession().sendObjectToVenue(event);
        } catch (CollaborationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    /**
     * @return the container
     */
    public SessionContainer getContainer() {
        return container;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#okToUnload()
     */
    @Override
    public boolean okToUnload() {
        // Though I hate this methods exists, it serves its purpose
        return false;
    }
}
