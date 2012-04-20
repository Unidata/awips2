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

import java.util.Collection;
import java.util.Iterator;

import org.eclipse.swt.graphics.RGB;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.collaboration.ui.ColorChangeEvent;
import com.raytheon.uf.viz.collaboration.ui.SessionColorManager;
import com.raytheon.uf.viz.collaboration.ui.login.LoginData;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent.CollaborationEventType;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.uf.viz.drawing.PathDrawingResourceData;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * A layer that extends Drawing Layer but allows other users to have things
 * drawn as well
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationDrawingLayer extends DrawingLayer {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationDrawingLayer.class);

    private Multimap<UserId, ShapeContainer> collaboratorShapes;

    private Multimap<UserId, ShapeContainer> deletedCollaboratorShapes;

    private String sessionId;

    private SessionColorManager colorManager;

    private IWireframeShape tempRemoteShape = null;

    private boolean allowDraw = false;

    /**
     * @param data
     * @param props
     */
    public CollaborationDrawingLayer(PathDrawingResourceData data,
            LoadProperties props) {
        super(data, props);
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        CollaborationDataManager mgr = CollaborationDataManager.getInstance();
        // TODO, needs to be modified
        for (String str : mgr.getSessions().keySet()) {
            mgr.getSession(str).registerEventHandler(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#initInternal(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (target instanceof DispatchGraphicsTarget) {
            // Ensure we paint to our own target only
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        super.initInternal(target);
        this.collaboratorShapes = LinkedHashMultimap.create();
        this.collaboratorShapes = Multimaps
                .synchronizedMultimap(this.collaboratorShapes);

        this.deletedCollaboratorShapes = LinkedHashMultimap.create();
        this.deletedCollaboratorShapes = Multimaps
                .synchronizedMultimap(this.deletedCollaboratorShapes);
        colorManager = SharedDisplaySessionMgr.getSessionContainer(sessionId)
                .getColorManager();
        LoginData data = CollaborationDataManager.getInstance().getLoginData();
        UserId id = new UserId(data.getUser(), data.getServer());
        color = colorManager.getColors().get(id);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#paintInternal(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        getCapability(ColorableCapability.class).setSuppressingMenuItems(true);
        getCapability(OutlineCapability.class).setSuppressingMenuItems(true);
        if (target instanceof DispatchGraphicsTarget) {
            // Ensure we paint to our own target only
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        super.paintInternal(target, paintProps);

        OutlineCapability outline = getCapability(OutlineCapability.class);
        // paint the shapes that come over from others
        synchronized (collaboratorShapes) {
            for (UserId userName : collaboratorShapes.keySet()) {
                for (ShapeContainer sh : collaboratorShapes.get(userName)) {
                    if (sh != null) {
                        color = colorManager.getColors().get(userName);
                        if (color == null) {
                            color = new RGB(255, 0, 0);
                        }
                        target.drawWireframeShape(sh.getShape(), color,
                                outline.getOutlineWidth(),
                                outline.getLineStyle());
                    }
                }
            }
        }
    }

    @Subscribe
    public void setColorEvent(ColorChangeEvent event) {
        this.color = event.getColor();
        colorManager.addUser(event.getUserName());
        issueRefresh();
    }

    @Subscribe
    public void handle(CollaborationDrawingEvent event) {
        switch (event.getType()) {
        case DRAW:
            addCollaborationShape(event.getUserName(), event.getContainer());
            break;
        case DISABLE:
            allowDraw = !allowDraw;
            getEventBus().post(event);
        case UNDO:
            UserId userName = event.getUserName();
            Collection<ShapeContainer> container = collaboratorShapes
                    .get(userName);
            Iterator<ShapeContainer> itr = container.iterator();
            ShapeContainer lastElement = null;
            if (itr.hasNext()) {
                lastElement = itr.next();
            }
            while (itr.hasNext()) {
                lastElement = itr.next();
            }
            deletedCollaboratorShapes.put(userName, lastElement);
            collaboratorShapes.get(userName).remove(lastElement);
            break;
        case REDO:
            userName = event.getUserName();
            container = deletedCollaboratorShapes.get(userName);
            itr = container.iterator();
            lastElement = null;
            if (itr.hasNext()) {
                lastElement = itr.next();
            }
            while (itr.hasNext()) {
                lastElement = itr.next();
            }
            collaboratorShapes.put(userName, lastElement);
            deletedCollaboratorShapes.get(userName).remove(lastElement);
            break;
        case CLEAR:
            resetTemp();
            clearSelfShapes(event.getUserName());
            break;
        case ERASE:
            // TODO need to functionize this as it is mostly the same as
            // DrawingLayer
            userName = event.getUserName();
            double extentPercentageX = paintProps.getView().getExtent()
                    .getWidth()
                    / (double) paintProps.getCanvasBounds().width;
            double cursorSize = 16;
            double size = extentPercentageX * cursorSize;
            Multimap<UserId, ShapeContainer> containers = HashMultimap.create();
            synchronized (collaboratorShapes) {
                for (ShapeContainer cont : collaboratorShapes.get(userName)) {
                    Geometry line = event.getContainer().getGeom();
                    if (line.buffer(size / 2).intersects(cont.getGeom())) {
                        Geometry intersection = line.buffer(size / 2)
                                .intersection(cont.getGeom());
                        Geometry finalGeom = null;
                        try {
                            finalGeom = cont.getGeom().difference(intersection);
                        } catch (TopologyException e) {
                            continue;
                        }
                        Geometry lString = null;
                        if (finalGeom instanceof MultiLineString) {
                            lString = (MultiLineString) finalGeom;
                            for (int j = 0; j < lString.getNumGeometries(); j++) {
                                LineString lineString = (LineString) lString
                                        .getGeometryN(j);
                                eraseWireframeShape = target
                                        .createWireframeShape(true, descriptor);
                                drawTempLinePrimitive(lineString,
                                        eraseWireframeShape);
                                ShapeContainer shCont = new ShapeContainer();
                                shCont.setGeom(lString);
                                shCont.setShape(eraseWireframeShape);
                                containers.put(userName, shCont);
                            }
                        } else if (finalGeom instanceof LineString) {
                            GeometryFactory factory = new GeometryFactory();
                            lString = (LineString) finalGeom;
                            Point point = factory.createPoint(lString
                                    .getCoordinates()[0]);
                            intersection = point.buffer(size / 2).intersection(
                                    cont.getGeom());
                            finalGeom = cont.getGeom().difference(intersection);
                            eraseWireframeShape = target.createWireframeShape(
                                    true, descriptor);
                            drawTempLinePrimitive(lString, eraseWireframeShape);
                            ShapeContainer shCont = new ShapeContainer();
                            shCont.setGeom(lString);
                            shCont.setShape(eraseWireframeShape);
                            containers.put(userName, shCont);
                        } else {
                            containers.put(userName, cont);
                        }
                    } else {
                        containers.put(userName, cont);
                    }
                }
            }
            collaboratorShapes = containers;
            break;
        }
        issueRefresh();
    }

    /**
     * @param userName
     */
    private void clearSelfShapes(UserId userName) {
        // TODO, fix this
        for (UserId cont : collaboratorShapes.keySet()) {
            if (cont.getFQName().equals(userName.getFQName())) {
                for (ShapeContainer shape : collaboratorShapes.get(cont)) {
                    shape.getShape().dispose();
                }
                collaboratorShapes.removeAll(cont);
            }
        }
        // for (ShapeContainer cont : collaboratorShapes.get(userName)) {
        // cont.getShape().dispose();
        // }
        collaboratorShapes.removeAll(userName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#addTempDrawLine(com.vividsolutions
     * .jts.geom.LineString)
     */
    @Override
    public void addTempDrawLine(LineString line) {
        super.addTempDrawLine(line);
        // for showing the line on the fly...
        // sendDrawEvent(line);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#addTempEraseLine(com.vividsolutions
     * .jts.geom.LineString)
     */
    @Override
    public void addTempEraseLine(LineString line) {
        super.addTempEraseLine(line);
        // for erasing the line on the fly...
        // sendEraseEvent(line);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingLayer#finalizeLine(com.vividsolutions
     * .jts.geom.LineString, java.lang.String)
     */
    @Override
    public void finalizeLine(LineString line, String uuid) {
        super.finalizeLine(line, uuid);
        // for showing the line only after the artist lets up on the mouse
        if (state == LayerState.DRAWING) {
            sendDrawEvent(line);
        } else if (state == LayerState.ERASING) {
            sendEraseEvent(line);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#undoAdd()
     */
    @Override
    public void undoAdd() {
        super.undoAdd();
        CollaborationDrawingEvent event = new CollaborationDrawingEvent();
        event.setType(CollaborationEventType.UNDO);
        LoginData data = CollaborationDataManager.getInstance().getLoginData();
        UserId userId = new UserId(data.getUser(), data.getServer());
        event.setUserName(userId);
        sendGenericEvent(event);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#redoAdd()
     */
    @Override
    public void redoAdd() {
        super.redoAdd();
        CollaborationDrawingEvent event = new CollaborationDrawingEvent();
        event.setType(CollaborationEventType.REDO);
        LoginData data = CollaborationDataManager.getInstance().getLoginData();
        UserId userId = new UserId(data.getUser(), data.getServer());
        event.setUserName(userId);
        sendGenericEvent(event);
    }

    public void sendDisableOthers() {
        CollaborationDrawingEvent event = new CollaborationDrawingEvent();
        event.setType(CollaborationEventType.DISABLE);
        sendGenericEvent(event);
    }

    private void sendEraseEvent(LineString line) {
        ShapeContainer container = new ShapeContainer();
        container.setGeom(line);
        CollaborationDrawingEvent eObject = new CollaborationDrawingEvent();
        eObject.setType(CollaborationEventType.ERASE);
        eObject.setContainer(container);
        LoginData data = CollaborationDataManager.getInstance().getLoginData();
        UserId userId = new UserId(data.getUser(), data.getServer());
        eObject.setUserName(userId);
        sendGenericEvent(eObject);
    }

    private void sendDrawEvent(LineString line) {
        ShapeContainer container = new ShapeContainer();
        container.setGeom(line);
        CollaborationDrawingEvent tObject = new CollaborationDrawingEvent();
        tObject.setType(CollaborationEventType.DRAW);
        tObject.setContainer(container);
        LoginData data = CollaborationDataManager.getInstance().getLoginData();
        UserId userId = new UserId(data.getUser(), data.getServer());
        tObject.setUserName(userId);
        sendGenericEvent(tObject);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#reset()
     */
    @Override
    public void reset() {
        super.reset();
        CollaborationDrawingEvent event = new CollaborationDrawingEvent();
        event.setType(CollaborationEventType.CLEAR);
        LoginData data = CollaborationDataManager.getInstance().getLoginData();
        UserId userId = new UserId(data.getUser(), data.getServer());
        event.setUserName(userId);
        sendGenericEvent(event);
    }

    private void sendGenericEvent(CollaborationDrawingEvent event) {
        try {
            SharedDisplaySessionMgr.getSessionContainer(sessionId).getSession()
                    .sendObjectToVenue(event);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.ERROR, "Unable to send event", e);
        }
    }

    public void addCollaborationShape(UserId userName, ShapeContainer container) {
        // if (tempRemoteShape == null){
        tempRemoteShape = target.createWireframeShape(false, getDescriptor());
        // }
        drawTempLinePrimitive(container.getGeom(), tempRemoteShape);
        container.setShape(tempRemoteShape);
        synchronized (collaboratorShapes) {
            collaboratorShapes.put(userName, container);
        }
    }

    public void removeCollaborationShape(Geometry geom, RGB color) {
        // synchronized (collaboratorShapes) {
        // deletedCollaboratorShapes.
        // collaboratorShapes.remove(geom, color);
        // }
    }

    /**
     * @return the allowDraw
     */
    public boolean isAllowDraw() {
        return allowDraw;
    }

    /**
     * @param sessionId
     *            the sessionId to set
     */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        // if (/* is session leader */false) {
        // // synchronized (collaboratorShapes) {
        // // for (IWireframeShape shape : collaboratorShapes.values()) {
        // // shape.dispose();
        // // }
        // // }
        //
        // for (ShapeContainer cont : collaboratorShapes.values()) {
        // cont.getShape().dispose();
        // }
        //
        // for (ShapeContainer cont : deletedCollaboratorShapes.values()) {
        // cont.getShape().dispose();
        // }
        //
        // collaboratorShapes.clear();
        // deletedCollaboratorShapes.clear();
        // }
    }
}
