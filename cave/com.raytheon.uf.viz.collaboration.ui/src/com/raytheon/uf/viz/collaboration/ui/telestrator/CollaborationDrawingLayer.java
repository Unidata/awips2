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
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.ColorChangeEvent;
import com.raytheon.uf.viz.collaboration.ui.SessionColorManager;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent.CollaborationEventType;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.uf.viz.drawing.PathDrawingResourceData;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
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

    private Multimap<String, ShapeContainer> collaboratorShapes;

    private Multimap<String, ShapeContainer> deletedCollaboratorShapes;

    private Map<String, RGB> colors;

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
        for (String str : mgr.getSessions().keySet()) {
            mgr.getSession(str).registerEventHandler(this);
            break;
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
        colors = SessionColorManager.getColorManager().getColors();
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
        if (target instanceof DispatchGraphicsTarget) {
            // Ensure we paint to our own target only
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        super.paintInternal(target, paintProps);

        OutlineCapability outline = getCapability(OutlineCapability.class);
        // paint the shapes that come over from others
        synchronized (collaboratorShapes) {
            for (String userName : collaboratorShapes.keySet()) {
                for (ShapeContainer sh : collaboratorShapes.get(userName)) {
                    if (sh != null) {
                        sh.getShape().clearLabels();
                        RGB color = SessionColorManager.getColorManager()
                                .getColors().get(userName);
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
        if (CollaborationDataManager.getInstance().getLoginId()
                .equals(event.getUserName())) {
            this.color = event.getColor();
        }
        colors.put(event.getUserName(), event.getColor());
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
            String userName = event.getUserName();
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
            // TODO check if session leader, otherwise only remove my wireframe
            // shapes
            if (/* if session leader */false) {
                disposeInternal();
            }
            break;
        case ERASE:
            userName = event.getUserName();
            double extentPercentageX = paintProps.getView().getExtent()
                    .getWidth()
                    / (double) paintProps.getCanvasBounds().width;
            double cursorSize = 16;
            double size = extentPercentageX * cursorSize;
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
                        if (finalGeom instanceof MultiLineString) {
                            Geometry lString = (MultiLineString) finalGeom;
                            for (int j = 0; j < lString.getNumGeometries(); j++) {
                                LineString lineString = (LineString) lString
                                        .getGeometryN(j);
                                drawTempLinePrimitive(lineString,
                                        eraseWireframeShape);
                                ShapeContainer shCont = new ShapeContainer();
                                shCont.setGeom(lString);
                                shCont.setShape(eraseWireframeShape);
                                collaboratorShapes.get(userName).add(shCont);
                            }
                        }
                    }
                }
            }
            break;
        }
        issueRefresh();
    }

    /**
     * @param userName
     */
    private void clearSelfShapes(String userName) {
        for (ShapeContainer cont : collaboratorShapes.get(userName)) {
            cont.getShape().dispose();
        }
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
        sendEraseEvent(line);
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
        event.setUserName(CollaborationDataManager.getInstance().getLoginId());
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
        event.setUserName(CollaborationDataManager.getInstance().getLoginId());
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
        eObject.setUserName(CollaborationDataManager.getInstance().getLoginId());
        sendGenericEvent(eObject);
    }

    private void sendDrawEvent(LineString line) {
        ShapeContainer container = new ShapeContainer();
        container.setGeom(line);
        CollaborationDrawingEvent tObject = new CollaborationDrawingEvent();
        tObject.setType(CollaborationEventType.DRAW);
        tObject.setContainer(container);
        tObject.setUserName(CollaborationDataManager.getInstance().getLoginId());
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
        event.setUserName(CollaborationDataManager.getInstance().getLoginId());
        sendGenericEvent(event);
    }

    private void sendGenericEvent(IDisplayEvent event) {
        Map<String, IVenueSession> sessions = CollaborationDataManager
                .getInstance().getSessions();
        for (String str : sessions.keySet()) {
            try {
                ((ISharedDisplaySession) sessions.get(str)).sendEvent(event);
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.ERROR, "Unable to send event", e);
            }
        }
    }

    public void addCollaborationShape(String userName, ShapeContainer container) {
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

    public void addColor(String userName, RGB color) {
        colors.put(userName, color);
    }

    /**
     * @return the allowDraw
     */
    public boolean isAllowDraw() {
        return allowDraw;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        if (/* is session leader */false) {
            // synchronized (collaboratorShapes) {
            // for (IWireframeShape shape : collaboratorShapes.values()) {
            // shape.dispose();
            // }
            // }

            for (ShapeContainer cont : collaboratorShapes.values()) {
                cont.getShape().dispose();
            }

            for (ShapeContainer cont : deletedCollaboratorShapes.values()) {
                cont.getShape().dispose();
            }

            collaboratorShapes.clear();
            deletedCollaboratorShapes.clear();
        }
    }
}
