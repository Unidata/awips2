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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.ClearDrawingEvent;
import com.raytheon.uf.viz.collaboration.ui.telestrator.event.CollaborationDrawingEvent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.drawing.DrawingLayer;
import com.raytheon.uf.viz.drawing.PathDrawingResourceData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

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

    private Multimap<RGB, IWireframeShape> collaboratorShapes;

    private List<IWireframeShape> deletedCollaboratorShapes;

    private RGB officialColor = null;

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
            if (editor.equals(mgr.getEditor(str))) {
                mgr.getSession(str).registerEventHandler(this);
                break;
            }
        }
    }

    @Subscribe
    public void handle(IDisplayEvent event) {
        if (event instanceof ClearDrawingEvent) {
            resetTemp();
            disposeInternal();
            issueRefresh();
        } else if (event instanceof CollaborationDrawingEvent) {
            CollaborationDrawingEvent collEvent = (CollaborationDrawingEvent) event;
            addCollaborationShape(collEvent.getGeom(),
                    getCapability(ColorableCapability.class).getColor());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#reset()
     */
    @Override
    public void reset() {
        super.reset();
        ClearDrawingEvent event = new ClearDrawingEvent();
        Map<String, IVenueSession> sessions = CollaborationDataManager
                .getInstance().getSessions();
        for (String str : sessions.keySet()) {
            try {
                ((ISharedDisplaySession) sessions.get(str)).sendEvent(event);
            } catch (CollaborationException e) {
                e.printStackTrace();
            }
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
        super.initInternal(target);
        this.collaboratorShapes = HashMultimap.create();
        this.collaboratorShapes = Multimaps
                .synchronizedMultimap(this.collaboratorShapes);
        this.deletedCollaboratorShapes = new ArrayList<IWireframeShape>();
        TelestratorColorManager colorManager = TelestratorColorManager
                .getColorManager();
        officialColor = colorManager.getColorFromUser(CollaborationDataManager
                .getInstance().getLoginId());
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
        super.paintInternal(target, paintProps);

        OutlineCapability outline = getCapability(OutlineCapability.class);
        // paint the shapes that come over from others
        synchronized (collaboratorShapes) {
            for (RGB color : collaboratorShapes.keySet()) {
                for (IWireframeShape sh : collaboratorShapes.get(color)) {
                    target.drawWireframeShape(sh, color,
                            outline.getOutlineWidth(), outline.getLineStyle());
                }
            }
        }
    }

    public void addCollaborationShape(Geometry geom, RGB color) {
        IWireframeShape shape = target.createWireframeShape(false,
                getDescriptor());
        drawTempLinePrimitive(geom, shape);
        synchronized (collaboratorShapes) {
            collaboratorShapes.put(color, shape);
        }
        issueRefresh();
    }

    public void removeCollaborationShape(Geometry geom, RGB color) {
        synchronized (collaboratorShapes) {
            // deletedCollaboratorShapes.
            // collaboratorShapes.remove(geom, color);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingLayer#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        synchronized (collaboratorShapes) {
            for (IWireframeShape shape : collaboratorShapes.values()) {
                shape.dispose();
            }
        }

        for (IWireframeShape shape : deletedCollaboratorShapes) {
            shape.dispose();
        }

        collaboratorShapes.clear();
        deletedCollaboratorShapes.clear();
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
        sendDrawEvent(line);
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
        // sendDrawEvent(line);
    }

    private void sendDrawEvent(LineString line) {
        Map<String, IVenueSession> sessions = CollaborationDataManager
                .getInstance().getSessions();
        CollaborationDrawingEvent tObject = new CollaborationDrawingEvent(line,
                officialColor);
        // get the color of the user here, before sending it off
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        for (String str : sessions.keySet()) {
            try {
                ((ISharedDisplaySession) sessions.get(str)).sendEvent(tObject);
            } catch (CollaborationException e) {
                e.printStackTrace();
            }
        }
    }
}
