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
package com.raytheon.uf.viz.collaboration.display.rsc.telestrator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Stack;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.collaboration.display.rsc.telestrator.CollaborationDrawingEvent.CollaborationEventType;
import com.raytheon.uf.viz.drawing.DrawingToolLayer;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Extension of DrawingToolLayer that forwards events for other participants to
 * keep in sync
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationDrawingToolLayer extends DrawingToolLayer {

    private CollaborationDrawingResource resource;

    private List<Coordinate> coordinates = new ArrayList<Coordinate>();

    /**
     * @param targetGeometry
     */
    public CollaborationDrawingToolLayer(GeneralGridGeometry targetGeometry,
            CollaborationDrawingResource resource) {
        super(targetGeometry);
        this.resource = resource;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingToolLayer#setDrawMode(com.raytheon
     * .uf.viz.drawing.DrawingToolLayer.DrawMode)
     */
    @Override
    public void setDrawMode(DrawMode drawMode) {
        super.setDrawMode(drawMode);
        resource.getResourceData().setResourceMode(drawMode);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.drawing.DrawingToolLayer#addCoordinate(com.vividsolutions
     * .jts.geom.Coordinate)
     */
    @Override
    public void addCoordinate(Coordinate coord) {
        super.addCoordinate(coord);
        coordinates.add(coord);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolLayer#doneDrawing()
     */
    @Override
    public void doneDrawing() {
        super.doneDrawing();
        sendCoordinateEvent(CollaborationEventType.DRAW);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolLayer#doneErasing()
     */
    @Override
    public void doneErasing() {
        super.doneErasing();
        sendCoordinateEvent(CollaborationEventType.ERASE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolLayer#undo()
     */
    @Override
    public void undo() {
        super.undo();
        sendSimpleEvent(CollaborationEventType.UNDO);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolLayer#redo()
     */
    @Override
    public void redo() {
        super.redo();
        sendSimpleEvent(CollaborationEventType.REDO);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.DrawingToolLayer#clear()
     */
    @Override
    public void clear() {
        super.clear();
        sendSimpleEvent(CollaborationEventType.CLEAR);
    }

    private void sendCoordinateEvent(CollaborationEventType type) {
        CollaborationDrawingEvent event = new CollaborationDrawingEvent(
                resource.getResourceData().getDisplayId());
        event.setType(type);
        event.setUserName(resource.getMyUser());
        event.setCoordinates(new ArrayList<Coordinate>(coordinates));
        resource.sendEvent(event);
        coordinates.clear();
    }

    private void sendSimpleEvent(CollaborationEventType type) {
        CollaborationDrawingEvent event = new CollaborationDrawingEvent(
                resource.getResourceData().getDisplayId());
        event.setType(type);
        event.setUserName(resource.getMyUser());
        resource.sendEvent(event);
    }

    public Stack<Collection<Geometry>> getUndoStack() {
        Stack<Collection<Geometry>> data = new Stack<Collection<Geometry>>();
        synchronized (currentData) {
            for (StackFrame undoFrame : undoStack) {
                data.push(undoFrame.geometries);
            }
        }
        return data;
    }

    public Stack<Collection<Geometry>> getRedoStack() {
        Stack<Collection<Geometry>> data = new Stack<Collection<Geometry>>();
        synchronized (currentData) {
            for (StackFrame redoFrame : redoStack) {
                data.push(redoFrame.geometries);
            }
        }
        return data;
    }

    public Collection<Geometry> getCurrentData() {
        Collection<Geometry> data = new ArrayList<Geometry>();
        synchronized (currentData) {
            data.addAll(currentData.geometries);
        }
        return data;
    }
}
