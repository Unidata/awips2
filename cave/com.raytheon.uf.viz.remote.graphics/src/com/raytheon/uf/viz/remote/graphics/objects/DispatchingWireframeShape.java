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
package com.raytheon.uf.viz.remote.graphics.objects;

import java.util.ArrayList;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.shapes.AllocatePointsEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.WireframeShapeDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.WireframeShapeDataEvent.Label;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Dispatching wireframe shape object created from graphics shape and forwards
 * key events to remote clients
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingWireframeShape extends
        AbstractDispatchingShape<IWireframeShape> implements IWireframeShape {

    private WireframeShapeDataEvent updateEvent;

    /**
     * @param targetObject
     * @param dispatcher
     */
    public DispatchingWireframeShape(IWireframeShape targetObject,
            Dispatcher dispatcher, GeneralGridGeometry targetGeometry) {
        super(targetObject, dispatcher);
        this.updateEvent = createNewUpdateEvent();
    }

    @Override
    public void flushInternalState() {
        if (updateEvent != null) {
            WireframeShapeDataEvent toSend = updateEvent;
            if (wrappedObject.isMutable()) {
                toSend = createNewUpdateEvent();
                toSend.setPixelCoordinates(new ArrayList<double[][]>(
                        updateEvent.getPixelCoordinates()));
                toSend.setWorldCoordiantes(new ArrayList<Coordinate[]>(
                        updateEvent.getWorldCoordiantes()));
                toSend.setLabels(new ArrayList<Label>(updateEvent.getLabels()));
            } else {
                updateEvent = null;
            }
            dispatch(toSend);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * compile()
     */
    @Override
    public void compile() {
        updateEvent.setCompile(true);
        // super.compile() will flush the state
        super.compile();
        // Event if original shape was mutable, once compiled we should not
        // accept more data
        updateEvent = null;
    }

    /**
     * @param latLong
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#addLineSegment(com.vividsolutions.jts.geom.Coordinate[])
     */
    public void addLineSegment(Coordinate[] latLong) {
        wrappedObject.addLineSegment(latLong);
        if (updateEvent != null) {
            updateEvent.addWorldCoordinates(latLong);
            markDirty();
        }
    }

    /**
     * @param screenCoordinates
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#addLineSegment(double[][])
     */
    public void addLineSegment(double[][] screenCoordinates) {
        wrappedObject.addLineSegment(screenCoordinates);
        if (updateEvent != null) {
            updateEvent.addPixelCoordinates(screenCoordinates);
            markDirty();
        }
    }

    /**
     * @param label
     * @param screenCoordinate
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#addLabel(java.lang.String,
     *      double[])
     */
    public void addLabel(String label, double[] screenCoordinate) {
        wrappedObject.addLabel(label, screenCoordinate);
        if (updateEvent != null) {
            updateEvent.addLabel(label, screenCoordinate);
            markDirty();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * dispose()
     */
    @Override
    public void dispose() {
        super.dispose();
        updateEvent = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.objects.AbstractDispatchingShape#
     * reset()
     */
    @Override
    public void reset() {
        super.reset();
        updateEvent = createNewUpdateEvent();
        markDirty();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#clearLabels()
     */
    public void clearLabels() {
        wrappedObject.clearLabels();
        if (updateEvent != null) {
            updateEvent.getLabels().clear();
            markDirty();
        }
    }

    /**
     * @param points
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#allocate(int)
     */
    public void allocate(int points) {
        wrappedObject.allocate(points);
        // Send allocation event
        AllocatePointsEvent event = RemoteGraphicsEventFactory.createEvent(
                AllocatePointsEvent.class, this);
        event.setNumberOfPoints(points);
        dispatch(event);
    }

    private WireframeShapeDataEvent createNewUpdateEvent() {
        return RemoteGraphicsEventFactory.createEvent(
                WireframeShapeDataEvent.class, this);
    }
}
