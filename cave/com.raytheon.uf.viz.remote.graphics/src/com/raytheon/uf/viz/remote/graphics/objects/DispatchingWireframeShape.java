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

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.remote.graphics.Activator;
import com.raytheon.uf.viz.remote.graphics.Dispatcher;
import com.raytheon.uf.viz.remote.graphics.DispatchingObject;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.AllocatePointsEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.SimpleWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.SimpleWireframeShapeEvent.EventAction;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.UpdateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.WireframeShapeData;
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
        DispatchingObject<IWireframeShape> implements IWireframeShape {

    private MathTransform worldToTargetGrid;

    private WireframeShapeData shapeData;

    private boolean dirty = true;

    /**
     * @param targetObject
     * @param dispatcher
     */
    public DispatchingWireframeShape(IWireframeShape targetObject,
            Dispatcher dispatcher, GeneralGridGeometry targetGeometry) {
        super(targetObject, dispatcher);
        this.shapeData = new WireframeShapeData();
        MathTransform worldToCRS = AbstractDescriptor
                .getWorldToCRSTransform(targetGeometry);
        if (worldToCRS != null) {
            try {
                MathTransform crsToGrid = targetGeometry.getGridToCRS()
                        .inverse();
                worldToTargetGrid = new DefaultMathTransformFactory()
                        .createConcatenatedTransform(worldToCRS, crsToGrid);
            } catch (Exception e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        "Error getting transform from base crs to target grid",
                        e);
            }
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#compile()
     */
    public void compile() {
        wrappedObject.compile();
        // flush data
        updateState();
        // Send compile event
        sendSimpleEvent(EventAction.COMPILE);
    }

    /**
     * 
     */
    public void updateState() {
        if (dirty) {
            if (shapeData != null) {
                UpdateWireframeShapeEvent event = RemoteGraphicsEventFactory
                        .createEvent(UpdateWireframeShapeEvent.class, this);
                event.setWireframeData(shapeData);
                shapeData = new WireframeShapeData();
                dispatch(event);
            }
            dirty = false;
        }
    }

    private void sendSimpleEvent(EventAction action) {
        SimpleWireframeShapeEvent event = RemoteGraphicsEventFactory
                .createEvent(SimpleWireframeShapeEvent.class, this);
        event.setAction(action);
        dispatch(event);
    }

    /**
     * @param latLong
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#addLineSegment(com.vividsolutions.jts.geom.Coordinate[])
     */
    public void addLineSegment(Coordinate[] latLong) {
        wrappedObject.addLineSegment(latLong);
        double[][] points = new double[latLong.length][];
        for (int i = 0; i < latLong.length; ++i) {
            if (worldToTargetGrid != null) {
                try {
                    double[] out = new double[2];
                    worldToTargetGrid.transform(new double[] { latLong[i].x,
                            latLong[i].y }, 0, out, 0, 1);
                    points[i] = out;
                } catch (TransformException e) {
                    // Ignore...
                }
            } else {
                points[i] = new double[] { latLong[i].x, latLong[i].y };
            }
        }
        addLineSegment(points);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IShape#isMutable()
     */
    public boolean isMutable() {
        return wrappedObject.isMutable();
    }

    /**
     * @param screenCoordinates
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#addLineSegment(double[][])
     */
    public void addLineSegment(double[][] screenCoordinates) {
        wrappedObject.addLineSegment(screenCoordinates);
        if (shapeData != null) {
            shapeData.addCoordinates(screenCoordinates);
            dirty = true;
        }
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.drawables.IShape#isDrawable()
     */
    public boolean isDrawable() {
        return wrappedObject.isDrawable();
    }

    /**
     * @param label
     * @param screenCoordinate
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#addLabel(java.lang.String,
     *      double[])
     */
    public void addLabel(String label, double[] screenCoordinate) {
        wrappedObject.addLabel(label, screenCoordinate);
        if (shapeData != null) {
            shapeData.addLabel(label, screenCoordinate);
            dirty = true;
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#dispose()
     */
    public void dispose() {
        wrappedObject.dispose();
        // Send dispose event
        dispatch(RemoteGraphicsEventFactory.createEvent(
                DisposeObjectEvent.class, this));
        shapeData = null;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IWireframeShape#clearLabels()
     */
    public void clearLabels() {
        wrappedObject.clearLabels();
        // Send clear labels event
        sendSimpleEvent(EventAction.CLEAR_LABELS);
        if (shapeData != null) {
            shapeData.getLabels().clear();
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

    /**
     * 
     * @see com.raytheon.uf.viz.core.drawables.IShape#reset()
     */
    public void reset() {
        wrappedObject.reset();
        // Send reset event
        sendSimpleEvent(EventAction.RESET);
        shapeData = new WireframeShapeData();
    }

}
