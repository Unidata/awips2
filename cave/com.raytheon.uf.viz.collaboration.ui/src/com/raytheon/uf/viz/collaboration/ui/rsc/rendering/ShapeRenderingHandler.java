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
package com.raytheon.uf.viz.collaboration.ui.rsc.rendering;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.shapes.AllocatePointsEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.CreateShadedShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.CreateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.DrawShadedShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.DrawShadedShapesEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.RenderWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.SetShadedShapeFillPattern;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ShadedShapeDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ShadedShapeDataEvent.DataSpace;
import com.raytheon.uf.viz.remote.graphics.events.shapes.ShadedShapeDataEvent.ShadedShapeData;
import com.raytheon.uf.viz.remote.graphics.events.shapes.WireframeShapeDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.WireframeShapeDataEvent.Label;

/**
 * Handles render events for IShapes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ShapeRenderingHandler extends CollaborationRenderingHandler {

    @Subscribe
    public void createWireframeShape(CreateWireframeShapeEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        int shapeId = event.getObjectId();
        IWireframeShape shape = null;
        if (event.getSimplificationLevel() != null) {
            if (event.isSpatialChopFlag() != null) {
                shape = target.createWireframeShape(event.isMutable(),
                        event.getGridGeometry(),
                        event.getSimplificationLevel(),
                        event.isSpatialChopFlag(), event.getIExtent());
            } else {
                shape = target
                        .createWireframeShape(event.isMutable(),
                                event.getGridGeometry(),
                                event.getSimplificationLevel());
            }
        } else {
            shape = target.createWireframeShape(event.isMutable(),
                    event.getGridGeometry());
        }
        dataManager.putRenderableObject(shapeId, shape);
    }

    @Subscribe
    public void createShadedShape(CreateShadedShapeEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        int shapeId = event.getObjectId();
        IShadedShape shape = target.createShadedShape(event.isMutable(),
                event.getTargetGeometry(), event.isTesselate());
        dataManager.putRenderableObject(shapeId, shape);
    }

    @Subscribe
    public void allocatePointsForShape(AllocatePointsEvent event) {
        IWireframeShape shape = dataManager.getRenderableObject(
                event.getObjectId(), IWireframeShape.class);
        if (shape != null) {
            shape.allocate(event.getNumberOfPoints());
        }
    }

    @Subscribe
    public void wireframeShapeDataArrived(WireframeShapeDataEvent event) {
        IWireframeShape shape = dataManager.getRenderableObject(
                event.getObjectId(), IWireframeShape.class);
        if (shape != null) {
            shape.reset();
            for (Label label : event.getLabels()) {
                shape.addLabel(label.getText(), label.getPoint());
            }
            for (double[][] coords : event.getCoordinates()) {
                shape.addLineSegment(coords);
            }
            if (event.isCompile()) {
                shape.compile();
            }
        }
    }

    @Subscribe
    public void shadedShapeDataArrived(ShadedShapeDataEvent event) {
        IShadedShape shape = dataManager.getRenderableObject(
                event.getObjectId(), IShadedShape.class);
        if (shape != null) {
            shape.reset();
            for (ShadedShapeData data : event.getShapeData()) {
                if (data.getDataSpace() == DataSpace.PIXEL) {
                    shape.addPolygonPixelSpace(data.getData(),
                            data.getDataColor());
                } else if (data.getDataSpace() == DataSpace.WORLD) {
                    shape.addPolygon(data.getData(), data.getDataColor());
                }
            }
            if (event.isCompile()) {
                shape.compile();
            }
        }
    }

    @Subscribe
    public void setShadedShapeFillPattern(SetShadedShapeFillPattern event) {
        int shapeId = event.getObjectId();
        IShadedShape shape = dataManager.getRenderableObject(shapeId,
                IShadedShape.class);
        if (shape != null) {
            shape.setFillPattern(event.getFillPattern());
        }
    }

    @Subscribe
    public void renderWireframeShape(RenderWireframeShapeEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        IWireframeShape shape = dataManager.getRenderableObject(
                event.getObjectId(), IWireframeShape.class);
        if (shape != null) {
            IFont font = null;
            if (event.getFontId() != null) {
                font = dataManager.getRenderableObject(event.getFontId(),
                        IFont.class);
            }
            try {
                if (event.getAlpha() == null) {
                    target.drawWireframeShape(shape, event.getColor(),
                            event.getLineWidth(), event.getLineStyle(), font);
                } else {
                    target.drawWireframeShape(shape, event.getColor(),
                            event.getLineWidth(), event.getLineStyle(), font,
                            event.getAlpha());
                }
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    @Subscribe
    public void renderShadedShapes(DrawShadedShapesEvent event) {
        DrawShadedShapeEvent[] shapeEvents = event.getObjects();
        float alpha = event.getAlpha();
        float brightness = event.getBrightness();
        IShadedShape[] shapes = new IShadedShape[shapeEvents.length];
        for (int i = 0; i < shapes.length; ++i) {
            shapes[i] = dataManager.getRenderableObject(
                    shapeEvents[i].getObjectId(), IShadedShape.class);
        }
        try {
            getGraphicsTarget().drawShadedShapes(alpha, brightness, shapes);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void disposeShape(IShape shape) {
        shape.dispose();
    }
}
