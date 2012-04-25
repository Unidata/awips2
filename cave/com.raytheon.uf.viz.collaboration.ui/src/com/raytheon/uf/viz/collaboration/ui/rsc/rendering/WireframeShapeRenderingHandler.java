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
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.AllocatePointsEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.CreateWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.RenderWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.SimpleWireframeShapeEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.WireframeShapeDataEvent;
import com.raytheon.uf.viz.remote.graphics.events.wireframe.WireframeShapeDataEvent.Label;

/**
 * Handles render events for wireframe shapes
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

public class WireframeShapeRenderingHandler extends
        CollaborationRenderingHandler {

    @Subscribe
    public void createWireframeShape(CreateWireframeShapeEvent event) {
        IGraphicsTarget target = getTarget();
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
        }
    }

    @Subscribe
    public void handleSimpleWireframeShapeEvent(SimpleWireframeShapeEvent event) {
        IWireframeShape shape = dataManager.getRenderableObject(
                event.getObjectId(), IWireframeShape.class);
        if (shape != null) {
            switch (event.getAction()) {
            case CLEAR_LABELS:
                shape.clearLabels();
                break;
            case COMPILE:
                shape.compile();
                break;
            case RESET:
                shape.reset();
                break;
            }
        }
    }

    @Subscribe
    public void renderWireframeShape(RenderWireframeShapeEvent event)
            throws VizException {
        IGraphicsTarget target = getTarget();
        IWireframeShape shape = dataManager.getRenderableObject(
                event.getObjectId(), IWireframeShape.class);
        if (shape != null) {
            IFont font = null;
            if (event.getFontId() != null) {
                font = dataManager.getRenderableObject(event.getFontId(),
                        IFont.class);
            }
            if (event.getAlpha() == null) {
                target.drawWireframeShape(shape, event.getColor(),
                        event.getLineWidth(), event.getLineStyle(), font);
            } else {
                target.drawWireframeShape(shape, event.getColor(),
                        event.getLineWidth(), event.getLineStyle(), font,
                        event.getAlpha());
            }
        }
    }

    @Subscribe
    public void disposeWireframeShape(IWireframeShape shape) {
        shape.dispose();
    }

}
