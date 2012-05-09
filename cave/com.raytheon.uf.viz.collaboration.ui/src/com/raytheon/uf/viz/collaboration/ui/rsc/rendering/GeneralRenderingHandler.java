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

import org.eclipse.swt.graphics.RGB;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.MouseLocationEvent;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.clipping.ClearClippingPane;
import com.raytheon.uf.viz.remote.graphics.events.clipping.SetupClippingPane;
import com.raytheon.uf.viz.remote.graphics.events.points.DrawPointsEvent;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Handles general rendering events, begin frame and dispose
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

public class GeneralRenderingHandler extends CollaborationRenderingHandler {

    /**
     * General dispose of a renderable object event
     * 
     * @param event
     */
    @Subscribe
    public void disposeRenderable(DisposeObjectEvent event) {
        dataManager.dispose(event.getObjectId());
    }

    @Subscribe
    public void handleDrawPoints(DrawPointsEvent event) {
        try {
            IGraphicsTarget target = getGraphicsTarget();
            target.drawPoints(event.getPointsCollection(), event.getColor(),
                    event.getStyle(), event.getMagnification());
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void handleEndFrame(MouseLocationEvent event) {
        // TODO: Draw the best icon for a cursor, for now will use what
        // VizDisplayPane uses, this is copypasta
        double[] mouseLoc = event.getMouseLocation();
        if (mouseLoc == null) {
            // Don't draw if no location
            return;
        }

        IGraphicsTarget target = getGraphicsTarget();
        PaintProperties paintProps = getPaintProperties();
        target.clearClippingPlane();
        // Calculate scale for image
        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();
        double middleValue = 5 / screenToWorldRatio;
        double outsideValue = 6 / screenToWorldRatio;
        double insideValue = 4 / screenToWorldRatio;

        Coordinate virtualCursor = new Coordinate(mouseLoc[0], mouseLoc[1]);

        try {
            target.drawRect(new PixelExtent(virtualCursor.x - middleValue,
                    virtualCursor.x + middleValue, virtualCursor.y
                            - middleValue, virtualCursor.y + middleValue),
                    new RGB(255, 255, 255), 1.0f, 1.0f);

            target.drawRect(new PixelExtent(virtualCursor.x - outsideValue,
                    virtualCursor.x + outsideValue, virtualCursor.y
                            - outsideValue, virtualCursor.y + outsideValue),
                    new RGB(0, 0, 0), 0.5f, 1.0f);

            target.drawRect(new PixelExtent(virtualCursor.x - insideValue,
                    virtualCursor.x + insideValue, virtualCursor.y
                            - insideValue, virtualCursor.y + insideValue),
                    new RGB(0, 0, 0), 0.5f, 1.0f);

            DrawableCircle circle = new DrawableCircle();
            circle.filled = true;
            circle.radius = 1.0 / screenToWorldRatio;
            circle.basics.color = new RGB(255, 255, 255);
            circle.numberOfPoints = 4;
            circle.setCoordinates(virtualCursor.x, virtualCursor.y);
            target.drawCircle(circle);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        } finally {
            target.setupClippingPlane(paintProps.getClippingPane());
        }
    }

    @Subscribe
    public void clearClippingPane(ClearClippingPane event) {
        getGraphicsTarget().clearClippingPlane();
    }

    @Subscribe
    public void setupClippingPane(SetupClippingPane event) {
        getGraphicsTarget().setupClippingPlane(event.getExtent());
    }
}
