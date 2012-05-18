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
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawCircleEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawCirclesEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawLineEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawLinesEvent;
import com.raytheon.uf.viz.remote.graphics.events.drawables.DrawRectEvent;

/**
 * Rendering handler for primitive shapes (lines, rectangles, circles)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PrimitiveRenderingHandler extends CollaborationRenderingHandler {

    @Subscribe
    public void drawCircles(DrawCirclesEvent event) {
        DrawCircleEvent[] events = event.getObjects();
        DrawableCircle[] circles = new DrawableCircle[events.length];
        for (int i = 0; i < events.length; ++i) {
            circles[i] = events[i].getDrawableCircle();
        }
        try {
            getGraphicsTarget().drawCircle(circles);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void drawLines(DrawLinesEvent event) {
        DrawLineEvent[] events = event.getObjects();
        DrawableLine[] lines = new DrawableLine[events.length];
        for (int i = 0; i < events.length; ++i) {
            lines[i] = events[i].getDrawableLine();
        }
        try {
            getGraphicsTarget().drawLine(lines);
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    @Subscribe
    public void drawRect(DrawRectEvent event) {
        IGraphicsTarget target = getGraphicsTarget();
        try {
            if (event.getFilled()) {
                target.drawShadedRect(event.getExtent(), event.getColor(),
                        event.getAlpha(), event.getFillPattern());
            } else {
                target.drawRect(event.getExtent(), event.getColor(),
                        event.getLineWidth(), event.getAlpha());
            }
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

}
