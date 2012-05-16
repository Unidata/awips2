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
package com.raytheon.uf.viz.remote.graphics.events.shapes;

import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for drawing a group of shaded shapes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DrawShadedShapesEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private float alpha;

    @DynamicSerializeElement
    private float brightness;

    @DynamicSerializeElement
    private DrawShadedShapeEvent[] shapes;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#createDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public DrawShadedShapesEvent createDiffObject(IRenderEvent event) {
        DrawShadedShapesEvent diff = (DrawShadedShapesEvent) event;
        DrawShadedShapesEvent diffEvent = new DrawShadedShapesEvent();
        if (diff.shapes != null) {
            if (shapes != null && diff.shapes.length == shapes.length) {
                diffEvent.shapes = new DrawShadedShapeEvent[diff.shapes.length];
                for (int i = 0; i < shapes.length; ++i) {
                    DrawShadedShapeEvent paintEvent = shapes[i];
                    DrawShadedShapeEvent diffPaintEvent = diff.shapes[i];
                    if (paintEvent.equals(diffPaintEvent) == false) {
                        diffEvent.shapes[i] = paintEvent
                                .createDiffObject(diffPaintEvent);
                    }
                }
            } else {
                diffEvent.shapes = diff.shapes;
            }
        }
        diffEvent.alpha = diff.alpha;
        diffEvent.brightness = diff.brightness;
        return diffEvent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        DrawShadedShapesEvent event = (DrawShadedShapesEvent) diffEvent;
        DrawShadedShapeEvent[] diffImageEvents = event.shapes;
        if (diffImageEvents == null) {
            shapes = null;
        } else if (shapes == null) {
            shapes = event.shapes;
        } else if (shapes.length != diffImageEvents.length) {
            shapes = event.shapes;
        } else {
            for (int i = 0; i < shapes.length; ++i) {
                DrawShadedShapeEvent diffPaintEvent = diffImageEvents[i];
                if (diffPaintEvent != null) {
                    shapes[i].applyDiffObject(diffPaintEvent);
                }
            }
        }
        alpha = event.alpha;
        brightness = event.brightness;
    }

    /**
     * @return the alpha
     */
    public float getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }

    /**
     * @return the brightness
     */
    public float getBrightness() {
        return brightness;
    }

    /**
     * @param brightness
     *            the brightness to set
     */
    public void setBrightness(float brightness) {
        this.brightness = brightness;
    }

    /**
     * @return the shapes
     */
    public DrawShadedShapeEvent[] getShapes() {
        return shapes;
    }

    /**
     * @param shapes
     *            the shapes to set
     */
    public void setShapes(DrawShadedShapeEvent[] shapes) {
        this.shapes = shapes;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DrawShadedShapesEvent other = (DrawShadedShapesEvent) obj;
        if (Float.floatToIntBits(alpha) != Float.floatToIntBits(other.alpha))
            return false;
        if (Float.floatToIntBits(brightness) != Float
                .floatToIntBits(other.brightness))
            return false;
        if (!Arrays.equals(shapes, other.shapes))
            return false;
        return true;
    }

}
