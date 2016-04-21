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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsBulkRenderEvent;
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
public class DrawShadedShapesEvent extends
        AbstractRemoteGraphicsBulkRenderEvent<DrawShadedShapeEvent> {

    @DynamicSerializeElement
    private float alpha;

    @DynamicSerializeElement
    private float brightness;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent
     * #createDiffObject(com.raytheon.uf.viz
     * .remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public DrawShadedShapesEvent createDiffObject(IRenderEvent event) {
        DrawShadedShapesEvent diffEvent = (DrawShadedShapesEvent) event;
        DrawShadedShapesEvent diffObject = (DrawShadedShapesEvent) super
                .createDiffObject(diffEvent);
        diffObject.alpha = diffEvent.alpha;
        diffObject.brightness = diffEvent.brightness;
        return diffObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent
     * #applyDiffObject(com.raytheon.uf.viz.
     * remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        super.applyDiffObject(diffEvent);
        DrawShadedShapesEvent diffObject = (DrawShadedShapesEvent) diffEvent;
        this.alpha = diffObject.alpha;
        this.brightness = diffObject.brightness;
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

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent#getObjectClass()
     */
    @Override
    protected Class<DrawShadedShapeEvent> getObjectClass() {
        return DrawShadedShapeEvent.class;
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
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        DrawShadedShapesEvent other = (DrawShadedShapesEvent) obj;
        if (Float.floatToIntBits(alpha) != Float.floatToIntBits(other.alpha))
            return false;
        if (Float.floatToIntBits(brightness) != Float
                .floatToIntBits(other.brightness))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsBulkRenderEvent#clone()
     */
    @Override
    public Object clone() {
        DrawShadedShapesEvent event = (DrawShadedShapesEvent) super.clone();
        event.alpha = alpha;
        event.brightness = brightness;
        return event;
    }

}
