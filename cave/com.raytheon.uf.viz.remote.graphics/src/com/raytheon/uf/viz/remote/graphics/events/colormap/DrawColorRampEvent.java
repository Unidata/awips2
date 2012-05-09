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
package com.raytheon.uf.viz.remote.graphics.events.colormap;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for rending a color map
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DrawColorRampEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private int colorMapId;

    @DynamicSerializeElement
    private float alpha = 1.0f;

    @DynamicSerializeElement
    private float brightness = 1.0f;

    @DynamicSerializeElement
    private float contrast = 1.0f;

    @DynamicSerializeElement
    private boolean interpolate = true;

    @DynamicSerializeElement
    private IExtent extent;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent#
     * applyDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent)
     */
    @Override
    public void applyDiffObject(IRenderEvent diffEvent) {
        DrawColorRampEvent diffObject = (DrawColorRampEvent) diffEvent;
        colorMapId = diffObject.colorMapId;
        alpha = diffObject.alpha;
        brightness = diffObject.brightness;
        contrast = diffObject.contrast;
        interpolate = diffObject.interpolate;
        if (diffObject.extent != null) {
            extent = diffObject.extent;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsRenderEvent
     * #createDiffObject(com.raytheon.uf.viz.remote
     * .graphics.events.rendering.IRenderEvent)
     */
    @Override
    public IRenderEvent createDiffObject(IRenderEvent event) {
        DrawColorRampEvent diffEvent = (DrawColorRampEvent) event;
        DrawColorRampEvent diffObject = new DrawColorRampEvent();
        diffObject.colorMapId = diffEvent.colorMapId;
        diffObject.alpha = diffEvent.alpha;
        diffObject.brightness = diffEvent.brightness;
        diffObject.contrast = diffEvent.contrast;
        diffObject.interpolate = diffEvent.interpolate;
        if (extent.equals(diffEvent.extent) == false) {
            diffObject.extent = diffEvent.extent;
        }
        return diffObject;
    }

    /**
     * @return the colorMapId
     */
    public int getColorMapId() {
        return colorMapId;
    }

    /**
     * @param colorMapId
     *            the colorMapId to set
     */
    public void setColorMapId(int colorMapId) {
        this.colorMapId = colorMapId;
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
     * @return the contrast
     */
    public float getContrast() {
        return contrast;
    }

    /**
     * @param contrast
     *            the contrast to set
     */
    public void setContrast(float contrast) {
        this.contrast = contrast;
    }

    /**
     * @return the interpolate
     */
    public boolean isInterpolate() {
        return interpolate;
    }

    /**
     * @param interpolate
     *            the interpolate to set
     */
    public void setInterpolate(boolean interpolate) {
        this.interpolate = interpolate;
    }

    /**
     * @return the extent
     */
    public IExtent getExtent() {
        return extent;
    }

    /**
     * @param extent
     *            the extent to set
     */
    public void setExtent(IExtent extent) {
        this.extent = extent;
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
        DrawColorRampEvent other = (DrawColorRampEvent) obj;
        if (Float.floatToIntBits(alpha) != Float.floatToIntBits(other.alpha))
            return false;
        if (Float.floatToIntBits(brightness) != Float
                .floatToIntBits(other.brightness))
            return false;
        if (colorMapId != other.colorMapId)
            return false;
        if (Float.floatToIntBits(contrast) != Float
                .floatToIntBits(other.contrast))
            return false;
        if (extent == null) {
            if (other.extent != null)
                return false;
        } else if (!extent.equals(other.extent))
            return false;
        if (interpolate != other.interpolate)
            return false;
        return true;
    }

}
