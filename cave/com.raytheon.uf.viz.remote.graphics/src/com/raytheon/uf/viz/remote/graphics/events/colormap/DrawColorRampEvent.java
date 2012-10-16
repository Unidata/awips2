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

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.DrawableColorMap;
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
    private Integer colorMapId;

    @DynamicSerializeElement
    private Float alpha;

    @DynamicSerializeElement
    private Float brightness;

    @DynamicSerializeElement
    private Float contrast;

    @DynamicSerializeElement
    private Boolean interpolate;

    @DynamicSerializeElement
    private IExtent extent;

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
        if (!equals(colorMapId, diffEvent.colorMapId))
            diffObject.colorMapId = diffEvent.colorMapId;
        if (!equals(alpha, diffEvent.alpha))
            diffObject.alpha = diffEvent.alpha;
        if (!equals(brightness, diffEvent.brightness))
            diffObject.brightness = diffEvent.brightness;
        if (!equals(contrast, diffEvent.contrast))
            diffObject.contrast = diffEvent.contrast;
        if (!equals(interpolate, diffEvent.interpolate))
            diffObject.interpolate = diffEvent.interpolate;
        if (!equals(extent, diffEvent.extent))
            diffObject.extent = diffEvent.extent;
        return diffObject;
    }

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
        if (diffObject.colorMapId != null)
            colorMapId = diffObject.colorMapId;
        if (diffObject.alpha != null)
            alpha = diffObject.alpha;
        if (diffObject.brightness != null)
            brightness = diffObject.brightness;
        if (diffObject.contrast != null)
            contrast = diffObject.contrast;
        if (diffObject.interpolate != null)
            interpolate = diffObject.interpolate;
        if (diffObject.extent != null)
            extent = diffObject.extent;
    }

    public void setDrawableColorMap(DrawableColorMap colorMap, int colorMapId) {
        this.colorMapId = colorMapId;
        this.alpha = colorMap.alpha;
        this.brightness = colorMap.brightness;
        this.contrast = colorMap.contrast;
        this.interpolate = colorMap.interpolate;
        this.extent = colorMap.extent;
    }

    public DrawableColorMap getDrawableColorMap(IColorMap cmap) {
        DrawableColorMap colorMap = new DrawableColorMap(cmap);
        colorMap.alpha = alpha;
        colorMap.brightness = brightness;
        colorMap.contrast = contrast;
        colorMap.interpolate = interpolate;
        colorMap.extent = extent;
        return colorMap;
    }

    /**
     * @return the colorMapId
     */
    public Integer getColorMapId() {
        return colorMapId;
    }

    /**
     * @param colorMapId
     *            the colorMapId to set
     */
    public void setColorMapId(Integer colorMapId) {
        this.colorMapId = colorMapId;
    }

    /**
     * @return the alpha
     */
    public Float getAlpha() {
        return alpha;
    }

    /**
     * @param alpha
     *            the alpha to set
     */
    public void setAlpha(Float alpha) {
        this.alpha = alpha;
    }

    /**
     * @return the brightness
     */
    public Float getBrightness() {
        return brightness;
    }

    /**
     * @param brightness
     *            the brightness to set
     */
    public void setBrightness(Float brightness) {
        this.brightness = brightness;
    }

    /**
     * @return the contrast
     */
    public Float getContrast() {
        return contrast;
    }

    /**
     * @param contrast
     *            the contrast to set
     */
    public void setContrast(Float contrast) {
        this.contrast = contrast;
    }

    /**
     * @return the interpolate
     */
    public Boolean getInterpolate() {
        return interpolate;
    }

    /**
     * @param interpolate
     *            the interpolate to set
     */
    public void setInterpolate(Boolean interpolate) {
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
