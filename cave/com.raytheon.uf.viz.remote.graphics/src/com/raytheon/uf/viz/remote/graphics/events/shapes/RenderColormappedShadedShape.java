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

import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for rendering a colormapped shaded shape
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class RenderColormappedShadedShape extends
        AbstractDispatchingObjectEvent implements IRenderEvent {

    @DynamicSerializeElement
    private Map<Integer, RGB> colorMap;

    @DynamicSerializeElement
    private Float alpha;

    @DynamicSerializeElement
    private Float brightness;

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
        RenderColormappedShadedShape e = (RenderColormappedShadedShape) event;
        RenderColormappedShadedShape diffObject = new RenderColormappedShadedShape();
        diffObject.setObjectId(e.getObjectId());
        if (alpha.equals(e.alpha) == false)
            diffObject.alpha = e.alpha;
        if (brightness.equals(e.brightness) == false)
            diffObject.brightness = e.brightness;
        if (colorMap.equals(e.colorMap) == false)
            diffObject.colorMap = e.colorMap;
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
        RenderColormappedShadedShape diffObject = (RenderColormappedShadedShape) diffEvent;
        setObjectId(diffObject.getObjectId());
        if (diffObject.alpha != null)
            alpha = diffObject.alpha;
        if (diffObject.brightness != null)
            brightness = diffObject.brightness;
        if (diffObject.colorMap != null)
            colorMap = diffObject.colorMap;
    }

    /**
     * @return the colorMap
     */
    public Map<Integer, RGB> getColorMap() {
        return colorMap;
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMap(Map<Integer, RGB> colorMap) {
        this.colorMap = colorMap;
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() {
        RenderColormappedShadedShape newInstance = new RenderColormappedShadedShape();
        newInstance.applyDiffObject(this);
        return newInstance;
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
        RenderColormappedShadedShape other = (RenderColormappedShadedShape) obj;
        if (alpha == null) {
            if (other.alpha != null)
                return false;
        } else if (!alpha.equals(other.alpha))
            return false;
        if (brightness == null) {
            if (other.brightness != null)
                return false;
        } else if (!brightness.equals(other.brightness))
            return false;
        if (colorMap == null) {
            if (other.colorMap != null)
                return false;
        } else if (!colorMap.equals(other.colorMap))
            return false;
        return true;
    }

}
