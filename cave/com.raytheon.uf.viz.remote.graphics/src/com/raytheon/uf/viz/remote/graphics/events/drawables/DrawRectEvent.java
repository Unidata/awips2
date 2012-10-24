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
package com.raytheon.uf.viz.remote.graphics.events.drawables;

import java.util.Arrays;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DrawRectEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private IExtent extent;

    @DynamicSerializeElement
    private RGB color;

    @DynamicSerializeElement
    private Float alpha;

    @DynamicSerializeElement
    private Float lineWidth;

    @DynamicSerializeElement
    private Boolean filled;

    @DynamicSerializeElement
    private byte[] fillPattern;

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
        DrawRectEvent diffEvent = (DrawRectEvent) event;
        DrawRectEvent diffObject = new DrawRectEvent();
        if (!equals(alpha, diffEvent.alpha))
            diffObject.alpha = diffEvent.alpha;
        if (!equals(color, diffEvent.color))
            diffObject.color = diffEvent.color;
        if (!equals(extent, diffEvent.extent))
            diffObject.extent = diffEvent.extent;
        if (!equals(filled, diffEvent.filled))
            diffObject.filled = diffEvent.filled;
        if (!equals(fillPattern, diffEvent.fillPattern))
            diffObject.fillPattern = diffEvent.fillPattern;
        if (!equals(lineWidth, diffEvent.lineWidth))
            diffObject.lineWidth = diffEvent.lineWidth;
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
        DrawRectEvent diffObject = (DrawRectEvent) diffEvent;
        if (diffObject.alpha != null)
            alpha = diffObject.alpha;
        if (diffObject.color != null)
            color = diffObject.color;
        if (diffObject.extent != null)
            extent = diffObject.extent;
        if (diffObject.filled != null)
            filled = diffObject.filled;
        if (diffObject.fillPattern != null)
            fillPattern = diffObject.fillPattern;
        if (diffObject.lineWidth != null)
            lineWidth = diffObject.lineWidth;
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

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
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
     * @return the lineWidth
     */
    public Float getLineWidth() {
        return lineWidth;
    }

    /**
     * @param lineWidth
     *            the lineWidth to set
     */
    public void setLineWidth(Float lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * @return the filled
     */
    public Boolean getFilled() {
        return filled;
    }

    /**
     * @param filled
     *            the filled to set
     */
    public void setFilled(Boolean filled) {
        this.filled = filled;
    }

    /**
     * @return the fillPattern
     */
    public byte[] getFillPattern() {
        return fillPattern;
    }

    /**
     * @param fillPattern
     *            the fillPattern to set
     */
    public void setFillPattern(byte[] fillPattern) {
        this.fillPattern = fillPattern;
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
        DrawRectEvent other = (DrawRectEvent) obj;
        if (alpha == null) {
            if (other.alpha != null)
                return false;
        } else if (!alpha.equals(other.alpha))
            return false;
        if (color == null) {
            if (other.color != null)
                return false;
        } else if (!color.equals(other.color))
            return false;
        if (extent == null) {
            if (other.extent != null)
                return false;
        } else if (!extent.equals(other.extent))
            return false;
        if (!Arrays.equals(fillPattern, other.fillPattern))
            return false;
        if (filled == null) {
            if (other.filled != null)
                return false;
        } else if (!filled.equals(other.filled))
            return false;
        if (lineWidth == null) {
            if (other.lineWidth != null)
                return false;
        } else if (!lineWidth.equals(other.lineWidth))
            return false;
        return true;
    }

}
