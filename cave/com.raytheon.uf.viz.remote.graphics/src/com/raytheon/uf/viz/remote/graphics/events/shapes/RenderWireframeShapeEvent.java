/**
 * This software was developed and / or modified by Raytheon Company;
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street; Suite 340
 *                         Mail Stop B8
 *                         Omaha; NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.remote.graphics.events.shapes;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event type for rendering a wireframe shape
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9; 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class RenderWireframeShapeEvent extends AbstractDispatchingObjectEvent
        implements IRenderEvent {

    @DynamicSerializeElement
    private RGB color;

    @DynamicSerializeElement
    private float lineWidth;

    @DynamicSerializeElement
    private LineStyle lineStyle;

    @DynamicSerializeElement
    private Integer fontId;

    @DynamicSerializeElement
    private Float alpha;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.remote.graphics.events.IRenderEvent#createDiffObject
     * (com.raytheon.uf.viz.remote.graphics.events.IRenderEvent)
     */
    @Override
    public IRenderEvent createDiffObject(IRenderEvent event) {
        return event;
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
        RenderWireframeShapeEvent event = (RenderWireframeShapeEvent) diffEvent;
        this.setObjectId(event.getObjectId());
        this.alpha = event.alpha;
        this.color = event.color;
        this.fontId = event.fontId;
        this.lineStyle = event.lineStyle;
        this.lineWidth = event.lineWidth;
    }

    /**
     * @return the lineWidth
     */
    public float getLineWidth() {
        return lineWidth;
    }

    /**
     * @param lineWidth
     *            the lineWidth to set
     */
    public void setLineWidth(float lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * @return the lineStyle
     */
    public LineStyle getLineStyle() {
        return lineStyle;
    }

    /**
     * @param lineStyle
     *            the lineStyle to set
     */
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /**
     * @return the fontId
     */
    public Integer getFontId() {
        return fontId;
    }

    /**
     * @param fontId
     *            the fontId to set
     */
    public void setFontId(Integer fontId) {
        this.fontId = fontId;
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
        RenderWireframeShapeEvent other = (RenderWireframeShapeEvent) obj;
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
        if (fontId == null) {
            if (other.fontId != null)
                return false;
        } else if (!fontId.equals(other.fontId))
            return false;
        if (lineStyle != other.lineStyle)
            return false;
        if (Float.floatToIntBits(lineWidth) != Float
                .floatToIntBits(other.lineWidth))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() {
        RenderWireframeShapeEvent newInstance = new RenderWireframeShapeEvent();
        newInstance.applyDiffObject(this);
        return newInstance;
    }

}
