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
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for drawing a circle
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
@DynamicSerialize
public class DrawCircleEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private double[] location;

    @DynamicSerializeElement
    private RGB color;

    @DynamicSerializeElement
    private Double screenRadius;

    @DynamicSerializeElement
    private Double radius;

    @DynamicSerializeElement
    private Boolean filled;

    @DynamicSerializeElement
    private Boolean includeSides;

    @DynamicSerializeElement
    private Integer numberOfPoints;

    @DynamicSerializeElement
    private Float startAzimuth;

    @DynamicSerializeElement
    private Float endAzimuth;

    @DynamicSerializeElement
    private Float lineWidth;

    @DynamicSerializeElement
    private LineStyle lineStyle;

    @DynamicSerializeElement
    private Float alpha;

    @DynamicSerializeElement
    private Boolean xorColors;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsRenderEvent
     * #createDiffObject(com.raytheon.uf.viz.remote
     * .graphics.events.rendering.IRenderEvent)
     */
    @Override
    public DrawCircleEvent createDiffObject(IRenderEvent event) {
        DrawCircleEvent diffEvent = (DrawCircleEvent) event;
        DrawCircleEvent diffObject = new DrawCircleEvent();
        if (Arrays.equals(location, diffEvent.location) == false)
            diffObject.location = diffEvent.location;
        if (color.equals(diffEvent.color) == false)
            diffObject.color = diffEvent.color;
        if (alpha != diffEvent.alpha)
            diffObject.alpha = diffEvent.alpha;
        if (xorColors != diffEvent.xorColors)
            diffObject.xorColors = diffEvent.xorColors;
        if (filled != diffEvent.filled)
            diffObject.filled = diffEvent.filled;
        if (startAzimuth != diffEvent.startAzimuth)
            diffObject.startAzimuth = diffEvent.startAzimuth;
        if (endAzimuth != diffEvent.endAzimuth)
            diffObject.endAzimuth = diffEvent.endAzimuth;
        if (numberOfPoints != diffEvent.numberOfPoints)
            diffObject.numberOfPoints = diffEvent.numberOfPoints;
        if (radius != diffEvent.radius)
            diffObject.radius = diffEvent.radius;
        if (screenRadius != diffEvent.screenRadius)
            diffObject.screenRadius = diffEvent.screenRadius;
        if (lineStyle != diffEvent.lineStyle)
            diffObject.lineStyle = diffEvent.lineStyle;
        if (lineWidth != diffEvent.lineWidth)
            diffObject.lineWidth = diffEvent.lineWidth;
        if (includeSides != diffEvent.includeSides)
            diffObject.includeSides = diffEvent.includeSides;
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
        DrawCircleEvent o = (DrawCircleEvent) diffEvent;
        if (o.location != null)
            location = o.location;
        if (o.color != null)
            color = o.color;
        if (o.alpha != null)
            alpha = o.alpha;
        if (o.xorColors != null)
            xorColors = o.xorColors;
        if (o.filled != null)
            filled = o.filled;
        if (o.startAzimuth != null)
            startAzimuth = o.startAzimuth;
        if (o.endAzimuth != null)
            endAzimuth = o.endAzimuth;
        if (o.numberOfPoints != null)
            numberOfPoints = o.numberOfPoints;
        if (o.lineStyle != null)
            lineStyle = o.lineStyle;
        if (o.lineWidth != null)
            lineWidth = o.lineWidth;
        if (o.includeSides != null) {
            includeSides = o.includeSides;
        }
        if (o.radius != null) {
            radius = o.radius;
            screenRadius = null;
        } else if (o.screenRadius != null) {
            screenRadius = o.screenRadius;
            radius = null;
        }
    }

    public void setDrawableCircle(DrawableCircle circle) {
        this.location = new double[] { circle.basics.x, circle.basics.y,
                circle.basics.z };
        this.color = circle.basics.color;
        this.alpha = circle.basics.alpha;
        this.xorColors = circle.basics.xOrColors;
        this.filled = circle.filled;
        this.startAzimuth = circle.startAzimuth;
        this.endAzimuth = circle.endAzimuth;
        this.numberOfPoints = circle.numberOfPoints;
        this.radius = circle.radius;
        this.screenRadius = circle.screenRadius;
        this.lineStyle = circle.lineStyle;
        this.lineWidth = circle.lineWidth;
        this.includeSides = circle.includeSides;
    }

    public DrawableCircle getDrawableCircle() {
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(location[0], location[1], location[2]);
        circle.basics.color = color;
        circle.basics.alpha = alpha;
        circle.basics.xOrColors = xorColors;
        circle.filled = filled;
        circle.startAzimuth = startAzimuth;
        circle.endAzimuth = endAzimuth;
        circle.numberOfPoints = numberOfPoints;
        circle.radius = radius;
        circle.screenRadius = screenRadius;
        circle.lineStyle = lineStyle;
        circle.lineWidth = lineWidth;
        circle.includeSides = includeSides;
        return circle;
    }

    /**
     * @return the location
     */
    public double[] getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(double[] location) {
        this.location = location;
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
     * @return the screenRadius
     */
    public Double getScreenRadius() {
        return screenRadius;
    }

    /**
     * @param screenRadius
     *            the screenRadius to set
     */
    public void setScreenRadius(Double screenRadius) {
        this.screenRadius = screenRadius;
    }

    /**
     * @return the radius
     */
    public Double getRadius() {
        return radius;
    }

    /**
     * @param radius
     *            the radius to set
     */
    public void setRadius(Double radius) {
        this.radius = radius;
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
     * @return the includeSides
     */
    public Boolean getIncludeSides() {
        return includeSides;
    }

    /**
     * @param includeSides
     *            the includeSides to set
     */
    public void setIncludeSides(Boolean includeSides) {
        this.includeSides = includeSides;
    }

    /**
     * @return the numberOfPoints
     */
    public Integer getNumberOfPoints() {
        return numberOfPoints;
    }

    /**
     * @param numberOfPoints
     *            the numberOfPoints to set
     */
    public void setNumberOfPoints(Integer numberOfPoints) {
        this.numberOfPoints = numberOfPoints;
    }

    /**
     * @return the startAzimuth
     */
    public Float getStartAzimuth() {
        return startAzimuth;
    }

    /**
     * @param startAzimuth
     *            the startAzimuth to set
     */
    public void setStartAzimuth(Float startAzimuth) {
        this.startAzimuth = startAzimuth;
    }

    /**
     * @return the endAzimuth
     */
    public Float getEndAzimuth() {
        return endAzimuth;
    }

    /**
     * @param endAzimuth
     *            the endAzimuth to set
     */
    public void setEndAzimuth(Float endAzimuth) {
        this.endAzimuth = endAzimuth;
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
     * @return the xorColors
     */
    public Boolean getXorColors() {
        return xorColors;
    }

    /**
     * @param xorColors
     *            the xorColors to set
     */
    public void setXorColors(Boolean xorColors) {
        this.xorColors = xorColors;
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
        DrawCircleEvent other = (DrawCircleEvent) obj;
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
        if (endAzimuth == null) {
            if (other.endAzimuth != null)
                return false;
        } else if (!endAzimuth.equals(other.endAzimuth))
            return false;
        if (filled == null) {
            if (other.filled != null)
                return false;
        } else if (!filled.equals(other.filled))
            return false;
        if (includeSides == null) {
            if (other.includeSides != null)
                return false;
        } else if (!includeSides.equals(other.includeSides))
            return false;
        if (lineStyle != other.lineStyle)
            return false;
        if (lineWidth == null) {
            if (other.lineWidth != null)
                return false;
        } else if (!lineWidth.equals(other.lineWidth))
            return false;
        if (!Arrays.equals(location, other.location))
            return false;
        if (numberOfPoints == null) {
            if (other.numberOfPoints != null)
                return false;
        } else if (!numberOfPoints.equals(other.numberOfPoints))
            return false;
        if (radius == null) {
            if (other.radius != null)
                return false;
        } else if (!radius.equals(other.radius))
            return false;
        if (screenRadius == null) {
            if (other.screenRadius != null)
                return false;
        } else if (!screenRadius.equals(other.screenRadius))
            return false;
        if (startAzimuth == null) {
            if (other.startAzimuth != null)
                return false;
        } else if (!startAzimuth.equals(other.startAzimuth))
            return false;
        if (xorColors == null) {
            if (other.xorColors != null)
                return false;
        } else if (!xorColors.equals(other.xorColors))
            return false;
        return true;
    }

}
