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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.remote.graphics.events.points.Point;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * DrawableLine event object
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
public class DrawLineEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private List<Point> points;

    @DynamicSerializeElement
    private LineStyle lineStyle;

    @DynamicSerializeElement
    private Float width;

    @DynamicSerializeElement
    private RGB color;

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
    public IRenderEvent createDiffObject(IRenderEvent event) {
        DrawLineEvent diffEvent = (DrawLineEvent) event;
        DrawLineEvent diffObject = new DrawLineEvent();
        if (!equals(points, diffEvent.points)) {
            List<Point> newPoints = new ArrayList<Point>(
                    diffEvent.points.size());
            int diffSize = diffEvent.points.size();
            int mySize = points.size();
            for (int i = 0; i < diffSize; ++i) {
                Point diffPoint = diffEvent.points.get(i);
                if (i < mySize && points.get(i).equals(diffPoint)) {
                    diffPoint = null;
                } else {
                    System.out.println("NEW POINT!");
                }
                newPoints.add(diffPoint);
            }
            System.out.println();
            diffObject.points = newPoints;
        }
        if (!equals(lineStyle, diffEvent.lineStyle))
            diffObject.lineStyle = diffEvent.lineStyle;
        if (!equals(width, diffEvent.width))
            diffObject.width = diffEvent.width;
        if (!equals(alpha, diffEvent.alpha))
            diffObject.alpha = diffEvent.alpha;
        if (!equals(color, diffEvent.color))
            diffObject.color = diffEvent.color;
        if (!equals(xorColors, diffEvent.xorColors))
            diffObject.xorColors = diffEvent.xorColors;
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
        DrawLineEvent diffObject = (DrawLineEvent) diffEvent;
        if (diffObject.points != null) {
            List<Point> newPoints = new ArrayList<Point>(
                    diffObject.points.size());
            int i = 0;
            for (Point p : diffObject.points) {
                if (p == null) {
                    p = points.get(i);
                }
                newPoints.add(p);
                ++i;
            }
            points = newPoints;
        }
        if (diffObject.lineStyle != null)
            lineStyle = diffObject.lineStyle;
        if (diffObject.width != null)
            width = diffObject.width;
        if (diffObject.alpha != null)
            alpha = diffObject.alpha;
        if (diffObject.color != null)
            color = diffObject.color;
        if (diffObject.xorColors != null)
            xorColors = diffObject.xorColors;
    }

    public void setDrawableLine(DrawableLine line) {
        points = new ArrayList<Point>(line.points.size());
        for (double[] point : line.points) {
            Point p = new Point();
            p.setPoint(point);
            points.add(p);
        }
        lineStyle = line.lineStyle;
        width = line.width;
        alpha = line.basics.alpha;
        color = line.basics.color;
        xorColors = line.basics.xOrColors;
    }

    public DrawableLine getDrawableLine() {
        DrawableLine line = new DrawableLine();
        for (Point p : points) {
            line.points.add(p.getPoint());
        }
        line.lineStyle = lineStyle;
        line.width = width;
        line.basics.alpha = alpha;
        line.basics.color = color;
        line.basics.xOrColors = xorColors;
        return line;
    }

    /**
     * @return the points
     */
    public List<Point> getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<Point> points) {
        this.points = points;
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
     * @return the width
     */
    public Float getWidth() {
        return width;
    }

    /**
     * @param width
     *            the width to set
     */
    public void setWidth(Float width) {
        this.width = width;
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
        DrawLineEvent other = (DrawLineEvent) obj;
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
        if (lineStyle != other.lineStyle)
            return false;
        if (points == null) {
            if (other.points != null)
                return false;
        } else if (!points.equals(other.points))
            return false;
        if (width == null) {
            if (other.width != null)
                return false;
        } else if (!width.equals(other.width))
            return false;
        if (xorColors == null) {
            if (other.xorColors != null)
                return false;
        } else if (!xorColors.equals(other.xorColors))
            return false;
        return true;
    }

}
