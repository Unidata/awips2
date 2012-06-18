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
package com.raytheon.uf.viz.remote.graphics.events.points;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.remote.graphics.events.rendering.AbstractRemoteGraphicsRenderEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.IRenderEvent;

/**
 * Event for specifying the rendering of a collection of points
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DrawPointsEvent extends AbstractRemoteGraphicsRenderEvent {

    @DynamicSerializeElement
    private Set<Point> points = new HashSet<Point>();

    @DynamicSerializeElement
    private Set<Point> removals = null;

    @DynamicSerializeElement
    private PointStyle style;

    @DynamicSerializeElement
    private float magnification;

    @DynamicSerializeElement
    private RGB color;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.remote.graphics.events.rendering.
     * AbstractRemoteGraphicsRenderEvent
     * #createDiffObject(com.raytheon.uf.viz.remote
     * .graphics.events.rendering.IRenderEvent)
     */
    @Override
    public IRenderEvent createDiffObject(IRenderEvent diffEvent) {
        DrawPointsEvent event = (DrawPointsEvent) diffEvent;
        DrawPointsEvent diffObject = new DrawPointsEvent();
        diffObject.color = event.color;
        diffObject.magnification = event.magnification;
        diffObject.style = event.style;

        Set<Point> additions = new HashSet<Point>(event.points);
        additions.removeAll(points);
        Set<Point> removals = new HashSet<Point>(points);
        removals.removeAll(event.points);
        if (additions.size() + removals.size() > event.points.size()) {
            // Just do a full replace
            diffObject.setRemovals(null);
            diffObject.setPoints(event.points);
        } else {
            diffObject.setPoints(additions);
            diffObject.setRemovals(removals);
        }
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
        DrawPointsEvent event = (DrawPointsEvent) diffEvent;
        color = event.color;
        magnification = event.magnification;
        style = event.style;

        synchronized (points) {
            if (event.removals != null) {
                points.removeAll(event.removals);
                points.addAll(event.points);
            } else {
                points = new HashSet<Point>(event.points);
            }
        }
    }

    /**
     * @return the points
     */
    public Set<Point> getPoints() {
        return points;
    }

    public Collection<double[]> getPointsCollection() {
        synchronized (points) {
            List<double[]> pointList = new ArrayList<double[]>(points.size());
            for (Point p : points) {
                pointList.add(p.getPoint());
            }
            return pointList;
        }
    }

    public void addPoint(double[] point) {
        Point p = new Point();
        p.setPoint(point);
        this.points.add(p);
    }

    public void addPoints(Collection<double[]> points) {
        for (double[] point : points) {
            addPoint(point);
        }
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(Set<Point> points) {
        this.points = points;
    }

    /**
     * @return the removals
     */
    public Set<Point> getRemovals() {
        return removals;
    }

    /**
     * @param removals
     *            the removals to set
     */
    public void setRemovals(Set<Point> removals) {
        this.removals = removals;
    }

    /**
     * @return the style
     */
    public PointStyle getStyle() {
        return style;
    }

    /**
     * @param style
     *            the style to set
     */
    public void setStyle(PointStyle style) {
        this.style = style;
    }

    /**
     * @return the magnification
     */
    public float getMagnification() {
        return magnification;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public void setMagnification(float magnification) {
        this.magnification = magnification;
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
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DrawPointsEvent other = (DrawPointsEvent) obj;
        if (color == null) {
            if (other.color != null)
                return false;
        } else if (!color.equals(other.color))
            return false;
        if (Float.floatToIntBits(magnification) != Float
                .floatToIntBits(other.magnification))
            return false;
        if (points == null) {
            if (other.points != null)
                return false;
        } else if (!points.equals(other.points))
            return false;
        if (removals == null) {
            if (other.removals != null)
                return false;
        } else if (!removals.equals(other.removals))
            return false;
        if (style != other.style)
            return false;
        return true;
    }

}
