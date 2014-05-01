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
package com.raytheon.viz.hydrobase.data;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Line Segment Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2009 2772       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LineSegment {
    /* points defining the segment */
    private Coordinate point1;

    private Coordinate point2;

    /* slope and base of the line of which the segment is a part */
    private boolean vertical;

    private double xValue;

    private double slope;

    private double base;

    /* points that define the box that surrounds the segment */
    private Coordinate min;

    private Coordinate max;

    /**
     * @return the point1
     */
    public Coordinate getPoint1() {
        return point1;
    }

    /**
     * @param point1 the point1 to set
     */
    public void setPoint1(Coordinate point1) {
        this.point1 = point1;
    }

    /**
     * @return the point2
     */
    public Coordinate getPoint2() {
        return point2;
    }

    /**
     * @param point2 the point2 to set
     */
    public void setPoint2(Coordinate point2) {
        this.point2 = point2;
    }

    /**
     * @return the vertical
     */
    public boolean isVertical() {
        return vertical;
    }

    /**
     * @param vertical the vertical to set
     */
    public void setVertical(boolean vertical) {
        this.vertical = vertical;
    }

    /**
     * @return the xValue
     */
    public double getXValue() {
        return xValue;
    }

    /**
     * @param value the xValue to set
     */
    public void setXValue(double value) {
        xValue = value;
    }

    /**
     * @return the slope
     */
    public double getSlope() {
        return slope;
    }

    /**
     * @param slope the slope to set
     */
    public void setSlope(double slope) {
        this.slope = slope;
    }

    /**
     * @return the base
     */
    public double getBase() {
        return base;
    }

    /**
     * @param base the base to set
     */
    public void setBase(double base) {
        this.base = base;
    }

    /**
     * @return the min
     */
    public Coordinate getMin() {
        return min;
    }

    /**
     * @param min the min to set
     */
    public void setMin(Coordinate min) {
        this.min = min;
    }

    /**
     * @return the max
     */
    public Coordinate getMax() {
        return max;
    }

    /**
     * @param max the max to set
     */
    public void setMax(Coordinate max) {
        this.max = max;
    }

}
