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

package com.raytheon.edex.plugin.gfe.util;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Port of CartDomain2D
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/06/09     1995        bphillip    Initial port
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class CartDomain2D {

    private boolean isNullDomain;

    private Coordinate origin;

    private Coordinate extent;

    private Orientation orientation;

    public enum Orientation {
        Y_UP_X_RIGHT, Y_DOWN_X_RIGHT, Y_UP_X_LEFT, Y_DOWN_X_LEFT
    };

    public CartDomain2D() {
        this(new Coordinate(0, 0), new Coordinate(0, 0),
                Orientation.Y_UP_X_RIGHT);
    }

    public CartDomain2D(Coordinate origin, Coordinate extent) {
        this(origin, extent, Orientation.Y_UP_X_RIGHT);
    }

    public CartDomain2D(Coordinate origin, Coordinate extent,
            Orientation orientation) {
        this.isNullDomain = false;
        this.origin = origin;
        this.extent = extent;
        this.orientation = orientation;
    }

    public static CartDomain2D createFromCorners(Coordinate lowerLeft,
            Coordinate upperRight) {
        Orientation orientation;

        boolean xDir = upperRight.x >= lowerLeft.x;
        if (upperRight.y >= lowerLeft.y) {
            orientation = xDir ? Orientation.Y_UP_X_RIGHT
                    : Orientation.Y_UP_X_LEFT;
        } else {
            orientation = xDir ? Orientation.Y_DOWN_X_RIGHT
                    : Orientation.Y_DOWN_X_LEFT;
        }

        Coordinate origin = new Coordinate(Math.min(lowerLeft.x, upperRight.x),
                Math.min(lowerLeft.y, upperRight.y));

        return new CartDomain2D(origin, new Coordinate(Math.max(lowerLeft.x,
                upperRight.x) - origin.x, Math.max(lowerLeft.y, upperRight.y)
                - origin.y), orientation);
    }

    @Override
    public boolean equals(Object rhs) {
        CartDomain2D obj = null;
        if (rhs instanceof CartDomain2D) {
            obj = (CartDomain2D) rhs;
        } else {
            return false;
        }

        return this.orientation.equals(obj.orientation)
                && this.isNullDomain == obj.isNullDomain
                && this.origin.equals(obj.origin)
                && this.extent.equals(obj.extent);
    }

    public void setOrigin(Coordinate origin) {
        this.origin = origin;
        this.isNullDomain = false;
    }

    public void setExtent(Coordinate extent) {
        if (extent.x < 0 || extent.y < 0) {
            this.extent = new Coordinate(0, 0);
        } else {
            this.extent = extent;
            this.isNullDomain = false;
        }
    }

    public Coordinate getOrigin() {
        return this.origin;
    }

    public Coordinate getExtent() {
        return this.extent;
    }

    public boolean yUp() {
        return orientation.equals(Orientation.Y_UP_X_LEFT)
                || orientation.equals(Orientation.Y_UP_X_RIGHT);
    }

    public boolean xRight() {
        return orientation.equals(Orientation.Y_DOWN_X_RIGHT)
                || orientation.equals(Orientation.Y_UP_X_RIGHT);
    }

    public Coordinate center() {
        return new Coordinate(origin.x + (extent.x / 2), origin.y
                + (extent.y / 2));
    }

    public Coordinate lowerLeft() {
        return new Coordinate(left(), bottom());
    }

    public Coordinate upperRight() {
        return new Coordinate(right(), top());
    }

    public Coordinate upperLeft() {
        return new Coordinate(left(), top());
    }

    public Coordinate lowerRight() {
        return new Coordinate(right(), bottom());
    }

    public double left() {
        return xRight() ? xMin() : xMax();
    }

    public double right() {
        return xRight() ? xMax() : xMin();
    }

    public double top() {
        return yUp() ? yMax() : yMin();
    }

    public double bottom() {
        return yUp() ? yMin() : yMax();
    }

    public double xMin() {
        return origin.x;
    }

    public double xMax() {
        return origin.x + extent.x;
    }

    public double yMin() {
        return origin.y;
    }

    public double yMax() {
        return origin.y + extent.y;
    }

    public boolean overlaps(CartDomain2D domain) {
        if (!this.orientation.equals(domain.orientation)) {
            return false;
        }

        if (this.isNull() || domain.isNull()) {
            return false;
        }

        double xMax1 = xMax();
        double xMin1 = xMin();
        double yMax1 = yMax();
        double yMin1 = yMin();

        double xMax2 = domain.xMax();
        double xMin2 = domain.xMin();
        double yMax2 = domain.yMax();
        double yMin2 = domain.yMin();

        boolean result = xMax() > domain.xMin() && xMin() < domain.xMax()
                && yMax() > domain.yMin() && yMin() < domain.yMax();

        return (result);
    }

    public boolean encloses(CartDomain2D domain) {
        if (orientation != domain.orientation) {
            return false;
        }
        if (isNull() || domain.isNull()) {
            return false;
        }
        return (xMax() > domain.xMin() && xMin() < domain.xMax()
                && yMax() > domain.yMin() && yMin() < domain.yMax());
    }

    public boolean within(Coordinate coord) {
        if (this.isNull()) {
            return false;
        }
        return (coord.x >= xMin() && coord.x <= xMax() && coord.y >= yMin() && coord.y <= yMax());
    }

    public boolean within(CartDomain2D domain) {
        if (isNull() || domain.isNull()) {
            return false;
        }
        if ((domain.left() >= left()) && (domain.top() <= top())
                && (domain.right() <= right()) && (domain.bottom() >= bottom())) {
            return true;
        }

        return false;
    }

    public void combineWith(CartDomain2D domain) {
        this.operatorOrEquals(domain);
    }

    public void expandToInclude(Coordinate coord) {
        this.operatorOrEquals(coord);
    }

    public void trim(CartDomain2D domain) {
        this.operatorAndEquals(domain);
    }

    public boolean isNull() {
        return isNullDomain;
    }

    public void setNull() {
        this.isNullDomain = true;
        this.origin.x = origin.y = extent.x = extent.y = 0;
    }

    public void operatorOrEquals(CartDomain2D rhs) {
        if (this.isNull()) {
            // TODO:Implement
        }

        if (rhs.isNull()) {
            return;
        }

        // Set extent to the max of the two domains
        extent.x = Math.max(xMax(), rhs.xMax());
        extent.y = Math.max(yMax(), rhs.yMax());

        // Set origin to the min of the two domains
        origin.x = Math.min(xMin(), rhs.xMin());
        origin.y = Math.min(yMin(), rhs.yMin());

        // Subtract the origin out of the extent
        extent.x = extent.x - origin.x;
        extent.y = extent.y - origin.y;
    }

    public void operatorOrEquals(Coordinate rhs) {
        if (isNull()) // if this is null
        {
            origin = rhs;
            isNullDomain = false;
        }

        if (rhs.x < xMin()) {
            extent.x += (origin.x - rhs.x);
            origin.x = rhs.x;
        } else if (rhs.x > xMax()) {
            extent.x = rhs.x - origin.x;
        }

        if (rhs.y < yMin()) {
            extent.y += (origin.y - rhs.y);
            origin.y = rhs.y;
        } else if (rhs.y > yMax()) {
            extent.y = rhs.y - origin.y;
        }
    }

    public void operatorAndEquals(CartDomain2D rhs) {
        // cases with null return
        if (isNull()) {
            return;
        }
        if (rhs.isNull()) {
            setNull();
        }

        if (orientation != rhs.orientation) {
            setNull();
        }

        // Handle the x coordinates.
        extent.x = Math.min(xMax(), rhs.xMax());
        origin.x = Math.max(xMin(), rhs.xMin());
        if (extent.x < origin.x) {
            setNull();
        }

        // Handle the y coordinates.
        extent.y = Math.min(yMax(), rhs.yMax());
        origin.y = Math.max(yMin(), rhs.yMin());
        if (extent.y < origin.y) {
            setNull();
        }

        // Subtract the origin out of the extent
        extent.x = extent.x - origin.x;
        extent.y = extent.y - origin.y;
    }

    @Override
    public String toString() {
        return "CartDomain2D [origin=" + origin + ", extent=" + extent
                + ", orientation=" + orientation + "]";
    }
}
