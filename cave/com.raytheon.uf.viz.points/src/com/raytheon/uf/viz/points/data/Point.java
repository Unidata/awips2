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

package com.raytheon.uf.viz.points.data;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.points.PointUtilities;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A Point is a user-defined, named, geospatial location as defined by a
 * latitude and longitude.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * October-2010            epolster    Initial Creation.
 * July 16, 1012 #875      rferrel     Converted for use with CAVE
 * 
 * 
 * </pre>
 * 
 * @author epolster
 * @version 1.0
 */

public class Point implements IPointNode, Comparable<IPointNode> {

    private String name;

    private Coordinate location;

    private boolean colorActive;

    private RGB color;

    private boolean isHidden;

    private boolean isMovable;

    private transient String group;

    private PointSize fontSize = PointSize.DEFAULT;

    // copy ctor
    public Point(Point point) {
        this.name = point.name;
        this.location = point.location;
        this.colorActive = point.colorActive;
        this.color = point.color;
        this.fontSize = point.fontSize;
        this.isHidden = point.isHidden;
        this.isMovable = point.isMovable;
        this.group = point.group;
    }

    public Point(String pointName, Coordinate p, boolean colorActive, RGB c,
            boolean hidden, boolean Movable, PointSize ms, String group) {
        this.name = PointUtilities.trimAll(pointName);
        this.location = p;
        this.colorActive = colorActive;
        this.color = c;
        this.isHidden = hidden;
        this.isMovable = Movable;
        this.fontSize = ms;
        this.group = group;
    }

    // constructor must take valid lat/lon coordinates!
    public Point(String pointName, double lat, double lon, boolean hidden,
            boolean movable, boolean colorActive, RGB c, String group) {
        this.name = PointUtilities.trimAll(pointName);
        this.fontSize = PointSize.DEFAULT;
        this.colorActive = colorActive;
        this.color = c;
        this.isMovable = movable;
        this.isHidden = hidden;
        this.location = new Coordinate(lon, lat);
        this.group = group.replace(' ', PointUtilities.DELIM_CHAR);
    }

    // constructor must take valid lat/lon coordinates!
    public Point(String pointName, double lat, double lon, boolean movable,
            boolean colorActive, RGB c, PointSize size, String group) {
        this.name = PointUtilities.trimAll(pointName);
        this.fontSize = size;
        this.colorActive = colorActive;
        this.color = c;
        this.isMovable = movable;
        this.isHidden = false;
        this.location = new Coordinate(lon, lat);
        this.group = group.replace(' ', PointUtilities.DELIM_CHAR);
    }

    /**
     * @return latitude
     */
    public double getLatitude() {
        return location.y;
    }

    /**
     * @param latitude
     */
    public void setLatitude(double latitude) {
        location = new Coordinate(location.x, latitude);
    }

    /**
     * @return longitude
     */
    public double getLongitude() {
        return location.x;
    }

    /**
     * @return coordinate
     */
    public Coordinate getCoordinate() {
        return new Coordinate(location);
    }

    /**
     * @param coordinate
     */
    public void setCoordinate(Coordinate coordinate) {
        location = coordinate;
    }

    /**
     * @param longitude
     */
    public void setLongitude(double longitude) {
        location = new Coordinate(longitude, location.y);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * @param pointName
     */
    public void setName(String pointName) {
        name = pointName;
    }

    /**
     * @param h
     */
    public void setHidden(boolean h) {
        isHidden = h;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#isHidden()
     */
    @Override
    public boolean isHidden() {
        return isHidden;
    }

    /**
     * @return
     */
    public boolean isColorActive() {
        return colorActive;
    }

    /**
     * @param colorActive
     */
    public void setColorActive(boolean colorActive) {
        this.colorActive = colorActive;
    }

    /**
     * @return rgb
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @param c
     */
    public void setColor(RGB c) {
        color = c;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    public Object clone() {
        Point m = new Point(this);
        return m;
    }

    /**
     * @return
     */
    public PointSize getFontSize() {
        return fontSize;
    }

    /**
     * @param fs
     */
    public void setFontSize(PointSize fs) {
        fontSize = fs;
    }

    /**
     * @return
     */
    public boolean isAtMaxSize() {
        return fontSize.isAtMaxSize();
    }

    /**
     * @return
     */
    public boolean isAtMinSize() {
        return fontSize.isAtMinSize();
    }

    /**
     * 
     */
    public void increaseFontSize() {
        fontSize = fontSize.getNextHigher();
    }

    /**
     * 
     */
    public void decreaseFontSize() {
        fontSize = fontSize.getNextLower();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#isMovable()
     */
    @Override
    public boolean isMovable() {
        return isMovable;
    }

    /**
     * @param notAnchored
     */
    public void setMovable(boolean notAnchored) {
        isMovable = notAnchored;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#getGroup()
     */
    @Override
    public String getGroup() {
        return group;
    }

    /**
     * @param group
     */
    public void setGroup(String group) {
        this.group = group;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.points.data.IPointNode#isGroup()
     */
    @Override
    public boolean isGroup() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("Point: \n");
        sb.append("\t Name: ").append(name).append("\n");
        sb.append("\t Location: ").append(location).append("\n");
        sb.append("\t Color Active: ").append(colorActive).append("\n");
        sb.append("\t Color: ")
                .append(color == null ? "DEFAULT" : color.toString())
                .append("\n");
        sb.append("\t isHidden: ").append(Boolean.toString(isHidden))
                .append("\n");
        sb.append("\t isMovable: ").append(Boolean.toString(isMovable))
                .append("\n");
        sb.append("\t fontSize: ").append(fontSize.toString()).append("\n");
        sb.append("\t group: \"").append(group).append("\"\n");
        sb.append("\t isGroup: ").append(isGroup()).append("\n");
        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.points.data.IPointNode#compareTo(com.raytheon.uf.
     * viz.points.data.IPointNode)
     */
    @Override
    public int compareTo(IPointNode o) {
        // Put groups at the top of a list
        if (isGroup()) {
            if (!o.isGroup()) {
                return -1;
            }
        } else if (o.isGroup()) {
            return 1;
        }
        return getName().compareToIgnoreCase(o.getName());
    }
}
